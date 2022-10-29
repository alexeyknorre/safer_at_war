# Script uses tidycensus package to query US Census API
# for American Community Survey 2019 5-year state-level estimates of population 
# by age by sex by racial/ethnic groups at the ZCTA level
# Then, we subset to zip codes within each city (or touching it in LA case)
# For these zip codes, script sums up the population estimates by each age group,
# sex, and race/ethnicity (races: White, Black, and all others together;
# ethnicity: Hispanic/non-Hispanic) within each zip code.

# We pool standard errors in a common way using standard ACS methodology:
# SE_pooled = sqrt(SE_1^2 + ... + SE_N^2)

prepare_population_estimates <- function() {
  message("--- Query and prepare ACS population estimates...")

  # These are used for aggregation by age groups
  acs_age_group_breaks <- c(0, 5, 10, 15, 18, 20, 25, 30, 35, 45, 55, 65, 75, 85)

  age_group_mapping <- data.frame(
    age_group = c(0, 18, 25, 45, 65),
    age_interval_middle = c(9, 22, 35, 55, 80),
    age_interval_labels = c("0-17", "18-24", "25-44", "45-64", "65+"),
    age_group_ny = c("<18", "18-24", "25-44", "45-64", "65+")
  )


  # This queries ACS API to get population estimates by zip codes
  get_acs_population_estimates_by_age <- function(city_name,
                                                  epsg = 4326) {
    message(city_name)

    # Custom rules for ACS queries for ZIP code subset for each city
    if (city_name == "Philadelphia") {
      zip_codes <- c("191")
      county_name <- "Philadelphia"
      state_name <- "Pennsylvania"
    }
    if (city_name == "New York") {
      zip_codes <- c("103", "116", "100", "101", "102", "111", "112", "113", "114", "104")
      state_name <- "NY"
      county_name <- c("New York", "Bronx", "Queens", "Kings", "Richmond")
    }
    if (city_name == "Chicago") {
      zip_codes <- c("606", "608")
      state_name <- "IL"
      county_name <- c("Cook")
    }
    # LA zip codes do not perfectly coincide with census tracts,
    # which is why we get more total city population
    if (city_name == "Los Angeles") {
      state_name <- "CA"
      county_name <- c("Los Angeles")

      la_shape <- read_sf("data/los_angeles_city_boundary/city_boundary") %>%
        st_transform(epsg)
    }

    # This loads variables from ACS codebook
    acs_vars <- load_variables(2019, "acs5", cache = TRUE)

    # This subsets ACS variables which contain total population by each sex,
    # race, and age group, for API query. Basically, this is a dataframe
    # with variable description of ACS counts
    
    acs_vars_selection <- acs_vars[grepl("B01001_|B01001B_|B01001I_|B01001A_", acs_vars$name) &
                                     grepl("Male:!!|Female:!!", acs_vars$label), ]
    
    # The hardest part about ACS is that we cannot extract the population
    # of non-Hispanic Blacks. Therefore, look separately at four groups
    # that overlap: racially White, racially Black, racially all others,
    # ethnically Hispanic. A person can be both Black and Hispanic. 
    # Thus, summing up Whites, Blacks, and Others gives the total population.
    # Hispanic is a separate population category. It should be okay,
    # because we are mainly interested in shooting shares for each group (Tab.2)
    
    # ACS categories coded in var B01001:
    # A White Alone
    # B Black Alone
    # C American Indian and Alaska Native Alone
    # D Asian Alone
    # E Pacific Islanders Alone
    # F Some other race Alone
    # G Two or more races
    # H Non-Hispanic White
    # I Hispanic
    
    acs_vars_selection$sex <- ifelse(grepl("Female", acs_vars_selection$label), "Female", "Male")
    
    acs_vars_selection <- acs_vars_selection %>% 
      mutate(race = substr(name,7,7),
             race = case_when(race == '_' ~ "Total",
                              race == 'B' ~ "Black",
                              race == 'A' ~ "White",
                              race == 'I' ~ "Hispanic"))
    
    acs_vars_selection$label <- gsub("Under", "0", acs_vars_selection$label)
    acs_vars_selection$age_group <- stri_extract_first_regex(acs_vars_selection$label, "[0-9]+")

    # API query
    acs_raw <- suppressMessages(
      get_acs(
        state = state_name,
        year = 2019,
        geography = "zcta",
        variables = acs_vars_selection$name,
        geometry = TRUE,
        cache_table = T,
        output = "wide"
      )
    )

    # Get only those ZCTAs from LA county which intersect with LA city boundary
    if (city_name == "Los Angeles") {
      acs_raw <- acs_raw %>%
        st_transform(epsg)
      st_agr(acs_raw) <- "constant"
      st_agr(la_shape) <- "constant"

      acs_raw <- st_intersection(acs_raw, la_shape)
      acs_raw$OBJECTID <- NULL
      acs_raw$CITY <- NULL
    }

    # Otherwise, subset county ZCTAs using a vector of relevant ZIP codes
    if (city_name != "Los Angeles") {
      acs_raw <- acs_raw %>%
        filter(substr(GEOID, 1, 3) %in% zip_codes)
    }


    acs <- acs_raw
    # Save ZCTA geometry
    acs_geometry <- acs %>%
      mutate(geo_label = NAME) %>%
      select(geo_label)

    ## Merge ACS population data with just created dataframe with
    ## variable descriptions:
    
    # Re-arrange data with population estimates and MOEs
    acs_long <- pivot_longer(acs, cols = starts_with("B"), values_to = "population") %>% 
      mutate(type = substr(name,nchar(name), nchar(name)),
             name = paste0(substr(name,1, nchar(name)-1),"E"))
      
    # Technically wide now, with pop and pop_se having their own rightful place
    acs_wide <- pivot_wider(acs_long, names_from = "type", values_from = "population") %>% 
      mutate(population = E,
                population_moe = M) %>% 
      select(-c(E,M))

    # Prepare for join
    acs_vars_selection_prep <- acs_vars_selection %>%
      mutate(name = paste0(name, "E"))

    # Add nice labels for categories
    acs_wide <- left_join(acs_wide, acs_vars_selection_prep, by = "name") %>%
      rename(geo_label = NAME) %>%
      mutate(population_se = population_moe/1.645) %>% 
      select(geo_label, sex, race, age_group, population, population_se)
    
    # Aggregate and sum by age groups
    acs_wide <- acs_wide %>%
      st_drop_geometry() %>%
      mutate(age_group = cut(as.numeric(age_group),
        breaks = c(acs_age_group_breaks, 120),
        labels = c(acs_age_group_breaks),
        right = F
      )) %>%
      group_by(geo_label, sex, age_group, race) %>%
      summarise(population_total = sum(population),
                population_total_se = sqrt(sum((population_se)^2))) %>% 
      ungroup()
    
    # Now a tricky part. Calculate population count of other races 
    # by total - (whites + blacks):
    acs_totals <- acs_wide %>% filter(race == "Total")
    
    # Calculating (whites + blacks):
    acs_races <- acs_wide %>% 
      filter(race %in% c("White","Black")) %>% 
      group_by(geo_label, age_group, sex) %>% 
      summarise(population_races = sum(population_total),
                population_races_se = sqrt(sum((population_total_se)^2))) %>% 
      select(geo_label, age_group, sex, population_races, population_races_se)
    
    # Setting up total - (whites + blacks):
    acs_other <- left_join(acs_totals, acs_races, by = c(
        "geo_label",
        "age_group",
        "sex"
      )) %>%
        mutate(population_total = population_total - population_races,
               population_total_se = sqrt(population_total_se^2 + population_races_se^2)) %>%
        select(geo_label, sex, age_group, population_total, population_total_se) %>%
        mutate(race = "Other") %>% 
      ungroup()
      
    # Joining it all together:
    acs_race_ethnicity <- rbind(acs_wide %>% filter(race != "Total"),
                                acs_other) %>% 
      arrange(geo_label, sex, age_group, race) %>% 
      mutate(population = population_total,
                population_se = population_total_se) %>% 
      select(geo_label, sex, age_group, race, population, population_se)
    
    # Let's merge with ZCTA geometry back
    acs_population <- merge(as.data.frame(acs_race_ethnicity),
      acs_geometry,
      all = T
    ) %>%
      st_as_sf() %>%
      st_transform(epsg)

    saveRDS(acs_population, paste0("data/zcta_acs/", city_name, "_acs.rds"))
  }

  city_names <- c(
    "Philadelphia",
    "Chicago",
    "New York",
    "Los Angeles"
  )

  # Loop over cities
  blank_list <- lapply(city_names, get_acs_population_estimates_by_age)
}

prepare_population_estimates()
