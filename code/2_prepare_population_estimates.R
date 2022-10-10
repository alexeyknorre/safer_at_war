# Script uses tidycensus package to query US Census API
# for ACS 2019 state-level estimates of population.
# Then, it leaves only zip codes within each city (or touching it in LA case)
# For these zip codes, script count the population estimates by each age group,
# sex, and race (white/non-white) within each zip code.

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

    # Custom rules for ACS queries for each city
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
    acs_vars_selection <- acs_vars[grepl("B01001_|B01001H_", acs_vars$name) &
      grepl("Male:!!|Female:!!", acs_vars$label), ]
    acs_vars_selection$sex <- ifelse(grepl("Female", acs_vars_selection$label), "Female", "Male")
    acs_vars_selection$race <- ifelse(grepl("WHITE", acs_vars_selection$concept), "White", "Total")
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

    # Remove margin of error
    acs <- acs_raw[, grep(".*(?<!M)$", names(acs_raw), perl = TRUE)]

    # Save ZCTA geometry
    acs_geometry <- acs %>%
      mutate(geo_label = NAME) %>%
      select(geo_label)

    ## Merge ACS population data with just created dataframe with
    ## variable descriptions:

    acs_long <- pivot_longer(acs, cols = starts_with("B"), values_to = "population")

    acs_vars_selection_prep <- acs_vars_selection %>%
      mutate(name = paste0(name, "E"))


    acs_wide <- left_join(acs_long, acs_vars_selection_prep, by = "name") %>%
      rename(geo_label = NAME) %>%
      select(geo_label, sex, race, age_group, population)

    ## We now have a dataframe with populations of
    ## each subgroup: totals and whites. Let's substract whites from totals
    ## to get non-whites/POC.

    acs_totals <- acs_wide %>%
      st_drop_geometry() %>%
      filter(race == "Total") %>%
      mutate(age_group = cut(as.numeric(age_group),
        breaks = c(acs_age_group_breaks, 120),
        labels = c(acs_age_group_breaks),
        right = F
      )) %>%
      group_by(geo_label, sex, age_group) %>%
      summarise(population_total = sum(population))

    acs_whites <- acs_wide %>%
      st_drop_geometry() %>%
      filter(race == "White") %>%
      mutate(population_White = population) %>%
      select(-one_of(c("race", "population")))

    # This gives population by age groups, sex, and race (white/non-white)
    acs_race_populations <- left_join(acs_totals, acs_whites, by = c(
      "geo_label",
      "age_group",
      "sex"
    )) %>%
      mutate(population_POC = population_total - population_White) %>%
      select(-one_of(c("population_total"))) %>%
      pivot_longer(
        cols = population_White:population_POC,
        names_to = c("population", "race"),
        names_sep = "_"
      ) %>%
      mutate(population = value) %>%
      select(-one_of(c("value"))) %>%
      ungroup()

    # Let's merge with ZCTA geometry back
    acs_population <- merge(as.data.frame(acs_race_populations),
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
