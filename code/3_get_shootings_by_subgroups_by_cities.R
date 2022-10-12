# Script aggregates shootings over ZCTAs subpopulations by
# location, age group, sex, and race.

calculate_shooting_counts <- function(city_name,
                                      years = c(2020, 2021),
                                      epsg = 4326) {
  message(city_name)

  acs_age_group_breaks <- c(0, 5, 10, 15, 18, 20, 25, 30, 35, 45, 55, 65, 75, 85)

  age_group_mapping <- data.frame(
    age_group = c(0, 18, 25, 45, 65),
    age_interval_middle = c(9, 22, 35, 55, 80),
    age_interval_labels = c("0-17", "18-24", "25-44", "45-64", "65+"),
    age_group_ny = c("<18", "18-24", "25-44", "45-64", "65+")
  )

  # Open previously created population estimates
  acs_zcta <- readRDS(paste0(
    "data/zcta_acs/", city_name,
    "_acs.rds"
  ))

  # Open previously created shooting victims
  shootings <- fread(file.path(
    "data",
    "shootings_csv",
    paste0(city_name, ".csv")
  ))
  # ... and use it as a sf-object
  shootings <- shootings %>%
    mutate(age_group = as.character(age_group)) %>%
    st_as_sf(.,
      coords = c("lng", "lat"),
      crs = 4326
    ) %>%
    st_transform(epsg)

  ### Aggregation stage

  # Leave unique ZCTA geometries
  acs_zcta_geometry <- acs_zcta[!duplicated(acs_zcta$geo_label), ] %>%
    select(geo_label)

  # Chicago and NYC have a different age group system in shootings data.
  # Fortunately, ACS age group are fine enough to accomodate for this.
  # Thus, we match ACS age groups 20-24 and 25-29 with Chicago shootings
  # age group 20-29. We are doing the same thing for NYC (18-24).

  if (city_name == "Chicago") {
    city_age_group_breaks <- c(0, 20, 30, 40, 50, 60, 70, 80)
    city_age_group_labels <- c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

    # Aggregate population counts by new age groups
    acs_zcta <- acs_zcta %>%
      mutate(age_group = cut(as.numeric(age_group),
        breaks = c(city_age_group_breaks, 120),
        labels = c(city_age_group_labels),
        right = F
      )) %>%
      mutate(age_group = as.character(age_group)) %>%
      group_by(geo_label, sex, age_group, race) %>%
      summarise(population = sum(population),
                population_se = sqrt(sum(population_se^2))) %>%
      ungroup()
  }

  if (city_name == "New York") {
    city_age_group_breaks <- c(0, 18, 25, 45, 65)
    city_age_group_labels <- c("0-17", "18-24", "25-44", "45-64", "65+")

    # Aggregate population counts by new age groups
    acs_zcta <- acs_zcta %>%
      mutate(age_group = cut(as.numeric(age_group),
        breaks = c(city_age_group_breaks, 120),
        labels = c(city_age_group_labels),
        right = F
      )) %>%
      mutate(age_group = as.character(age_group)) %>%
      group_by(geo_label, sex, age_group, race) %>%
      summarise(population = sum(population),
                population_se = sqrt(sum(population_se^2))) %>%
      ungroup()
  }

  st_agr(acs_zcta_geometry) <- "constant"
  st_agr(shootings) <- "constant"

  # Project shootings onto ZCTAs
  shootings_projected <- st_intersection(acs_zcta_geometry, shootings)
  # Aggregate shooting counts over ZCTAs, sex, race, age groups
  shootings_aggregated <- shootings_projected %>%
    st_drop_geometry() %>%
    group_by(geo_label, sex, race, age_group) %>%
    summarize(
      lethal_shootings_count = sum(shooting_type == "lethal"),
      nonlethal_shootings_count = sum(shooting_type == "nonlethal")
    )

  # Merge shooting counts with population totals
  shooting_counts <- left_join(acs_zcta, shootings_aggregated,
    by = c("geo_label", "age_group", "sex", "race")
  ) %>%
    mutate(
      lethal_shootings_count = ifelse(is.na(lethal_shootings_count),
        0,
        lethal_shootings_count
      ),
      nonlethal_shootings_count = ifelse(is.na(nonlethal_shootings_count),
        0,
        nonlethal_shootings_count
      )
    )
  shooting_counts$city <- city_name

  st_agr(shooting_counts) <- "constant"

  shooting_counts <- shooting_counts %>%
    st_cast("MULTIPOLYGON") %>%
    select(
      city, geo_label, age_group, sex, race, population, population_se,
      lethal_shootings_count, nonlethal_shootings_count, geometry
    )

  return(shooting_counts)
}


loop_over_cities <- function() {
  message("--- Calculating shooting counts by ZCTA population subgroups...")
  city_names <- c(
    "Philadelphia",
    "Chicago",
    "New York",
    "Los Angeles"
  )

  cities <- lapply(city_names, calculate_shooting_counts)

  cities <- rbindlist(cities, use.names = T)

  saveRDS(cities, "data/shootings_counts.rds")
}

loop_over_cities()
