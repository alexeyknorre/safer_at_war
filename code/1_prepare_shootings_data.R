### Script downloads and prepares shootings data
### For each city, it subsets the data to years 2020 and 2021,
### recodes age, race, and sex, and removes wrongly geocoded incidents

### We treat race (White, Black, and others) and Hispanic ethnicity as a single variable
### due to ACS limitation in getting separate population counts for these subgroups
### In shooting data, we treat Hispanic identity of victims as the main one


prepare_shootings_data <- function(years = c(2020, 2021)) {
  message("--- Download and prepare shootings data...")
  
  ### Download datasets
  ## Philadelphia
  # Info page: https://www.opendataphilly.org/dataset/shooting-victims
  philadelphia_shootings_url <- "https://phl.carto.com/api/v2/sql?q=SELECT+*,+ST_Y(the_geom)+AS+lat,+ST_X(the_geom)+AS+lng+FROM+shootings&filename=shootings&format=csv&skipfields=cartodb_id"
  if (!file.exists("data/raw_shootings/philadelphia_shootings.csv")){
    download.file(philadelphia_shootings_url, "data/raw_shootings/philadelphia_shootings.csv")
  }
  
  
  ## Chicago
  # Info page: https://data.cityofchicago.org/Public-Safety/Violence-Reduction-Victims-of-Homicides-and-Non-Fa/gumc-mgzr
  chicago_shootings_url <- "https://data.cityofchicago.org/api/views/gumc-mgzr/rows.csv?accessType=DOWNLOAD"
  
  if (!file.exists("data/raw_shootings/chicago_shootings.csv")){
    download.file(chicago_shootings_url, "data/raw_shootings/chicago_shootings.csv")
  }
  
  ## NYC
  # Info page: https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8
  nyc_shootings_url <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"
  if (!file.exists("data/raw_shootings/nyc_shootings.csv")){
    download.file(nyc_shootings_url, "data/raw_shootings/nyc_shootings.csv")
  }
  
  ## LA
  # Info page: https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8
  # NB: crime data, 126 MB
  la_crime_url <- "https://data.lacity.org/api/views/2nrs-mtv8/rows.csv?accessType=DOWNLOAD"
  if (!file.exists("data/raw_shootings/la_crime.csv")){
    download.file(la_crime_url, "data/raw_shootings/la_crime.csv")
    }
  
  # Starts of age intervals for merge with ACS population data
  acs_age_group_breaks <- c(0, 5, 10, 15, 18, 20, 25, 30, 35, 45, 55, 65, 75, 85)

  ### Philadelphia
  city_crime <- fread("data/raw_shootings/philadelphia_shootings.csv")
  city_crime$datetime <- ymd_hms(paste0(city_crime$date_, " ", city_crime$time),
    quiet = T
  )
  city_crime$year <- year(city_crime$datetime)
  city_crime <- city_crime[
    year %in% years &
      !is.na(age),
    c("fatal", "race","latino", "sex", "age", "lat", "lng")
  ]
  city_crime <- city_crime %>%
    mutate(
      race = recode(race,
        "B" = "Black",
        "A" = "Other",
        "W" = "White",
        "U" = "Other"
      ),
      race = ifelse(latino == 1, "Hispanic", race),
      sex = recode(sex,
        F = "Female",
        M = "Male"
      )
    ) %>%
    mutate(age_group = cut(as.numeric(age),
      breaks = c(acs_age_group_breaks, 120),
      labels = c(acs_age_group_breaks),
      right = F
    )) %>%
    mutate(shooting_type = ifelse(fatal == 1, "lethal", "nonlethal")) %>%
    # drop wrongly geocoded incidents
    filter(substr(lat, 1, 2) != 28) %>%
    select(-one_of(c("age", "fatal"))) %>%
    select(shooting_type, age_group, race, sex, lat, lng) %>%
    na.omit()

  fwrite(city_crime, "data/shootings_csv/Philadelphia.csv")

  ### New York City
  city_crime <- fread("data/raw_shootings/nyc_shootings.csv")

  city_crime$datetime <- mdy_hms(paste(city_crime$OCCUR_DATE, city_crime$OCCUR_TIME))

  city_crime <- city_crime %>%
    filter(year(datetime) %in% years) %>%
    select(datetime, VIC_AGE_GROUP, VIC_SEX, VIC_RACE, Longitude, Latitude, STATISTICAL_MURDER_FLAG)

  # Rename
  names(city_crime) <- c("datetime", "age", "sex", "race", "lng", "lat", "lethal")

  city_crime <- city_crime %>%
    mutate(
      race = recode(race,
                    "WHITE" = "White",
                    "BLACK" = "Black",
                    "BLACK HISPANIC" = "Hispanic",
                    "WHITE HISPANIC" = "Hispanic",
                    .default = "Other"),
      sex = recode(sex, F = "Female", M = "Male"),
      age_group = recode(age,
        `<18` = "0-17"
      )
    ) %>%
    mutate(shooting_type = ifelse(lethal == T, "lethal", "nonlethal")) %>%
    select(shooting_type, age_group, race, sex, lat, lng) %>%
    na.omit()

  fwrite(city_crime, "data/shootings_csv/New York.csv")

  ### Chicago
  city_crime <- fread("data/raw_shootings/chicago_shootings.csv")

  # Date
  city_crime$date <- parse_date_time(city_crime$DATE, "m/d/y IMS p")
  city_crime$year <- year(city_crime$date)

  # Subset years
  city_crime <- city_crime[year %in% years]

  # Subset city_crime resulted in injury
  city_crime <- city_crime[GUNSHOT_INJURY_I == "YES"]

  city_crime <- city_crime %>%
    mutate(shooting_type = recode(INCIDENT_PRIMARY,
      "HOMICIDE" = "lethal", .default = "nonlethal"
    )) %>%
    mutate(age_group = ifelse(AGE == "UNKNOWN", NA, AGE)) %>%
    mutate(race = recode(RACE,
      "WHI" = "White",
      "BLK" = "Black",
      "WBH" = "Hispanic",
      "WWH" = "Hispanic",
      .default = "Other")) %>%
    mutate(
      sex = SEX,
      sex = recode(sex,
        "F" = "Female",
        "M" = "Male", .default = "Male"
      )
    ) %>%
    select(shooting_type, age_group, race, sex, LATITUDE, LONGITUDE) %>%
    na.omit()

  # Rename
  names(city_crime) <- c("shooting_type", "age_group", "race", "sex", "lat", "lng")

  fwrite(city_crime, "data/shootings_csv/Chicago.csv")

  ### Los Angeles
  city_crime <- fread("data/raw_shootings/la_crime.csv")

  # Parse date and time
  city_crime$datetime <- parse_date_time(
    paste(
      substring(city_crime$`DATE OCC`, 0, 11),
      str_pad(city_crime$`TIME OCC`, 4, pad = "0")
    ),
    "%m/%d/%Y %H%M"
  )

  city_crime$year <- year(city_crime$datetime)

  # Subset years
  city_crime <- city_crime[year %in% years, ]

  # MO Code == 1430 means victims shot
  city_crime <- city_crime[grepl("0430", Mocodes, fixed = T)]

  # Generate variables for data quality analysis
  city_crime$shooting_type <- dplyr::recode(city_crime$`Crm Cd Desc`,
    "CRIMINAL HOMICIDE" = "lethal",
    "MANSLAUGHTER, NEGLIGENT" = "lethal",
    .default = "nonlethal"
  )
  
  city_crime$race <- dplyr::recode(city_crime$`Vict Descent`,
                                   "W" = "White",
                                   "B" = "Black",
                                   "H" = "Hispanic",
                                   .default = "Other")

  city_crime <- city_crime %>%
    mutate(age = as.numeric(`Vict Age`)) %>%
    # drop weird ages
    mutate(age = ifelse(age > 90 | age < 1, NA, age)) %>%
    mutate(age_group = cut(as.numeric(age),
      breaks = c(acs_age_group_breaks, 120),
      labels = c(acs_age_group_breaks),
      right = F
    )) %>%
    mutate(
      sex = `Vict Sex`,
      sex = recode(sex,
        "F" = "Female",
        "M" = "Male",
        .default = "Male"
      )
    ) %>%
    select(shooting_type, age_group, race, sex, LAT, LON) %>%
    filter(LAT > 0) %>%
    na.omit()

  names(city_crime) <- c("shooting_type", "age_group", "race", "sex", "lat", "lng")
  fwrite(city_crime, "data/shootings_csv/Los Angeles.csv")
}

prepare_shootings_data()
