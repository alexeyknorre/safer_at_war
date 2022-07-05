### Script prepares shootings data and stores it in a unified CSV format
### For each city, it subsets the data to years 2020 and 2021,
### recodes age, race, and sex, and removes wrongly geocoded incidents

prepare_shootings_data <- function(years = c(2020,2021)){
  
  message("--- Prepare shootings data...")
  
  # Starts of age intervals for merge with ACS population data
  acs_age_group_breaks <- c(0,5,10,15,18,20,25,30,35,45,55,65,75,85)
  
  ### Philadelphia
  city_crime <- fread("data/raw_shootings/Philadelphia/shootings.csv")
  city_crime$datetime <- ymd_hms(paste0(city_crime$date_," ", city_crime$time),
                                 quiet = T)
  city_crime$year <- year(city_crime$datetime)
  city_crime <- city_crime[year %in% years &
                             !is.na(age),
                           c("fatal","race","sex","age","lat","lng")]
  city_crime <- city_crime %>%
    mutate(race = recode(race,
                         "B"="POC",
                         "A" = "POC",
                         "W" = "White"),
           sex = recode(sex,
                        F="Female",
                        M="Male")) %>% 
    mutate(age_group = cut(as.numeric(age),
                           breaks = c(acs_age_group_breaks,120),
                           labels = c(acs_age_group_breaks),
                           right = F)) %>% 
    mutate(shooting_type = ifelse(fatal == 1,"lethal","nonlethal")) %>% 
    # drop wrongly geocoded incidents
    filter(substr(lat,1,2) != 28) %>% 
    select(-one_of(c("age","fatal"))) %>% 
    select(shooting_type,age_group, race, sex, lat, lng) %>% 
    na.omit()
  
  fwrite(city_crime, "data/shootings_csv/Philadelphia.csv")
  
  ### New York City
  city_crime <- fread("data/raw_shootings/New York/NYPD_Shooting_Incident_Data__Historic_.csv")
  
  city_crime$datetime <- mdy_hms(paste(city_crime$OCCUR_DATE,city_crime$OCCUR_TIME))
  
  city_crime <- city_crime %>%
    filter(year(datetime) %in% years) %>% 
    select(datetime,VIC_AGE_GROUP, VIC_SEX, VIC_RACE, Longitude,Latitude, STATISTICAL_MURDER_FLAG)
  
  # Rename
  names(city_crime) <- c("datetime","age","sex","race","lng","lat","lethal")
  
  city_crime <- city_crime %>%
    mutate(race = recode(race, WHITE="White",.default = "POC"),
           sex = recode(sex, F="Female",M="Male"),
           age_group = recode(age,
                              `<18` = "0-17")) %>% 
    mutate(shooting_type = ifelse(lethal == T,"lethal","nonlethal")) %>% 
    select(shooting_type,age_group, race, sex, lat, lng) %>% 
    na.omit()
  
  fwrite(city_crime, "data/shootings_csv/New York.csv")
  
  ### Chicago
  city_crime <- fread("data/raw_shootings/Chicago/Violence_Reduction_-_Victims_of_Homicides_and_Non-Fatal_Shootings.csv")
  
  # Date
  city_crime$date <- parse_date_time(city_crime$DATE, "m/d/y IMS p")
  city_crime$year <- year(city_crime$date)
  
  # Subset years
  city_crime <- city_crime[year %in% years]
  
  #Subset city_crime resulted in injury
  city_crime <- city_crime[GUNSHOT_INJURY_I == "YES"]
  
  city_crime %<>% 
    mutate(shooting_type = recode(INCIDENT_PRIMARY,
                                  "HOMICIDE" = "lethal", .default = "nonlethal")) %>% 
    mutate(age_group = ifelse(AGE == "UNKNOWN",NA, AGE)) %>% 
    mutate(race = ifelse(RACE == "UNKNOWN",NA,RACE),
           race = recode(race,
                         "WHI" = "White",
                         .default = 'POC')) %>% 
    mutate(sex = SEX,
           sex = recode(sex,
                        "F" = "Female",
                        "M" = "Male", .default = "Male")) %>% 
    select(shooting_type, age_group, race,sex,LATITUDE,LONGITUDE) %>% 
    na.omit()

  # Rename
  names(city_crime) <- c("shooting_type","age_group","race","sex","lat", "lng")
  
  fwrite(city_crime, "data/shootings_csv/Chicago.csv")
  
  ### Los Angeles
  city_crime <- fread("data/raw_shootings/Los Angeles/Crime_Data_from_2020_to_Present.csv")
  
  # Parse date and time
  city_crime$datetime <- parse_date_time(paste(substring(city_crime$`DATE OCC`,0,11),
                                              str_pad(city_crime$`TIME OCC`, 4, pad="0")),
                                        '%m/%d/%Y %H%M')
  
  city_crime$year <- year(city_crime$datetime)
  
  # Subset years
  city_crime <- city_crime[year %in% years,]
  
  # MO Code == 1430 means victims shot
  city_crime <- city_crime[grepl("0430",Mocodes, fixed = T)]
  
  # Generate variables for data quality analysis
  city_crime$shooting_type <- dplyr::recode(city_crime$`Crm Cd Desc`,
                                           "CRIMINAL HOMICIDE" = "lethal",
                                           "MANSLAUGHTER, NEGLIGENT" = "lethal",
                                           .default = 'nonlethal')
  
  city_crime$race <- dplyr::recode(city_crime$`Vict Descent`,
                                  "W" = "White",
                                  .default = 'POC')
  
  city_crime <- city_crime %>% mutate(age = as.numeric(`Vict Age`)) %>% 
    # drop weird ages
    mutate(age = ifelse(age > 90  | age < 1, NA, age)) %>% 
    mutate(age_group = cut(as.numeric(age),
                           breaks = c(acs_age_group_breaks,120),
                           labels = c(acs_age_group_breaks),
                           right = F)) %>% 
    mutate(sex = `Vict Sex`,
           sex = recode(sex, "F" = "Female",
                        "M" = "Male",
                        .default = "Male")) %>% 
    select(shooting_type,age_group, race, sex, LAT, LON) %>% 
    filter(LAT > 0) %>% 
    na.omit()
  
  names(city_crime) <- c("shooting_type","age_group","race","sex","lat", "lng")
  fwrite(city_crime, "data/shootings_csv/Los Angeles.csv")
}

prepare_shootings_data()