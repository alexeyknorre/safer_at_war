# This script uses calculated shootings rates by cities, zip areas, age, sex,
# and race groups and war mortality estimates to create tables for paper.
message("--- Calculate rates, compare with war estimates, produce tables...")
cities <- readRDS("data/shootings_counts.rds")

### Table 2: City-level shootings rates and population table
## First create sub-tables to bind into the table with city-level estimates
# Overall city-level statistics: summarize yearly shooting rates and
# population at the city-level
tab2 <- cities %>%
  group_by(city) %>%
  summarise(
    # This seemingly weird filtering on race condition
    # is used, because Hispanic is a separate category,
    # and Whites + Blacks + Others = Total
    pop = sum(population[race != "Hispanic"], na.rm = T),
    pop_se = sqrt(sum(population_se[race != "Hispanic"]^2)),
    lethal_shootings_yearly = sum(lethal_shootings_count,
      na.rm = T
    ) / 2,
    nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
      na.rm = T
    ) / 2,
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  mutate(type = "Overall")

# Subset ZCTAs with > 20000 population and select one most violent for each city
tab2_first_zcta <- cities %>%
  group_by(city, geo_label) %>%
  summarise(
    pop = sum(population[race != "Hispanic"], na.rm = T),
    pop_se = sqrt(sum(population_se[race != "Hispanic"]^2)),
    lethal_shootings_yearly = sum(lethal_shootings_count,
      na.rm = T
    ) / 2,
    nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
      na.rm = T
    ) / 2,
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  arrange(desc(lethal_shootings_rate)) %>%
  filter(pop > 20000) %>%
  ungroup() %>%
  group_by(city) %>%
  slice_max(lethal_shootings_rate, n = 1) %>%
  mutate(type = "Top 1 ZIP code")

# Get the most violent ZCTA in each city and
# calculate shootings stats for male youth there
tab2_first_zcta_young_male <- cities %>%
  filter(sex == "Male" &
    # Age groups intersect because shootings data in different cities
    # uses different age aggregation. Single numbers show the start of
    # age interval
    age_group %in% c("18", "20", "25", "20-29", "18-24") &
    geo_label %in% tab2_first_zcta$geo_label) %>%
  group_by(city, geo_label) %>%
  summarise(
    pop = sum(population[race != "Hispanic"], na.rm = T),
    pop_se = sqrt(sum(population_se[race != "Hispanic"]^2)),
    lethal_shootings_yearly = sum(lethal_shootings_count,
      na.rm = T
    ) / 2,
    nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
      na.rm = T
    ) / 2,
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  arrange(desc(lethal_shootings_rate)) %>%
  ungroup() %>%
  mutate(type = "Top 1 ZIP code, males youth")

# Subset ZCTAs with > 20000 population and select 10% with highest homicide rate
# Save intermediate to use for ZCTA subsetting below
tab2_top10zcta_intermediate <- cities %>%
  group_by(city, geo_label) %>%
  summarise(
    pop = sum(population[race != "Hispanic"], na.rm = T),
    pop_se = sqrt(sum(population_se[race != "Hispanic"]^2)),
    lethal_shootings_yearly = sum(lethal_shootings_count,
      na.rm = T
    ) / 2,
    nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
      na.rm = T
    ) / 2,
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  arrange(desc(lethal_shootings_rate)) %>%
  filter(pop > 20000) %>%
  ungroup() %>%
  group_by(city) %>%
  slice_max(lethal_shootings_rate, prop = .1)

# Note two group_by/summarize steps: first at ZCTA level to sum up over
# population subgroups, then - below - to pool ZCTAs together
tab2_top10zcta <- tab2_top10zcta_intermediate %>%
  summarise(
    pop = sum(pop, na.rm = T),
    pop_se = sqrt(sum(pop_se^2)),
    lethal_shootings_yearly = sum(lethal_shootings_yearly,
      na.rm = T
    ),
    nonlethal_shootings_yearly = sum(nonlethal_shootings_yearly,
      na.rm = T
    ),
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  mutate(type = "Top 10% ZIP codes")

# Calculate shootings stats for male youth in top 10% violent ZCTAs
tab2_top10zcta_youth_male <- cities %>%
  group_by(city, geo_label) %>%
  filter(sex == "Male" &
    age_group %in% c("18", "20", "25", "20-29", "18-24") &
    geo_label %in% tab2_top10zcta_intermediate$geo_label) %>%
  summarise(
    pop = sum(population[race != "Hispanic"], na.rm = T),
    pop_se = sqrt(sum(population_se[race != "Hispanic"]^2)),
    lethal_shootings_yearly = sum(lethal_shootings_count,
      na.rm = T
    ) / 2,
    nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
      na.rm = T
    ) / 2,
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  arrange(desc(lethal_shootings_rate)) %>%
  ungroup() %>%
  group_by(city) %>%
  summarise(
    pop = sum(pop, na.rm = T),
    pop_se = sqrt(sum(pop_se^2)),
    lethal_shootings_yearly = sum(lethal_shootings_yearly,
      na.rm = T
    ),
    nonlethal_shootings_yearly = sum(nonlethal_shootings_yearly,
      na.rm = T
    ),
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  mutate(type = "Top 10% ZIP codes, males youth")

## Same subsetting as above but for racial categories
# Top 1 ZCTA
tab2_first_zcta_young_male_by_race <- cities %>%
  filter(sex == "Male" &
    age_group %in% c("18", "20", "25", "20-29", "18-24") &
    geo_label %in% tab2_first_zcta$geo_label) %>%
  group_by(city, geo_label, race) %>%
  summarise(
    pop = sum(population, na.rm = T),
    pop_se = sqrt(sum(population_se^2)),
    lethal_shootings_yearly = sum(lethal_shootings_count,
      na.rm = T
    ) / 2,
    nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
      na.rm = T
    ) / 2,
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  arrange(desc(lethal_shootings_rate)) %>%
  ungroup() %>%
  mutate(type = "Top 1 ZIP code, males youth by race")

# Top 10% ZCTAs
tab2_top10zcta_youth_male_by_race <- cities %>%
  group_by(city, geo_label, race) %>%
  filter(sex == "Male" &
    age_group %in% c("18", "20", "25", "20-29", "18-24") &
    geo_label %in% tab2_top10zcta_intermediate$geo_label) %>%
  summarise(
    pop = sum(population, na.rm = T),
    pop_se = sqrt(sum(population_se^2)),
    lethal_shootings_yearly = sum(lethal_shootings_count,
      na.rm = T
    ) / 2,
    nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
      na.rm = T
    ) / 2,
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  arrange(desc(lethal_shootings_rate)) %>%
  ungroup() %>%
  group_by(city,race) %>%
  summarise(
    pop = sum(pop, na.rm = T),
    pop_se = sqrt(sum(pop_se^2)),
    lethal_shootings_yearly = sum(lethal_shootings_yearly,
      na.rm = T
    ),
    nonlethal_shootings_yearly = sum(nonlethal_shootings_yearly,
      na.rm = T
    ),
    lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
    nonlethal_shootings_rate = nonlethal_shootings_yearly /
      pop * 100000
  ) %>%
  mutate(type = "Top 10% ZIP codes, males youth by race")

## Bind it all together
tab2_cities <- rbindlist(list(
  tab2,
  tab2_top10zcta,
  tab2_top10zcta_youth_male,
  tab2_top10zcta_youth_male_by_race,
  tab2_first_zcta,
  tab2_first_zcta_young_male,
  tab2_first_zcta_young_male_by_race
), fill = T) %>%
  arrange(city, type, race) %>%
  select(
    city, type, race, pop, pop_se, lethal_shootings_yearly, nonlethal_shootings_yearly,
    lethal_shootings_rate, nonlethal_shootings_rate
  ) %>%
  mutate_if(is.numeric, round, digits = 2)

# Save for Table 1 calculations
tab2_cities_saved <- tab2_cities

# Subset for males, recalculate totals
tab2_cities <- tab2_cities %>%
  filter(!(type %in% c("Overall", "Top 10% ZIP codes", "Top 1 ZIP code"))) %>%
  mutate(
    total_firearm_homicides = lethal_shootings_yearly * 2,
    total_nonlethal_shootings = nonlethal_shootings_yearly * 2,
    is_top10 = grepl("Top 10%", type, fixed = T),
    is_race_subgroup = grepl("race", type, fixed = T),
    race_arranged = factor(race, levels = c("Black","Hispanic","White","Other"))
  ) %>%
  arrange(match(city, c(
    "Chicago",
    "Philadelphia",
    "Los Angeles",
    "New York"
  )), is_top10,is_race_subgroup, race_arranged) %>%
  select(city, type, pop, pop_se,
         total_firearm_homicides, total_nonlethal_shootings,
         is_top10,is_race_subgroup, race_arranged)

# Calculate the totals across all cities
tab2_cities_total <- tab2_cities %>%
  group_by(is_top10, is_race_subgroup, race_arranged) %>%
  summarise(
    pop = sum(pop),
    pop_se = sqrt(sum(pop_se^2)),
    total_firearm_homicides = sum(total_firearm_homicides),
    total_nonlethal_shootings = sum(total_nonlethal_shootings)
  ) %>%
  ungroup() %>%
  arrange(is_top10, is_race_subgroup, race_arranged) %>%
  mutate(
    type = paste0(ifelse(is_top10,"Top 1 ZIP code, ","Top 10% ZIP codes, "),
                 ifelse(is_race_subgroup,
                        paste0("males youth (",as.character(race_arranged),")"),
                        "males youth")),
    city = "Total"
  ) %>%
  select(
    city, type, pop, pop_se, total_firearm_homicides,
    total_nonlethal_shootings, is_top10, is_race_subgroup, race_arranged
  )

# Bind totals with city-levels estimates
tab2_cities_with_totals <- rbind(tab2_cities, tab2_cities_total)

# Calculate percentages by each racial category
tab2_cities_percentages_total <- tab2_cities_with_totals %>% 
  filter(!is_race_subgroup) %>% 
  select(-race_arranged)

tab2_cities_percentages_subgroups <- tab2_cities_with_totals %>%
  filter(is_race_subgroup) %>% 
  mutate(total_firearm_homicides_sub = total_firearm_homicides,
         total_nonlethal_shootings_sub = total_nonlethal_shootings) %>% 
  select(city, is_top10, race_arranged,
         total_firearm_homicides_sub,total_nonlethal_shootings_sub)

tab2_cities_percentages_by_subgroups <- left_join(tab2_cities_percentages_total,
                                                  tab2_cities_percentages_subgroups,
                                                  by = c("city","is_top10")) %>% 
  mutate(perc_lethal = round(total_firearm_homicides_sub /
                               total_firearm_homicides, 3),
         perc_nonlethal = round(total_nonlethal_shootings_sub /
                                  total_nonlethal_shootings, 3)
  ) %>%
  mutate(
    perc_lethal = paste0(format(perc_lethal * 100), "%"),
    perc_nonlethal = paste0(format(perc_nonlethal * 100), "%")
  ) %>%
  select(city, is_top10, race_arranged, perc_lethal, perc_nonlethal)

# Bind together
tab2_cities_binded <- left_join(tab2_cities_with_totals,
                                tab2_cities_percentages_by_subgroups,
  by = c("city", "is_top10", "race_arranged")
)

# Prettify:
tab2_cities_binded <- tab2_cities_binded %>%
  mutate(type = ifelse(is_top10,"Top 10% most violent ZIP codes","Most violent ZIP code"),
         type = ifelse(is_race_subgroup,
                       as.character(race_arranged),
                       type)) %>% 
  select(
    city, type, pop, pop_se, total_firearm_homicides, total_nonlethal_shootings,
    perc_lethal, perc_nonlethal
  )

tab2_cities_binded[is.na(tab2_cities_binded)] <- "-"

tab2_cities_publish <- tab2_cities_binded %>%
  mutate(city = case_when(
    city == "Chicago" ~ "Chicago, males 20-29",
    city == "Los Angeles" ~ "Los Angeles, males 18-29",
    city == "New York" ~ "New York, males 18-24",
    city == "Philadelphia" ~
      "Philadelphia, males 18-29",
    city == "Total" ~ "Total"
  ),
  pop_se = round(pop_se)) %>%
  add_row(type = "Total",.before = 41) %>% 
  add_row(type = "New York, males 18-24",.before = 31) %>% 
  add_row(type = "Los Angeles, males 18-29",.before = 21) %>% 
  add_row(type = "Philadelphia, males 18-29",.before = 11) %>% 
  add_row(type = "Chicago, males 20-29",.before = 1) %>% 
  select(-city) %>% 
  setNames(c(
    "Subset", "Pop.", "Pop. (SE)",
    "Total firearm homicides", "Total nonlethal shootings",
    "% of firearm homicides", "% of nonfatal shootings"
  ))

tab2_cities_publish[is.na(tab2_cities_publish)] <- "-"

openxlsx::write.xlsx(tab2_cities_publish, "tables/tab2_cities_detailed.xlsx")


### Table 1

## Read war mortality data
tab1_wars <- read_excel("data/war_casualties.xlsx")
# Prepare for binding
tab1_wars <- tab1_wars %>%
  select(type, wia_per_100kty, td_per_100kty) %>%
  mutate(city = "Wars",
         lethal_shootings_rate_lower = NA,
         lethal_shootings_rate_upper = NA,
         nonlethal_shootings_rate_lower = NA,
         nonlethal_shootings_rate_upper = NA) %>%
  select(
    city, type,
    td_per_100kty,
    lethal_shootings_rate_lower,
    lethal_shootings_rate_upper,
    wia_per_100kty,
    nonlethal_shootings_rate_lower,
    nonlethal_shootings_rate_upper
  ) %>%
  setNames(c("city", "type", "lethal_shootings_rate",
             "lethal_shootings_rate_lower",
             "lethal_shootings_rate_upper",
             "nonlethal_shootings_rate",
             "nonlethal_shootings_rate_lower",
             "nonlethal_shootings_rate_upper"))

# Calculate confidence intervals for rates
tab2_cities_ci <- tab2_cities_saved %>% 
  filter(is.na(race)) %>% 
  mutate(lethal_shootings_rate_upper = lethal_shootings_yearly / (pop - 1.96*pop_se) * 100000,
         lethal_shootings_rate_lower = lethal_shootings_yearly / (pop + 1.96*pop_se) * 100000,
         nonlethal_shootings_rate_upper = nonlethal_shootings_yearly / (pop - 1.96*pop_se) * 100000,
         nonlethal_shootings_rate_lower = nonlethal_shootings_yearly / (pop + 1.96*pop_se) * 100000)

# Subset city-level estimates for comparison
tab1_cities_subset <- tab2_cities_ci %>%
  filter(grepl(", males", type)) %>%
  select(city, type,
         lethal_shootings_rate,
         lethal_shootings_rate_lower,
         lethal_shootings_rate_upper,
         nonlethal_shootings_rate,
         nonlethal_shootings_rate_lower,
         nonlethal_shootings_rate_upper)

# Bind city-level and war-level estimates
tab1_overall <- rbind(
  tab1_cities_subset,
  tab1_wars
) %>%
  # Arrange first wars, then cities
  arrange(
    match(city, c(
      "Wars",
      "Chicago", "Philadelphia", "Los Angeles", "New York"
    )),
    desc(lethal_shootings_rate)
  ) %>%
  # Round rates, calculate RR using Afghan war as baselevel
  mutate(
    lethal_rate_ratio = round(lethal_shootings_rate /
      tab1_wars[tab1_wars$type == "Afghan War", ][[c("lethal_shootings_rate")]], 2),
    lethal_rate_ratio_lower = round(lethal_shootings_rate_lower /
                                tab1_wars[tab1_wars$type == "Afghan War", ][[c("lethal_shootings_rate")]], 2),
    lethal_rate_ratio_upper = round(lethal_shootings_rate_upper /
                                tab1_wars[tab1_wars$type == "Afghan War", ][[c("lethal_shootings_rate")]], 2),
    nonlethal_shootings_rate = round(nonlethal_shootings_rate),
    nonlethal_rate_ratio = round(nonlethal_shootings_rate /
      tab1_wars[tab1_wars$type == "Afghan War", ][[c("nonlethal_shootings_rate")]], 2),
    nonlethal_rate_ratio_lower = round(nonlethal_shootings_rate_lower /
                                   tab1_wars[tab1_wars$type == "Afghan War", ][[c("nonlethal_shootings_rate")]], 2),
    nonlethal_rate_ratio_upper = round(nonlethal_shootings_rate_upper /
                                   tab1_wars[tab1_wars$type == "Afghan War", ][[c("nonlethal_shootings_rate")]], 2),
    lethal_shootings_rate = format(lethal_shootings_rate,digits = 1, big.mark = ","),
    lethal_shootings_rate_lower = format(lethal_shootings_rate_lower,digits = 1, big.mark = ","),
    lethal_shootings_rate_upper = format(lethal_shootings_rate_upper,digits = 1, big.mark = ","),
    nonlethal_shootings_rate = format(nonlethal_shootings_rate,digits = 1, big.mark = ","),
    nonlethal_shootings_rate_upper = format(nonlethal_shootings_rate_upper,digits = 1, big.mark = ","),
    nonlethal_shootings_rate_lower = format(nonlethal_shootings_rate_lower,digits = 1, big.mark = ","),
  ) %>%
  mutate(across(c(lethal_rate_ratio,
                  lethal_rate_ratio_lower,
                  lethal_rate_ratio_upper,
                  nonlethal_rate_ratio,
                  nonlethal_rate_ratio_lower,
                  nonlethal_rate_ratio_upper),
                ~ format(.x, nsmall = 2))) %>%
  mutate(across(c(lethal_shootings_rate,
                  lethal_shootings_rate_lower,
                  lethal_shootings_rate_upper,
                  nonlethal_shootings_rate,
                  nonlethal_shootings_rate_upper,
                  nonlethal_shootings_rate_lower),
                ~ trimws(.x))) %>% 
  mutate(lethal_rate_ratio = ifelse(city != "Wars",
                                    paste0(lethal_rate_ratio,
                                   " (",lethal_rate_ratio_lower,"-",
                                   lethal_rate_ratio_upper,")"),
                                   lethal_rate_ratio),
         nonlethal_rate_ratio = ifelse(city != "Wars",
                                       paste0(nonlethal_rate_ratio,
                                    " (",nonlethal_rate_ratio_lower,"-",
                                    nonlethal_rate_ratio_upper,")"),
                                    nonlethal_rate_ratio),
         lethal_shootings_rate = ifelse(city != "Wars",
                                     paste0(lethal_shootings_rate,
                                            " (",lethal_shootings_rate_lower,"-",
                                            lethal_shootings_rate_upper,")"),
                                     lethal_shootings_rate),
         nonlethal_shootings_rate = ifelse(city != "Wars",
                                        paste0(nonlethal_shootings_rate,
                                               " (",nonlethal_shootings_rate_lower,"-",
                                               nonlethal_shootings_rate_upper,")"),
                                        nonlethal_shootings_rate)) %>% 
  select(
    city, type, lethal_shootings_rate, lethal_rate_ratio,
    nonlethal_shootings_rate, nonlethal_rate_ratio
  ) %>%
  filter(type != "Vietnam War")

tab1_overall_publish <- tab1_overall %>%
  mutate(city = case_when(
    city == "Wars" ~
      "US combatants of all ages",
    city == "Chicago" ~
      "Chicago, males 20-29",
    city == "Los Angeles" ~
      "Los Angeles, males 18-29",
    city == "New York" ~
      "New York, males 18-24",
    city == "Philadelphia" ~
      "Philadelphia, males 18-29"
  )) %>%
  mutate(type = case_when(
    type == "Top 1 ZIP code, males youth" ~
      "Most violent ZIP code",
    type == "Top 10% ZIP codes, males youth" ~
      "Top 10% most violent ZIP codes",
    type == "Top 10% ZIP codes, non-White males youth" ~
      "(non-white males only)",
    TRUE ~ type
  )) %>%
  setNames(c(
    "city", "Subset", "Gun homicides per 100K person-years", "RR",
    "Violent injuries per 100K person-years", "RR"
  ))


tab1_overall_publish_simple <- tab1_overall %>%
  mutate(city = case_when(
    city == "Wars" ~
      "US combatants of all ages",
    city == "Chicago" ~
      "Chicago, males 20-29",
    city == "Los Angeles" ~
      "Los Angeles, males 18-29",
    city == "New York" ~
      "New York, males 18-24",
    city == "Philadelphia" ~
      "Philadelphia, males 18-29"
  )) %>%
  mutate(type = case_when(
    type == "Top 1 ZIP code, males youth" ~
      "Most violent ZIP code",
    type == "Top 10% ZIP codes, males youth" ~
      "Top 10% most violent ZIP codes",
    type == "Top 10% ZIP codes, non-White males youth" ~
      "(non-white males only)",
    TRUE ~ type
  )) %>%
  add_row(type = "New York, males 18-24",.before = 9) %>% 
  add_row(type = "Los Angeles, males 18-29",.before = 7) %>% 
  add_row(type = "Philadelphia, males 18-29",.before = 5) %>% 
  add_row(type = "Chicago, males 20-29",.before = 3) %>% 
  add_row(type = "US combatants of all ages",.before = 1) %>% 
  select(-city) %>% 
  setNames(c(
    "Subset", "Gun homicides per 100K person-years", "RR",
    "Violent injuries per 100K person-years", "RR"
  ))

tab1_overall_publish_simple[is.na(tab1_overall_publish_simple)] <- ""

openxlsx::write.xlsx(tab1_overall_publish_simple, "tables/tab1_wars_and_cities.xlsx")
