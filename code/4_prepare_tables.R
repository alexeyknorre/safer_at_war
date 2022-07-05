# This script uses calculated shootings rates by cities, zip areas, age, sex,
# and race groups and war mortality estimates to create tables for paper.
message("--- Calculate rates, compare with war estimates, produce tables...")
cities <- readRDS("data/shooting_rates.rds")

### Table 2: City-level shootings rates and population table
## First create sub-tables to bind into the table with city-level estimates
# Overall city-level statistics: summarize yearly shooting rates and 
# population at the city-level
tab2 <- cities %>% group_by(city) %>% 
  summarise(pop = sum(population, na.rm = T),
            lethal_shootings_yearly = sum(lethal_shootings_count,
                                          na.rm = T) / 2,
            nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
                                             na.rm = T) / 2,
            lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
            nonlethal_shootings_rate = nonlethal_shootings_yearly /
                                                          pop * 100000) %>% 
  mutate(type = "Overall")

# Subset ZCTAs with > 20000 population and select one most violent for each city
tab2_first_zcta <- cities %>% group_by(city,geo_label) %>% 
  summarise(pop = sum(population, na.rm = T),
            lethal_shootings_yearly = sum(lethal_shootings_count,
                                          na.rm = T) / 2,
            nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
                                             na.rm = T) / 2,
            lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
            nonlethal_shootings_rate = nonlethal_shootings_yearly /
                                                    pop * 100000) %>% 
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
           age_group %in% c("18","20","25","20-29","18-24") &
           geo_label %in% tab2_first_zcta$geo_label) %>% 
  group_by(city,geo_label) %>% 
  summarise(pop = sum(population, na.rm = T),
            lethal_shootings_yearly = sum(lethal_shootings_count,
                                          na.rm = T) / 2,
            nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
                                             na.rm = T) / 2,
            lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
            nonlethal_shootings_rate = nonlethal_shootings_yearly /
                                                             pop * 100000) %>% 
  arrange(desc(lethal_shootings_rate)) %>% 
  ungroup() %>% 
  mutate(type = "Top 1 ZIP code, males youth")

# Subset ZCTAs with > 20000 population and select 10% with highest homicide rate
# Save intermediate to use for ZCTA subsetting below
tab2_top10zcta_intermediate <- cities %>% group_by(city,geo_label) %>% 
  summarise(pop = sum(population, na.rm = T),
            lethal_shootings_yearly = sum(lethal_shootings_count,
                                          na.rm = T) / 2,
            nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
                                             na.rm = T) / 2,
            lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
            nonlethal_shootings_rate = nonlethal_shootings_yearly /
                                                            pop * 100000) %>% 
  arrange(desc(lethal_shootings_rate)) %>% 
  filter(pop > 20000) %>% 
  ungroup() %>% 
  group_by(city) %>% 
  slice_max(lethal_shootings_rate, prop = .1)

# Note two group_by/summarize steps: first at ZCTA level to sum up over 
# population subgroups, then - below - to pool ZCTAs together
tab2_top10zcta <- tab2_top10zcta_intermediate %>% 
     summarise(pop  = sum(pop, na.rm = T),
               lethal_shootings_yearly = sum(lethal_shootings_yearly,
                                             na.rm = T),
               nonlethal_shootings_yearly = sum(nonlethal_shootings_yearly,
                                                na.rm = T),
               lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
               nonlethal_shootings_rate = nonlethal_shootings_yearly /
                                                            pop * 100000) %>% 
  mutate(type = "Top 10% ZIP codes")

# Calculate shootings stats for male youth in top 10% violent ZCTAs
tab2_top10zcta_youth_male <- cities %>% group_by(city,geo_label) %>% 
  filter(sex == "Male" & 
           age_group %in% c("18","20","25","20-29","18-24") &
           geo_label %in% tab2_top10zcta_intermediate$geo_label) %>% 
  summarise(pop = sum(population, na.rm = T),
            lethal_shootings_yearly = sum(lethal_shootings_count,
                                          na.rm = T) / 2,
            nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
                                             na.rm = T) / 2,
            lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
            nonlethal_shootings_rate = nonlethal_shootings_yearly /
                                                            pop * 100000) %>% 
  arrange(desc(lethal_shootings_rate)) %>% 
  ungroup() %>% 
  group_by(city) %>% 
  summarise(pop  = sum(pop, na.rm = T),
            lethal_shootings_yearly = sum(lethal_shootings_yearly,
                                          na.rm = T),
            nonlethal_shootings_yearly = sum(nonlethal_shootings_yearly,
                                             na.rm = T),
            lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
            nonlethal_shootings_rate = nonlethal_shootings_yearly /
                                                          pop * 100000) %>% 
  mutate(type = "Top 10% ZIP codes, males youth")

## Same subsetting as above but for non-whites only
# Top 1 ZCTA
tab2_first_zcta_young_male_nonw <- cities %>% 
  filter(sex == "Male" &
           race == "POC" &
           age_group %in% c("18","20","25","20-29","18-24") &
           geo_label %in% tab2_first_zcta$geo_label) %>% 
  group_by(city,geo_label) %>% 
  summarise(pop = sum(population, na.rm = T),
            lethal_shootings_yearly = sum(lethal_shootings_count,
                                          na.rm = T) / 2,
            nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
                                             na.rm = T) / 2,
            lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
            nonlethal_shootings_rate = nonlethal_shootings_yearly /
                                                            pop * 100000) %>% 
  arrange(desc(lethal_shootings_rate)) %>% 
  ungroup() %>% 
  mutate(type = "Top 1 ZIP code, non-White males youth")

# Top 10% ZCTAs
tab2_top10zcta_youth_male_nonw <- cities %>% group_by(city,geo_label) %>% 
  filter(sex == "Male" & 
           race == "POC" &
           age_group %in% c("18","20","25","20-29","18-24") &
           geo_label %in% tab2_top10zcta_intermediate$geo_label) %>% 
  summarise(pop = sum(population, na.rm = T),
            lethal_shootings_yearly = sum(lethal_shootings_count,
                                          na.rm = T) / 2,
            nonlethal_shootings_yearly = sum(nonlethal_shootings_count,
                                             na.rm = T) / 2,
            lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
            nonlethal_shootings_rate = nonlethal_shootings_yearly /
              pop * 100000) %>% 
  arrange(desc(lethal_shootings_rate)) %>% 
  ungroup() %>% 
  group_by(city) %>% 
  summarise(pop  = sum(pop, na.rm = T),
            lethal_shootings_yearly = sum(lethal_shootings_yearly,
                                          na.rm = T),
            nonlethal_shootings_yearly = sum(nonlethal_shootings_yearly,
                                             na.rm = T),
            lethal_shootings_rate = lethal_shootings_yearly / pop * 100000,
            nonlethal_shootings_rate = nonlethal_shootings_yearly /
                                                            pop * 100000) %>% 
  mutate(type = "Top 10% ZIP codes, non-White males youth")

## Bind it all together
tab2_cities <- rbindlist(list(tab2,
                              tab2_top10zcta,
                              tab2_top10zcta_youth_male,
                              tab2_top10zcta_youth_male_nonw,
                              tab2_first_zcta,
                              tab2_first_zcta_young_male,
                              tab2_first_zcta_young_male_nonw),fill = T) %>% 
  arrange(city, lethal_shootings_rate) %>% 
  select(city, type, pop,lethal_shootings_yearly,nonlethal_shootings_yearly,
         lethal_shootings_rate,nonlethal_shootings_rate) %>% 
  mutate_if(is.numeric, round, digits=2)

# Save for Table 1 calculations
tab2_cities_saved <- tab2_cities

# Subset for males, recalculate totals
tab2_cities <- tab2_cities %>% 
  filter(!(type %in% c("Overall","Top 10% ZIP codes","Top 1 ZIP code"))) %>% 
  mutate(total_firearm_homicides = lethal_shootings_yearly * 2,
         total_nonlethal_shootings = nonlethal_shootings_yearly * 2,
         is_top10 = grepl("Top 10%",type,fixed = T),
         is_poc = grepl("non-White",type,fixed = T)) %>% 
  arrange(match(city, c("Chicago",
                        "Philadelphia",
                        "Los Angeles",
                        "New York")), is_top10, is_poc) %>% 
  select(city, type, pop, total_firearm_homicides, total_nonlethal_shootings, is_top10,is_poc)

# Calculate the totals across all cities
tab2_cities_total <- tab2_cities %>% 
  group_by(is_top10, is_poc) %>% 
  summarise(pop = sum(pop),
            total_firearm_homicides = sum(total_firearm_homicides),
            total_nonlethal_shootings = sum(total_nonlethal_shootings)) %>% 
  ungroup() %>% 
  arrange(is_top10, is_poc) %>% 
  mutate(type = c("Top 1 ZIP code, males youth",
                  "Top 1 ZIP code, non-White males youth",
                  "Top 10% ZIP codes, males youth",
                  "Top 10% ZIP codes, non-White males youth"),
         city = "Total") %>% 
  select(city, type, pop, total_firearm_homicides,
         total_nonlethal_shootings, is_top10, is_poc)

# Bind totals with city-levels estimates
tab2_cities_with_totals <- rbind(tab2_cities, tab2_cities_total)

# Calculate percentages of non-white victims
tab2_cities_percenages <- tab2_cities_with_totals %>% 
  select(city,is_poc, is_top10, total_firearm_homicides,
         total_nonlethal_shootings) %>% 
  pivot_wider(names_from = c("is_poc"),
              values_from = c("total_firearm_homicides",
                              "total_nonlethal_shootings")) %>% 
  mutate(perc_lethal = round(total_firearm_homicides_TRUE /
                               total_firearm_homicides_FALSE,3),
         perc_nonlethal = round(total_nonlethal_shootings_TRUE /
                                  total_nonlethal_shootings_FALSE,3)) %>% 
  mutate(perc_lethal = paste0(format(perc_lethal * 100),"%"),
         perc_nonlethal = paste0(format(perc_nonlethal * 100),"%")) %>% 
  select(city, is_top10, perc_lethal, perc_nonlethal) %>% 
  mutate(is_poc = TRUE)

# Bind together
tab2_cities_binded <- left_join(tab2_cities_with_totals,
                            tab2_cities_percenages,
                            by = c("city", "is_top10", "is_poc"))

# Prettify:
tab2_cities_binded <- tab2_cities_binded %>% 
  select(city, type, pop, total_firearm_homicides, total_nonlethal_shootings, 
         perc_lethal, perc_nonlethal)
  
tab2_cities_binded[is.na(tab2_cities_binded)] <- "-"

tab2_cities_publish <- tab2_cities_binded %>% 
  mutate(type = case_when(type == "Top 1 ZIP code, males youth" ~
                            "Most violent ZIP code",
                          type == "Top 1 ZIP code, non-White males youth" ~
                            "(non-white males only)",
                          type == "Top 10% ZIP codes, males youth" ~ 
                            "Top 10% most violent ZIP codes",
                          type == "Top 10% ZIP codes, non-White males youth" ~
                            "(non-white males only)")) %>% 
  mutate(city = case_when(city == "Chicago" ~ "Chicago, males 20-29",
                          city == "Los Angeles" ~ "Los Angeles, males 18-29",
                          city == "New York" ~ "New York, males 18-24",
                          city == "Philadelphia" ~
                            "Philadelphia, males 18-29",
                          city == "Total" ~ "Total")) %>% 
  setNames(c("city","Subset","Pop.",
             "Total firearm homicides","Total nonlethal shootings",
             "% of firearm homicides","% on nonfatal shootings")) 

# Check it fast
#tab2_cities_publish

# Export
kbl(tab2_cities_publish %>% select(-city),
    format = "latex",booktabs = T,
    format.args = list(big.mark = ","),
    align = "lrrrrrrrr",
    linesep = "",
    caption = "Gun-related mortality and wound rates in major US cities",
    label = "cities_detailed") %>% 
  pack_rows(index = table(forcats::fct_inorder(tab2_cities_publish$city))) %>% 
  save_kable(.,file = "tables/cities_detailed.tex")


### Table 1

## Read war mortality data
tab1_wars <- read_excel("data/war_casualties.xlsx") 
# Prepare for binding
tab1_wars <- tab1_wars %>% 
  select(type,wia_per_100kty,td_per_100kty) %>% 
  mutate(city = "Wars") %>% 
  select(city,type,
         td_per_100kty,
         wia_per_100kty) %>% 
  setNames(c("city","type","lethal_shootings_rate","nonlethal_shootings_rate"))

# Subset city-level estimates for comparison
tab1_cities_subset <- tab2_cities_saved %>% 
  filter(grepl(', males',type)) %>% 
  select(city,type, lethal_shootings_rate, nonlethal_shootings_rate)

# Bind city-level and war-level estimates
tab1_overall <- rbind(tab1_cities_subset,
                      tab1_wars) %>% 
  # Arrange first wars, then cities
  arrange(match(city, c("Wars",
                        "Chicago","Philadelphia","Los Angeles","New York")),
          desc(lethal_shootings_rate)) %>% 
  # Round rates, calculate RR using Afghan war as baselevel
  mutate(lethal_shootings_rate = round(lethal_shootings_rate),
         lethal_rate_ratio = round(lethal_shootings_rate / 
                                   tab1_wars[tab1_wars$type == 'Afghan War',
                                    ][[c("lethal_shootings_rate")]],2),
         nonlethal_shootings_rate = round(nonlethal_shootings_rate),
         nonlethal_rate_ratio = round(nonlethal_shootings_rate /
                                      tab1_wars[tab1_wars$type == 'Afghan War',
                                    ][[c("nonlethal_shootings_rate")]],2),) %>% 
  select(city, type, lethal_shootings_rate, lethal_rate_ratio,
         nonlethal_shootings_rate, nonlethal_rate_ratio) %>% 
  filter(type !="Vietnam War")

tab1_overall_publish <- tab1_overall %>% 
  mutate(city = case_when(city == "Wars" ~
                            "US combatants of all ages",
                          city == "Chicago" ~
                            "Chicago, males 20-29",
                          city == "Los Angeles" ~
                            "Los Angeles, males 18-29",
                          city == "New York" ~ 
                            "New York, males 18-24",
                          city == "Philadelphia" ~
                            "Philadelphia, males 18-29")) %>%
  mutate(type = case_when(type == "Top 1 ZIP code, males youth" ~
                            "Most violent ZIP code",
                          type == "Top 10% ZIP codes, males youth" ~ 
                            "Top 10% most violent ZIP codes",
                          type == "Top 10% ZIP codes, non-White males youth" ~
                            "(non-white males only)",
                          TRUE ~ type)) %>% 
  setNames(c("city","Subset","Gun homicides per 100K person-years","RR",
             "Violent injuries per 100K person-years","RR"))
  

# Check it fast
#tab1_overall_publish
  
# Export to tex
kbl(tab1_overall_publish[,-1],
    format = "latex",booktabs = T,
    format.args = list(big.mark = ","),
    align = "lrrrrrrrrrr",
    linesep = "",
    caption = "Gun-related mortality and wound rates
    in recent wars and major US cities",
    label = "wars_and_cities") %>% 
  pack_rows(index = table(forcats::fct_inorder(tab1_overall_publish$city))) %>% 
  save_kable(.,file = "tables/wars_and_cities.tex")

