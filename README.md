# Replication files for "Safer at war: Comparing risks of firearm-related death and injury among young adult males in US cities to wartime service in Iraq and Afghanistan"

## Dependencies

While all the packages used are uploaded by `pacman` in the first script, you need to [set up an API key](https://walker-data.com/tidycensus/reference/census_api_key.html) for `tidycensus` to access US Census API in order to obtain ZCTA-level population estimates from American Community Survey. 

The code uses `sf` package that can be cumbersome to install for MacOS users. See [the guide](https://r-spatial.github.io/sf/#macos).

## Reproducing steps

Using R, run `code/master_script.R` to recreate the tables from the scratch.

You might also want to examine individual files created at the each step. Raw shootings data is not in this repository due to the large size of some files, and will be downloaded once you run `master_script.R`. 

Note that counts of victims might slightly differ. The official crime data sometimes retrospectively changes due to police departments correcting their records over time.

## Repository structure
```
\
├── code
│   ├── 0_initialize_packages.R
│   ├── 1_prepare_shootings_data.R
│   ├── 2_prepare_population_estimates.R
│   ├── 3_get_shootings_by_subgroups_by_cities.R
│   ├── 4_prepare_tables.R
│   └── master_script.R
├── data
│   ├── los_angeles_city_boundary # Folder with Los Angeles city boundary
│   │   └── city_boundary
│   │       ├── city_boundary.cpg
│   │       ├── city_boundary.dbf
│   │       ├── city_boundary.prj
│   │       ├── city_boundary.sbn
│   │       ├── city_boundary.sbx
│   │       ├── city_boundary.shp
│   │       └── city_boundary.shx
│   ├── raw_shootings # Folder to contain downloaded raw shooting data
│   ├── shootings_counts.rds # Ultimate dataset in serialized R format with shooting counts and populations by zip codes, age groups, sex, and race; used to produce tables
│   ├── shootings_csv # Folder with preprocessed shooting data
│   │   ├── Chicago.csv
│   │   ├── Los Angeles.csv
│   │   ├── New York.csv
│   │   └── Philadelphia.csv
│   ├── war_casualties.xlsx # Estimates of wartime casualty rates
│   └── zcta_acs # Folder with city-level ACS data
│       ├── Chicago_acs.rds
│       ├── Los Angeles_acs.rds
│       ├── New York_acs.rds
│       └── Philadelphia_acs.rds
└── tables # Tables for paper
    ├── cities_detailed.tex
    └── wars_and_cities.tex
```

## Notes

City boundary of Los Angeles is borrowed from [the official website of LA City](https://data.lacity.org/City-Infrastructure-Service-Requests/City-Boundary-of-Los-Angeles/ppge-zfr4).

## Contacts
[Alex Knorre](https://alexknorre.com)
a.v.knorre@gmail.com

