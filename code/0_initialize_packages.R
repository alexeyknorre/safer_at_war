# Load all packages
# NB: tidycensus needs an API key!
# See: https://walker-data.com/tidycensus/reference/census_api_key.html
# NB2: package "sf" might be cumbersome to install due to GDAL dependency

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,
               lubridate,
               tidycensus,
               sf,
               tidyr,
               dplyr,
               stringi,
               stringr,
               kableExtra,
               readxl)

# Supress irrelevant messages
options(dplyr.summarise.inform = F)
options(datatable.showProgress = FALSE)
