# Load all packages
# NB: tidycensus needs an API key!
# See: https://walker-data.com/tidycensus/reference/census_api_key.html
# NB2: package "sf" might be cumbersome to install due to GDAL dependency

library(data.table)
library(lubridate)
library(tidycensus)
library(sf)
library(tidyr)
library(dplyr)
library(stringi)
library(stringr)
library(kableExtra)
library(readxl)

# Supress irrelevant messages
options(dplyr.summarise.inform = F)
options(datatable.showProgress = FALSE)
