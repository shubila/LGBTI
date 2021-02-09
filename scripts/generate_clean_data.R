#clear the global environment
rm(list = ls(all.names = TRUE))

# Set-up  -----------------------------------------------------------------

# load packages
library(foreign)
library(haven)
library(tidyverse)

# Read the raw files, .dta and .sav 

lgbti_dta <- read_dta("./data/raw/ZA7604_v1-0-0.dta")

lgbti_sav <- as.data.frame(read.spss("./data/raw/ZA7604_v1-0-0.sav"))

saveRDS(lgbti_dta,"./data/clean/lgbti_dta.rds")
saveRDS(lgbti_sav,"./data/clean/lgbti_sav.rds")
