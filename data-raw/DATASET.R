## code to prepare `michigan_2014_fqai` dataset goes here

#read in the data, skipping misc info that is listed at the top of the csv file
michigan_fqai <-
  read.csv("~/Desktop/michigan2014/data-raw/michigan_2014_FQA_database.csv",
           skip = 11)

#load the janitor package for cleaning names
library(janitor)

#clean the names
michigan_2014_fqai <- clean_names(michigan_fqai)

#use this dataset  (not viewable to package user)
usethis::use_data(michigan_2014_fqai, overwrite = TRUE, internal = TRUE)

#-------------------------------------------------------------------------------

## code to prepare `crooked_island_sitelist` dataset goes here

#read in the data, skipping misc info that is listed at the top of the csv file
crooked_island_site <-
  read.csv("~/Desktop/michigan2014/data-raw/crooked_island_open_dunes_FQA.csv", skip = 63)

#load the janitor package for cleaning names
library(janitor)
library(tidyverse)

#clean the names, select relevant cols
crooked_island <- clean_names(crooked_island_site) %>%
  select(scientific_name, acronym, common_name)

#use this dataset  (not viewable to package user)
usethis::use_data(crooked_island, overwrite = TRUE)
