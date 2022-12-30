## code to prepare `crooked_island` dataset
library(here)
library(dplyr)
library(stringr)

#read in the data, skipping misc info that is listed at the top of the csv file
crooked_island_site <-
  read.csv(here("data-raw", "crooked_island_open_dunes_FQA.csv"), skip = 63)

#clean the names, select relevant cols
crooked_island <- clean_names(crooked_island_site) %>%
  select(scientific_name, acronym, common_name) %>%
  mutate(name = str_remove(scientific_name, ";.*")) %>%
  select(-scientific_name)

#use this dataset  (not viewable to package user)
usethis::use_data(crooked_island, overwrite = TRUE)
