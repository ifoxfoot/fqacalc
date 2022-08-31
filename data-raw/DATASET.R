## code to prepare `fqa_db` dataset goes here

#load required packaged
library(here)
library(janitor)
library(tidyverse)
library(readxl)


#-------------------------------------------------------------------------------
#FOR UNIVERSAL FQA DBS


#create list of file names
univ_files <- list.files(path = here("data-raw", "FQA_databases"),
                         pattern = "*.csv",
                         full.names = F)

#read them in and create new col with region
univ_list <- lapply(univ_files, function(x)
  read_csv(paste0("./data-raw/FQA_databases/", x), skip = 11) %>%
    mutate(fqa_db = x))

#bind together
univ_fqa <- bind_rows(univ_list) %>%
  clean_names() %>%
  mutate(synonym = NA)

#FOR NEW ENGLAND DBS

#create list of file names
ne_files <- list.files(path = here("data-raw", "FQA_databases", "not_from_universal_calc"),
                         pattern = "*_2013.csv",
                         full.names = F)

#read them in and create new col with region
ne_list <- lapply(ne_files, function(x)
  readxl::read_xlsx(paste0("./data-raw/FQA_databases/not_from_universal_calc/", x)) %>%
                    mutate(fqa_db = x))

#bind together
ne_compiled <- bind_rows(ne_list)


#clean up col names
ne_clean <- ne_compiled %>%
  mutate(scientific_name = Taxon) %>%
  mutate(synonym = NA) %>%
  mutate(family = NA) %>%
  mutate(acronym = PLANTSSymbol) %>%
  mutate(native = "undetermined") %>%
  mutate(c = Score) %>%
  mutate(w = NA) %>%
  mutate(physiognomy = NA) %>%
  mutate(duration = NA) %>%
  mutate(common_name = CommonName) %>%
  select(scientific_name, synonym, family, acronym,
         native, c, w, physiognomy, duration, common_name, fqa_db)

#-------------------------------------------------------------------------------
#FLORIDA

florida <- read_csv(here("data-raw",
                         "FQA_databases",
                         "not_from_universal_calc",
                         "florida_2011.csv")) %>%
  clean_names() %>%
  filter(if_any(everything(), ~ !is.na(.)))

florida_clean <- florida %>%
  mutate(scientific_name = taxa_name) %>%
  mutate(synonym = NA) %>%
  mutate(family = NA) %>%
  mutate(acronym = NA) %>%
  mutate(native = nativity) %>%
  mutate(c = c_of_c_score) %>%
  mutate(w = NA) %>%
  mutate(physiognomy = NA) %>%
  mutate(common_name = NA) %>%
  mutate(fqa_db = "florida_2011") %>%
  select(scientific_name, synonym, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db)

#-------------------------------------------------------------------------------
# FLORIDA_SOUTH
florida_south <- read_csv(here("data-raw",
                         "FQA_databases",
                         "not_from_universal_calc",
                         "florida_south_2009.csv")) %>%
  clean_names()

florida_south_clean <- florida_south %>%
  mutate(synonym = NA) %>%
  mutate(acronym = NA) %>%
  mutate(w = NA) %>%
  mutate(physiognomy = NA) %>%
  mutate(duration = NA) %>%
  mutate(fqa_db = "florida_south_2009") %>%
  select(scientific_name, synonym, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db)

#-------------------------------------------------------------------------------
#MISSISSISSIPPI
ms <- read_xlsx(here("data-raw",
                     "FQA_databases",
                     "not_from_universal_calc",
                     "mississippi_north_central_wetlands_2005.xlsx")) %>%
  clean_names()

ms_clean <- ms %>%
  mutate(scientific_name = species) %>%
  mutate(synonym = NA) %>%
  mutate(acronym = NA) %>%
  mutate(native = origin) %>%
  mutate(c = ave_cc) %>%
  mutate(w = NA) %>%
  mutate(physiognomy = physiogynomy) %>%
  mutate(duration = NA) %>%
  mutate(common_name = common) %>%
  mutate(fqa_db = "mississippi_north_central_wetlands_2005") %>%
  select(scientific_name, synonym, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db)

#-------------------------------------------------------------------------------
#MONTANA
montana <- read_xlsx(here("data-raw",
                          "FQA_databases",
                          "not_from_universal_calc",
                          "montana_2017.csv")) %>%
  clean_names()

montana_clean <- montana %>%
  mutate(scientific_name = scientific_name_mtnhp) %>%
  mutate(synonym = synonym_s) %>%
  mutate(family = family_name) %>%
  mutate(acronym = NA) %>%
  mutate(native = origin_in_montana) %>%
  mutate(c = montana_c_value) %>%
  mutate(w = NA) %>%
  mutate(physiognomy = NA) %>%
  mutate(duration = NA) %>%
  mutate(fqa_db = "montana_2017") %>%
  select(scientific_name, synonym, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db)

#-------------------------------------------------------------------------------
#WYOMING
wyoming <- read_xlsx(here("data-raw",
                          "FQA_databases",
                          "not_from_universal_calc",
                          "wyoming_2017.csv"), skip = 1) %>%
  clean_names()

wyoming_clean <- wyoming %>%
  mutate(family = minor_taxonomic_group) %>%
  mutate(synonym = synonyms) %>%
  mutate(acronym = NA) %>%
  mutate(native = statewide_origin) %>%
  mutate(c = wyoming_coefficient_of_conservatism) %>%
  mutate(w = NA) %>%
  mutate(physiognomy = NA) %>%
  mutate(duration = NA) %>%
  mutate(fqa_db = "wyoming_2017") %>%
  select(scientific_name, synonym, family, acronym, native, c, w, physiognomy, duration, common_name, fqa_db) %>%
  slice(., 1:(n() - 1))


#-------------------------------------------------------------------------------
#NOW CLEANING ALL TOGETHER

#bind all together
fqa_db_bind <- rbind(ne_clean,
                florida_clean,
                florida_south_clean,
                ms_clean,
                montana_clean,
                wyoming_clean,
                univ_fqa) %>%
  #remove csv from end of fqa_db column
  mutate(fqa_db = str_remove_all(fqa_db, ".csv")) %>%
  #covert things to uppercase
  mutate(scientific_name = toupper(scientific_name)) %>%
  mutate(synonym = toupper(synonym))

# #cleaning latin names
# fqa_db_latin <- fqa_db_bind %>%
#   mutate(synonym = str_remove_all(synonym, c("\\[INCLUDING] ", "\\[INCLUDES]", )))

#clean
fqa_db <- fqa_db_bind %>%
  mutate(native = case_when(
    native %in% c("Native", "N", "Native/Naturalized", "Native/Adventive", "Likely Native")
    ~ "native", T ~ native)) %>%
  mutate(native = case_when(
    native %in% c("Exotic", "I", "Likely Exotic", "Nonnative", "non-native")
    ~ "exotic", T ~ native)) %>%
  mutate(native = case_when(
    !native %in% c("native", "exotic") ~ "undetermined", T ~ native)) %>%
  # mutate(native = case_when(
  #   native == "undetermined" & c > 0 ~ "native", T ~ native)) %>%
  #fix c values later!!!
  mutate(c = as.numeric(c))


#use this dataset  (not viewable to package user)
usethis::use_data(fqa_db, overwrite = TRUE, internal = TRUE, compress = "bzip2")

#-------------------------------------------------------------------------------

## code to prepare `crooked_island` dataset

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
