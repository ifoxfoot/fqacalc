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
  mutate(synonym = NA) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "[?]", " X "))

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
         native, c, w, physiognomy, duration, common_name, fqa_db) %>%
  distinct()

#-------------------------------------------------------------------------------
#CHICAGO
chicago <- read_csv(here("data-raw",
                         "FQA_databases",
                         "not_from_universal_calc",
                         "chicago_region_2017.csv")) %>%
  clean_names()

chicago_clean <- chicago %>%
  mutate(scientific_name = scientific_name_nwpl_mohlenbrock_wilhelm_rericha) %>%
  mutate(synonym = scientific_name_synonym_swink_wilhelm_wilhelm_rericha) %>%
  mutate(family = scientific_family_name) %>%
  mutate(native = nativity) %>%
  mutate(c = coefficient_of_conservatism) %>%
  mutate(w = wetness_coefficient) %>%
  mutate(physiognomy = habit) %>%
  mutate(common_name = common_name_nwpl_mohlenbrock_wilhelm_rericha) %>%
  mutate(fqa_db = "chicago_region_2017") %>%
  select(scientific_name, synonym, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db)

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
  mutate(c = c_of_c_value) %>%
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
  mutate(duration = x9) %>%
  mutate(duration = case_when(duration == "A" ~ "annual",
                              duration == "P" ~ "perennial",
                              T ~ duration)) %>%
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
                          "wyoming_wetlands_2017.csv"), skip = 1) %>%
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
                     chicago_clean,
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

#clean up cols (other than latin name)
fqa_db_clean_cols <- fqa_db_bind %>%
  #clean nativity
  mutate(native = case_when(
    native %in% c("Native", "N", "Native/Naturalized", "Native/Adventive", "Likely Native")
    ~ "native", T ~ native)) %>%
  mutate(native = case_when(
    native %in% c("Exotic", "exotiv", "I", "Likely Exotic", "Nonnative")
    ~ "non-native", T ~ native)) %>%
  mutate(native = case_when(
    !native %in% c("native", "exotic") ~ "undetermined", T ~ native)) %>%
  #clean duration
  mutate(duration = str_to_title(duration)) %>%
  #clean physiognomy
  mutate(physiognomy = str_to_title(physiognomy)) %>%
  mutate(physiognomy = case_when(physiognomy == "Shurb" ~ "Shrub",
                                 physiognomy == "Shrub/Forb" ~ "Forb/Shrub",
                                 physiognomy %in% c("H-Vine", "W-Vine") ~ "Vine",
                                 #physiognomy == "Gram" ~ "Grass",
                                 physiognomy == "Frob" ~ "Forb",
                                 T ~ physiognomy)) %>%
  #clean family
  mutate(family = str_to_title(family)) %>%
  mutate(family = str_remove_all(family, "[0-9]*-")) %>%
  mutate(family = case_when(family == "Iso�Taceae" ~ "Isoetaceae",
                            family == "Azollaceae\r\nAzollaceae" ~ "Azollaceae",
                            family == "Hydrocharitaceae\r\nHydrocharitaceae" ~
                              "Hydrocharitaceae",
                            family == "#N/A" ~ NA_character_,
                            family == "As" ~ "Asteraceae",
                            T ~ family)) %>%
  #clean commmon name
  mutate(common_name = str_to_title(common_name)) %>%
  #clean C Value
  mutate(c = as.numeric(c)) %>%
  mutate(scientific_name = case_when(scientific_name == "BOTRYCHIUM SP. (NON-SOC)" ~
                                       "BOTRYCHIUM SP. NON-SOC",
                                     scientific_name == "BOTRYCHIUM SP. (SOC)" ~
                                       "BOTRYCHIUM SP. SOC",
                                     T ~ scientific_name))


#cleaning latin names
fqa_db_latin <- fqa_db_clean_cols %>%
  #making separators consistent
  mutate(scientific_name = str_replace_all(scientific_name, "_", " ")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "SYN. ", "; ")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "[{]", ";")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "=", ";")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "[(]", ";")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "[\\[]", ";")) %>%
  #making sure abbreviations are consistent
  mutate(scientific_name = str_replace_all(scientific_name, " SUBSP. ", " SSP. ")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, " VAR ", " VAR. ")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, " V. ", " VAR. ")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "�", "X ")) %>%
  #fixing white spaces
  mutate(scientific_name = str_squish(scientific_name)) %>%
  mutate(scientific_name = str_trim(scientific_name, side = "both")) %>%
  mutate(scientific_name = case_when(!str_detect(scientific_name, pattern = " ") ~
                                       paste(scientific_name, "SP."),
                                     T ~ scientific_name)) %>%
  #fixing odds and ends
  mutate(scientific_name = case_when(scientific_name == "APOCYNUM XFLORIBUNDUM" ~
                                       "APOCYNUM X FLORIBUNDUM",
                                     scientific_name == "BETULA XCAERULEA" ~
                                       "BETULA X CAERULEA",
                                     scientific_name == "MENTHA X X VERTICILLATA" ~
                                       "MENTHA X VERTICILLATA",
                                     scientific_name == "SPARTINA XCAESPITOSA" ~
                                       "SPARTINA X CAESPITOSA",
                                     scientific_name == "SCUTELLARIA XCHURCHILLIANA" ~
                                       "SCUTELLARIA X CHURCHILLIANA",
                                     scientific_name == "SCIRPUS XPECKII" ~
                                       "SCIRPUS X PECKII",
                                     scientific_name == "POTAMOGETON XMYSTICUS" ~
                                       "POTAMOGETON X MYSTICUS",
                                     scientific_name == "DRYOPTERIS XULIGINOSA" ~
                                       "DRYOPTERIS X ULIGINOSA",
                                     scientific_name == "ACER XFREEMANII" ~
                                       "ACER X FREEMANII",
                                     scientific_name == "QUERCUS XSUBFALCATA" ~
                                       "QUERCUS X SUBFALCATA",
                                     scientific_name == "QUERCUS XSAULII" ~
                                       "QUERCUS X SAULII",
                                     scientific_name == "QUERCUS XRUDKINII" ~
                                       "QUERCUS X RUDKINII",
                                     scientific_name == "QUERCUS XHETEROPHYLLA" ~
                                       "QUERCUS X HETEROPHYLLA",
                                     scientific_name == "QUERCUS XGIFFORDII" ~
                                       "QUERCUS X GIFFORDII",
                                     scientific_name == "QUERCUS XFILIALIS" ~
                                       "QUERCUS X FILIALIS",
                                     scientific_name == "QUERCUS XFERNOWII" ~
                                       "QUERCUS X FERNOWII",
                                     scientific_name == "QUERCUS XBEADLEI" ~
                                       "QUERCUS X BEADLEI",
                                     scientific_name == "POPULUS XJACKII" ~
                                       "POPULUS X JACKII",
                                     scientific_name == "PLATANTHERA XCANBYI" ~
                                       "PLATANTHERA X CANBYI",
                                     scientific_name == "PETUNIA XHYBRIDA" ~
                                       "PETUNIA X HYBRIDA",
                                     scientific_name == "LYCOPODIELLA XCOPELANDII" ~
                                       "LYCOPODIELLA X COPELANDII",
                                     scientific_name == "ILEX XATTENUATA" ~
                                       "ILEX X ATTENUATA",
                                     scientific_name == "DRYOPTERIS XBOOTTII" ~
                                       "DRYOPTERIS X BOOTTII",
                                     scientific_name == "DICHANTHELIUM XSCOPARIOIDES; PANICUM SCOPARIOIDES; P. VILLOSISSIMUM VAR. SCOPARIOIDES" ~
                                       "DICHANTHELIUM X SCOPARIOIDES; PANICUM SCOPARIOIDES; P. VILLOSISSIMUM VAR. SCOPARIOIDES",
                                     scientific_name == "KALANCHOE XHOUGHTONII" ~
                                       "KALANCHOE X HOUGHTONII",
                                     scientific_name == "ARABIS XDIVARICARPA" ~
                                       "ARABIS X DIVARICARPA",
                                     scientific_name == "AMELANCHIER X INTERMEDIA" ~
                                       "AMELANCHIER X INTERMEDIA",
                                     scientific_name == "VIOLA XBERNARDII" ~
                                       "VIOLA X BERNARDII",
                                     scientific_name == "VIOLA XPRIMULIFOLIA" ~
                                       "VIOLA X PRIMULIFOLIA",
                                     scientific_name == "VIOLA XPALMATA" ~
                                       "VIOLA X PALMATA",
                                     scientific_name == "SOLIDAGO XASPERULA" ~
                                       "SOLIDAGO X ASPERULA",
                                     scientific_name == "SALIX XSEPULCRALIS" ~
                                       "SALIX X SEPULCRALIS",
                                     scientific_name == "SALIX XPENDULINA" ~
                                       "SALIX X PENDULINA",
                                     scientific_name == "OCLEMENA XBLAKEI" ~
                                       "OCLEMENA X BLAKEI",
                                     scientific_name == "DRYOPTERIS XTRIPLOIDEA" ~
                                       "DRYOPTERIS X TRIPLOIDEA",
                                     scientific_name == "VITIS XNOVAE-ANGLIAE"~
                                       "VITIS X NOVAE-ANGLIAE",
                                     scientific_name == "SOLIDAGO XCALCICOLA" ~
                                       "SOLIDAGO X CALCICOLA",
                                     scientific_name == "SALIX XRUBENS" ~
                                       "SALIX X RUBENS",
                                     scientific_name == "MENTHA XPIPERITA" ~
                                       "MENTHA X PIPERITA",
                                     scientific_name == "MENTHA XGRACILIS" ~
                                       "MENTHA X GRACILIS",
                                     scientific_name == "HYPERICUM XDISSIMULATUM" ~
                                       "HYPERICUM X DISSIMULATUM",
                                     scientific_name == "BETULA XCAERULEA VAR. GRANDIS" ~
                                       "BETULA X CAERULEA VAR. GRANDIS",
                                     scientific_name == "ELEOCHARIS ACICULARIS / WILL SUGGESTS REMOVING, MAY BE MORE MONTANE" ~
                                       "ELEOCHARIS ACICULARIS",
                                     scientific_name == "PHYTOLACCA AMERICANA VAR, AMERICANA" ~
                                       "PHYTOLACCA AMERICANA VAR. AMERICANA",
                                     scientific_name == "SALVINIA SPP." ~
                                      " SALVINIA SP.",
                                     scientific_name == "CARDARIA DRABA, LEPIDIUM DRABA" ~
                                       "CARDARIA DRABA; LEPIDIUM DRABA",
                                     scientific_name == "TYPHA XGLAUCA" ~
                                       "TYPHA X GLAUCA",
                                     T ~ scientific_name)) %>%
  #separating by semicolon
  separate(scientific_name, c("scientific_name", "synonym1"), ";", extra = "merge")


#clean synonym1
fqa_db_synonym1 <- fqa_db_latin %>%
  #removing useless symbols
  mutate(synonym1 = str_remove_all(synonym1, "[}]")) %>%
  mutate(synonym1 = str_remove_all(synonym1, "[)]")) %>%
  mutate(synonym1 = str_remove_all(synonym1, "\\.")) %>%
  mutate(synonym1 = str_remove_all(synonym1, "^;")) %>%
  mutate(synonym1 = str_replace_all(synonym1, ";", "; ")) %>%
  #fixing white spaces
  mutate(synonym1 = str_squish(synonym1)) %>%
  mutate(synonym1 = str_trim(synonym1, side = "both"))

fqa_db_synonym <- fqa_db_synonym1 %>%
  #making sure abreviations are consistent
  mutate(synonym = str_replace_all(synonym, " SUBSP. ", " SSP. ")) %>%
  mutate(synonym = str_replace_all(synonym, " VAR ", " VAR. ")) %>%
  mutate(synonym = str_replace_all(synonym, ",", ";")) %>%
  mutate(synonym = str_remove_all(synonym, "[\\[]]")) %>%
  #fixing white spaces
  mutate(synonym = str_squish(synonym)) %>%
  mutate(synonym = str_trim(synonym, side = "both")) %>%
  mutate(synonym = case_when(!str_detect(synonym, pattern = " ") ~
                               paste(synonym, "SP."),
                             T ~ synonym))
#unique latin
#unique synonym
unique_latin <- data.frame(unique(fqa_db_latin$scientific_name))

#unique synonym
unique_syn <- data.frame(unique(fqa_db_synonym$synonym))

#unique synonym1
unique_syn1 <- data.frame(unique(fqa_db_synonym1$synonym1))

#sort data frame column alphabetically
#fqa_db_synonym[order(df$fqa_db_synonym), ]

#AT SOME POINT LOOK INTO DUP SCI NAMES
test <- fqa_db_synonym %>%
  group_by(scientific_name, fqa_db) %>%
  count() %>%
  filter(n>1)
#-------------------------------------------------------------------------------
#saving dataset MAKE SURE IT IS CLEAN VERSION!!!

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
