## code to prepare `fqa_db` dataset goes here

#load required packaged
library(here)
library(janitor)
library(tidyverse)
library(readxl)
library(splitstackshape)
library(naniar)


#FOR UNIVERSAL FQA DBS---------------------------------------------------------------


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
  #clean names
  clean_names() %>%
  #eliminate rows that are exactly the same
  distinct()

univ_cleanish <- univ_fqa %>%
  #replace subsp., spp. with ssp.
  mutate(scientific_name = str_replace_all(scientific_name, " subsp.", " ssp.")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, " spp.", " ssp.")) %>%
  #fixing up var
  mutate(scientific_name = str_replace_all(scientific_name, " var ", " var. ")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, " v. ", " var. ")) %>%
  #replace corrupt x
  mutate(scientific_name = str_replace_all(scientific_name, "�", "x ")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "_", " x ")) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "[?]", " X ")) %>%
  #making separators consistent
  mutate(scientific_name = str_replace_all(scientific_name, "\\(=", ";")) %>%
  mutate(scientific_name = case_when(fqa_db == "nebraska_2003.csv"
                                     ~ str_replace(scientific_name, "\\(", ";"),
                                     T ~ scientific_name)) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "[\\[]", ";")) %>%
  mutate(scientific_name = str_remove(scientific_name, "\\(including.+\\)"))

univ_syn_sep <- univ_cleanish %>%
  #separate plants by first ";" into sci name and syn
  separate(scientific_name, c("scientific_name", "synonym"), ";", extra = "merge") %>%
  #remove leading/trailing white spaces
  mutate(scientific_name = str_squish(scientific_name)) %>%
  mutate(scientific_name = str_trim(scientific_name, side = "both")) %>%
  mutate(synonym = str_squish(synonym)) %>%
  mutate(synonym = str_trim(synonym, side = "both")) %>%
  #replace empty cells with NA
  mutate(synonym = na_if(synonym, "")) %>%
  mutate(synonym = na_if(synonym, ".")) %>%
  #get rid of syn starting with ".;"
  mutate(synonym = str_remove(synonym, "^.;"))

#split synonym into many columns
syn_split <- cSplit(univ_syn_sep, 'synonym', ';')

#list of syn columns
syn_cols <- c("synonym_1", "synonym_2", "synonym_3", "synonym_4", "synonym_5", "synonym_6")

#replace fist character of syn cols with upper case
syn_upper <- syn_split %>%
  mutate(across(.cols = syn_cols, .fns = ~ str_replace(., "^\\w{1}", toupper)))

#replace initials with full names from column before
syn_initials <- syn_upper %>%
    mutate(
    synonym_1 = if_else(
      str_extract(synonym_1, "^\\w") == str_extract(scientific_name, "^\\w"),
      str_replace(synonym_1, "^\\w\\.", str_extract(scientific_name, "^\\w+")),
      synonym_1),
    synonym_1 = if_else(
      str_extract(synonym_1, "(?<=\\s)\\w") == str_extract(scientific_name, "(?<=\\s)\\w"),
      str_replace(synonym_1, "\\w\\.$", str_extract(scientific_name, "\\w+$")),
      synonym_1)) %>%
  mutate(
    synonym_2 = if_else(
      str_extract(synonym_2, "^\\w") == str_extract(synonym_1, "^\\w"),
      str_replace(synonym_2, "^\\w\\.", str_extract(synonym_1, "^\\w+")),
      synonym_2),
    synonym_2 = if_else(
      str_extract(synonym_2, "(?<=\\s)\\w") == str_extract(synonym_1, "(?<=\\s)\\w"),
      str_replace(synonym_2, "\\w\\.$", str_extract(synonym_1, "\\w+$")),
      synonym_2)) %>%
  mutate(
    synonym_3 = if_else(
      str_extract(synonym_3, "^\\w") == str_extract(synonym_2, "^\\w"),
      str_replace(synonym_3, "^\\w\\.", str_extract(synonym_2, "^\\w+")),
      synonym_3),
    synonym_3 = if_else(
      str_extract(synonym_3, "(?<=\\s)\\w") == str_extract(synonym_2, "(?<=\\s)\\w"),
      str_replace(synonym_3, "\\w\\.$", str_extract(synonym_2, "\\w+$")),
      synonym_3)) %>%
  mutate(
    synonym_4 = if_else(
      str_extract(synonym_4, "^\\w") == str_extract(synonym_3, "^\\w"),
      str_replace(synonym_4, "^\\w\\.", str_extract(synonym_3, "^\\w+")),
      synonym_4),
    synonym_4 = if_else(
      str_extract(synonym_4, "(?<=\\s)\\w") == str_extract(synonym_3, "(?<=\\s)\\w"),
      str_replace(synonym_4, "\\w\\.$", str_extract(synonym_3, "\\w+$")),
      synonym_4)) %>%
  mutate(
    synonym_5 = if_else(
      str_extract(synonym_5, "^\\w") == str_extract(synonym_4, "^\\w"),
      str_replace(synonym_5, "^\\w\\.", str_extract(synonym_4, "^\\w+")),
      synonym_5),
    synonym_5 = if_else(
      str_extract(synonym_5, "(?<=\\s)\\w") == str_extract(synonym_4, "(?<=\\s)\\w"),
      str_replace(synonym_5, "\\w\\.$", str_extract(synonym_4, "\\w+$")),
      synonym_5))

#use row number as unique ID
syn_row_name <- syn_initials %>%
  mutate(ID = as.numeric(row.names.default(.)))

#pivot longer
syn_pivot <- syn_row_name %>%
  mutate(proper_name = scientific_name) %>%
  pivot_longer(cols = c("scientific_name", syn_cols),
               names_to = "name_origin",
               values_to = "name") %>%
  filter(!is.na(name))

#keep only distinct rows (rows with diff IDs not distinct)
syn_distinct <- syn_pivot %>%
  distinct(ID, name, .keep_all = TRUE)

#if the plant is a synonym, us NA for acronym (to avoid repeating acronyms)
syn_dist_acronyms <- syn_distinct %>%
  mutate(acronym = case_when(str_detect(name_origin, "synonym_") ~ NA_character_,
                             T ~ acronym)) %>%
  rename(scientific_name = name) %>%
  mutate(w = as.character(w))

# syn_acronym_var <- syn_dist_acronyms %>%
#   mutate(acronym = case_when(str_detect(toupper(scientific_name), "VAR.")
#                              & fqa_db == "pennsylvania_piedmont_2013.csv"
#                              & !is.na(acronym)
#                              ~ paste0(acronym, toupper(sub(".*\\s+(\\S)\\S+$", "\\1", scientific_name))),
#                              T ~ acronym)) %>%
#   mutate(acronym = case_when(str_detect(toupper(scientific_name), "SSP.")
#                              & fqa_db == "pennsylvania_piedmont_2013.csv"
#                              & !is.na(acronym)
#                              ~ paste0(acronym, toupper(sub(".*\\s+(\\S)\\S+$", "\\1", scientific_name))),
#                              T ~ acronym))

universal_dups <- syn_acronym_var %>%
  group_by(acronym, fqa_db) %>%
  count() %>%
  filter(n > 1) %>%
  filter(!is.na(acronym))

#SOUTH EASTERN DBS---------------------------------------------------------------

#read in data
southeastern <- read_xlsx(here("data-raw", "FQA_databases", "not_from_universal_calc",
                               "southeastern_wetland_database_2014.xlsx")) %>%
  clean_names()

#clean whole thing
southeastern_clean <- southeastern %>%
  mutate(acronym = case_when(main_vs_syn == "Syn" ~ usda_synonym_symbol,
                             T ~ usda_accepted_symbol)) %>%
  mutate(name_origin = case_when(main_vs_syn == "Syn" ~ "synonym",
                                 main_vs_syn == "MAIN" ~ "main")) %>%
  rename(scientific_name = usda_scientific_name) %>%
  mutate(family = NA) %>%
  rename(native = native_status) %>%
  rename(physiognomy = growth_habit) %>%
  rename(common_name = usda_common_name) %>%
  group_by(usda_accepted_symbol) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(proper_name = first(scientific_name)) %>%
  ungroup()

southeastern_cols <- southeastern_clean %>%
  select(ID, proper_name, name_origin, scientific_name, family, acronym, native,
         ave_c_value_southern_coastal_plain,
         ave_c_value_plains,
         ave_c_value_piedmont,
         ave_c_value_mountains,
         ave_c_value_interior_plateau,
         nwpl_e_mtns,
         nwpl_cstl_plain,
         common_name,
         duration,
         physiognomy,
         native) %>%
  mutate(native = case_when(name_origin == "main" & is.na(native) ~ "NA",
                            T ~ native)) %>%
  fill(native) %>%
  mutate(ave_c_value_southern_coastal_plain = case_when(name_origin == "main" & is.na(ave_c_value_southern_coastal_plain) ~ "NA",
                                        T ~ ave_c_value_southern_coastal_plain)) %>%
  fill(ave_c_value_southern_coastal_plain) %>%
  mutate(ave_c_value_plains = case_when(name_origin == "main" & is.na(ave_c_value_plains) ~ "NA",
                            T ~ ave_c_value_plains)) %>%
  fill(ave_c_value_plains) %>%
  mutate(ave_c_value_piedmont = case_when(name_origin == "main" & is.na(ave_c_value_piedmont) ~ "NA",
                                        T ~ ave_c_value_piedmont)) %>%
  fill(ave_c_value_piedmont) %>%
  mutate(ave_c_value_mountains = case_when(name_origin == "main" & is.na(ave_c_value_mountains) ~ "NA",
                                          T ~ ave_c_value_mountains)) %>%
  fill(ave_c_value_mountains) %>%
  mutate(ave_c_value_interior_plateau = case_when(name_origin == "main" & is.na(ave_c_value_interior_plateau ) ~ "NA",
                                           T ~ ave_c_value_interior_plateau )) %>%
  fill(ave_c_value_interior_plateau) %>%
  mutate(nwpl_e_mtns = case_when(name_origin == "main" & is.na(nwpl_e_mtns) ~ "NA",
                                                  T ~ nwpl_e_mtns)) %>%
  fill(nwpl_e_mtns) %>%
  mutate(nwpl_cstl_plain = case_when(name_origin == "main" & is.na(nwpl_cstl_plain) ~ "NA",
                                 T ~ nwpl_cstl_plain)) %>%
  fill(nwpl_cstl_plain) %>%
  mutate(common_name = case_when(name_origin == "main" & is.na(common_name) ~ "NA",
                                     T ~ common_name)) %>%
  fill(common_name) %>%
  mutate(duration = case_when(name_origin == "main" & is.na(duration ) ~ "NA",
                                 T ~ duration )) %>%
  fill(duration ) %>%
  mutate(physiognomy = case_when(name_origin == "main" & is.na(physiognomy) ~ "NA",
                              T ~ physiognomy)) %>%
  fill(physiognomy)

#southern_coastal
southern_coastal_plain <- southeastern_cols %>%
  select(ID, proper_name, scientific_name, name_origin, family, acronym,
         ave_c_value_southern_coastal_plain, physiognomy,
         duration, common_name, nwpl_cstl_plain, native) %>%
  mutate(fqa_db = "southeastern_southern_coastal_plain_2014") %>%
  rename(c = ave_c_value_southern_coastal_plain) %>%
  rename(w = nwpl_cstl_plain) %>%
  filter(!c == "NA") %>%
  replace_with_na(replace = list(c = "UND"))

#southeastern plains
southeastern_plain <- southeastern_cols %>%
  select(ID, proper_name, scientific_name, name_origin, family, acronym,
         ave_c_value_plains, physiognomy,
         duration, common_name, nwpl_cstl_plain, native) %>%
  mutate(fqa_db = "southeastern_plain_2014") %>%
  rename(c = ave_c_value_plains) %>%
  rename(w = nwpl_cstl_plain) %>%
  filter(!c == "NA") %>%
  filter(!is.na(name_origin)) %>%
  replace_with_na(replace = list(c = "UND"))

#southern piedmont
southeastern_piedmont <- southeastern_cols %>%
  select(ID, proper_name, scientific_name, name_origin, family, acronym,
         ave_c_value_piedmont, physiognomy,
         duration, common_name, nwpl_e_mtns, native) %>%
  mutate(fqa_db = "southeastern_piedmont_2014") %>%
  rename(c = ave_c_value_piedmont) %>%
  rename(w = nwpl_e_mtns) %>%
  filter(!c == "NA") %>%
  replace_with_na(replace = list(c = "UND"))

#southern mointians
southeastern_mountains <- southeastern_cols %>%
  select(ID, proper_name, scientific_name, name_origin, family, acronym,
         ave_c_value_mountains, physiognomy,
         duration, common_name, nwpl_e_mtns, native) %>%
  mutate(fqa_db = "southeastern_mountains_2014") %>%
  rename(c = ave_c_value_mountains) %>%
  rename(w = nwpl_e_mtns) %>%
  filter(!c == "NA") %>%
  replace_with_na(replace = list(c = "UND"))

#southern plat
southeastern_plateau <- southeastern_cols %>%
  select(ID, proper_name, scientific_name, name_origin, family, acronym,
         ave_c_value_interior_plateau, physiognomy,
         duration, common_name, nwpl_e_mtns, native) %>%
  mutate(fqa_db = "southeastern_interior_plateau_2014") %>%
  rename(c = ave_c_value_interior_plateau) %>%
  rename(w = nwpl_e_mtns) %>%
  filter(!c == "NA") %>%
  replace_with_na(replace = list(c = "UND"))

southeastern_complete <- rbind(southeastern_mountains,
                               southeastern_piedmont,
                               southeastern_plain,
                               southeastern_plateau,
                               southern_coastal_plain)

#FOR NEW ENGLAND DBS--------------------------------------------------------------
#
# #create list of file names
# ne_files <- list.files(path = here("data-raw", "FQA_databases", "not_from_universal_calc"),
#                          pattern = "*_2013.csv",
#                          full.names = F)
#
# #read them in and create new col with region
# ne_list <- lapply(ne_files, function(x)
#   readxl::read_xlsx(paste0("./data-raw/FQA_databases/not_from_universal_calc/", x)) %>%
#                     mutate(fqa_db = x))
#
# #bind together
# ne_compiled <- bind_rows(ne_list)
#
# #clean up col names
# ne_clean <- ne_compiled %>%
#   mutate(scientific_name = Taxon) %>%
#   mutate(synonym = TaxaBotanist) %>%
#   mutate(family = NA) %>%
#   mutate(acronym = PLANTSSymbol) %>%
#   mutate(native = "undetermined") %>%
#   mutate(c = as.numeric(Score)) %>%
#   mutate(w = NA) %>%
#   mutate(physiognomy = NA) %>%
#   mutate(duration = NA) %>%
#   mutate(common_name = CommonName) %>%
#   select(scientific_name, synonym, family, acronym,
#          native, c, w, physiognomy, duration, common_name, fqa_db) %>%
#   distinct() %>%
#   mutate(ID = row_number()) %>%
#   #make sure to delete dups. if there are dups with different c scores, pick lowest score
#   group_by(fqa_db, scientific_name, acronym) %>%
#   slice_min(n = 1, order_by = c)


#CHICAGO------------------------------------------------------------------------


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
         c, w, physiognomy, duration, common_name, fqa_db) %>%
  mutate(acronym = case_when(acronym == "Betula X sandbergii" ~ "ARAPYCA",
                             T ~ acronym)) %>%
  group_by(scientific_name) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup() %>%
  cSplit(., 'synonym', ';') %>%
  mutate(synonym_1 = case_when(tolower(synonym_1) == tolower(scientific_name) ~ NA_character_,
                               T ~ synonym_1))

#list of syn columns
syn_cols <- c("synonym_1", "synonym_2", "synonym_3", "synonym_4", "synonym_5", "synonym_6", "synonym_7")


chic_piv <- chicago_clean %>%
  mutate(proper_name = scientific_name) %>%
  pivot_longer(cols = c("scientific_name", all_of(syn_cols)),
               names_to = "name_origin",
               values_to = "scientific_name") %>%
  filter(!is.na(scientific_name)) %>%
  distinct(scientific_name, name_origin, ID, .keep_all = TRUE)

# chic_dup <- chic_piv %>%
#   group_by(acronym) %>%
#   count()

#COLORADO-----------------------------------------------------------------------

colorado <- read_xlsx(here("data-raw",
                         "FQA_databases",
                         "not_from_universal_calc",
                         "colorado_2020.xlsx")) %>%
  clean_names()

colorado_clean <- colorado %>%
  mutate(scientific_name = fqa_sci_name_no_authority) %>%
  mutate(synonym = national_sci_name_no_authority) %>%
  mutate(ID = row_number()) %>%
  mutate(family = fqa_family) %>%
  mutate(acronym = fqa_usda_symbol) %>%
  mutate(native = fqa_native_status) %>%
  mutate(c = fqa_c_value2020_numeric) %>%
  mutate(w = wmvc_wet_indicator) %>%
  mutate(physiognomy = usda_growth_habit_simple) %>%
  mutate(duration = usda_duration) %>%
  mutate(common_name = NA) %>%
  mutate(fqa_db = "colorado_2020") %>%
  select(scientific_name, synonym, ID, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db) %>%
  #if sci name and syn name match, delete syn
  mutate(synonym = case_when(scientific_name == synonym ~ NA_character_, T ~ synonym)) %>%
  #remove genus with no C score
  mutate(remove_me = case_when(is.na(c) & str_detect(scientific_name, " ", negate = TRUE) ~ "remove")) %>%
  filter(is.na(remove_me)) %>%
  select(-remove_me)

colorado_pivot <- colorado_clean %>%
  mutate(proper_name = scientific_name) %>%
  pivot_longer(cols = c("scientific_name", "synonym"),
               names_to = "name_origin",
               values_to = "scientific_name") %>%
  filter(!is.na(scientific_name)) %>%
  mutate(acronym = case_when(name_origin == "synonym" ~ NA_character_,
                             T ~ acronym))

# colorado_dup <- colorado_pivot %>%
#   group_by(scientific_name, name_origin) %>%
#   count()

#FLORIDA------------------------------------------------------------------------


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
  mutate(c = as.numeric(c_of_c_value)) %>%
  mutate(w = NA) %>%
  mutate(physiognomy = NA) %>%
  mutate(common_name = NA) %>%
  mutate(fqa_db = "florida_2011") %>%
  select(scientific_name, synonym, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db)

florida_pivot <- florida_clean %>%
  separate(scientific_name, into = c("scientific_name", "synonym"), sep = "syn.") %>%
  mutate(ID = row_number()) %>%
  mutate(scientific_name = case_when(scientific_name == "Eleocharis (submersed viviparous but unable to ID to species)" ~ "Eleocharis sp.",
                                     T ~ scientific_name)) %>%
  mutate(scientific_name = str_remove_all(scientific_name, "[()]")) %>%
  mutate(synonym = str_remove_all(synonym, "[()]")) %>%
  mutate(proper_name = scientific_name) %>%
  pivot_longer(cols = c("scientific_name", "synonym"),
               names_to = "name_origin",
               values_to = "scientific_name") %>%
  filter(!is.na(scientific_name))


#FLORIDA_SOUTH-------------------------------------------------------------------


florida_south <- read_csv(here("data-raw",
                         "FQA_databases",
                         "not_from_universal_calc",
                         "florida_south_2009.csv")) %>%
  clean_names()

florida_south_clean <- florida_south %>%
  mutate(name_origin = "scientific_name") %>%
  mutate(proper_name = scientific_name) %>%
  mutate(ID = row_number()) %>%
  mutate(acronym = NA) %>%
  mutate(c = as.numeric(c)) %>%
  mutate(w = NA) %>%
  mutate(physiognomy = NA) %>%
  mutate(duration = NA) %>%
  mutate(fqa_db = "florida_south_2009") %>%
  select(scientific_name, proper_name, name_origin, ID, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db) %>%
  mutate(scientific_name = str_replace_all(scientific_name, "subsp.", "ssp."))

#MISSISSISSIPPI------------------------------------------------------------------


ms <- read_xlsx(here("data-raw",
                     "FQA_databases",
                     "not_from_universal_calc",
                     "mississippi_north_central_wetlands_2005.xlsx")) %>%
  clean_names()

ms_clean <- ms %>%
  mutate(scientific_name = species) %>%
  mutate(name_origin = "scientific_name") %>%
  mutate(proper_name = scientific_name) %>%
  mutate(ID = row_number()) %>%
  mutate(acronym = NA) %>%
  mutate(native = origin) %>%
  mutate(c = ave_cc) %>%
  mutate(w = wetland_indicator_status) %>%
  mutate(physiognomy = physiogynomy) %>%
  mutate(duration = x9) %>%
  mutate(duration = case_when(duration == "A" ~ "annual",
                              duration == "P" ~ "perennial",
                              T ~ duration)) %>%
  mutate(common_name = common) %>%
  mutate(fqa_db = "mississippi_north_central_wetlands_2005") %>%
  select(scientific_name, proper_name, name_origin, ID, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db)


#MONTANA------------------------------------------------------------------------

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
  mutate(c = as.numeric(montana_c_value)) %>%
  mutate(w = NA) %>%
  mutate(physiognomy = NA) %>%
  mutate(duration = NA) %>%
  mutate(fqa_db = "montana_2017") %>%
  select(scientific_name, synonym, family, acronym, native,
         c, w, physiognomy, duration, common_name, fqa_db) %>%
  mutate(synonym = case_when(scientific_name == "Eriogonum brevicaule var. canum"
                             ~ "Eriogonum lagopus; Eriogonum pauciflorum var. canum",
                             T ~ synonym)) %>%
  distinct()

#list of syn columns
syn_cols <- c("synonym_1", "synonym_2", "synonym_3", "synonym_4", "synonym_5", "synonym_6", "synonym_7")

montana_pivot <- montana_clean %>%
  mutate(ID = row_number()) %>%
  mutate(synonym = str_remove_all(synonym, "\\[.*\\]")) %>%
  cSplit(., 'synonym', ';') %>%
  mutate(proper_name = scientific_name) %>%
  pivot_longer(cols = c("scientific_name", syn_cols),
               names_to = "name_origin",
               values_to = "scientific_name") %>%
  filter(!is.na(scientific_name))

# montana_dups <- montana_pivot %>%
#   group_by(scientific_name, name_origin) %>%
#   count()

#OHIO--------------------------------------------------------------------------

ohio <- read_xlsx(here("data-raw",
                       "FQA_databases",
                       "not_from_universal_calc",
                       "ohio_2014.xlsx")) %>%
  clean_names() %>%
  #removing random mostly empty rows
  filter(usda_id != "CAREX")

ohio_clean <- ohio %>%
  mutate(name_origin = "scientific_name") %>%
  mutate(ID = row_number()) %>%
  mutate(proper_name = scientific_name) %>%
  mutate(native = oh_status) %>%
  mutate(c = cofc) %>%
  mutate(w = wet) %>%
  mutate(physiognomy = form) %>%
  mutate(duration = habit) %>%
  mutate(fqa_db = "ohio_2014") %>%
  select(scientific_name, proper_name, name_origin, ID, family, acronym, native, c, w,
         physiognomy, duration, common_name, fqa_db) %>%
  distinct() %>%
  mutate(remove_me = case_when(is.na(c) & str_detect(scientific_name, " sp.") ~ "remove")) %>%
  filter(is.na(remove_me)) %>%
  select(-remove_me) %>%
  mutate(acronym = case_when(scientific_name == "Symphyotrichum laeve" ~ "SYNLAE",
                     scientific_name == "Solidago speciosa Nutt. var. rigidiuscula" ~ "SOLSPR",
                     scientific_name == "Cuscuta epithymum" ~ "CUSEPT",
                     scientific_name == "Collinsonia verticillata" ~ "COLVET",
                     scientific_name == "Chenopodium glaucum" ~ "CHEGLU", T ~ acronym))

# ohio_dubs <- ohio_clean %>%
#   group_by(scientific_name) %>%
#   count()

#WYOMING------------------------------------------------------------------------

wyoming <- read_xlsx(here("data-raw",
                          "FQA_databases",
                          "not_from_universal_calc",
                          "wyoming_2017.xlsx"), skip = 1) %>%
  clean_names()

wyoming_cols <- wyoming %>%
  mutate(family = family_scientific_name) %>%
  mutate(synonym = synonyms) %>%
  mutate(acronym = NA) %>%
  mutate(native = statewide_origin) %>%
  mutate(c = wyoming_coefficient_of_conservatism) %>%
  mutate(w = wetland_indicator_status_arid_west) %>%
  mutate(physiognomy = NA) %>%
  mutate(duration = NA) %>%
  mutate(fqa_db = "wyoming_2017") %>%
  select(scientific_name, synonym, family, acronym, native, c, w, physiognomy, duration, common_name, fqa_db) %>%
  slice(., 1:(n() - 1))

wyoming_pivot <- wyoming_cols %>%
  cSplit(., 'synonym', ',') %>%
  mutate(ID = row_number()) %>%
  mutate(proper_name = scientific_name) %>%
  pivot_longer(cols = c("scientific_name", starts_with("synonym_")),
               names_to = "name_origin",
               values_to = "scientific_name") %>%
  filter(!is.na(scientific_name))

# wyoming_dups <- wyoming_pivot %>%
#   group_by(scientific_name, name_origin) %>%
#   count()

#NOW CLEANING ALL TOGETHER-----------------------------------------------------

#bind all together
fqa_db_bind <- rbind(syn_dist_acronyms,
                     southeastern_complete,
                     #ne_clean,
                     colorado_pivot,
                     chic_piv,
                     florida_pivot,
                     florida_south_clean,
                     ms_clean,
                     montana_pivot,
                     ohio_clean,
                     wyoming_pivot
                     ) %>%
  #remove csv from end of fqa_db column
  mutate(fqa_db = str_remove_all(fqa_db, ".csv")) %>%
  #covert things to uppercase
  mutate(scientific_name = toupper(scientific_name)) %>%
  rename(name = scientific_name)

#get unique values to clean
unique_native <- data.frame(unique(fqa_db_bind$native))
unique_w <- data.frame(unique(fqa_db_bind$w))
unique_physiog <- data.frame(unique(fqa_db_bind$physiognomy))
unique_duration <- data.frame(unique(fqa_db_bind$duration))


#cleaning up native column
fqa_native <- fqa_db_bind %>%
  mutate(native = case_when(str_detect(native, "L48 \\(N\\)") ~ "native",
                            str_detect(native, "L48 \\(NI\\)") ~ "native",
                            str_detect(native, "L48 \\(I\\)") ~ "non-native",
                            T ~ native)) %>%
  mutate(native = case_when(native %in% c("native",
                                          "Native",
                                          "N",
                                          "Native/Naturalized",
                                          "Native/Adventive",
                                          "Likely Native",
                                          "Native/Exotic") ~ "native",
                            native %in% c("non-native",
                                          "Nonnative",
                                          "cryptogenic",
                                          "adventive",
                                          "Likely Exotic",
                                          "I",
                                          "Exotic",
                                          "Adventive",
                                          "Cryptogenic",
                                          "Non-native") ~ "non-native",
                            T ~ "undetermined")) %>%
  rename(nativity = native)

#cleaning up wet coef column
fqa_wet <- fqa_native %>%
  mutate(w = str_remove_all(w, "[()]")) %>%
  mutate(w = case_when(w %in% c("NA", "ND", "NI") ~ NA_character_,
                       w %in% c("UPL") ~ "5",
                       w %in% c("FACU", "FACU-", "FACU+") ~ "3",
                       w %in% c("FAC", "FAC-", "FAC+") ~ "0",
                       w %in% c("FACW", "FACW-", "FACW+") ~ "-3",
                       w %in% c("OBL") ~ "-5",
                       T ~ w)) %>%
  mutate(w = as.numeric(w))

#cleaning physiog column
fqa_physiog <- fqa_wet %>%
  mutate(physiognomy = tolower(physiognomy)) %>%
  mutate(physiognomy = str_remove(physiognomy, ",.*")) %>%
  mutate(physiognomy = str_remove(physiognomy, "\\/.*")) %>%
  mutate(physiognomy = str_replace(physiognomy, "shurb", "shrub")) %>%
  mutate(physiognomy = str_replace(physiognomy, "sm tree", "tree")) %>%
  mutate(physiognomy = str_replace(physiognomy, "subshrub", "shrub")) %>%
  mutate(physiognomy = str_replace(physiognomy, "frob", "forb")) %>%
  mutate(physiognomy = str_replace(physiognomy, "graminoid", "grass")) %>%
  mutate(physiognomy = str_replace(physiognomy, "gram", "grass")) %>%
  mutate(physiognomy = str_replace(physiognomy, "h-vine", "vine")) %>%
  mutate(physiognomy = str_replace(physiognomy, "w-vine", "vine")) %>%
  mutate(physiognomy = str_replace(physiognomy, "^bryo$", "bryophyte"))

#cleaning up duration column
fqa_duration <- fqa_physiog %>%
  mutate(duration = tolower(duration)) %>%
  mutate(duration = str_replace(duration, "n\\/a \\(non-vascular\\)", "none")) %>%
  mutate(duration = str_remove(duration, ",.*")) %>%
  mutate(duration = str_remove(duration, "\\/.*")) %>%
  mutate(duration = str_replace(duration, "^an$", "annual")) %>%
  mutate(duration = str_replace(duration, "^w$", "perennial")) %>%
  mutate(duration = str_replace(duration, "^pe$", "perennial")) %>%
  mutate(duration = str_replace(duration, "^bi$", "biennial")) %>%
  mutate(duration = str_replace(duration, "^br$", "none")) %>%
  mutate(duration = str_replace(duration, "^nd$", NA_character_))

#cleaning up name_origin column
fqa_origin <- fqa_duration %>%
  mutate(name_origin = case_when(str_detect(name_origin, "synonym") ~ "synonym",
                                 name_origin %in% c("scientific_name", "main") ~ "proper_name",
                                 T ~ name_origin))


#   #clean commmon name
#   mutate(common_name = str_to_title(common_name)) %>%
#   #clean C Value
#   mutate(c = as.numeric(c)) %>%
#   mutate(scientific_name = case_when(scientific_name == "BOTRYCHIUM SP. (NON-SOC)" ~
#                                        "BOTRYCHIUM SP. NON-SOC",
#                                      scientific_name == "BOTRYCHIUM SP. (SOC)" ~
#                                        "BOTRYCHIUM SP. SOC",
#                                      T ~ scientific_name))
#
#
# #cleaning latin names
# fqa_db_latin <- fqa_db_clean_cols %>%
#   #making separators consistent
#   mutate(scientific_name = str_replace_all(scientific_name, "_", " ")) %>%
#   mutate(scientific_name = str_replace_all(scientific_name, "SYN. ", "; ")) %>%
#   mutate(scientific_name = str_replace_all(scientific_name, "[{]", ";")) %>%
#   mutate(scientific_name = str_replace_all(scientific_name, "=", ";")) %>%
#   mutate(scientific_name = str_replace_all(scientific_name, "[(]", ";")) %>%
#   mutate(scientific_name = str_replace_all(scientific_name, "[\\[]", ";")) %>%
#   #making sure abbreviations are consistent
#   mutate(scientific_name = str_replace_all(scientific_name, " SUBSP. ", " SSP. ")) %>%
#   mutate(scientific_name = str_replace_all(scientific_name, " VAR ", " VAR. ")) %>%
#   mutate(scientific_name = str_replace_all(scientific_name, " V. ", " VAR. ")) %>%
#   mutate(scientific_name = str_replace_all(scientific_name, "�", "X ")) %>%
#   #fixing white spaces
#   mutate(scientific_name = str_squish(scientific_name)) %>%
#   mutate(scientific_name = str_trim(scientific_name, side = "both")) %>%
#   mutate(scientific_name = case_when(!str_detect(scientific_name, pattern = " ") ~
#                                        paste(scientific_name, "SP."),
#                                      T ~ scientific_name)) %>%
#   #fixing odds and ends
#   mutate(scientific_name = case_when(scientific_name == "APOCYNUM XFLORIBUNDUM" ~
#                                        "APOCYNUM X FLORIBUNDUM",
#                                      scientific_name == "BETULA XCAERULEA" ~
#                                        "BETULA X CAERULEA",
#                                      scientific_name == "MENTHA X X VERTICILLATA" ~
#                                        "MENTHA X VERTICILLATA",
#                                      scientific_name == "SPARTINA XCAESPITOSA" ~
#                                        "SPARTINA X CAESPITOSA",
#                                      scientific_name == "SCUTELLARIA XCHURCHILLIANA" ~
#                                        "SCUTELLARIA X CHURCHILLIANA",
#                                      scientific_name == "SCIRPUS XPECKII" ~
#                                        "SCIRPUS X PECKII",
#                                      scientific_name == "POTAMOGETON XMYSTICUS" ~
#                                        "POTAMOGETON X MYSTICUS",
#                                      scientific_name == "DRYOPTERIS XULIGINOSA" ~
#                                        "DRYOPTERIS X ULIGINOSA",
#                                      scientific_name == "ACER XFREEMANII" ~
#                                        "ACER X FREEMANII",
#                                      scientific_name == "QUERCUS XSUBFALCATA" ~
#                                        "QUERCUS X SUBFALCATA",
#                                      scientific_name == "QUERCUS XSAULII" ~
#                                        "QUERCUS X SAULII",
#                                      scientific_name == "QUERCUS XRUDKINII" ~
#                                        "QUERCUS X RUDKINII",
#                                      scientific_name == "QUERCUS XHETEROPHYLLA" ~
#                                        "QUERCUS X HETEROPHYLLA",
#                                      scientific_name == "QUERCUS XGIFFORDII" ~
#                                        "QUERCUS X GIFFORDII",
#                                      scientific_name == "QUERCUS XFILIALIS" ~
#                                        "QUERCUS X FILIALIS",
#                                      scientific_name == "QUERCUS XFERNOWII" ~
#                                        "QUERCUS X FERNOWII",
#                                      scientific_name == "QUERCUS XBEADLEI" ~
#                                        "QUERCUS X BEADLEI",
#                                      scientific_name == "POPULUS XJACKII" ~
#                                        "POPULUS X JACKII",
#                                      scientific_name == "PLATANTHERA XCANBYI" ~
#                                        "PLATANTHERA X CANBYI",
#                                      scientific_name == "PETUNIA XHYBRIDA" ~
#                                        "PETUNIA X HYBRIDA",
#                                      scientific_name == "LYCOPODIELLA XCOPELANDII" ~
#                                        "LYCOPODIELLA X COPELANDII",
#                                      scientific_name == "ILEX XATTENUATA" ~
#                                        "ILEX X ATTENUATA",
#                                      scientific_name == "DRYOPTERIS XBOOTTII" ~
#                                        "DRYOPTERIS X BOOTTII",
#                                      scientific_name == "DICHANTHELIUM XSCOPARIOIDES; PANICUM SCOPARIOIDES; P. VILLOSISSIMUM VAR. SCOPARIOIDES" ~
#                                        "DICHANTHELIUM X SCOPARIOIDES; PANICUM SCOPARIOIDES; P. VILLOSISSIMUM VAR. SCOPARIOIDES",
#                                      scientific_name == "KALANCHOE XHOUGHTONII" ~
#                                        "KALANCHOE X HOUGHTONII",
#                                      scientific_name == "ARABIS XDIVARICARPA" ~
#                                        "ARABIS X DIVARICARPA",
#                                      scientific_name == "AMELANCHIER X INTERMEDIA" ~
#                                        "AMELANCHIER X INTERMEDIA",
#                                      scientific_name == "VIOLA XBERNARDII" ~
#                                        "VIOLA X BERNARDII",
#                                      scientific_name == "VIOLA XPRIMULIFOLIA" ~
#                                        "VIOLA X PRIMULIFOLIA",
#                                      scientific_name == "VIOLA XPALMATA" ~
#                                        "VIOLA X PALMATA",
#                                      scientific_name == "SOLIDAGO XASPERULA" ~
#                                        "SOLIDAGO X ASPERULA",
#                                      scientific_name == "SALIX XSEPULCRALIS" ~
#                                        "SALIX X SEPULCRALIS",
#                                      scientific_name == "SALIX XPENDULINA" ~
#                                        "SALIX X PENDULINA",
#                                      scientific_name == "OCLEMENA XBLAKEI" ~
#                                        "OCLEMENA X BLAKEI",
#                                      scientific_name == "DRYOPTERIS XTRIPLOIDEA" ~
#                                        "DRYOPTERIS X TRIPLOIDEA",
#                                      scientific_name == "VITIS XNOVAE-ANGLIAE"~
#                                        "VITIS X NOVAE-ANGLIAE",
#                                      scientific_name == "SOLIDAGO XCALCICOLA" ~
#                                        "SOLIDAGO X CALCICOLA",
#                                      scientific_name == "SALIX XRUBENS" ~
#                                        "SALIX X RUBENS",
#                                      scientific_name == "MENTHA XPIPERITA" ~
#                                        "MENTHA X PIPERITA",
#                                      scientific_name == "MENTHA XGRACILIS" ~
#                                        "MENTHA X GRACILIS",
#                                      scientific_name == "HYPERICUM XDISSIMULATUM" ~
#                                        "HYPERICUM X DISSIMULATUM",
#                                      scientific_name == "BETULA XCAERULEA VAR. GRANDIS" ~
#                                        "BETULA X CAERULEA VAR. GRANDIS",
#                                      scientific_name == "ELEOCHARIS ACICULARIS / WILL SUGGESTS REMOVING, MAY BE MORE MONTANE" ~
#                                        "ELEOCHARIS ACICULARIS",
#                                      scientific_name == "PHYTOLACCA AMERICANA VAR, AMERICANA" ~
#                                        "PHYTOLACCA AMERICANA VAR. AMERICANA",
#                                      scientific_name == "SALVINIA SPP." ~
#                                       " SALVINIA SP.",
#                                      scientific_name == "CARDARIA DRABA, LEPIDIUM DRABA" ~
#                                        "CARDARIA DRABA; LEPIDIUM DRABA",
#                                      scientific_name == "TYPHA XGLAUCA" ~
#                                        "TYPHA X GLAUCA",
#                                      T ~ scientific_name)) %>%
#   #separating by semicolon
#   separate(scientific_name, c("scientific_name", "synonym1"), ";", extra = "merge")
#


#sort data frame column alphabetically
fqa_db_cols <- fqa_origin[order(fqa_origin$fqa_db), ]

#get desired column order
fqa_db <- fqa_db_cols %>%
  select(name, name_origin, acronym, proper_name, everything()) %>%
  mutate(c = as.numeric(c)) %>%
  select(-ID)


#-------------------------------------------------------------------------------
#saving dataset MAKE SURE IT IS CLEAN VERSION!!!

#use this dataset  (not viewable to package user)
usethis::use_data(fqa_db, overwrite = TRUE, internal = TRUE, compress = "bzip2")

#-------------------------------------------------------------------------------

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
