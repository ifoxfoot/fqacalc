#testing accepted_entries() errors---------------------------------------------

test_that("accepted_entries() errors work for missing arguments", {
  expect_error(accepted_entries(key = "acronym", db = "michigan_2014", native = F),
               "argument x is missing, with no default.")
  expect_error(accepted_entries(crooked_island, key = "acronym", native = F),
               "argument db is missing, with no default.")
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014"),
               "argument native is missing, with no default.")
})

test_that("accepted_entries() throws error if x is not a df", {
  expect_error(accepted_entries(character_string, key = "acronym", db = "michigan_2014", native = F),
               "character_string must be a data frame")
})

test_that("accepted_entries() throws error if key is not in df", {
  expect_error(accepted_entries(crooked_island, key = "acronym1", db = "michigan_2014", native = F),
               "'key' argument must be equal to 'acronym' or 'name'.")

  expect_error(accepted_entries(bad_names, key = "acronym", db = "michigan_2014", native = F),
               "bad_names does not have a column named acronym.")
})

test_that("accepted_entries() throws error if db is not recognized", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan", native = F),
               "michigan not recognized.")
})

test_that("accepted_entries() throws error if logical agruments aren't logical", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = "yes"),
               "'native' can only be set to TRUE or FALSE")
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = T,
                                cover_weighted = "percent"),
               "'cover_weighted' can only be set to TRUE or FALSE")
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = T,
                                allow_duplicates = "percent"),
               "'allow_duplicates' can only be set to TRUE or FALSE")
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = T,
                                allow_no_c = "percent"),
               "'allow_no_c' can only be set to TRUE or FALSE")
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = T,
                                allow_non_veg = "percent"),
               "'allow_non_veg' can only be set to TRUE or FALSE")
})

test_that("accepted_entries() throws error if cover is not a column in x", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = F, cover_weighted = T),
               "If 'cover = TRUE', crooked_island must have a column named cover.")
})

test_that("accepted_entries() throws error if cover contains NAs", {
  expect_error(accepted_entries(crooked_island %>% dplyr::mutate(cover = NA), key = "acronym", db = "michigan_2014", native = F, cover_weighted = T),
               "'cover' column cannot contain missing values.")
})

test_that("accepted_entries() throws error if cover_class isn't right", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = F,
                                cover_class = "percent"),
               "percent is not an accepted cover-method. See function documentation.")
})

test_that("accepted_entries() throws error if plot_id isn't a column", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = F,
                                plot_id = "percent"),
               "'plot_id' must be the name of a column in crooked_island.")
})

test_that("accepted_entries() throws error if key = acronym and db does not have complete acronyms", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "florida_2011", native = F),
               "florida_2011 does not have a complete set of acronyms, please set key equal to 'name'.")
})

#testing accepted_entries() messages-------------------------------------------

test_that("accepted_entries() throws a message if duplicates are detected-noncover", {
  expect_message(accepted_entries(duplicate, key = "acronym", db = "michigan_2014", native = F),
               "Duplicate entries detected. Duplicates will only be counted once.")
})

test_that("accepted_entries() throws a message if duplicates are detected-cover", {
  expect_message(accepted_entries(transect, key = "acronym", db = "michigan_2014", native = F, cover_weighted = TRUE),
                 "Duplicate entries detected. Duplicates will only be counted once. Cover values of duplicate species will be added together.")
})

test_that("accepted_entries() throws a message if duplicates are detected in same plot-cover", {
  expect_message(accepted_entries(transect_dup, key = "acronym", db = "michigan_2014", native = F, plot_id = "quad_id",
                                  allow_duplicates = TRUE, allow_non_veg = TRUE),
                 "Duplicate entries detected in the same plot. Duplicates in the same plot will be counted once. Cover values of duplicate species will be added together.")
})

test_that("accepted_entries() throws a message if species not recognized", {
  expect_message(accepted_entries(typo, key = "acronym", db = "michigan_2014", native = F),
                 "Species TYPO not listed in database. It will be discarded.")
})

test_that("accepted_entries() throws a message if species does not have C Value", {
  expect_message(accepted_entries(no_c_test, key = "name", db = "montana_2017", native = F),
                 "Species ABRONIA FRAGRANS is recognized but has not been assigned a C Value.")
})

test_that("accepted_entries() throws warning if nas are introduced to cover", {
  expect_message(accepted_entries(na_intro_cover, key = "acronym", db = "michigan_2014", native = F,
                                  cover_weighted = TRUE, cover_class = "braun-blanquet"),
                 "NAs were introduced during the conversion to the braun-blanquet system. Are you using the right cover class?")
})

test_that("accepted_entries() throws warning if nas are introduced to cover", {
  expect_message(accepted_entries(na_intro_cover, key = "acronym", db = "michigan_2014", native = F,
                                  cover_weighted = TRUE, cover_class = "percent_cover"),
                 "NAs were introduced during the conversion to the percent_cover system. Species with NA cover values will be removed.")
})

test_that("accepted_entries() throws a message if two species entered have same id", {
  expect_message(accepted_entries(same_id, db = "wyoming_2017", native = F, allow_duplicates = T),
                 "Species ABIES BIFOLIA, ABIES LASIOCARPA are synonyms and will be treated as one species.")
})

test_that("accepted_entries() throws a message if two species entered have same id", {
  expect_message(accepted_entries(same_id, db = "wyoming_2017", native = F, allow_duplicates = F, cover_weighted = T),
                 "Species ABIES BIFOLIA, ABIES LASIOCARPA are synonyms and will be treated as one species.  If allow_duplicates = FALSE, cover values of synonyms will be added together.")
})

test_that("accepted_entries() throws a message if one species entered returns two + matches and both are syn", {
  expect_message(accepted_entries(same_syn, db = "wyoming_2017", native = F, allow_duplicates = T),
                 "CAREX MURICATA is a synonym to multiple species. It will be omitted. To include this species, use the accepted scientific name.")

  expect_message(accepted_entries(same_syn, db = "wyoming_2017", native = F, allow_duplicates = T),
                 "POTENTILLA NANA is a synonym to multiple species. It will be omitted. To include this species, use the accepted scientific name.")
})

test_that("accepted_entries() throws a message if one species entered returns two + matches and both are syn", {
  expect_message(accepted_entries(same_syn_sci, db = "wyoming_2017", native = F, allow_duplicates = T),
                 "CAREX FOENEA is an accepted scientific name and a synonym. It will default to accepted scientific name.")
})

#testing accepted_entries() results---------------------------------------------

#testing normal result
test_that("accepted_entries works in perfect setting", {
  expect_equal(accepted_entries(x = accepted_perfect, key = "acronym", db = "michigan_2014", native = FALSE),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae"),
                          nativity = c("introduced", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))

})

#testing native result
test_that("accepted_entries works in perfect setting with native = TRUE", {
  expect_equal(accepted_entries(x = accepted_perfect, key = "acronym", db = "michigan_2014", native = TRUE),

               data.frame(acronym = c("ABIBAL"),
                          name = c("ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name"),
                          accepted_scientific_name = c("Abies balsamea"),
                          family = c("Pinaceae"),
                          nativity = c("native"),
                          c = c(3),
                          w = c(0),
                          wetland_indicator = c(NA_character_),
                          physiognomy = c("tree"),
                          duration = c("perennial"),
                          common_name = c("Balsam Fir"),
                          fqa_db = c("michigan_2014")))
})

#testing cover-weighted result (percent cover)
test_that("accepted_entries works in perfect setting with cover_weighted = TRUE", {
  expect_equal(accepted_entries(x = accepted_perfect, key = "acronym", db = "michigan_2014",
                                native = FALSE, cover_weighted = TRUE),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          cover = c(60, 50),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae"),
                          nativity = c("introduced", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing accepted_entries() cover methods---------------------------------------

#testing cover-weighted result (braun-blanquet)
test_that("accepted_entries works with br-bl", {
  expect_equal(accepted_entries(x = accepted_br, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                allow_duplicates = TRUE,
                                cover_weighted = TRUE,
                                cover_class = "braun-blanquet"),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          cover = c(87.5, 0.1),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae"),
                          nativity = c("introduced", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing br cover value when dups need to be added together
test_that("accepted_entries works with br-bl and dups in same plot", {
  expect_equal(accepted_entries(x = accepted_br_dup, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                allow_duplicates = TRUE,
                                cover_weighted = TRUE,
                                cover_class = "braun-blanquet",
                                plot_id = "quad_id"),

               data.frame(quad_id = c(1,2),
                          acronym = c("ABEESC", "ABIBAL"),
                          cover = c(87.5, 37.6),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae"),
                          nativity = c("introduced", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing cover-weighted result (carolina)
test_that("accepted_entries works with carolina cover method", {
  expect_equal(accepted_entries(x = accepted_cover_method, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                cover_weighted = TRUE,
                                cover_class = "carolina_veg_survey"),

               data.frame(
                          acronym = c("ABEESC", "ABIBAL"),
                          cover = c(0.1, 1.5),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae"),
                          nativity = c("introduced", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing cover-weighted result (daubenmire)
test_that("accepted_entries works with daubenmire cover method", {
  expect_equal(accepted_entries(x = accepted_cover_method, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                cover_weighted = TRUE,
                                cover_class = "daubenmire"),

               data.frame(
                 acronym = c("ABEESC", "ABIBAL"),
                 cover = c(2.5, 37.5),
                 name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                 name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                 accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                 family = c("Malvaceae", "Pinaceae"),
                 nativity = c("introduced", "native"),
                 c = c(0, 3),
                 w = c(5, 0),
                 wetland_indicator = c(NA_character_, NA_character_),
                 physiognomy = c("forb", "tree"),
                 duration = c("annual", "perennial"),
                 common_name = c("Okra; Gumbo", "Balsam Fir"),
                 fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing cover-weighted result (usfs)
test_that("accepted_entries works with usfs method", {
  expect_equal(accepted_entries(x = accepted_cover_method, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                cover_weighted = TRUE,
                                cover_class = "usfs_ecodata"),

               data.frame(
                 acronym = c("ABEESC", "ABIBAL"),
                 cover = c(0.5, 3),
                 name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                 name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                 accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                 family = c("Malvaceae", "Pinaceae"),
                 nativity = c("introduced", "native"),
                 c = c(0, 3),
                 w = c(5, 0),
                 wetland_indicator = c(NA_character_, NA_character_),
                 physiognomy = c("forb", "tree"),
                 duration = c("annual", "perennial"),
                 common_name = c("Okra; Gumbo", "Balsam Fir"),
                 fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing accepted_entries() duplicates---------------------------------------------

#testing allow_duplicate result
test_that("accepted_entries works in perfect setting with allow_duplicates = TRUE", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                allow_duplicates = TRUE),

               data.frame(acronym = c("ABEESC", "ABIBAL", "ABIBAL"),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae", "Pinaceae"),
                          nativity = c("introduced", "native", "native"),
                          c = c(0, 3, 3),
                          w = c(5, 0, 0),
                          wetland_indicator = c(NA_character_, NA_character_, NA_character_),
                          physiognomy = c("forb", "tree", "tree"),
                          duration = c("annual", "perennial", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014", "michigan_2014")))
})

#testing allow_duplicate result when plot_id is set
test_that("accepted_entries with allow_duplicates = TRUE, plot_id set", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014", native = FALSE,
                                allow_duplicates = TRUE, plot_id = "quad_id"),

               data.frame(quad_id = c(1, 2),
                          acronym = c("ABEESC", "ABIBAL"),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae"),
                          nativity = c("introduced", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing don't allow_duplicate result when plot_id and cover-weighted are set
test_that("accepted_entries with allow_duplicates = TRUE, cover_weighted = TRUE, plot id is set", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014", native = FALSE,
                                cover_weighted = TRUE, allow_duplicates = TRUE, plot_id = "quad_id"),

               data.frame(quad_id = c(1, 2),
                          acronym = c("ABEESC", "ABIBAL"),
                          cover = c(60, 100),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae"),
                          nativity = c("introduced", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing don't allow_duplicate result cover weighted
test_that("accepted_entries with allow_duplicates = FALSE, cover_weighted = TRUE", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014", native = FALSE,
                                cover_weighted = TRUE, allow_duplicates = FALSE, plot_id = NULL),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          cover = c(60, 100),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae"),
                          nativity = c("introduced", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing don't allow_duplicate result non-cover weighted
test_that("accepted_entries with allow_duplicates = FALSE, cover_weighted = FALSE", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014", native = FALSE,
                                cover_weighted = FALSE, allow_duplicates = FALSE),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus", "Abies balsamea"),
                          family = c("Malvaceae", "Pinaceae"),
                          nativity = c("introduced", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("Okra; Gumbo", "Balsam Fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing accepted_entries() no C---------------------------------------------

#testing allow_non_c
test_that("accepted_entries with allow_no_c = TRUE", {
  expect_equal(accepted_entries(x = accepted_no_c, db = "montana_2017", native = FALSE, allow_no_c = TRUE),

               data.frame(name = c("ABRONIA FRAGRANS", "ACER GLABRUM"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          acronym = c(NA_character_, NA_character_),
                          accepted_scientific_name = c("Abronia fragrans", "Acer glabrum"),
                          family = c("Nyctaginaceae", "Aceraceae"),
                          nativity = c("native", "native"),
                          c = c(NA, 4),
                          w = c(NA_real_, NA_real_),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c(NA_character_, NA_character_),
                          duration = c(NA_character_, NA_character_),
                          common_name = c("Fragrant White Sand-Verbena", "Rocky Mountain Maple"),
                          fqa_db = c("montana_2017", "montana_2017")))
})

#testing don't allow_non_c
test_that("accepted_entries with allow_no_c = FALSE", {
  expect_equal(accepted_entries(x = accepted_no_c, db = "montana_2017", native = FALSE, allow_no_c = FALSE),

               data.frame(name = c("ACER GLABRUM"),
                          name_origin = c("accepted_scientific_name"),
                          acronym = c(NA_character_),
                          accepted_scientific_name = c("Acer glabrum"),
                          family = c("Aceraceae"),
                          nativity = c("native"),
                          c = c(4),
                          w = c(NA_real_),
                          wetland_indicator = c(NA_character_),
                          physiognomy = c(NA_character_),
                          duration = c(NA_character_),
                          common_name = c("Rocky Mountain Maple"),
                          fqa_db = c("montana_2017")))
})

#testing accepted_entries() non-veg---------------------------------------------

#testing using non-veg
test_that("accepted_entries with allow_non_veg = TRUE", {
  expect_equal(accepted_entries(x = accepted_non_veg, key = "acronym", db = "michigan_2014", native = FALSE,
                                allow_non_veg = TRUE),

               data.frame(acronym = c("ABEESC", "GROUND", "WATER"),
                          name = c("ABELMOSCHUS ESCULENTUS", "UNVEGETATED GROUND", "UNVEGETATED WATER"),
                          name_origin = c("accepted_scientific_name", NA_character_, NA_character_),
                          accepted_scientific_name = c("Abelmoschus esculentus", "UNVEGETATED GROUND", "UNVEGETATED WATER"),
                          family = c("Malvaceae", "Unvegetated Ground", "Unvegetated Water"),
                          nativity = c("introduced", NA, NA),
                          c = c(0, 0, 0),
                          w = c(5, NA, NA),
                          wetland_indicator = c(NA_character_, NA_character_, NA_character_),
                          physiognomy = c("forb", "Unvegetated Ground", "Unvegetated Water"),
                          duration = c("annual", "Unvegetated Ground", "Unvegetated Water"),
                          common_name = c("Okra; Gumbo", NA, NA),
                          fqa_db = c("michigan_2014", "michigan_2014", "michigan_2014")))
})

#testing non-recognized species
test_that("accepted_entries with allow_non_veg = FALSE", {
  expect_equal(accepted_entries(x = accepted_non_veg, key = "acronym", db = "michigan_2014", native = FALSE,
                                allow_non_veg = FALSE),

               data.frame(acronym = c("ABEESC"),
                          name = c("ABELMOSCHUS ESCULENTUS"),
                          name_origin = c("accepted_scientific_name"),
                          accepted_scientific_name = c("Abelmoschus esculentus"),
                          family = c("Malvaceae"),
                          nativity = c("introduced"),
                          c = c(0),
                          w = c(5),
                          wetland_indicator = c(NA_character_),
                          physiognomy = c("forb"),
                          duration = c("annual"),
                          common_name = c("Okra; Gumbo"),
                          fqa_db = c("michigan_2014")))
})

#testing accepted_entries() synonyms--------------------------------------------

#testing same name, diff ID where both names are syns
test_that("accepted_entries deletes observations that are synonyms to > 1 species", {
  expect_equal(accepted_entries(x = same_syn, db = "wyoming_2017", native = FALSE),

               data.frame(name = c("ABIES BIFOLIA"),
                          name_origin = c("accepted_scientific_name"),
                          acronym = c(NA_character_),
                          accepted_scientific_name = c("Abies bifolia"),
                          family = c("Pinaceae"),
                          nativity = c("native"),
                          c = c(6),
                          w = c(1),
                          wetland_indicator = c("FACU"),
                          physiognomy = c(NA_character_),
                          duration = c(NA_character_),
                          common_name = c("Subalpine Fir"),
                          fqa_db = c("wyoming_2017")))
})

#testing same name, diff ID where one name is a sci name
test_that("accepted_entries keeps sci name if name is both syn and sci name", {
  expect_equal(accepted_entries(x = same_syn_sci, db = "wyoming_2017", native = FALSE),

               data.frame(name = c("CAREX FOENEA", "ABIES BIFOLIA"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          acronym = c(NA_character_, NA_character_),
                          accepted_scientific_name = c("Carex foenea", "Abies bifolia"),
                          family = c("Cyperaceae", "Pinaceae"),
                          nativity = c("native", "native"),
                          c = c(6, 6),
                          w = c(1, 1),
                          wetland_indicator = c("FACU", "FACU"),
                          physiognomy = c(NA_character_, NA_character_),
                          duration = c(NA_character_, NA_character_),
                          common_name = c("Bronze Sedge", "Subalpine Fir"),
                          fqa_db = c("wyoming_2017", "wyoming_2017")))
})

#testing same name, diff ID where one name is a sci name, also duplicates
test_that("accepted_entries keeps sci name if name is both syn and sci name, also keeps dup syns", {
  expect_equal(accepted_entries(x = same_syn_sci_2, db = "wyoming_2017", native = FALSE,
                                allow_duplicates = TRUE),

               data.frame(name = c("CAREX FOENEA","CAREX FOENEA", "ABIES MENZIESII", "ABIES MENZIESII"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name", "synonym", "synonym"),
                          acronym = c(NA_character_, NA_character_, NA_character_, NA_character_),
                          accepted_scientific_name = c("Carex foenea", "Carex foenea", "Pseudotsuga menziesii", "Pseudotsuga menziesii"),
                          family = c("Cyperaceae", "Cyperaceae", "Pinaceae", "Pinaceae"),
                          nativity = c("native", "native", "native", "native"),
                          c = c(6, 6, 6, 6),
                          w = c(1, 1, 1, 1),
                          wetland_indicator = c("FACU", "FACU", "FACU", "FACU"),
                          physiognomy = c(NA_character_, NA_character_, NA_character_, NA_character_),
                          duration = c(NA_character_, NA_character_, NA_character_, NA_character_),
                          common_name = c("Bronze Sedge","Bronze Sedge", "Rocky Mountain Douglas-Fir", "Rocky Mountain Douglas-Fir"),
                          fqa_db = c("wyoming_2017", "wyoming_2017", "wyoming_2017", "wyoming_2017")))
})

#testing diff name, same ID
test_that("accepted_entries considers two names names associated with same ID to be same plant", {
  expect_equal(accepted_entries(x = same_id, db = "wyoming_2017", native = FALSE),

               data.frame(name = c("ABIES BIFOLIA", "ABIES MENZIESII"),
                          name_origin = c("accepted_scientific_name", "synonym"),
                          acronym = c(NA_character_, NA_character_),
                          accepted_scientific_name = c("Abies bifolia", "Pseudotsuga menziesii"),
                          family = c("Pinaceae", "Pinaceae"),
                          nativity = c("native", "native"),
                          c = c(6, 6),
                          w = c(1, 1),
                          wetland_indicator = c("FACU", "FACU"),
                          physiognomy = c(NA_character_, NA_character_),
                          duration = c(NA_character_, NA_character_),
                          common_name = c("Subalpine Fir", "Rocky Mountain Douglas-Fir"),
                          fqa_db = c("wyoming_2017", "wyoming_2017")))
})

#testing diff name, same ID
test_that("accepted_entries considers two names names associated with same ID to be same plant", {
  expect_equal(accepted_entries(x = same_id, db = "wyoming_2017", native = FALSE, allow_duplicates = TRUE),

               data.frame(name = c("ABIES BIFOLIA", "ABIES BIFOLIA", "ABIES BIFOLIA", "ABIES MENZIESII"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name", "accepted_scientific_name", "synonym"),
                          acronym = c(NA_character_, NA_character_, NA_character_, NA_character_),
                          accepted_scientific_name = c("Abies bifolia", "Abies bifolia", "Abies bifolia", "Pseudotsuga menziesii"),
                          family = c("Pinaceae", "Pinaceae", "Pinaceae", "Pinaceae"),
                          nativity = c("native", "native", "native", "native"),
                          c = c(6, 6, 6, 6),
                          w = c(1, 1, 1, 1),
                          wetland_indicator = c("FACU", "FACU", "FACU", "FACU"),
                          physiognomy = c(NA_character_, NA_character_, NA_character_, NA_character_),
                          duration = c(NA_character_, NA_character_, NA_character_, NA_character_),
                          common_name = c("Subalpine Fir", "Subalpine Fir", "Subalpine Fir", "Rocky Mountain Douglas-Fir"),
                          fqa_db = c("wyoming_2017", "wyoming_2017", "wyoming_2017", "wyoming_2017")))
})

#testing diff name, same ID, allow_duplicates = FALSE
test_that("accepted_entries considers two names names associated with same ID to be same plant, deletes dups", {
  expect_equal(accepted_entries(x = same_id, db = "wyoming_2017", native = FALSE, allow_duplicates = FALSE),

               data.frame(name = c("ABIES BIFOLIA", "ABIES MENZIESII"),
                          name_origin = c("accepted_scientific_name", "synonym"),
                          acronym = c(NA_character_, NA_character_),
                          accepted_scientific_name = c("Abies bifolia", "Pseudotsuga menziesii"),
                          family = c("Pinaceae", "Pinaceae"),
                          nativity = c("native",  "native"),
                          c = c(6, 6),
                          w = c(1, 1),
                          wetland_indicator = c("FACU", "FACU"),
                          physiognomy = c(NA_character_, NA_character_),
                          duration = c(NA_character_,  NA_character_),
                          common_name = c("Subalpine Fir", "Rocky Mountain Douglas-Fir"),
                          fqa_db = c("wyoming_2017", "wyoming_2017")))
})
