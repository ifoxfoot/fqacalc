#-------------------------------------------------------------------------------
#testing accepted_entries() errors

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
               "'key' argument must be equal to 'acronym' or 'scientific_name'.")

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
               "If 'cover = TRUE' crooked_island must have a column named cover.")
})

test_that("accepted_entries() throws error if cover contains NAs", {
  expect_error(accepted_entries(crooked_island %>% dplyr::mutate(cover = NA), key = "acronym", db = "michigan_2014", native = F, cover_weighted = T),
               "'cover' column cannot contain missing values.")
})

test_that("accepted_entries() throws error if cover_metric isn't right", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = F,
                                cover_metric = "percent"),
               "percent is not an accepted cover-method. See documentation.")
})

test_that("accepted_entries() throws error if olot_id isn't a column", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = F,
                                plot_id = "percent"),
               "'plot_id' must be the name of a column in crooked_island.")
})


#-------------------------------------------------------------------------------
#testing accepted_entries() messages

test_that("accepted_entries() throws a message if duplicates are detected-noncover", {
  expect_message(accepted_entries(duplicate, key = "acronym", db = "michigan_2014", native = F),
               "Duplicate entries detected. Duplicates will only be counted once.")
})

test_that("accepted_entries() throws a message if duplicates are detected-cover", {
  expect_message(accepted_entries(transect, key = "acronym", db = "michigan_2014", native = F, cover_weighted = TRUE),
                 "Duplicate entries detected. Duplicates will only be counted once. Cover values of duplicate species will be added together.")
})

test_that("accepted_entries() throws a message if duplicates are detected-cover", {
  expect_message(accepted_entries(transect_dup, key = "acronym", db = "michigan_2014", native = F, plot_id = "quad_id",
                                  allow_duplicates = TRUE, allow_non_veg = TRUE),
                 "Duplicate entries detected in the same plot. Duplicates in the same plot will be counted once. Cover values of duplicate species will be added together.")
})

test_that("accepted_entries() throws a message if species not recognized", {
  expect_message(accepted_entries(typo, key = "acronym", db = "michigan_2014", native = F),
                 "Species TYPO not listed in database. It will be discarded.")
})

test_that("accepted_entries() throws a message if species does not have C Value", {
  expect_message(accepted_entries(no_c_test, key = "scientific_name", db = "montana_2017", native = F),
                 "Species ABRONIA FRAGRANS is recognized but has not been assigned a C Value.")
})

test_that("accepted_entries() throws warning if nas are introduced to cover", {
  expect_message(accepted_entries(na_intro_cover, key = "acronym", db = "michigan_2014", native = F,
                                  cover_weighted = TRUE, cover_metric = "braun-blanquet"),
                 "NAs were introduced during the conversion to the braun-blanquet system. Species with NA cover values will be removed.")
})

#-------------------------------------------------------------------------------
#testing accepted_entries() results

#testing normal result
test_that("accepted_entries works in perfect setting", {
  expect_equal(accepted_entries(x = accepted_perfect, key = "acronym", db = "michigan_2014", native = FALSE),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae"),
                          native = c("exotic", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))

})

#testing native result
test_that("accepted_entries works in perfect setting with native = TRUE", {
  expect_equal(accepted_entries(x = accepted_perfect, key = "acronym", db = "michigan_2014", native = TRUE),

               data.frame(acronym = c("ABIBAL"),
                          scientific_name = c("ABIES BALSAMEA"),
                          synonym = c(NA_character_),
                          family = c("Pinaceae"),
                          native = c("native"),
                          c = c(3),
                          w = c(0),
                          physiognomy = c("tree"),
                          duration = c("perennial"),
                          common_name = c("balsam fir"),
                          fqa_db = c("michigan_2014")))
})

#testing cover-weighted result (percent cover)
test_that("accepted_entries works in perfect setting with cover_weighted = TRUE", {
  expect_equal(accepted_entries(x = accepted_perfect, key = "acronym", db = "michigan_2014",
                                native = FALSE, cover_weighted = TRUE),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          cover = c(60, 50),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae"),
                          native = c("exotic", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing cover-weighted result (braun-blanquet)
test_that("accepted_entries works with br-bl", {
  expect_equal(accepted_entries(x = accepted_br, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                allow_duplicates = TRUE,
                                cover_weighted = TRUE,
                                cover_metric = "braun-blanquet"),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          cover = c(87.5, 0.1),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae"),
                          native = c("exotic", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing br cover value when dups need to be added together
test_that("accepted_entries works with br-bl and dups in same plot", {
  expect_equal(accepted_entries(x = accepted_br_dup, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                allow_duplicates = TRUE,
                                cover_weighted = TRUE,
                                cover_metric = "braun-blanquet",
                                plot_id = "quad_id"),

               data.frame(quad_id = c(1,2),
                          acronym = c("ABEESC", "ABIBAL"),
                          cover = c(87.5, 37.6),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae"),
                          native = c("exotic", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing cover-weighted result (carolina)
test_that("accepted_entries works with carolina cover method", {
  expect_equal(accepted_entries(x = accepted_cover_method, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                cover_weighted = TRUE,
                                cover_metric = "carolina_veg_survey"),

               data.frame(
                          acronym = c("ABEESC", "ABIBAL"),
                          cover = c(0.1, 1.5),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae"),
                          native = c("exotic", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing cover-weighted result (daubenmire)
test_that("accepted_entries works with carolina cover method", {
  expect_equal(accepted_entries(x = accepted_cover_method, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                cover_weighted = TRUE,
                                cover_metric = "daubenmire"),

               data.frame(
                 acronym = c("ABEESC", "ABIBAL"),
                 cover = c(2.5, 37.5),
                 scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                 synonym = c(NA_character_, NA_character_),
                 family = c("Malvaceae", "Pinaceae"),
                 native = c("exotic", "native"),
                 c = c(0, 3),
                 w = c(5, 0),
                 physiognomy = c("forb", "tree"),
                 duration = c("annual", "perennial"),
                 common_name = c("okra; gumbo", "balsam fir"),
                 fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing cover-weighted result (usfs)
test_that("accepted_entries works with usfs method", {
  expect_equal(accepted_entries(x = accepted_cover_method, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                cover_weighted = TRUE,
                                cover_metric = "usfs_ecodata"),

               data.frame(
                 acronym = c("ABEESC", "ABIBAL"),
                 cover = c(0.5, 3),
                 scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                 synonym = c(NA_character_, NA_character_),
                 family = c("Malvaceae", "Pinaceae"),
                 native = c("exotic", "native"),
                 c = c(0, 3),
                 w = c(5, 0),
                 physiognomy = c("forb", "tree"),
                 duration = c("annual", "perennial"),
                 common_name = c("okra; gumbo", "balsam fir"),
                 fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing allow_duplicate result
test_that("accepted_entries works in perfect setting with allow_duplicates = TRUE", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014",
                                native = FALSE,
                                allow_duplicates = TRUE),

               data.frame(acronym = c("ABEESC", "ABIBAL", "ABIBAL"),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae", "Pinaceae"),
                          native = c("exotic", "native", "native"),
                          c = c(0, 3, 3),
                          w = c(5, 0, 0),
                          physiognomy = c("forb", "tree", "tree"),
                          duration = c("annual", "perennial", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014", "michigan_2014")))
})

#testing allow_duplicate result when plot_id is set
test_that("accepted_entries with allow_duplicates = TRUE, plot_id set", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014", native = FALSE,
                                allow_duplicates = TRUE, plot_id = "quad_id"),

               data.frame(quad_id = c(1, 2),
                          acronym = c("ABEESC", "ABIBAL"),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae"),
                          native = c("exotic", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing don't allow_duplicate result when plot_id and cover-weighted are set
test_that("accepted_entries with allow_duplicates = TRUE, cover_weighted = TRUE, plot id is set", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014", native = FALSE,
                                cover_weighted = TRUE, allow_duplicates = TRUE, plot_id = "quad_id"),

               data.frame(quad_id = c(1, 2),
                          acronym = c("ABEESC", "ABIBAL"),
                          cover = c(60, 100),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae"),
                          native = c("exotic", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing don't allow_duplicate result cover weighted
test_that("accepted_entries with allow_duplicates = FALSE, cover_weighted = TRUE", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014", native = FALSE,
                                cover_weighted = TRUE, allow_duplicates = FALSE, plot_id = NULL),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          cover = c(60, 100),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae"),
                          native = c("exotic", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing don't allow_duplicate result non-cover weighted
test_that("accepted_entries with allow_duplicates = FALSE, cover_weighted = FALSE", {
  expect_equal(accepted_entries(x = accepted_dup, key = "acronym", db = "michigan_2014", native = FALSE,
                                cover_weighted = FALSE, allow_duplicates = FALSE),

               data.frame(acronym = c("ABEESC", "ABIBAL"),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "ABIES BALSAMEA"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Malvaceae", "Pinaceae"),
                          native = c("exotic", "native"),
                          c = c(0, 3),
                          w = c(5, 0),
                          physiognomy = c("forb", "tree"),
                          duration = c("annual", "perennial"),
                          common_name = c("okra; gumbo", "balsam fir"),
                          fqa_db = c("michigan_2014", "michigan_2014")))
})

#testing allow_non_c
test_that("accepted_entries with allow_no_c = TRUE", {
  expect_equal(accepted_entries(x = accepted_no_c, db = "montana_2017", native = FALSE, allow_no_c = TRUE),

               data.frame(scientific_name = c("ABRONIA FRAGRANS", "ACER GLABRUM"),
                          synonym = c(NA_character_, NA_character_),
                          family = c("Nyctaginaceae", "Aceraceae"),
                          acronym = c(NA_character_, NA_character_),
                          native = c("native", "native"),
                          c = c(NA, 4),
                          w = c(NA_real_, NA_real_),
                          physiognomy = c(NA_character_, NA_character_),
                          duration = c(NA_character_, NA_character_),
                          common_name = c("Fragrant White Sand-verbena", "Rocky Mountain Maple"),
                          fqa_db = c("montana_2017", "montana_2017")))
})

#testing don't allow_non_c
test_that("accepted_entries with allow_no_c = FALSE", {
  expect_equal(accepted_entries(x = accepted_no_c, db = "montana_2017", native = FALSE, allow_no_c = FALSE),

               data.frame(scientific_name = c("ACER GLABRUM"),
                          synonym = c(NA_character_),
                          family = c("Aceraceae"),
                          acronym = c(NA_character_),
                          native = c("native"),
                          c = c(4),
                          w = c(NA_real_),
                          physiognomy = c(NA_character_),
                          duration = c(NA_character_),
                          common_name = c("Rocky Mountain Maple"),
                          fqa_db = c("montana_2017")))
})

#testing using non-veg
test_that("accepted_entries with allow_non_veg = TRUE", {
  expect_equal(accepted_entries(x = accepted_non_veg, key = "acronym", db = "michigan_2014", native = FALSE,
                                allow_non_veg = TRUE),

               data.frame(acronym = c("ABEESC", "GROUND", "WATER"),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS", "UNVEGETATED GROUND", "UNVEGETATED WATER"),
                          synonym = c(NA_character_, NA_character_, NA_character_),
                          family = c("Malvaceae", "Unvegetated Ground", "Unvegetated Water"),
                          native = c("exotic", NA, NA),
                          c = c(0, 0, 0),
                          w = c(5, 0, 0),
                          physiognomy = c("forb", "Unvegetated Ground", "Unvegetated Water"),
                          duration = c("annual", "Unvegetated Ground", "Unvegetated Water"),
                          common_name = c("okra; gumbo", NA, NA),
                          fqa_db = c("michigan_2014", "michigan_2014", "michigan_2014")))
})

#testing non-recognized species
test_that("accepted_entries with unrec species", {
  expect_equal(accepted_entries(x = accepted_non_veg, key = "acronym", db = "michigan_2014", native = FALSE,
                                allow_non_veg = FALSE),

               data.frame(acronym = c("ABEESC"),
                          scientific_name = c("ABELMOSCHUS ESCULENTUS"),
                          synonym = c(NA_character_),
                          family = c("Malvaceae"),
                          native = c("exotic"),
                          c = c(0),
                          w = c(5),
                          physiognomy = c("forb"),
                          duration = c("annual"),
                          common_name = c("okra; gumbo"),
                          fqa_db = c("michigan_2014")))
})
