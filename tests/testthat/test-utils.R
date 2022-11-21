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

})

test_that("accepted_entries() throws error if cover is not a column in x", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = F, cover_weighted = T),
               "If 'cover = TRUE' crooked_island must have a column named cover.")
})

test_that("accepted_entries() throws error if cover is not a column in x", {
  expect_error(accepted_entries(crooked_island %>% dplyr::mutate(cover = NA), key = "acronym", db = "michigan_2014", native = F, cover_weighted = T),
               "'cover' column cannot contain missing values.")
})

test_that("accepted_entries() throws error if cover_metric isn't right", {
  expect_error(accepted_entries(crooked_island, key = "acronym", db = "michigan_2014", native = F,
                                cover_metric = "percent"),
               "percent is not an accepted cover-method. See documentation.")
})


test_that("accepted_entries() throws warning if nas are introduced to cover", {
  expect_message(accepted_entries(na_intro_cover, key = "acronym", db = "michigan_2014", native = F,
                                   cover_weighted = TRUE, cover_metric = "braun-blanquet"),
                 "NAs were introduced during the conversion to the braun-blanquet system. Species with NA cover values will be removed.")
})

#-------------------------------------------------------------------------------
#testing accepted_entries() messages

test_that("accepted_entries() throws a message if cover_metric isn't right", {
  expect_message(accepted_entries(duplicate, key = "acronym", db = "michigan_2014", native = F),
               "Duplicate entries detected. Duplicates will only be counted once.")
})

test_that("accepted_entries() throws a message if species not recognized", {
  expect_message(accepted_entries(typo, key = "acronym", db = "michigan_2014", native = F),
                 "Species TYPO not listed in database. It will be discarded.")
})

test_that("accepted_entries() throws a messafe if species does not have C Value", {
  expect_message(accepted_entries(no_c_test, key = "scientific_name", db = "montana_2017", native = F),
                 "Species ABRONIA FRAGRANS is recognized but has not been assigned a C Value.")
})

