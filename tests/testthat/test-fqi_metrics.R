
#-------------------------------------------------------------------------------
#testing total_species_richness()

#perfect case-crooked island
test_that("total_species_richness() calculates total species richness in perfect setting", {
  expect_equal(total_species_richness(crooked_island, db = "michigan_2014"), 35)
})

#testing errors
test_that("total_species_richness() calculates errors correctly", {
  expect_error(total_species_richness(), "x is missing")
  expect_error(total_species_richness(fake_df), "does not exist.")
  expect_error(total_species_richness(character_string), "must be a data frame.")
  expect_error(total_species_richness(numbers), "must be a data frame.")
  expect_error(total_species_richness(bad_names), "must have a column named")
  expect_error(total_species_richness(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(total_species_richness(bad_names, key = "acronym"), "must have a column named")
})

#test duplicate behavior
test_that("total_species_richness() does not count duplicates", {
  expect_equal(total_species_richness(duplicate, db = "michigan_2014"), 3)
  expect_message(total_species_richness(duplicate, db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(total_species_richness(duplicate, key = "acronym", db = "michigan_2014"), 3)
  expect_message(total_species_richness(duplicate, key = "acronym", db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(total_species_richness(duplicate, key = "scientific_name", db = "michigan_2014"), 3)
  expect_message(total_species_richness(duplicate, key = "scientific_name", db = "michigan_2014"), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("total_species_richness() drops non-matching plants", {
  expect_equal(total_species_richness(typo, db = "michigan_2014"), 3)
  expect_message(total_species_richness(typo, db = "michigan_2014"), "not listed in database")
  expect_equal(total_species_richness(typo, key = "acronym", db = "michigan_2014"), 3)
  expect_message(total_species_richness(typo, key = "acronym", db = "michigan_2014"), "not listed in database")
  expect_equal(total_species_richness(typo, key = "scientific_name", db = "michigan_2014"), 3)
  expect_message(total_species_richness(typo, key = "scientific_name", db = "michigan_2014"), "not listed in database")
})

#testing fuzzy matching
test_that("total_species_richness() kind of fuzzy matches", {
  expect_equal(total_species_richness(fuzzy, db = "michigan_2014"), 4)
  expect_equal(total_species_richness(fuzzy, key = "acronym", db = "michigan_2014"), 4)
})

#-------------------------------------------------------------------------------
#testing native_species_richness()

test_that("native_species_richness() calculates total species richness", {
  expect_equal(native_species_richness(crooked_island, db = "michigan_2014"), 28)
})

#testing errors
test_that("native_species_richness() calculates errors correctly", {
  expect_error(native_species_richness(), "x is missing")
  expect_error(native_species_richness(fake_df), "does not exist.")
  expect_error(native_species_richness(character_string), "must be a data frame.")
  expect_error(native_species_richness(numbers), "must be a data frame.")
  expect_error(native_species_richness(bad_names), "must have a column named")
  expect_error(native_species_richness(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(native_species_richness(bad_names, key = "acronym"), "must have a column named")
})

#test duplicate behavior
test_that("native_species_richness() does not count duplicates", {
  expect_equal(native_species_richness(duplicate, db = "michigan_2014"), 2)
  expect_message(native_species_richness(duplicate, db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(native_species_richness(duplicate, key = "acronym", db = "michigan_2014"), 2)
  expect_message(native_species_richness(duplicate, key = "acronym", db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(native_species_richness(duplicate, key = "scientific_name", db = "michigan_2014"), 2)
  expect_message(native_species_richness(duplicate, key = "scientific_name", db = "michigan_2014"), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("native_species_richness() drops non-matching plants", {
  expect_equal(native_species_richness(typo, db = "michigan_2014"), 2)
  expect_message(native_species_richness(typo, db = "michigan_2014"), "not listed in database")
  expect_equal(native_species_richness(typo, key = "acronym", db = "michigan_2014"), 2)
  expect_message(native_species_richness(typo, key = "acronym", db = "michigan_2014"), "not listed in database")
  expect_equal(native_species_richness(typo, key = "scientific_name", db = "michigan_2014"), 2)
  expect_message(native_species_richness(typo, key = "scientific_name", db = "michigan_2014"), "not listed in database")
})

#testing fuzzy matching
test_that("native_species_richness() kind of fuzzy matches", {
  expect_equal(native_species_richness(fuzzy, db = "michigan_2014"), 3)
  expect_equal(native_species_richness(fuzzy, key = "acronym", db = "michigan_2014"), 3)
})

#-------------------------------------------------------------------------------
#testing total_mean_c()

test_that("total_mean_c() calculates total species richness", {
  expect_equal(total_mean_c(crooked_island, db = "michigan_2014"), 5.3714286)
})

#testing errors
test_that("total_mean_c() calculates errors correctly", {
  expect_error(total_mean_c(), "x is missing")
  expect_error(total_mean_c(fake_df), "does not exist.")
  expect_error(total_mean_c(character_string), "must be a data frame.")
  expect_error(total_mean_c(numbers), "must be a data frame.")
  expect_error(total_mean_c(bad_names), "must have a column named")
  expect_error(total_mean_c(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(total_mean_c(bad_names, key = "acronym"), "must have a column named")
})

#test duplicate behavior
test_that("total_mean_c() does not count duplicates", {
  expect_equal(total_mean_c(duplicate, db = "michigan_2014"), 4.33333333)
  expect_message(total_mean_c(duplicate, db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(total_mean_c(duplicate, key = "acronym", db = "michigan_2014"), 4.33333333)
  expect_message(total_mean_c(duplicate, key = "acronym", db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(total_mean_c(duplicate, key = "scientific_name", db = "michigan_2014"), 4.33333333)
  expect_message(total_mean_c(duplicate, key = "scientific_name", db = "michigan_2014"), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("total_mean_c() drops non-matching plants", {
  expect_equal(total_mean_c(typo, db = "michigan_2014"), 4.33333333)
  expect_message(total_mean_c(typo, db = "michigan_2014"), "not listed in database")
  expect_equal(total_mean_c(typo, key = "acronym", db = "michigan_2014"),  4.33333333)
  expect_message(total_mean_c(typo, key = "acronym", db = "michigan_2014"), "not listed in database")
  expect_equal(total_mean_c(typo, key = "scientific_name", db = "michigan_2014"), 4.33333333)
  expect_message(total_mean_c(typo, key = "scientific_name", db = "michigan_2014"), "not listed in database")
})

#testing fuzzy matching
test_that("total_mean_c() kind of fuzzy matches", {
  expect_equal(total_mean_c(fuzzy, db = "michigan_2014"),  5.75)
  expect_equal(total_mean_c(fuzzy, key = "acronym", db = "michigan_2014"), 5.75)
})

#-------------------------------------------------------------------------------
#testing native_mean_c()

test_that("native_mean_c() calculates total species richness", {
  expect_equal(native_mean_c(crooked_island, db = "michigan_2014"), 6.7142857)
})

#testing errors
test_that("native_mean_c() calculates errors correctly", {
  expect_error(native_mean_c(), "x is missing")
  expect_error(native_mean_c(fake_df), "does not exist.")
  expect_error(native_mean_c(character_string), "must be a data frame.")
  expect_error(native_mean_c(numbers), "must be a data frame.")
  expect_error(native_mean_c(bad_names), "must have a column named")
  expect_error(native_mean_c(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(native_mean_c(bad_names, key = "acronym"), "must have a column named")
})

#test duplicate behavior
test_that("native_mean_c() does not count duplicates", {
  expect_equal(native_mean_c(duplicate, db = "michigan_2014"), 6.5)
  expect_message(native_mean_c(duplicate, db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(native_mean_c(duplicate, key = "acronym", db = "michigan_2014"), 6.5)
  expect_message(native_mean_c(duplicate, key = "acronym", db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(native_mean_c(duplicate, key = "scientific_name", db = "michigan_2014"), 6.5)
  expect_message(native_mean_c(duplicate, key = "scientific_name", db = "michigan_2014"), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("native_mean_c() drops non-matching plants", {
  expect_equal(native_mean_c(typo, db = "michigan_2014"), 6.5)
  expect_message(native_mean_c(typo, db = "michigan_2014"), "not listed in database")
  expect_equal(native_mean_c(typo, key = "acronym", db = "michigan_2014"), 6.5)
  expect_message(native_mean_c(typo, key = "acronym", db = "michigan_2014"), "not listed in database")
  expect_equal(native_mean_c(typo, key = "scientific_name", db = "michigan_2014"), 6.5)
  expect_message(native_mean_c(typo, key = "scientific_name", db = "michigan_2014"), "not listed in database")
})

#testing fuzzy matching
test_that("native_mean_c() kind of fuzzy matches", {
  expect_equal(native_mean_c(fuzzy, db = "michigan_2014"),  7.6666667)
  expect_equal(native_mean_c(fuzzy, key = "acronym", db = "michigan_2014"), 7.6666667)
})


#-------------------------------------------------------------------------------
#testing total_FQI()

test_that("total_FQI() calculates total species richness", {
  expect_equal(total_FQI(crooked_island, db = "michigan_2014"), 31.7778)
})

#testing errors
test_that("total_FQI() calculates errors correctly", {
  expect_error(total_FQI(), "x is missing")
  expect_error(total_FQI(fake_df), "does not exist.")
  expect_error(total_FQI(character_string), "must be a data frame.")
  expect_error(total_FQI(numbers), "must be a data frame.")
  expect_error(total_FQI(bad_names), "must have a column named")
  expect_error(total_FQI(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(total_FQI(bad_names, key = "acronym"), "must have a column named")
})

#test duplicate behavior
test_that("total_FQI() does not count duplicates", {
  expect_equal(total_FQI(duplicate, db = "michigan_2014"), 7.5055535)
  expect_message(total_FQI(duplicate, db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(total_FQI(duplicate, key = "acronym", db = "michigan_2014"), 7.5055535)
  expect_message(total_FQI(duplicate, key = "acronym", db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(total_FQI(duplicate, key = "scientific_name", db = "michigan_2014"), 7.5055535)
  expect_message(total_FQI(duplicate, key = "scientific_name", db = "michigan_2014"), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("total_FQI() drops non-matching plants", {
  expect_equal(total_FQI(typo, db = "michigan_2014"), 7.5055535)
  expect_message(total_FQI(typo, db = "michigan_2014"), "not listed in database")
  expect_equal(total_FQI(typo, key = "acronym", db = "michigan_2014"), 7.5055535)
  expect_message(total_FQI(typo, key = "acronym", db = "michigan_2014"), "not listed in database")
  expect_equal(total_FQI(typo, key = "scientific_name", db = "michigan_2014"), 7.5055535)
  expect_message(total_FQI(typo, key = "scientific_name", db = "michigan_2014"), "not listed in database")
})

#testing fuzzy matching
test_that("total_FQI() kind of fuzzy matches", {
  expect_equal(total_FQI(fuzzy, db = "michigan_2014"),  11.5)
  expect_equal(total_FQI(fuzzy, key = "acronym", db = "michigan_2014"), 11.5)
})

#-------------------------------------------------------------------------------
#testing native_FQI()

test_that("native_FQI() calculates total species richness", {
  expect_equal(native_FQI(crooked_island, db = "michigan_2014"), 35.52866)
})

#testing errors
test_that("native_FQI() calculates errors correctly", {
  expect_error(native_FQI(), "x is missing")
  expect_error(native_FQI(fake_df), "does not exist.")
  expect_error(native_FQI(character_string), "must be a data frame.")
  expect_error(native_FQI(numbers), "must be a data frame.")
  expect_error(native_FQI(bad_names), "must have a column named")
  expect_error(native_FQI(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(native_FQI(bad_names, key = "acronym"), "must have a column named")
})

#test duplicate behavior
test_that("native_FQI() does not count duplicates", {
  expect_equal(native_FQI(duplicate, db = "michigan_2014"), 9.19238815543)
  expect_message(native_FQI(duplicate, db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(native_FQI(duplicate, key = "acronym", db = "michigan_2014"), 9.19238815543)
  expect_message(native_FQI(duplicate, key = "acronym", db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(native_FQI(duplicate, key = "scientific_name", db = "michigan_2014"), 9.19238815543)
  expect_message(native_FQI(duplicate, key = "scientific_name", db = "michigan_2014"), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("native_FQI() drops non-matching plants", {
  expect_equal(native_FQI(typo, db = "michigan_2014"), 9.19238815)
  expect_message(native_FQI(typo, db = "michigan_2014"), "not listed in database")
  expect_equal(native_FQI(typo, key = "acronym", db = "michigan_2014"), 9.19238815)
  expect_message(native_FQI(typo, key = "acronym", db = "michigan_2014"), "not listed in database")
  expect_equal(native_FQI(typo, key = "scientific_name", db = "michigan_2014"), 9.19238815)
  expect_message(native_FQI(typo, key = "scientific_name", db = "michigan_2014"), "not listed in database")
})

#testing fuzzy matching
test_that("native_FQI() kind of fuzzy matches", {
  expect_equal(native_FQI(fuzzy, db = "michigan_2014"),  13.27905619)
  expect_equal(native_FQI(fuzzy, key = "acronym", db = "michigan_2014"), 13.27905619)
})

#-------------------------------------------------------------------------------
#testing adjusted_FQI()

test_that("adjusted_FQI() calculates total species richness", {
  expect_equal(adjusted_FQI(crooked_island, db = "michigan_2014"), 60.054397)
})

#testing errors
test_that("adjusted_FQI() calculates errors correctly", {
  expect_error(adjusted_FQI(), "x is missing")
  expect_error(adjusted_FQI(fake_df), "does not exist.")
  expect_error(adjusted_FQI(character_string), "must be a data frame.")
  expect_error(adjusted_FQI(numbers), "must be a data frame.")
  expect_error(adjusted_FQI(bad_names), "must have a column named")
  expect_error(adjusted_FQI(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(adjusted_FQI(bad_names, key = "acronym"), "must have a column named")
})

#test duplicate behavior
test_that("adjusted_FQI() does not count duplicates", {
  expect_equal(adjusted_FQI(duplicate, db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(duplicate, db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(adjusted_FQI(duplicate, key = "acronym", db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(duplicate, key = "acronym", db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(adjusted_FQI(duplicate, key = "scientific_name", db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(duplicate, key = "scientific_name", db = "michigan_2014"), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("adjusted_FQI() drops non-matching plants", {
  expect_equal(adjusted_FQI(typo, db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(typo, db = "michigan_2014"), "not listed in database")
  expect_equal(adjusted_FQI(typo, key = "acronym", db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(typo, key = "acronym", db = "michigan_2014"), "not listed in database")
  expect_equal(adjusted_FQI(typo, key = "scientific_name", db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(typo, key = "scientific_name", db = "michigan_2014"), "not listed in database")
})

#testing fuzzy matching
test_that("adjusted_FQI() kind of fuzzy matches", {
  expect_equal(adjusted_FQI(fuzzy, db = "michigan_2014"),  66.3952812)
  expect_equal(adjusted_FQI(fuzzy, key = "acronym", db = "michigan_2014"), 66.3952812)
})


