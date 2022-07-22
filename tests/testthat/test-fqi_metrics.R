#creating data sets to test on


#data frame with incorrect column names
bad_names <- data.frame(acronot  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                        scientific_not = c(c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus"))
                        )

#data frame with one incorrect column name
one_bad_names <- data.frame(acronot  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                        scientific_name = c(c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus"))
                        )

#character string
character_string <- c("character", "string", "not", "df")

#integers
numbers <- 1:5

#data frame with duplicates in both rows
duplicate <- data.frame(acronym  = c("ABIBAL", "ABIBAL", "AMMBRE", "ANTELE"),
                        scientific_name = c(c("Abies balsamea", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus"))
                        )

#data frame with incorrect entries in both rows
typo <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "typo"),
                   scientific_name = c(c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "typo"))
                   )

#-------------------------------------------------------------------------------
#testing total_species_richness()

#perfect case-crooked island
test_that("total_species_richness() calculates total species richness in perfect setting", {
  expect_equal(total_species_richness(crooked_island), 35)
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
  expect_equal(total_species_richness(duplicate), 3)
  expect_message(total_species_richness(duplicate), "Duplicate entries detected")
  expect_equal(total_species_richness(duplicate, key = "acronym"), 3)
  expect_message(total_species_richness(duplicate, key = "acronym"), "Duplicate entries detected")
  expect_equal(total_species_richness(duplicate, key = "scientific_name"), 3)
  expect_message(total_species_richness(duplicate, key = "scientific_name"), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("total_species_richness() does not count duplicates", {
  expect_equal(total_species_richness(typo), 3)
  expect_message(total_species_richness(typo), "not listed in database")
  expect_equal(total_species_richness(typo, key = "acronym"), 3)
  expect_message(total_species_richness(typo, key = "acronym"), "not listed in database")
  expect_equal(total_species_richness(typo, key = "scientific_name"), 3)
  expect_message(total_species_richness(typo, key = "scientific_name"), "not listed in database")
})

#-------------------------------------------------------------------------------
#testing native_species_richness()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(native_species_richness(crooked_island), 28)
})

#testing errors
test_that("total_species_richness() calculates errors correctly", {
  expect_error(native_species_richness(), "x is missing")
  expect_error(native_species_richness(fake_df), "does not exist.")
  expect_error(native_species_richness(character_string), "must be a data frame.")
  expect_error(native_species_richness(numbers), "must be a data frame.")
  expect_error(native_species_richness(bad_names), "must have a column named")
  expect_error(native_species_richness(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(native_species_richness(bad_names, key = "acronym"), "must have a column named")
})

#-------------------------------------------------------------------------------
#testing total_mean_c()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_mean_c(crooked_island), 5.3714286)
})

#testing errors
test_that("total_species_richness() calculates errors correctly", {
  expect_error(total_mean_c(), "x is missing")
  expect_error(total_mean_c(fake_df), "does not exist.")
  expect_error(total_mean_c(character_string), "must be a data frame.")
  expect_error(total_mean_c(numbers), "must be a data frame.")
  expect_error(total_mean_c(bad_names), "must have a column named")
  expect_error(total_mean_c(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(total_mean_c(bad_names, key = "acronym"), "must have a column named")
})

#-------------------------------------------------------------------------------
#testing native_mean_c()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(native_mean_c(crooked_island), 6.7142857)
})

#testing errors
test_that("total_species_richness() calculates errors correctly", {
  expect_error(native_mean_c(), "x is missing")
  expect_error(native_mean_c(fake_df), "does not exist.")
  expect_error(native_mean_c(character_string), "must be a data frame.")
  expect_error(native_mean_c(numbers), "must be a data frame.")
  expect_error(native_mean_c(bad_names), "must have a column named")
  expect_error(native_mean_c(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(native_mean_c(bad_names, key = "acronym"), "must have a column named")
})

#-------------------------------------------------------------------------------
#testing total_FQI()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_FQI(crooked_island), 31.7778)
})

#testing errors
test_that("total_species_richness() calculates errors correctly", {
  expect_error(total_FQI(), "x is missing")
  expect_error(total_FQI(fake_df), "does not exist.")
  expect_error(total_FQI(character_string), "must be a data frame.")
  expect_error(total_FQI(numbers), "must be a data frame.")
  expect_error(total_FQI(bad_names), "must have a column named")
  expect_error(total_FQI(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(total_FQI(bad_names, key = "acronym"), "must have a column named")
})

#-------------------------------------------------------------------------------
#testing native_FQI()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(native_FQI(crooked_island), 35.52866)
})

#testing errors
test_that("total_species_richness() calculates errors correctly", {
  expect_error(native_FQI(), "x is missing")
  expect_error(native_FQI(fake_df), "does not exist.")
  expect_error(native_FQI(character_string), "must be a data frame.")
  expect_error(native_FQI(numbers), "must be a data frame.")
  expect_error(native_FQI(bad_names), "must have a column named")
  expect_error(native_FQI(crooked_island, key = "bad_key"), "key must be equal to")
  expect_error(native_FQI(bad_names, key = "acronym"), "must have a column named")
})

#-------------------------------------------------------------------------------
#testing adjusted_FQI()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(adjusted_FQI(crooked_island), 60.054397)
})

