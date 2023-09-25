
#-------------------------------------------------------------------------------
#testing species_richness()

#perfect case-crooked island
test_that("species_richness() calculates species richness in perfect setting", {
  expect_equal(species_richness(crooked_island, db = "michigan_2014", native = F), 35)
  expect_equal(species_richness(crooked_island, db = "michigan_2014", native = T), 28)
})

#testing errors
test_that("species_richness() calculates errors correctly", {
  expect_error(species_richness(), "x is missing")
  expect_error(species_richness(character_string, db = "michigan_2014", native = F), "must be a data frame.")
  expect_error(species_richness(numbers, db = "michigan_2014", native = F), "must be a data frame.")
  expect_error(species_richness(bad_names, db = "michigan_2014", native = F), "does not have a column named")
  expect_error(species_richness(crooked_island, key = "bad_key", db = "michigan_2014", native = F), "'key' argument must be equal to")
  expect_error(species_richness(bad_names, key = "acronym", db = "michigan_2014", native = F), "does not have a column named")
})

#test duplicate behavior
test_that("species_richness() does not count duplicates", {
  expect_equal(species_richness(duplicate, db = "michigan_2014", native = F), 3)
  expect_message(species_richness(duplicate, db = "michigan_2014", native = F), "Duplicate entries detected")
  expect_equal(species_richness(duplicate, key = "acronym", db = "michigan_2014", native = F), 3)
  expect_message(species_richness(duplicate, key = "acronym", db = "michigan_2014", native = F), "Duplicate entries detected")
  expect_equal(species_richness(duplicate, key = "name", db = "michigan_2014", native = F), 3)
  expect_message(species_richness(duplicate, key = "name", db = "michigan_2014", native = F), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("species_richness() drops non-matching plants", {
  expect_equal(species_richness(typo, db = "michigan_2014", native = F), 3)
  expect_message(species_richness(typo, db = "michigan_2014", native = F), "TYPO not listed in database")
  expect_equal(species_richness(typo, key = "acronym", db = "michigan_2014", native = F), 3)
  expect_message(species_richness(typo, key = "acronym", db = "michigan_2014", native = F), "TYPO not listed in database")
  expect_equal(species_richness(typo, key = "name", db = "michigan_2014", native = F), 3)
  expect_message(species_richness(typo, key = "name", db = "michigan_2014", native = F), "TYPO not listed in database")
})

#testing fuzzy matching
test_that("species_richness() kind of fuzzy matches", {
  expect_equal(species_richness(fuzzy, db = "michigan_2014", native = F), 4)
  expect_equal(species_richness(fuzzy, key = "acronym", db = "michigan_2014", native = F), 4)
})


#-------------------------------------------------------------------------------
#testing mean_c()

test_that("mean_c() calculates mean c", {
  expect_equal(mean_c(crooked_island, db = "michigan_2014", native = F), 5.3714286)
  expect_equal(mean_c(crooked_island, db = "michigan_2014", native = T), 6.7142857)
})

#testing errors
test_that("mean_c() calculates errors correctly", {
  expect_error(mean_c(), "x is missing")
  expect_error(mean_c(character_string, db = "michigan_2014", native = F), "must be a data frame.")
  expect_error(mean_c(numbers, db = "michigan_2014", native = F), "must be a data frame.")
  expect_error(mean_c(bad_names, db = "michigan_2014", native = F), "does not have a column named")
  expect_error(mean_c(crooked_island, key = "bad_key", db = "michigan_2014", native = F), "'key' argument must be equal to")
  expect_error(mean_c(bad_names, key = "acronym", db = "michigan_2014", native = F), "does not have a column named")
})

#test duplicate behavior
test_that("mean_c() does not count duplicates", {
  expect_equal(mean_c(duplicate, db = "michigan_2014", native = F), 4.33333333)
  expect_message(mean_c(duplicate, db = "michigan_2014", native = F), "Duplicate entries detected")
  expect_equal(mean_c(duplicate, key = "acronym", db = "michigan_2014", native = F), 4.33333333)
  expect_message(mean_c(duplicate, key = "acronym", db = "michigan_2014", native = F), "Duplicate entries detected")
  expect_equal(mean_c(duplicate, key = "name", db = "michigan_2014", native = F), 4.33333333)
  expect_message(mean_c(duplicate, key = "name", db = "michigan_2014", native = F), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("mean_c() drops non-matching plants", {
  expect_equal(mean_c(typo, db = "michigan_2014", native = F), 4.33333333)
  expect_message(mean_c(typo, db = "michigan_2014", native = F), "not listed in database")
  expect_equal(mean_c(typo, key = "acronym", db = "michigan_2014", native = F),  4.33333333)
  expect_message(mean_c(typo, key = "acronym", db = "michigan_2014", native = F), "not listed in database")
  expect_equal(mean_c(typo, key = "name", db = "michigan_2014", native = F), 4.33333333)
  expect_message(mean_c(typo, key = "name", db = "michigan_2014", native = F), "not listed in database")
})

#testing fuzzy matching
test_that("mean_c() kind of fuzzy matches", {
  expect_equal(mean_c(fuzzy, db = "michigan_2014", native = F),  5.75)
  expect_equal(mean_c(fuzzy, key = "acronym", db = "michigan_2014", native = F), 5.75)
})

#-------------------------------------------------------------------------------
#testing FQI()

test_that("FQI() calculates correct FQI", {
  expect_equal(FQI(crooked_island, db = "michigan_2014", native = F), 31.7778)
  expect_equal(FQI(crooked_island, db = "michigan_2014", native = T), 35.52866)
})

#testing errors
test_that("FQI() calculates errors correctly", {
  expect_error(FQI(), "x is missing")
  expect_error(FQI(character_string, db = "michigan_2014", native = F), "must be a data frame.")
  expect_error(FQI(numbers, db = "michigan_2014", native = F), "must be a data frame.")
  expect_error(FQI(bad_names, db = "michigan_2014", native = F), "does not have a column named")
  expect_error(FQI(crooked_island, key = "bad_key", db = "michigan_2014", native = F), "'key' argument must be equal to")
  expect_error(FQI(bad_names, key = "acronym", db = "michigan_2014", native = F), "does not have a column named")
})

#test duplicate behavior
test_that("FQI() does not count duplicates", {
  expect_equal(FQI(duplicate, db = "michigan_2014", native = F), 7.5055535)
  expect_message(FQI(duplicate, db = "michigan_2014", native = F), "Duplicate entries detected")
  expect_equal(FQI(duplicate, key = "acronym", db = "michigan_2014", native = F), 7.5055535)
  expect_message(FQI(duplicate, key = "acronym", db = "michigan_2014", native = F), "Duplicate entries detected")
  expect_equal(FQI(duplicate, key = "name", db = "michigan_2014", native = F), 7.5055535)
  expect_message(FQI(duplicate, key = "name", db = "michigan_2014", native = F), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("FQI() drops non-matching plants", {
  expect_equal(FQI(typo, db = "michigan_2014", native = F), 7.5055535)
  expect_message(FQI(typo, db = "michigan_2014", native = F), "not listed in database")
  expect_equal(FQI(typo, key = "acronym", db = "michigan_2014", native = F), 7.5055535)
  expect_message(FQI(typo, key = "acronym", db = "michigan_2014", native = F), "not listed in database")
  expect_equal(FQI(typo, key = "name", db = "michigan_2014", native = F), 7.5055535)
  expect_message(FQI(typo, key = "name", db = "michigan_2014", native = F), "not listed in database")
})

#testing fuzzy matching
test_that("FQI() kind of fuzzy matches", {
  expect_equal(FQI(fuzzy, db = "michigan_2014", native = F),  11.5)
  expect_equal(FQI(fuzzy, key = "acronym", db = "michigan_2014", native = F), 11.5)
})

#-------------------------------------------------------------------------------
#testing adjusted_FQI()

test_that("adjusted_FQI() calculates adjusted FQI", {
  expect_equal(adjusted_FQI(crooked_island, db = "michigan_2014"), 60.054397)
})

#testing errors
test_that("adjusted_FQI() calculates errors correctly", {
  expect_error(adjusted_FQI(), "x is missing")
  expect_error(adjusted_FQI(character_string, db = "michigan_2014"), "must be a data frame.")
  expect_error(adjusted_FQI(numbers, db = "michigan_2014"), "must be a data frame.")
  expect_error(adjusted_FQI(bad_names, db = "michigan_2014"), "does not have a column named")
  expect_error(adjusted_FQI(crooked_island, key = "bad_key", db = "michigan_2014"), "'key' argument must be equal to")
  expect_error(adjusted_FQI(bad_names, key = "acronym", db = "michigan_2014"), "does not have a column named")
})

#test duplicate behavior
test_that("adjusted_FQI() does not count duplicates", {
  expect_equal(adjusted_FQI(duplicate, db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(duplicate, db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(adjusted_FQI(duplicate, key = "acronym", db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(duplicate, key = "acronym", db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(adjusted_FQI(duplicate, key = "name", db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(duplicate, key = "name", db = "michigan_2014"), "Duplicate entries detected")
})

#test unrecognized behavior
test_that("adjusted_FQI() drops non-matching plants", {
  expect_equal(adjusted_FQI(typo, db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(typo, db = "michigan_2014"), "not listed in database")
  expect_equal(adjusted_FQI(typo, key = "acronym", db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(typo, key = "acronym", db = "michigan_2014"), "not listed in database")
  expect_equal(adjusted_FQI(typo, key = "name", db = "michigan_2014"), 53.0722777)
  expect_message(adjusted_FQI(typo, key = "name", db = "michigan_2014"), "not listed in database")
})

#testing fuzzy matching
test_that("adjusted_FQI() kind of fuzzy matches", {
  expect_equal(adjusted_FQI(fuzzy, db = "michigan_2014"),  66.3952812)
  expect_equal(adjusted_FQI(fuzzy, key = "acronym", db = "michigan_2014"), 66.3952812)
})


#-------------------------------------------------------------------------------
#testing all_metrics()

test_that("all_metrics() calculates metrics correctly", {
  expect_equal(all_metrics(x = crooked_island, key = "acronym", db = "michigan_2014"),

               data.frame(metrics = c("Total Species Richness",
                                      "Native Species Richness",
                                      "Introduced Species Richness",
                                      "% of Species with no C Value",
                                      "% of Species with 0 C Value",
                                      "% of Species with 1-3 C Value",
                                      "% of Species with 4-6 C Value",
                                      "% of Species with 7-10 C Value",
                                      "Mean C",
                                      "Native Mean C",
                                      "Total FQI",
                                      "Native FQI",
                                      "Adjusted FQI",
                                      "Mean Wetness",
                                      "Native Mean Wetness",
                                      "% Hydrophytes"),
                          values = c(35,
                                     28,
                                     7,
                                     0,
                                     20,
                                     8.571429,
                                     34.285714,
                                     37.142857,
                                     5.37142857,
                                     6.71428571,
                                     31.77779998,
                                     35.52866046,
                                     60.05439711,
                                     0.71428571,
                                     0.85714286,
                                     37.1428571
                                     )))
})


#testing errors
test_that("all_metrics() calculates errors correctly", {
  expect_error(all_metrics(), "x is missing")
  expect_error(all_metrics(character_string, db = "michigan_2014"), "must be a data frame.")
  expect_error(all_metrics(numbers, db = "michigan_2014"), "must be a data frame.")
  expect_error(all_metrics(bad_names, db = "michigan_2014"), "does not have a column named")
  expect_error(all_metrics(crooked_island, key = "bad_key", db = "michigan_2014"), "'key' argument must be equal to")
  expect_error(all_metrics(bad_names, key = "acronym", db = "michigan_2014"), "does not have a column named")
})


#test duplicate behavior
test_that("all_metrics() does not count duplicates", {
  expect_message(all_metrics(duplicate, db = "michigan_2014"), "Duplicate entries detected")
  expect_equal(all_metrics(duplicate, db = "michigan_2014"),
               data.frame(metrics = c("Total Species Richness",
                                      "Native Species Richness",
                                      "Introduced Species Richness",
                                      "% of Species with no C Value",
                                      "% of Species with 0 C Value",
                                      "% of Species with 1-3 C Value",
                                      "% of Species with 4-6 C Value",
                                      "% of Species with 7-10 C Value",
                                      "Mean C",
                                      "Native Mean C",
                                      "Total FQI",
                                      "Native FQI",
                                      "Adjusted FQI",
                                      "Mean Wetness",
                                      "Native Mean Wetness",
                                      "% Hydrophytes"),
                          values = c(fqacalc::species_richness(duplicate, db = "michigan_2014"),
                                     fqacalc::species_richness(duplicate, db = "michigan_2014", native = TRUE),
                                     1, #introduced
                                     0, #no c score
                                     33.333333, #0 c score
                                     33.333333, #1-3 c sscore
                                     0, #4-6 c score
                                     33.333333, #7-10 c score
                                     fqacalc::mean_c(duplicate, db = "michigan_2014"),
                                     fqacalc::mean_c(duplicate, db = "michigan_2014", native = TRUE),
                                     fqacalc::FQI(duplicate, db = "michigan_2014"),
                                     fqacalc::FQI(duplicate, db = "michigan_2014", native = TRUE),
                                     fqacalc::adjusted_FQI(duplicate, db = "michigan_2014"),
                                     fqacalc::mean_w(duplicate, db = "michigan_2014"),
                                     fqacalc::mean_w(duplicate, db = "michigan_2014", native = TRUE),
                                     0 #%hydro
                          )))
})


#test synonym behavior
test_that("all_metrics() works with synonyms", {
  expect_message(all_metrics(same_syn_sci, db = "wyoming_2017"),
                 "CAREX FOENEA is an accepted scientific name and a synonym.")
  expect_equal(all_metrics(same_syn_sci, db = "wyoming_2017"),
               data.frame(metrics = c("Total Species Richness",
                                      "Native Species Richness",
                                      "Introduced Species Richness",
                                      "% of Species with no C Value",
                                      "% of Species with 0 C Value",
                                      "% of Species with 1-3 C Value",
                                      "% of Species with 4-6 C Value",
                                      "% of Species with 7-10 C Value",
                                      "Mean C",
                                      "Native Mean C",
                                      "Total FQI",
                                      "Native FQI",
                                      "Adjusted FQI",
                                      "Mean Wetness",
                                      "Native Mean Wetness",
                                      "% Hydrophytes"),
                          values = c(fqacalc::species_richness(same_syn_sci, db = "wyoming_2017"),
                                     fqacalc::species_richness(same_syn_sci, db = "wyoming_2017", native = TRUE),
                                     0, #introduced
                                     0, #no c score
                                     0, #0 c score
                                     0, #1-3 c sscore
                                     100, #4-6 c score
                                     0, #7-10 c score
                                     fqacalc::mean_c(same_syn_sci, db = "wyoming_2017"),
                                     fqacalc::mean_c(same_syn_sci, db = "wyoming_2017", native = TRUE),
                                     fqacalc::FQI(same_syn_sci, db = "wyoming_2017"),
                                     fqacalc::FQI(same_syn_sci, db = "wyoming_2017", native = TRUE),
                                     fqacalc::adjusted_FQI(same_syn_sci, db = "wyoming_2017"),
                                     fqacalc::mean_w(same_syn_sci, db = "wyoming_2017"),
                                     fqacalc::mean_w(same_syn_sci, db = "wyoming_2017", native = TRUE),
                                     0 #%hydro
                          )))
})
