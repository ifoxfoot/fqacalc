#creating data sets to test on

#data frame with duplicates in both rows
full_dup <- data.frame (acronym  = c("ABIBAL", "ABIBAL", "AMMBRE", "ANTELE"),
                         scientific_name = c(c("Abies balsamea", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus"))
                        )


#data frame with duplicates in one row, separate matching cases in another row
partial_dup_match <- data.frame(acronym  = c("ABIBAL", "ARCUVA", "AMMBRE", "ANTELE"),
                                scientific_name = c("Abies balsamea", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus")
                                )

#data frame with duplicates in one row, separate matching cases in another row
partial_dup_partial_match <- data.frame(acronym  = c("ABIBAL", "typo", "AMMBRE", "ANTELE"),
                                        scientific_name = c("Abies balsamea", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus")
                                        )

#non_match in one column
partial_no_match <- data.frame(acronym  = c("ABIBAL", "typo", "AMMBRE", "ANTELE"),
                               scientific_name = c("Abies balsamea", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus")
                               )

#non_match in one column
full_no_match <- data.frame(acronym  = c("ABIBAL", "typo", "AMMBRE", "ANTELE"),
                            scientific_name = c("Abies balsamea", "typo", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus")
                            )


#-------------------------------------------------------------------------------
#testing total_species_richness()

#perfect case
test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_species_richness(crooked_island), 35)
})

#duplication that extends across both columns
test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_species_richness(full_dup), 3)
  expect_message(total_species_richness(full_dup))
})

#duplication in scientific name, no duplication in acronym column
test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_species_richness(partial_dup_match), 4)
})

#duplication in scientific name, no duplication in acronym column
test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_species_richness(partial_dup_match, key = "scientific_name"), 3)
  expect_message(total_species_richness(partial_dup_match, key = "scientific_name"))
})

#duplication in one column, one non-match in other
test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_species_richness(partial_dup_partial_match), 3)
  expect_message(total_species_richness(partial_dup_partial_match))
})

#-------------------------------------------------------------------------------
#testing native_species_richness()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(native_species_richness(crooked_island), 28)
})

#-------------------------------------------------------------------------------
#testing total_mean_c()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_mean_c(crooked_island), 5.3714286)
})

#-------------------------------------------------------------------------------
#testing native_mean_c()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(native_mean_c(crooked_island), 6.7142857)
})

#-------------------------------------------------------------------------------
#testing total_FQI()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_FQI(crooked_island), 31.7778)
})

#-------------------------------------------------------------------------------
#testing native_FQI()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(native_FQI(crooked_island), 35.52866)
})

#-------------------------------------------------------------------------------
#testing adjusted_FQI()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(adjusted_FQI(crooked_island), 60.054397)
})

