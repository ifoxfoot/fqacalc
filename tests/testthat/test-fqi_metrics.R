
#-------------------------------------------------------------------------------
#testing total_species_richness()

test_that("total_species_richness() calculates total species richness", {
  expect_equal(total_species_richness(crooked_island), 35)
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

