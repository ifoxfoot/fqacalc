#-------------------------------------------------------------------------------
#testing mean_w()

test_that("mean_w() works for all species", {
  expect_equal(mean_w(crooked_island, key = "acronym", db = "michigan_2014", native = FALSE),
               0.71428571)
})

test_that("mean_w() works for native species", {
  expect_equal(mean_w(crooked_island, key = "acronym", db = "michigan_2014", native = TRUE),
               0.85714286)
})
