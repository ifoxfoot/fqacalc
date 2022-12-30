#-------------------------------------------------------------------------------
#testing mean_w()

test_that("mean_w() throws message for state with multiple wetland indicator regions", {
  expect_message(mean_w(same_syn_sci, key = "name", db = "colorado_2020", native = FALSE),
               "The Colorado FQA database is associated with multiple wetland indicator status regions.")
})

test_that("mean_w() throws message for state with multiple wetland indicator regions", {
  expect_message(mean_w(same_syn_sci, key = "name", db = "wyoming_2017", native = FALSE),
                 "The Wyoming FQA database is associated with multiple wetland indicator status regions.")
})

test_that("mean_w() works for all species", {
  expect_equal(mean_w(crooked_island, key = "acronym", db = "michigan_2014", native = FALSE),
               0.71428571)
})

test_that("mean_w() works for native species", {
  expect_equal(mean_w(crooked_island, key = "acronym", db = "michigan_2014", native = TRUE),
               0.85714286)
})
