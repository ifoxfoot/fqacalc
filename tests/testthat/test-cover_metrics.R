#-------------------------------------------------------------------------------
#testing quadrat_mean_c()

test_that("plot_mean_c() works in perfect setting", {
  expect_equal(plot_mean_c(x = quadrat, key = "acronym", db = "michigan_2014", native = FALSE),
               4.92307692)
})


#-------------------------------------------------------------------------------
#testing transect_mean_c()

test_that("transect_mean_c() works in perfect setting", {
  expect_equal(transect_mean_c(x = transect, key = "acronym", db = "michigan_2014", native = FALSE),
               5.94605809)
})

#-------------------------------------------------------------------------------
#testing cover_FQI()

test_that("cover_FQI() works in perfect setting", {
  expect_equal(cover_FQI(x = transect, key = "acronym", db = "michigan_2014", native = FALSE),
               11.8921162)
})

# #-------------------------------------------------------------------------------
# #testing relative_freq()
#
# test_that("relative_freq() works in perfect setting", {
#   expect_equal(relative_freq(transect, key = "acronym", db = "michigan_2014", native = F, physiog = "tree"),
#                28.5714285)
# })
#
# #-------------------------------------------------------------------------------
# #testing relative_cover()
#
# test_that("relative_cover() works in perfect setting", {
#   expect_equal(relative_cover(transect, key = "acronym", db = "michigan_2014", native = F, physiog = "tree"),
#                5.2132701)
# })
#
# #-------------------------------------------------------------------------------
# #testing relative_importance()
#
# test_that("relative_importance() works in perfect setting", {
#   expect_equal(relative_importance(transect, key = "acronym", db = "michigan_2014", native = F, physiog = "tree"),
#                16.8923494)
# })
