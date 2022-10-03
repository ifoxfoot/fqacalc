
#-------------------------------------------------------------------------------
#testing relative_freq()

test_that("relative_freq() works in perfect setting", {
  expect_equal(relative_freq(transect, key = "acronym", db = "michigan_2014", col = "physiog"),
               data.frame(physiognomy = c("forb", "grass", "tree"),
                          rel_freq = c(42.857143, 28.571429, 28.571429)))
})

#-------------------------------------------------------------------------------
#testing relative_cover()

test_that("relative_cover() works in perfect setting", {
  expect_equal(relative_cover(transect, key = "acronym", db = "michigan_2014", col = "physiog"),
               data.frame(physiognomy = c("forb", "grass", "tree"),
                          rel_cov = c(56.8720379, 37.9146919, 5.2132701)))
})

#-------------------------------------------------------------------------------
#testing relative_importance()

test_that("relative_importance() works in perfect setting", {
  expect_equal(relative_importance(transect, key = "acronym", db = "michigan_2014", col = "family"),
               data.frame(family = c("Malvaceae", "Melanthiaceae", "Pinaceae", "Poaceae"),
                          rel_import = c(35.6127285, 14.2518619, 16.8923494, 33.2430603)))
})

#-------------------------------------------------------------------------------
#testing species_summary()

test_that("species_summary() works in perfect setting", {
  expect_equal(species_summary(transect, key = "acronym", db = "michigan_2014"),

               data.frame(scientific_name = c("ABELMOSCHUS ESCULENTUS",
                                              "ABIES BALSAMEA",
                                              "AMMOPHILA BREVILIGULATA",
                                              "ANTICLEA ELEGANS; ZIGADENUS GLAUCUS"),
                          acronym = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                          native = c("exotic", "native", "native", "native"),
                          c = c(0, 3, 10, 10),
                          w = c(5, 0, 5, -3),
                          frequency = c(2,2,2,1),
                          coverage = c(90, 11, 80, 30),
                          rel_freq = c(28.5714286, 28.5714286, 28.5714286, 14.2857143),
                          rel_cov = c(42.6540284, 5.2132701, 37.9146919, 14.2180095),
                          rel_import = c(35.6127285, 16.8923494, 33.2430603, 14.2518619)))
})
