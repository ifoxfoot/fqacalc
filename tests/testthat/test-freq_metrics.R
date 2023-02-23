
#-------------------------------------------------------------------------------
#testing relative_freq()

test_that("relative_frequency() works", {
  expect_equal(relative_frequency(transect, key = "acronym", db = "michigan_2014", col = "physiog"),
               data.frame(physiognomy = c("forb", "grass", "tree"),
                          relative_frequency = c(42.857143, 28.571429, 28.571429)))
  expect_error(relative_frequency(transect, key = "acronym", db = "michigan_2014", col = "plant"),
               "'col' argument can only be set to 'species', 'family', or 'physiog'")
})

#-------------------------------------------------------------------------------
#testing relative_cover()

test_that("relative_cover() works", {
  expect_equal(relative_cover(transect, key = "acronym", db = "michigan_2014", col = "physiog"),
               data.frame(physiognomy = c("forb", "grass", "tree"),
                          relative_cover = c(56.8720379, 37.9146919, 5.2132701)))
  expect_error(relative_cover(transect, key = "acronym", db = "michigan_2014", col = "plant"),
               "'col' argument can only be set to 'species', 'family', or 'physiog'")
})

#-------------------------------------------------------------------------------
#testing relative_importance()

test_that("relative_importance() works", {
  expect_equal(relative_importance(transect, key = "acronym", db = "michigan_2014", col = "family"),
               data.frame(family = c("Malvaceae", "Melanthiaceae", "Pinaceae", "Poaceae"),
                          relative_importance = c(35.6127285, 14.2518619, 16.8923494, 33.2430603)))

  expect_error(relative_importance(transect, key = "acronym", db = "michigan_2014", col = "plant"),
               "'col' argument can only be set to 'species', 'family', or 'physiog'")
})

#-------------------------------------------------------------------------------
#testing species_summary()

test_that("species_summary() works in perfect setting", {
  expect_equal(species_summary(transect, key = "acronym", db = "michigan_2014"),

               data.frame(acronym = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                          name = c("ABELMOSCHUS ESCULENTUS",
                                              "ABIES BALSAMEA",
                                              "AMMOPHILA BREVILIGULATA",
                                              "ANTICLEA ELEGANS"),
                          nativity = c("introduced", "native", "native", "native"),
                          c = c(0, 3, 10, 10),
                          w = c(5, 0, 5, -3),
                          frequency = c(2,2,2,1),
                          coverage = c(90, 11, 80, 30),
                          relative_frequency = c(28.5714286, 28.5714286, 28.5714286, 14.2857143),
                          relative_cover = c(42.6540284, 5.2132701, 37.9146919, 14.2180095),
                          relative_importance = c(35.6127285, 16.8923494, 33.2430603, 14.2518619)))
})

#-------------------------------------------------------------------------------
#testing physiog_summary()

test_that("physiog_summary() works in perfect setting", {
  expect_equal(physiog_summary(transect, key = "acronym", db = "michigan_2014"),

               data.frame(physiognomy = c("forb",
                                          "grass",
                                          "tree"),
                          frequency = c(3,2,2),
                          coverage = c(120, 80, 11),
                          relative_frequency = c(42.85714286, 28.5714286, 28.5714286),
                          relative_cover = c(56.87203791, 37.91469194, 5.21327014),
                          relative_importance = c(49.86459,  33.2430603, 16.8923494)))
})
