
#-------------------------------------------------------------------------------
#testing view_db()

test_that("view_db() throws error if db is not recognized", {
  expect_error(view_db("michigaan_2014"),
               "michigaan_2014 is not recognized.")
})

#test that output has the right dimensions
test_that("view_db() throws error if db is not recognized", {
  expect_equal(dim(view_db("michigan_2014")),
                   c(3356, 13))
})

#test that output has the right column names
test_that("view_db() throws error if db is not recognized", {
  expect_equal(names(view_db("michigan_2014")),
               c("name", "name_origin", "acronym", "accepted_scientific_name",
                 "family", "nativity", "c", "w", "wetland_indicator", "physiognomy", "duration",
                 "common_name", "fqa_db"))
})

#-------------------------------------------------------------------------------
#testing unassigned_plants()

#testing don't allow_non_c
test_that("unassigned plants works in perfect setting", {
  expect_equal(unassigned_plants(no_c_test, key = "name", db = "montana_2017"),

               data.frame(name = c("ABRONIA FRAGRANS", "ACER GRANDIDENTATUM"),
                          name_origin = c("accepted_scientific_name", "accepted_scientific_name"),
                          acronym = c(NA_character_, NA_character_),
                          accepted_scientific_name = c("Abronia fragrans", "Acer grandidentatum"),
                          family = c("Nyctaginaceae", "Aceraceae"),
                          nativity = c("native", "undetermined"),
                          c = c(NA_real_, NA_real_),
                          w = c(NA_real_, NA_real_),
                          wetland_indicator = c(NA_character_, NA_character_),
                          physiognomy = c(NA_character_, NA_character_),
                          duration = c(NA_character_, NA_character_),
                          common_name = c("Fragrant White Sand-Verbena", "Bigtooth Maple"),
                          fqa_db = c("montana_2017", "montana_2017")))
})
