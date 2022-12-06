#this file contains toy datasets for the purpose of testing/examples

#data frame with incorrect column names
bad_names <- data.frame(acronot  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                        scientific_not = c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus"))

#data frame with one incorrect column name
one_bad_names <- data.frame(acronot  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                            scientific_name = c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus"))

#character string
character_string <- list("character", "string", "not", "df")

#integers
numbers <- 1:5

#data frame with duplicates in both rows
duplicate <- data.frame(acronym  = c("ABIBAL", "ABIBAL", "AMMBRE", "ABEESC"),
                           scientific_name = c("Abies balsamea", "Abies balsamea", "Ammophila breviligulata", "Abelmoschus esculentus"))

#data frame with incorrect entries in both rows
typo <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "typo"),
                   scientific_name = c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "typo"))

#data frame with incorrect entries in both rows
fuzzy <- data.frame(acronym  = c("abeesc", "ABIBAL", "AMMBRE", "ANTELE"),
                    scientific_name = c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans"))

#transect test
transect <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
                      cover = c(50, 4, 20, 30, 40, 7, 60),
                      quad_id = c(1, 1, 1, 1, 2, 2, 2))
#na C Value
no_c_test <- data.frame(scientific_name = c("ABRONIA FRAGRANS", "ACER GLABRUM", "ACER GRANDIDENTATUM", "BOOP", "ACER PLATANOIDES"))

#cover method test
cover_test <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                      scientific_name = c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans"),
                      cover = c(1, 2, 3, 4))

#na in cover test
na_intro_cover <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                       cover = c(50, "A", 20, 30))

#transects with duplicates in same plot and with non-veg components
transect_dup <- data.frame(acronym  = c("GROUND", "GROUND", "ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "WATER", "GROUND", "ABEESC", "ABIBAL", "AMMBRE"),
                           cover = c(60, 40, 50, 4, 20, 30, 20, 20, 40, 7, 60),
                           quad_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2))

#quadrant test
quadrat <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                      scientific_name = c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans"),
                      cover = c(50, 4, 20, 30))

accepted_perfect <- data.frame(acronym  = c("ABEESC", "ABIBAL"),
                       cover = c(60, 50),
                       quad_id = c(1, 1))

accepted_dup <- data.frame(acronym  = c("ABEESC", "ABIBAL", "ABIBAL"),
                               cover = c(60, 50, 50),
                               quad_id = c(1, 2, 2))

accepted_non_veg <- data.frame(acronym  = c("ABEESC", "GROUND", "WATER"),
                           cover = c(60, 50, 1),
                           quad_id = c(1, 2, 2))

accepted_no_c <- data.frame(scientific_name  = c("ABRONIA FRAGRANS", "ACER GLABRUM"))

accepted_br <-  data.frame(acronym  = c("ABEESC", "ABIBAL", "ABIBAL"),
                             cover = c(5, "+", 50),
                             quad_id = c(1, 2, 2))

accepted_br_dup <-  data.frame(acronym  = c("ABEESC", "ABIBAL", "ABIBAL"),
                           cover = c(5, "+", 3),
                           quad_id = c(1, 2, 2))

accepted_cover_method <-  data.frame(acronym  = c("ABEESC", "ABIBAL"),
                               cover = c(1, 3))

accepted_bad_cov_val <-  data.frame(acronym  = c("ABEESC", "ABIBAL"),
                                     cover = c(25, 3))
