#creating data sets to test on

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
                    scientific_name = c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus"))

#quadrat test
quadrat <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                      scientific_name = c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus"),
                      cover = c(50, 4, 20, 30))

#transect test
transect <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
                      cover = c(50, 4, 20, 30, 40, 7, 60),
                      quad_id = c(1, 1, 1, 1, 2, 2, 2))
#na c score
no_c_test <- data.frame(scientific_name = c("ABRONIA FRAGRANS", "ACER GLABRUM", "ACER GRANDIDENTATUM", "BOOP", "ACER PLATANOIDES"))
