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

#cover test
quadrat <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
                      scientific_name = c("Abelmoschus esculentus", "Abies balsamea", "Ammophila breviligulata", "Anticlea elegans; zigadenus glaucus"),
                      cover = c(50, 4, 20, 30))
