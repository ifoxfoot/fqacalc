
#this file contains `db_names()`, `view_db()`, and `unassigned plants()`

#-------------------------------------------------------------------------------

#' Look Up the Names of Regional FQA Databases
#'
#' Create a data frame containing the names of regional FQA databases contained in
#' this package as well as their certification status.
#'
#' @return A data frame of regional FQA database names. The column `name` contains
#' the names of the databases. These are acceptable values for `db` in other `fqacalc`
#' functions. The column `status` notes whether the database has been fully approved or
#' approved with reservations by the US Army Corps of Engineers.
#' @export
#'
#' @examples
#' db_names()

db_names <- function() {

  #filter system data for db names
  df <- data.frame(name = c(unique(fqa_db$fqa_db))) %>%
    dplyr::mutate(usace_status = "Approved 2021") %>%
    #note approval status
    dplyr::mutate(usace_status = dplyr::case_when(name %in%
                                              c("atlantic_coastal_pine_barrens_2018",
                                                "delaware_2013",
                                                "maine_new_brunswick_2018")
                                            ~ "Approved with reservations 2021",
                                            TRUE ~ usace_status)) %>%
    dplyr::mutate(usace_status = dplyr::case_when(name %in%
                                              c("chicago_region_2014",
                                                "dakotas_excluding_black_hills_2017",
                                                "kansas_2014")
                                            ~ "Previously Certified",
                                            TRUE ~ usace_status))

  #return names
  return(df)

}

#-------------------------------------------------------------------------------

#' View a Regional FQA Database
#'
#' Create a data frame containing an entire regional FQA database.
#'
#' @inheritParams accepted_entries
#'
#' @return A data frame with 12 variables:
#' \describe{
#'   \item{name}{Latin name, either proper name or synonym}
#'   \item{name_origin}{Indicates if the name is the accepted scientific name--"accepted_scientific_name"--or a synonym}
#'   \item{acronym}{A unique acronym for each species. Not always consistent between FQA data bases}
#'   \item{accepted_scientific_name}{The accepted/official scientific name}
#'   \item{family}{Taxonomic family of species}
#'   \item{nativity}{Nativity status. native, non-native, and undetermined are values}
#'   \item{c}{Coefficient of Conservatism (C Value)}
#'   \item{w}{Wetland Indicator Rating}
#'   \item{physiognomy}{Categories based on plant habit (architectural characteristics),
#'   life history, and certain taxonomic classes}
#'   \item{duration}{Categories based on life cycle}
#'   \item{common_name}{Common name(s) for plant}
#'   \item{fqa_db}{Regional FQA database}
#'   ...
#' }
#' @export
#'
#' @examples
#' view_db("michigan_2014")

view_db <- function(db) {

  #error if db is not a legit db
  if( !db %in% db_names()$name )
    stop(paste0(db," is not recognized. Run 'db_names()' for a list of acceptable db values."))

  #filter system data for correct db
  df <- fqa_db %>%
    dplyr::filter(fqa_db == db)

  #return db
  return(df)

}

#-------------------------------------------------------------------------------

#' Return Data Frame of Plant Species That Have No C Value
#'
#' Some regional FQA lists contain species which have not been assigned a C Value.
#' This is usually because the plant is unfamiliar to the botanists who assigned
#' the C Values or because there is little known about the plant. `unassigned_plants`
#' returns a data frame of plants in `x` that can be matched to a regional FQA database
#' but have no C Value. These observations can optionally be discarded in other `fqacalc`
#' functions.
#'
#' @inheritParams accepted_entries
#'
#' @return A data frame
#' @export
#'
#' @examples
#' no_c_test <- data.frame(name = c("ABRONIA FRAGRANS", "ACER GLABRUM",
#' "ACER GRANDIDENTATUM", "ACER PLATANOIDES"))
#'
#' unassigned_plants(no_c_test, key = "name", db = "montana_2017")
#'

unassigned_plants <- function(x, key = "name", db) {

  #call accepted entries
  entries_matched <- accepted_entries(x, key, db,
                                      native = FALSE,
                                      cover_weighted = FALSE,
                                      cover_metric = "percent_cover",
                                      allow_duplicates = FALSE,
                                      allow_no_c = TRUE,
                                      allow_non_veg = FALSE,
                                      plot_id = NULL) %>%
    #filter for No C value plants
    dplyr::filter(is.na(c))

 #return this for now
 return(entries_matched)

}


