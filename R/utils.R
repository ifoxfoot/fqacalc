
#this file contains `db_names()`, `view_db()`, and `unassigned plants()`

#-------------------------------------------------------------------------------

#' Look Up the Names of Regional FQA Databases
#'
#' Create a data frame of regional FQA database names, approval status, notes, and citations.
#' The column `fqa_db` contains the names of the databases. These are acceptable values for `db`
#' in other `fqacalc` functions.
#'
#' @return A data frame.
#'
#' @format A data frame with 44 rows and 4 variables:
#' \describe{
#'   \item{fqa_db}{Regional FQA database}
#'   \item{recommendation}{Indicates if the regional FQA database was recommended for use by the U.S. Army Corps of Engineers in 2020}
#'   \item{notes}{Notes on the limitations or recommended usage of the regional FQA database}
#'   \item{citation}{A citation for the regional FQA database}
#' }
#'
#' @export
#'
#' @examples
#' db_names()

db_names <- function() {

  #return names
  return(fqadata::fqa_citations)

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
#'   \item{name}{Latin name for species, either accepted name or synonym}
#'   \item{name_origin}{Indicates if the name is the accepted scientific name or a synonym}
#'   \item{acronym}{A unique acronym for each species. Not always consistent between FQA databases}
#'   \item{accepted_scientific_name}{The accepted botanical nomenclature}
#'   \item{family}{Taxonomic family of species}
#'   \item{nativity}{Nativity status. Native, introduced, and undetermined are possible values}
#'   \item{c}{Coefficient of Conservatism (C Value)}
#'   \item{w}{Wetness Coefficient}
#'   \item{wetland_indicator}{Wetland indicator status}
#'   \item{physiognomy}{Structure or physical appearance of species}
#'   \item{duration}{Lifespan of species}
#'   \item{common_name}{Common name(s) for species}
#'   \item{fqa_db}{Regional FQA database}
#' }
#' @export
#'
#' @source See `db_names` function for citations
#'
#' @examples
#' view_db("michigan_2014")

view_db <- function(db) {

  #error if db is not a legit db
  if( !db %in% db_names()$fqa_db )
    stop(paste0(db," is not recognized. Run 'db_names()' for a list of acceptable db values."))

  #filter system data for correct db
  df <- fqadata::fqa_db %>%
    dplyr::filter(.data$fqa_db == db)

  #return db
  return(df)

}

#-------------------------------------------------------------------------------

#' Return Data Frame of Plant Species That Have No C Value
#'
#' Some regional FQA lists contain species which have not been assigned a C Value.
#' `unassigned_plants` returns a data frame of plants in `x` that can be matched
#' to a regional FQA database but have no C Value. These observations can optionally
#' be discarded in other `fqacalc` functions.
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
                                      cover_class = "percent_cover",
                                      allow_duplicates = FALSE,
                                      allow_no_c = TRUE,
                                      allow_non_veg = FALSE,
                                      plot_id = NULL) %>%
    #filter for No C value plants
    dplyr::filter(is.na(c))

 #return this for now
 return(entries_matched)

}


