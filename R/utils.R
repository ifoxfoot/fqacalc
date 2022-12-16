
#this file contains `db_names()`, `view_db()`, and `unassigned plants()`

#-------------------------------------------------------------------------------

#' Look Up the Names of Regional FQA Databases
#'
#' Create a data frame containing the names of regional FQA data bases contained in
#' this package as well as their certification status.
#'
#' @return A data frame of regional FQA database names. The column `name` contains
#' the names of the databases. These are acceptable values for `db` in other `fqacalc`
#' functions.The column `status` notes whether the database has been fully approved or
#' approved with reservations by the US Army Corps of Engineers.
#' @export
#'
#' @examples
#' db_names()

db_names <- function() {

  #filter system data for db names
  df <- data.frame(name = c(unique(fqa_db$fqa_db))) %>%
    dplyr::mutate(status = "Approved") %>%
    #note approval status
    dplyr::mutate(status = dplyr::case_when(name %in%
      c("atlantic_coastal_pine_barrens_2018",
      "connecticut_2013",
      "deleware_2013",
      "maine_2014",
      "maine_new_brunswick_2018",
      "vermont_2013")
      ~ "Approved with reservations", TRUE ~ status))

  #return names
  return(df)

}

#-------------------------------------------------------------------------------

#' Call a Regional FQA Database
#'
#' Create a data frame containing an entire regional FQA database.
#'
#' @param db A character string representing the name of the regional FQA database
#' to retrieve. Generally, the format is "place_year".
#'
#' @return A data frame with 11 variables:
#' \describe{
#'   \item{name}{Latin name, either proper name or synonym}
#'   \item{name_origin}{Indicates if the name is the accepted scientific name--"accepted_scientific_name"--or a synonym}
#'   \item{acronym}{A unique acronym for each species. Not always consistent between FQA data bases}
#'   \item{accepted_scientific_name}{The accepted/official scientific name}
#'   \item{family}{Taxonomic family of species}
#'   \item{nativity}{Nativity status. native, non-native, and undetermined are values}
#'   \item{c}{Coefficient of Conservatism (C Value)}
#'   \item{w}{Wetland Indicator Rating}
#'   \item{physiognomy}{Structure or physical appearance of plant}
#'   \item{duration}{Lifespan of plant}
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
    stop(paste(db," is not recognized. Run 'db_names()' for a list of acceptable db values."))

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
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `name` or `acronym`.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"name"`. `"name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
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

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "name") )
    stop("'key' argument must be equal to 'acronym' or 'name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

  #error if db is not a legit db
  if( !db %in% unique(fqa_db$fqa_db) )
    stop(paste(db, " not recognized. Run 'db_names()' for a list of acceptable db values."))

  #get distinct values
  cols <- x %>%
    dplyr::distinct(!!as.name(key))

 #join scores from FQAI to user's assessment
 entries_joined <-
   dplyr::inner_join(cols %>%
                      dplyr::mutate(!!key := toupper(!!as.name(key))),
                    fqa_db %>%
                      dplyr::filter(fqa_db == db),
                    by = key)

 for ( i in x[, key] ) {
   #send message to user if site assessment contains plant not in FQA database
   if( !toupper(i) %in% entries_joined[,key] )
     message(paste("Species", toupper(i), "not listed in database. It will be discarded."))
  }

 #discard entries that have no match
 entries_matched <- entries_joined %>%
   dplyr::filter(is.na(entries_joined$c))

 #return this for now
 return(entries_matched)

}


