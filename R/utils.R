
#this file contains `db_names()`, `view_db()`, and `accepted_entries()`

#-------------------------------------------------------------------------------

#' Look Up the Names of Regional FQAI Databases
#'
#' @return A list of regional FQAI data base names. These are acceptable values
#' for `db` in other `fqacalc` functions.
#' @export
#'
#' @examples
#' db_names()

db_names <- function() {

  #filter system data for db names
  names <- unique(fqa_db$fqa_db)

  #return names
  return(names)

}

#-------------------------------------------------------------------------------

#' Call a Regional FQAI Database
#'
#' @param db A character string representing the name of the regional FQAI data
#' base to retrieve. Generally, the format is "place_year".
#'
#' @return The regional FQAI data base.
#' @export
#'
#' @examples
#' view_db("michigan_2014")

view_db <- function(db) {

  #error if db is not a legit db
  if( !db %in% db_names() )
    stop(paste(db," is not recognized. Run 'db_names()' for a list of acceptable db values."))

  #filter system data for correct db
  df <- fqa_db %>%
    dplyr::filter(fqa_db == db)

  #return db
  return(df)

}

#-------------------------------------------------------------------------------

#' Return Data Frame of Successfully Matched Plant Species
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#'
#' @return A data frame containing the 'key' column --either `acronym` or
#' `scientific_name` -- as well as columns from the Michigan 2014 fqai database.
#' These columns include `family`, `native`, `c` (which represents the C score),
#' `w` (which represents wetness score), `physiognomy`, `duration`, and `common_name`
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' adjusted_FQI(x = plant_list, key = "acronym", db = "michigan_2014")

accepted_entries <- function(x, key = "acronym", db) {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

  #error if db is not a legit db
  if( !db %in% unique(fqa_db$fqa_db) )
    stop(paste(db, " not recognized. Run 'db_names()' for a list of acceptable db values."))

  #send message to user if site assessment contains duplicate entries
  if( sum(duplicated(x[,key])) > 0 )
    message("Duplicate entries detected. Duplicates will only be counted once.")

  #select only unique entries
  unique_entries <- x %>%
    dplyr::distinct(!!as.name(key))

  #join scores from FQAI to user's assessment
  unique_entries_joined <-
    dplyr::left_join(unique_entries %>%
                       dplyr::mutate(!!key := toupper(!!as.name(key))),
                     fqa_db %>%
                       dplyr::filter(fqa_db == db),
                     by = key)

  #send message to user if site assessment contains plant not in FQAI database
  if( any(is.na(unique_entries_joined$c)) )
    message(paste("species", unique_entries_joined[is.na(unique_entries_joined$c), key],
                  "not listed in database. It will be discarded."))

  #discard entries that have no c score, select native entries
  unique_entries_matched <- unique_entries_joined %>%
    dplyr::filter(!is.na(c))

  return(unique_entries_matched)

}
