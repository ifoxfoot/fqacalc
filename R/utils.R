
#this file contains `db_names()`, `view_db()`, `accepted_entries()`, and `unassigned plants()`

#-------------------------------------------------------------------------------

#' Look Up the Names of Regional FQA Databases
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
#' @param db A character string representing the name of the regional FQA database
#' to retrieve. Generally, the format is "place_year".
#'
#' @return A data frame with 11 variables:
#' \describe{
#'   \item{scientific_name}{Latin name}
#'   \item{synonym}{Alternate latin name(s)}
#'   \item{family}{Taxonomic family of species}
#'   \item{acronym}{A unique acronym for each species. Not always consistent between FQA data bases}
#'   \item{native}{Nativity status. native, non-native, and undetermined are values}
#'   \item{c}{Coefficient of Conservation (C Value)}
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

#' Return Data Frame of Successfully Matched Plant Species
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#' @param cover_weighted Boolean (TRUE or FALSE). If TRUE, keep `cover` column in output.
#' Note: if `cover_weighted = TRUE`, `x` must have a column named `cover`. This parameter
#' is used to calculate cover-weighted metrics such as plot mean c, transect mean c, and
#' cover-weighted FQI.
#' @param cover_metric a character string representing the cover method used. Acceptable
#' cover methods are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default and is
#' recommended because it is the most accurate.
#' @param allow_duplicates Boolean (TRUE or FALSE). If TRUE, allow `x` to have
#' duplicate observations for the same species. This is only recommended for
#' calculating transect and frequency metrics.
#' @param allow_no_c Boolean (TRUE or FALSE). If TRUE, allow species that are found in the
#' regional database but have not been assigned a C Values. If FALSE, omit species that have not
#' been assigned C Values.
#'
#' @return A data frame containing the 'key' column--either `acronym` or
#' `scientific_name`--as well as columns from the relevant FQA database.
#' These columns include `family`, `native`, `c` (which represents the C Value),
#' `w` (which represents wetness score), `physiognomy`, `duration`, and `common_name`
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' plant_list <- crooked_island
#' accepted_entries(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)

accepted_entries <- function(x, key = "scientific_name", db,
                             native = c(TRUE, FALSE),
                             cover_weighted = FALSE,
                             cover_metric = "percent_cover",
                             allow_duplicates = FALSE,
                             allow_no_c = FALSE) {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if db argument is missing
  if( missing(db) )
    stop("argument db is missing, with no default.")

  #error if db argument is missing
  if( missing(native) )
    stop("argument native is missing, with no default.")

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("'key' argument must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), "does not have a column named", key, "."))

  #error if db is not a legit db
  if( !db %in% unique(fqa_db$fqa_db) )
    stop(paste(db, "not recognized. Run 'db_names()' for a list of acceptable db values."))

  #native must be TRUE or FALSE
  if( !is.logical(native) )
    stop("'native' can only be set to TRUE or FALSE")

  #include_no_c must be TRUE or FALSE
  if( !is.logical(allow_no_c) )
    stop("'allow_no_c' can only be set to TRUE or FALSE")

  #cover_weighted must be TRUE or FALSE
  if( !is.logical(cover_weighted) )
    stop("'cover_weighted' can only be set to TRUE or FALSE")

  #if cover is true, then there must be a column named cover in input df
  if( cover_weighted & !("cover" %in% colnames(x)))
    stop(paste("If 'cover = TRUE'", deparse(substitute(x)), "must have a column named cover."))

  #if cover is missing, write error
  if( cover_weighted && any(is.na(x$cover)) )
    stop(paste("Cover column cannot contain missing values."))

  #cover metric must be defined
  if( !cover_metric %in% c("percent_cover", "carolina_veg_survey",
                           "braun-blanquet","daubenmire",
                           "usfs_ecodata"))
    stop(paste(cover_metric, "is not an accepted cover-method. See documentation."))

  #allow__duplicates must be T or F
  if( !is.logical(allow_duplicates) )
    stop("'allow_duplicates' can only be set to TRUE or FALSE")

  #send message to user if site assessment contains duplicate entries
  if( sum(duplicated(x[,key])) > 0 && !allow_duplicates){
    if(cover_weighted == TRUE){
      message("Duplicate entries detected. Duplicates will only be counted once. Cover values of duplicate species will be added together.")}
    else{
      message("Duplicate entries detected. Duplicates will only be counted once.")}
  }

  #if cover parameter is true, select unique sci names and cover
  if( cover_weighted )
    {cols <- x %>%
      dplyr::select({{key}}, .data$cover) %>%
      dplyr::mutate(cover = as.character(x$cover))

    #if cover method is percent, just convert to numeric
    if(cover_metric == "percent_cover") {
      cols <- cols %>%
        dplyr::mutate(cover = suppressWarnings(as.numeric(cols$cover)))
    }

    #if cover method is usfs_ecodata, just convert to numeric because they use midpoint as class label
    if(cover_metric == "usfs_ecodata") {
      cols <- cols %>%
        dplyr::mutate(cover = dplyr::case_when(.data$cover == "1" ~ 0.5,
                                               TRUE ~ suppressWarnings(as.numeric(cols$cover))
                        ))
    }

    #if cover method is carolina, transform to 10 classes
    if(cover_metric == "carolina_veg_survey") {
      cols <- cols %>%
        dplyr::mutate(cover = dplyr::case_when(.data$cover == "1" ~ 0.1,
                                               .data$cover == "2" ~ 0.5,
                                               .data$cover == "3" ~ 1.5,
                                               .data$cover == "4" ~ 3.5,
                                               .data$cover == "5" ~ 7.5,
                                               .data$cover == "6" ~ 17.5,
                                               .data$cover == "7" ~ 37.5,
                                               .data$cover == "8" ~ 62.5,
                                               .data$cover == "9" ~ 85,
                                               .data$cover == "10" ~ 97.5))
    }

    #if cover method is daubenmire, transform to six classes
    if(cover_metric == "daubenmire") {
      cols <- cols %>%
        dplyr::mutate(cover = dplyr::case_when(.data$cover == "1" ~ 2.5,
                                               .data$cover == "2" ~ 15,
                                               .data$cover == "3" ~ 37.5,
                                               .data$cover == "4" ~ 62.5,
                                               .data$cover == "5" ~ 85,
                                               .data$cover == "6" ~ 97.5))
    }

    #if cover method is braun-blanquet, transform to 5 classes
    if(cover_metric == "braun-blanquet") {
      cols <- cols %>%
        dplyr::mutate(cover = dplyr::case_when(.data$cover == "+" ~ 0.1,
                                               .data$cover == "1" ~ 2.5,
                                               .data$cover == "2" ~ 15,
                                               .data$cover == "3" ~ 37.5,
                                               .data$cover == "4" ~ 62.5,
                                               .data$cover == "5" ~ 87.5))
    }


  } else( cols <- x %>%
            dplyr::select({{key}}))

  #warning if NAs get introduced after converting cover metric
  if( cover_weighted && any(is.na(cols$cover)) )
    stop(paste("NAs were introduced during the conversion to the ",
               cover_metric, "system."))

  #if allow duplicates is false, do not allow duplicates
  if( !allow_duplicates ) {
    if ( !cover_weighted ){ cols <- cols %>% dplyr::distinct() }
    #if allow dups is false but cover weight is true, add cover values for like species together
    else(cols <- cols %>%
           dplyr::group_by(!!as.name(key)) %>%
           dplyr::summarise(cover = sum(.data$cover)))
    }

  #join scores from FQAI to user's assessment
  entries_joined <-
    dplyr::left_join(cols %>%
                       dplyr::mutate({{key}} := toupper(!!as.name(key))),
                     fqa_db %>%
                       dplyr::filter(fqa_db == db) %>%
                       #so i can tell if observation came from regional list
                       dplyr::mutate(p = "p"),
                     by = key)

  #warning if species not present in regional list
  if( any(is.na(entries_joined$p)) )
       message(paste("Species", entries_joined[is.na(entries_joined$p), key],
                     "not listed in database. It will be discarded."))

  #now get rid of observations not in regional list
  entries_joined <- entries_joined[!is.na(entries_joined$p),] %>%
    dplyr::select(-.data$p)

  #if native = T, filter for only native species
  if (native) {
    entries_joined <- entries_joined %>%
      dplyr::filter(native == "native")
  }

  #send message to user if site assessment contains plant not in FQAI database
  if( any(is.na(entries_joined$c)) )
    message(paste("Species", entries_joined[is.na(entries_joined$c), key],
                  "is recognized but has not been assigned a C Value. It can optionally be included in species richness but will not be included in any FQI metrics. "))

  #if allow no c is false, get rid of observations with no c value
  if( !allow_no_c ) {
    entries_joined <- entries_joined[!is.na(entries_joined$c),]  }

  return(entries_joined)
}

#-------------------------------------------------------------------------------

#' Return Data Frame of Plant Species That Have No C Value
#'
#' Some regional FQA lists contain species which have not been assigned a C Value.
#' This is usually because the plant is unfamiliar to the botanists who assigned
#' the C Values or because there is little known about the plant. `unassigned_plants`
#' returns a data frame of plants in `x` that can be matched to a regional FQA database
#' but have no C Value. These observations are discarded in other `fqacalc` functions.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' no_c_test <- data.frame(scientific_name = c("ABRONIA FRAGRANS", "ACER GLABRUM",
#' "ACER GRANDIDENTATUM", "ACER PLATANOIDES"))
#'
#' unassigned_plants(no_c_test, key = "scientific_name", db = "montana_2017")
#'

unassigned_plants <- function(x, key = "scientific_name", db) {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if db argument is missing
  if( missing(db) )
    stop("argument db is missing, with no default.")

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("'key' argument must be equal to 'acronym' or 'scientific_name'.")

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


