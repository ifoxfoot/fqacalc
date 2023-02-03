
#this file contains `accepted_entries()`

#-------------------------------------------------------------------------------

#' Return A Data Frame of Plant Species That Successfully Match to the Regional
#' FQA Database of choice
#'
#' `accepted_entries` takes a data frame of user-entered plant species and returns
#' a data frame of plant species that are successfully matched to the regional FQA
#' database of choice. `accepted_entries` is a utility function that is used in all
#' other metric-calculating functions in this package.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `name` or `acronym`.
#' @param key A character string representing the column that will be used to join
#' the input data frame `x` with the regional FQA database. If a value is not specified, the
#' default is `"name"`. `"name"` and `"acronym"` are the only acceptable
#' values for `key`.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#' @param wetland_warning Boolean (TRUE or FALSE). If TRUE, show user messages regarding
#' issues with wetland coefficients.
#' @param cover_weighted Boolean (TRUE or FALSE). If TRUE, keep `cover` column in output.
#' Note: if `cover_weighted = TRUE`, `x` must have a column named `cover`. This parameter
#' is used to calculate cover-weighted metrics such as plot mean c, transect mean c, and
#' cover-weighted FQI.
#' @param cover_class a character string representing the cover classification used. Acceptable
#' cover classes are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default.
#' @param allow_duplicates Boolean (TRUE or FALSE). If TRUE, allow `x` to have
#' duplicate observations for the same species. This is only recommended for
#' calculating transect and frequency/abundance metrics.
#' @param allow_no_c Boolean (TRUE or FALSE). If TRUE, allow species that are found in the
#' regional database but have not been assigned a C Values. If FALSE, omit species that have not
#' been assigned C Values.
#' @param allow_non_veg Boolean (TRUE or FALSE). If TRUE, allow input to contain un-vegetated
#' ground and un-vegetated water.
#' @param plot_id (optional) A character string representing the column in `x` that contains plot
#' identification values.
#'
#' @return A data frame containing the `key` column--either `acronym` or
#' `name`--as well as columns from the relevant FQA database.
#' These columns include `family`, `native`, `c` (which represents the C Value),
#' `w` (which represents wetness score), `physiognomy`, `duration`, and `common_name`
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #with native and non natives
#' accepted_entries(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #with only native species
#' accepted_entries(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)
#'
#' #an example with duplicates allowed
#' duplicate_df <- data.frame(acronym  = c("ABEESC", "ABIBAL", "ABIBAL"),
#' cover = c(60, 50, 50),
#' quad_id = c(1, 2, 2))
#'
#' accepted_entries(x = duplicate_df, key = "acronym",
#' db = "michigan_2014", native = FALSE, allow_duplicates = TRUE)
#'
#' #an example of duplicates not allowed
#' accepted_entries(x = duplicate_df, key = "acronym",
#' db = "michigan_2014", native = FALSE, allow_duplicates = FALSE)
#'
#' #an example of duplicates not allowed, adding cover values
#' accepted_entries(x = duplicate_df, key = "acronym",
#' db = "michigan_2014", native = FALSE, allow_duplicates = FALSE,
#' cover_weighted = TRUE)
#'
#' #an example where some entries are synonyms shared by more than one species
#' same_syn <- data.frame(name = c("CAREX MURICATA", "POTENTILLA NANA", "ABIES BIFOLIA"),
#' cover = c(80, 60, 10))
#'
#' accepted_entries(x = same_syn, key = "name",
#' db = "wyoming_2017", native = FALSE)
#'
#' #an example where species is both a synonym and an accepted name
#' same_syn2 <- data.frame(name = c("CAREX FOENEA", "CAREX FOENEA", "ABIES BIFOLIA"),
#' cover = c(80, 60, 10))
#'
#' accepted_entries(x = same_syn2, key = "name",
#' db = "wyoming_2017", native = FALSE)
#'

accepted_entries <- function(x, key = "name", db,
                             native = c(TRUE, FALSE),
                             wetland_warning = TRUE,
                             cover_weighted = FALSE,
                             cover_class = "percent_cover",
                             allow_duplicates = FALSE,
                             allow_no_c = FALSE,
                             allow_non_veg = FALSE,
                             plot_id = NULL) {

  #ERRORS

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
  if( !key %in% c("acronym", "name") )
    stop("'key' argument must be equal to 'acronym' or 'name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), "does not have a column named", key, "."))

  #error if db is not a legit db
  if( !db %in% db_names()$name )
    stop(paste(db, "not recognized. Run 'db_names()' for a list of acceptable db values."))

  #native must be TRUE or FALSE
  if( !is.logical(native) )
    stop("'native' can only be set to TRUE or FALSE")

  #wetland_warning must be TRUE or FALSE
  if( !is.logical(wetland_warning) )
    stop("'wetland_warning' can only be set to TRUE or FALSE")

  #messages about wetland status indicator defaults
  if (wetland_warning & db == "wyoming_2017")
    message("The Wyoming FQA database is associated with multiple wetland indicator status regions. This package defaults to the Arid West wetland indicator region when calculating Wyoming metrics.")

  if (wetland_warning & db == "colorado_2020")
    message("The Colorado FQA database is associated with multiple wetland indicator status regions. This package defaults to the Western Mountains, Valleys, and Coasts indicator region when calculating Colorado metrics.")

  #native must be TRUE or FALSE
  if( !is.logical(native) )
    stop("'native' can only be set to TRUE or FALSE")

  #allow_no_c must be TRUE or FALSE
  if( !is.logical(allow_no_c) )
    stop("'allow_no_c' can only be set to TRUE or FALSE")

  #allow_non_veg must be TRUE or FALSE
  if( !is.logical(allow_non_veg) )
    stop("'allow_non_veg' can only be set to TRUE or FALSE")

  #cover_weighted must be TRUE or FALSE
  if( !is.logical(cover_weighted) )
    stop("'cover_weighted' can only be set to TRUE or FALSE")

  #allow_duplicates must be T or F
  if( !is.logical(allow_duplicates) )
    stop("'allow_duplicates' can only be set to TRUE or FALSE")

  #if cover is true, then there must be a column named cover in input df
  if( cover_weighted & !("cover" %in% colnames(x)))
    stop(paste("If 'cover = TRUE'", deparse(substitute(x)), "must have a column named cover."))

  #if cover is missing, write error
  if( cover_weighted && any(is.na(x$cover)) )
    stop(paste("'cover' column cannot contain missing values."))

  #cover metric must be defined
  if( !cover_class %in% c("percent_cover", "carolina_veg_survey",
                           "braun-blanquet","daubenmire",
                           "usfs_ecodata"))
    stop(paste(cover_class, "is not an accepted cover-method. See documentation."))

  #plot_id argument must be NULL or a column name in input data frame x
  if( !is.null(plot_id) && !(plot_id %in% colnames(x)) )
    stop(paste0("'plot_id' must be the name of a column in ", deparse(substitute(x)), " ."))

  #CONVERTING COVER CLASSES

  #if cover parameter is true, select unique sci names and cover
  if( cover_weighted )
  {cols <- x %>%
    dplyr::select({{plot_id}}, {{key}}, "cover") %>%
    dplyr::mutate(cover = as.character(x$cover))

  #if cover method is percent, just convert to numeric
  if(cover_class == "percent_cover") {
    cols <- cols %>%
      dplyr::mutate(cover = suppressWarnings(as.numeric(cols$cover)))
  }

  #if cover method is usfs_ecodata
  if(cover_class == "usfs_ecodata") {
    cols <- cols %>%
      dplyr::mutate(cover = dplyr::case_when(cover == "1" ~ 0.5,
                                             cover == "3" ~ 3,
                                             cover == "10" ~ 10,
                                             cover == "20" ~ 20,
                                             cover == "30" ~ 30,
                                             cover == "40" ~ 40,
                                             cover == "50" ~ 50,
                                             cover == "60" ~ 60,
                                             cover == "70" ~ 70,
                                             cover == "80" ~ 80,
                                             cover == "90" ~ 90,
                                             cover == "98" ~ 98))
  }

  #if cover method is carolina, transform to 10 classes
  if(cover_class == "carolina_veg_survey") {
    cols <- cols %>%
      dplyr::mutate(cover = dplyr::case_when(cover == "1" ~ 0.1,
                                             cover == "2" ~ 0.5,
                                             cover == "3" ~ 1.5,
                                             cover == "4" ~ 3.5,
                                             cover == "5" ~ 7.5,
                                             cover == "6" ~ 17.5,
                                             cover == "7" ~ 37.5,
                                             cover == "8" ~ 62.5,
                                             cover == "9" ~ 85,
                                             cover == "10" ~ 97.5))
  }

  #if cover method is daubenmire, transform to six classes
  if(cover_class == "daubenmire") {
    cols <- cols %>%
      dplyr::mutate(cover = dplyr::case_when(cover == "1" ~ 2.5,
                                             cover == "2" ~ 15,
                                             cover == "3" ~ 37.5,
                                             cover == "4" ~ 62.5,
                                             cover == "5" ~ 85,
                                             cover == "6" ~ 97.5))
  }

  #if cover method is braun-blanquet, transform to 5 classes
  if(cover_class == "braun-blanquet") {
    cols <- cols %>%
      dplyr::mutate(cover = dplyr::case_when(cover == "+" ~ 0.1,
                                             cover == "1" ~ 2.5,
                                             cover == "2" ~ 15,
                                             cover == "3" ~ 37.5,
                                             cover == "4" ~ 62.5,
                                             cover == "5" ~ 87.5))
  }

  } else ( cols <- x %>%
             dplyr::select({{plot_id}}, {{key}}) )

  #warning if NAs get introduced after converting cover metric
  if( cover_weighted ) {
    if( all(is.na(cols$cover)) ){
      message(paste("NAs were introduced during the conversion to the",
                    cover_class, "system. Are you using the right cover class?"))
    }
    else if( any(is.na(cols$cover)) ) {
    message(paste("NAs were introduced during the conversion to the",
                  cover_class, "system. Species with NA cover values will be removed."))
    }
    #remove NAs from cover col
    cols <- cols %>%
      dplyr::filter(!is.na(cols$cover))
  }

  #MESSAGING ABOUT DUPLICATES

  #send message to user if site assessment contains duplicate entries
  if( sum(duplicated(cols[,key])) > 0 & !allow_duplicates){
    if(cover_weighted == TRUE){
      message("Duplicate entries detected. Duplicates will only be counted once. Cover values of duplicate species will be added together.")}
    else{
      message("Duplicate entries detected. Duplicates will only be counted once.")}
  }

  #message if there are duplicates in same plot
  if( !is.null(plot_id) && allow_duplicates && any(duplicated(dplyr::select(cols, {{key}}, {{plot_id}}))) )
    message("Duplicate entries detected in the same plot. Duplicates in the same plot will be counted once. Cover values of duplicate species will be added together.")

  #PREPARING REGIONAL LIST FOR JOINING

  #get fqai db
  regional_fqai <- fqadata::fqai_db %>%
    dplyr::filter(.data$fqa_db == db)

  #error if fqai db does not have complete set of acronyms
  if( key == "acronym" &
      any(is.na(regional_fqai$acronym) & regional_fqai$name_origin == "accepted_scientific_name"))
    stop(paste(db, "does not have a complete set of acronyms, please set key to 'name'."))

  #warning if fqai db does not wetland scores
  if( wetland_warning & all(is.na(regional_fqai$w)))
    message(paste(db, "does not have wetland coefficients, wetland metrics cannot be calculated."))

  if (allow_non_veg) {
    regional_fqai <- rbind(
      #create df with water and ground
      data.frame(name = c("UNVEGETATED GROUND", "UNVEGETATED WATER"),
                 name_origin = c(NA, NA),
                 acronym = c("GROUND", "WATER"),
                 accepted_scientific_name = c("UNVEGETATED GROUND", "UNVEGETATED WATER"),
                 family = c("Unvegetated Ground", "Unvegetated Water"),
                 nativity = c(NA, NA),
                 c = c(0, 0),
                 w = c(0, 0),
                 physiognomy = c("Unvegetated Ground", "Unvegetated Water"),
                 duration = c("Unvegetated Ground", "Unvegetated Water"),
                 common_name = c(NA, NA),
                 fqa_db = c({{db}}, {{db}})),
      #bind to regional fqai
      regional_fqai)
  }

  #JOINING DATA ENTERED TO REGIONAL LIST

  #join scores from FQAI to user's assessment
  entries_joined <-
    dplyr::left_join(cols %>%
                       dplyr::mutate({{key}} := toupper(!!as.name(key))) %>%
                       dplyr::mutate(row = dplyr::row_number()),
                     regional_fqai,
                     by = key,
                     multiple = "all")

  #if a species is not present in regional list
  if( any(is.na(entries_joined$accepted_scientific_name)) ) {

    #send message to user
    message(paste("Species", unique(entries_joined[is.na(entries_joined$accepted_scientific_name), key]),
                  "not listed in database. It will be discarded."))

    #get rid of observations not in regional list
    entries_joined <- entries_joined %>%
      dplyr::filter(!is.na(.data$accepted_scientific_name))
  }

  #SAME NAME, DIFFERENT accepted_scientific_nameS

  #get duplicated names associated with diff accepted_scientific_names
  same_name_diff_accepted_scientific_name <- entries_joined %>%
    dplyr::group_by(.data$name) %>%
    dplyr::filter(dplyr::n_distinct(.data$accepted_scientific_name) > 1)

  #If a species name is associated with two separate species with separate accepted_scientific_names
  if( nrow(same_name_diff_accepted_scientific_name) > 0 ) {

    #one name is main name
    one_main <- same_name_diff_accepted_scientific_name %>%
      dplyr::group_by(.data$name) %>%
      dplyr::filter(any(.data$name_origin == "accepted_scientific_name"))

    #both are synonyms
    both_syn <- same_name_diff_accepted_scientific_name %>%
      dplyr::group_by(.data$name) %>%
      dplyr::filter(all(.data$name_origin != "accepted_scientific_name"))

    #message if one name is a main name
    for(i in unique(one_main$name)) {
      message(i, " is an accepted scientific name and a synonym. It will default to accepted scientific name.")
    }

    #message if both are synonyms
    for(i in unique(both_syn$name)) {
      message(i, " is a synonym to multiple species. It will be omited. To include this species, use the accepted scientific name.")
    }

    #if species are duplicated, keep only sci name
    entries_joined <- entries_joined %>%
      dplyr::group_by(.data$row) %>%
      dplyr::mutate(dup = dplyr::n() > 1) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!.data$dup | .data$name_origin == "accepted_scientific_name") %>%
      dplyr::select(-"dup")
  }

  #DIFFERENT NAMES, SAME accepted_scientific_nameS

  #get synonyms
  synonyms <- entries_joined %>%
    dplyr::group_by(.data$accepted_scientific_name) %>%
    dplyr::filter(dplyr::n_distinct(.data$name) > 1)

  #If a species is entered twice under different names/synonyms
  if( nrow(synonyms) > 0 ) {

    #get list of names in each accepted_scientific_name group
    list <- split(synonyms$name, synonyms$accepted_scientific_name)

    #use for loop to create warning for each set of synonyms
    for(i in 1:length(list)) {
      #send message
      if(!cover_weighted) {
        message("Species ", shQuote(unique(list[[i]])), " are synonyms and will be treated as one species.")
      } else {
        message("Species ", shQuote(unique(list[[i]])), " are synonyms and will be treated as one species. If allow_duplicates = FALSE, cover values of synonyms will be added together.")
      }
    }

    #replace diff names in same accepted_scientific_name group with first name
    entries_joined <- entries_joined %>%
      dplyr::group_by(.data$accepted_scientific_name) %>%
      dplyr::mutate(name = dplyr::first(.data$name),
                    name_origin = dplyr::first(.data$name_origin),
                    acronym = dplyr::first(.data$acronym))
  }

  #get rid of row column, no longer needed
  entries_joined <- dplyr::select(entries_joined, -"row")

  #TREATING DUPLICATES (FROM USER OR JOINING)

  #if allow duplicates is false, do not allow duplicates
  if( !allow_duplicates ) {
    if ( !cover_weighted ){
      entries_joined <- entries_joined %>%
        dplyr::distinct() }
    #if allow dups is false but cover weight is true, add cover values for like species together
    else(entries_joined <- entries_joined %>%
           dplyr::group_by(.data$accepted_scientific_name) %>%
           dplyr::mutate(cover = sum(as.numeric(.data$cover))) %>%
           dplyr::distinct() %>%
           dplyr::ungroup() )
  }

  #remove duplicates in the same plot
  if( !is.null(plot_id) & allow_duplicates ){
    if( cover_weighted ) {
      entries_joined <- entries_joined %>%
        dplyr::group_by(.data$accepted_scientific_name, !!as.name(plot_id)) %>%
        dplyr::mutate(cover = sum(as.numeric(.data$cover))) %>%
        dplyr::distinct() %>%
        dplyr::ungroup() }
    else {entries_joined <- dplyr::distinct(entries_joined,
                                            .data$accepted_scientific_name, !!as.name(plot_id),
                                            .keep_all = TRUE) }
  }

  #NATIVITY AND C SCORE

  #if native = T, filter for only native species
  if (native) {
    entries_joined <- entries_joined %>%
      dplyr::filter(.data$nativity == "native")
  }

  #If site assessment contains a plant that has no C value
  if( any(is.na(entries_joined$c)) & !allow_no_c ) {

    #sent message to user
    message(paste("Species", entries_joined[is.na(entries_joined$c), key],
                  "is recognized but has not been assigned a C Value. It can optionally be included in species richness but will not be included in any FQI metrics."))

    #get rid of no C value
    entries_joined <- entries_joined %>%
      dplyr::filter(!is.na(c))
  }

  #If site assessment contains a plant that has no W value
  if( wetland_warning & any(is.na(entries_joined$w)) & !all(is.na(entries_joined$w)) ) {

    #sent message to user
    message(paste("Species", entries_joined[is.na(entries_joined$w), key],
                  "does not have a wetland coefficient. It will be omitted from wetness metric calculations."))
  }

  #return accepted entries df
  return(as.data.frame(entries_joined))
}
