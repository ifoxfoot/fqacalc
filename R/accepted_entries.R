
#this file contains `accepted_entries()`

#-------------------------------------------------------------------------------

#' Return A Data Frame of Plant Species That Successfully Match to the Regional
#' FQA Database of choice
#'
#' `accepted_entries` takes a data frame of user-entered plant species and returns
#' a data frame of plant species that are successfully matched to the regional FQA
#' database of choice. Regional databases are stored in the `fqadata` R package.
#' `accepted_entries` is a utility function that is used in all other metric-calculating
#' functions in this package.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `name` or `acronym`. For cover-weighted or
#' relative functions, this data frame must also have a column called `cover` containing
#' cover values and ideally a column containing plot IDs.
#' @param key A character string representing the column that will be used to join
#' the input data frame `x` with the regional FQA database. If a value is not specified, the
#' default is `"name"`. `"name"` and `"acronym"` are the only acceptable
#' values for `key`.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names` for a list of potential values and the `fqadata` R package
#' where the databases are hosted.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#' @param wetland_warning Boolean (TRUE or FALSE). If TRUE, show user messages regarding
#' issues with wetness coefficients.
#' @param cover_weighted Boolean (TRUE or FALSE). If TRUE, keep `cover` column in output.
#' Note: if `cover_weighted = TRUE`, `x` must have a column named `cover`. This parameter
#' is used to calculate cover-weighted metrics such as plot mean c, transect mean c, and
#' cover-weighted FQI.
#' @param cover_class a character string representing the cover classification used. Acceptable
#' cover classes are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default.
#' @param allow_duplicates Boolean (TRUE or FALSE). If TRUE, allow `x` to have
#' duplicate observations for the same species. This is only recommended for
#' calculating transect and relative frequency/abundance metrics. For non cover-weighted (inventory)
#' assessments allow_duplicates is always FALSE. For cover-weighted functions, allow_duplicates
#' can be set to TRUE for transect level metrics or FALSE for plot level metrics.
#' @param allow_no_c Boolean (TRUE or FALSE). If TRUE, allow species that are found in the
#' regional FQA database but have not been assigned a C Values. If FALSE, omit species that have not
#' been assigned C Values.
#' @param allow_non_veg Boolean (TRUE or FALSE). If TRUE, allow input to contain un-vegetated
#' ground and un-vegetated water.
#' @param plot_id A character string representing the column in `x` that contains plot
#' identification values. `plot_id` is a required argument in `plot_summary`, where it acts
#' as a grouping variable. `plot_id` is optional but highly recommended for
#' cover-weighted functions and relative functions. If `plot_id` is set in a
#' cover-weighted function or a relative function, it only prevents duplicates
#' from occurring in the same plot. It does not act as a grouping variable.
#'
#' @return A data frame containing the `key` column--either `acronym` or
#' `name`--as well as columns from the relevant FQA database.
#' These columns include `name_origin` `accepted_name`, `family`, `nativity`, `c` (which represents the C Value),
#' `w` (which represents wetness score), `physiognomy`, `duration`, and `common_name`
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #with native and introduced species
#' accepted_entries(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #with only native species
#' accepted_entries(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)
#'
#' #an example with duplicates allowed
#' duplicate_df <- data.frame(acronym  = c("ABEESC", "ABIBAL", "ABIBAL"),
#' cover = c(60, 50, 50))
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
#' #produces a warning saying CAREX MURICATA is a synonym to multiple species and will be omitted.
#' #To include this species, use the accepted scientific name.
#' accepted_entries(x = same_syn, key = "name",
#' db = "wyoming_2017", native = FALSE)
#'
#' #an example where species is both a synonym and an accepted name
#' same_syn2 <- data.frame(name = c("CAREX FOENEA", "ABIES BIFOLIA"),
#' cover = c(80, 10))
#'
#' #produces a warning saying CAREX FOENEA is an accepted scientific name and a synonym.
#' #It will default to accepted scientific name.
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
  if( !db %in% db_names()$fqa_db )
    stop(paste(db, "not recognized. Run 'db_names()' for a list of acceptable db values."))

  #native must be TRUE or FALSE
  if( !is.logical(native) )
    stop("'native' can only be set to TRUE or FALSE")

  #wetland_warning must be TRUE or FALSE
  if( !is.logical(wetland_warning) )
    stop("'wetland_warning' can only be set to TRUE or FALSE")

  #messages about wetland status indicator defaults
  if (wetland_warning & db == "wyoming_2017")
    message(paste(strwrap(
    "The Wyoming FQA database is associated with multiple wetland indicator
    status regions. This package defaults to the Arid West wetland indicator
    region when calculating Wyoming metrics."),
    collapse = " "))

  if (wetland_warning & db == "colorado_2020")
    message(paste(strwrap(
      "The Colorado FQA database is associated with multiple wetland indicator
      status regions. This package defaults to the Western Mountains, Valleys,
      and Coasts indicator region when calculating Colorado metrics."),
      collapse = " "))

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

  #allow_duplicates must be TRUE or FALSE
  if( !is.logical(allow_duplicates) )
    stop("'allow_duplicates' can only be set to TRUE or FALSE")

  #if cover is true, then there must be a column named cover in input df
  if( cover_weighted & !("cover" %in% colnames(x)))
    stop(paste("If 'cover = TRUE',", deparse(substitute(x)),
               "must have a column named cover."))

  #if cover is missing, write error
  if( cover_weighted && any(is.na(x$cover)) )
    stop(paste("'cover' column cannot contain missing values."))

  #cover metric must be defined
  if( !cover_class %in% c("percent_cover", "carolina_veg_survey",
                          "braun-blanquet","daubenmire",
                          "usfs_ecodata"))
    stop(paste(cover_class,
               "is not an accepted cover-method. See function documentation."))

  #plot_id argument must be NULL or a column name in input data frame x
  if( !is.null(plot_id) && !(plot_id %in% colnames(x)) )
    stop(paste0("'plot_id' must be the name of a column in ",
                deparse(substitute(x)), "."))

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

  #if cover method is braun-blanquet, transform to 6 classes
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
                    cover_class,
                    "system. Species with NA cover values will be removed."))
    }
    #remove NAs from cover col
    cols <- cols %>%
      dplyr::filter(!is.na(cols$cover))
  }

  #MESSAGING ABOUT DUPLICATES

  #send message to user if site assessment contains duplicate entries
  if( sum(duplicated(cols[,key])) > 0 & !allow_duplicates){
    if(cover_weighted == TRUE){
      message(paste(strwrap("Duplicate entries detected. Duplicates will only be
                      counted once. Cover values of duplicate species will be
                      added together."), collapse = " "))}
    else{
      message("Duplicate entries detected. Duplicates will only be counted once.")}
  }

  #message if there are duplicates in same plot
  if( !is.null(plot_id) && allow_duplicates && any(duplicated(dplyr::select(
    cols, {{key}}, {{plot_id}}))) )
    message(paste(strwrap(
      "Duplicate entries detected in the same plot. Duplicates in the same plot
    will be counted once. Cover values of duplicate species will be added
    together."), collapse = " "))

  #PREPARING REGIONAL DATABASE FOR JOINING

  #get fqa db
  regional_fqa <- fqadata::fqa_db %>%
    dplyr::filter(.data$fqa_db == db)

  #error if fqa db does not have complete set of acronyms
  if( key == "acronym" &
      any(is.na(regional_fqa$acronym) &
          regional_fqa$name_origin == "accepted_scientific_name"))
    stop(paste(strwrap(paste(db, "does not have a complete set of acronyms, please set
                       key equal to 'name'.")), collapse = " "))

  #warning if fqa db does not wetland scores
  if( wetland_warning & all(is.na(regional_fqa$w)))
    message(paste(strwrap(paste(db, "does not have wetness coefficients, wetland
                          metrics cannot be calculated.")), collapse = " "))

  if (allow_non_veg) {
    regional_fqa <- rbind(
      #create df with water and ground
      data.frame(name = c("UNVEGETATED GROUND", "UNVEGETATED WATER"),
                 name_origin = c(NA, NA),
                 acronym = c("GROUND", "WATER"),
                 accepted_scientific_name = c("UNVEGETATED GROUND",
                                              "UNVEGETATED WATER"),
                 family = c("Unvegetated Ground", "Unvegetated Water"),
                 nativity = c(NA, NA),
                 c = c(0, 0),
                 w = c(NA, NA),
                 wetland_indicator = c(NA_character_, NA_character_),
                 physiognomy = c("Unvegetated Ground", "Unvegetated Water"),
                 duration = c("Unvegetated Ground", "Unvegetated Water"),
                 common_name = c(NA, NA),
                 fqa_db = c({{db}}, {{db}})),
      #bind to regional fqa
      regional_fqa)
  }

  #JOINING DATA ENTERED TO REGIONAL DATABASE

  #join scores from FQA to user's assessment
  entries_joined <-
    dplyr::left_join(cols %>%
                       dplyr::mutate({{key}} := toupper(!!as.name(key))) %>%
                       dplyr::mutate(row = dplyr::row_number()),
                     regional_fqa,
                     by = key,
                     relationship = "many-to-many")

  #if a species is not present in regional db
  if( any(is.na(entries_joined$accepted_scientific_name)) ) {

    #send message to user
    message(paste("Species",
                  unique(entries_joined[is.na(entries_joined$accepted_scientific_name),
                                        key]),
                  "not listed in database. It will be discarded."))

    #get rid of observations not in regional db
    entries_joined <- entries_joined %>%
      dplyr::filter(!is.na(.data$accepted_scientific_name))
  }

  #SAME NAME, DIFFERENT ACCEPTED NAMES

  #get duplicated names associated with diff accepted_scientific_names
  same_name_diff_accepted_scientific_name <- entries_joined %>%
    dplyr::group_by(.data$name) %>%
    dplyr::filter(dplyr::n_distinct(.data$accepted_scientific_name) > 1)

  #If a species name is associated with separate accepted_scientific_names
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
      message(paste(strwrap(paste(i, "is an accepted scientific name and a synonym. It
      will default to accepted scientific name.")), collapse = " "))
    }

    #message if both are synonyms
    for(i in unique(both_syn$name)) {
      message(paste(strwrap(paste(i,
                                  "is a synonym to multiple species. It will be omitted. To include this
      species, use the accepted scientific name.")), collapse = " "))
    }

    #if species are duplicated, keep only sci name
    entries_joined <- entries_joined %>%
      dplyr::group_by(.data$row) %>%
      dplyr::mutate(dup = dplyr::n() > 1) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!.data$dup | .data$name_origin == "accepted_scientific_name") %>%
      dplyr::select(-"dup")
  }

  #DIFFERENT NAMES, SAME ACCEPTED NAME

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
        message("Species ", paste(unique(list[[i]]), collapse=", "),
                " are synonyms and will be treated as one species.")
      } else {
        message("Species ", paste(unique(list[[i]]), collapse = ", "), " ",
                paste(strwrap("are synonyms and will be treated as one species.
                              If allow_duplicates = FALSE, cover values of synonyms
                              will be added together."), collapse = " "))
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
    #if allow dups is false but cover weight is true, add cover vals together
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
                                            .data$accepted_scientific_name,
                                            !!as.name(plot_id),
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
    message(paste(strwrap(paste("Species", entries_joined[is.na(entries_joined$c),key],
                                "is recognized but has not been assigned a C Value. If using
                   the shiny web application, this species will only be included
                  in metrics that don't require a C Value. Otherwise, the
                    option to include species with no C Value in certain metrics
                    can be set using the 'allow_no_c' argument.")), collapse = " "))

    #get rid of no C value
    entries_joined <- entries_joined %>%
      dplyr::filter(!is.na(c))
  }

  #If site assessment contains a plant that has no W value
  if(
    wetland_warning &
    any(is.na(entries_joined$w)) &
    !all(is.na(entries_joined$w))
  )

  {
    #sent message to user
    message(paste(strwrap(paste("Species", entries_joined[is.na(entries_joined$w), key],
                                "does not have a wetness coefficient. It will be omitted from
                  wetness metric calculations.")), collapse = " "))
  }

  #return accepted entries df
  return(as.data.frame(entries_joined))
}
