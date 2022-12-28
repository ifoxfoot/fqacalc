
#this file contains frequency  metrics
#relative_frequency(), relative_cover(), relative_importance(), species_summary() and physiog_summary()

#-------------------------------------------------------------------------------

#' Calculate Relative Frequency
#'
#' `relative_frequency()` calculates the frequency of one species, taxonomic family,
#' or physiognomic group multiplied by 100, divided by the frequency of all observations.
#'
#' @inheritParams accepted_entries
#' @param col A character string representing the categorical variable to calculate
#' the relative frequency of. Can be set to "species", "family" or "physiog" (for physiognomy).
#'
#' @return A data frame with categorical variables set by the col argument and their relative frequency.
#' @export
#'
#' @examples
#' transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' relative_frequency(transect, key = "acronym", db = "michigan_2014", col = "physiog")
#'
#' #can also include bare ground and unveg water
#' transect_unveg <- data.frame(acronym  = c("GROUND", "ABEESC", "ABIBAL", "AMMBRE",
#' "ANTELE", "WATER", "GROUND", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(60, 50, 4, 20, 30, 20, 20, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
#'
#' relative_frequency(transect_unveg, key = "acronym", db = "michigan_2014",
#' col = "physiog")

relative_frequency <- function(x, key = "name", db,
                          col = c("species", "family", "physiog"),
                          allow_no_c = TRUE,
                          allow_non_veg = TRUE) {

  #declaring rel_freq as null so I can use as a veriable name
  rel_freq <- NULL

  #col argument must be right
  if( !col %in% c("species", "family", "physiog"))
    stop("'col' argument can only be set to 'species', 'family', or 'physiog'")

  #which column is being called?
  name <- if(col == "species")  {"name"}
  else if(col == "family") {"family"} else if (col == "physiog") {"physiognomy"}

  #join entries to database in order to get info on family, physiognomy
  entries <- accepted_entries(x, key, db, native = FALSE, allow_duplicates = TRUE,
                              cover_weighted = FALSE,
                              cover_metric = "percent_cover",
                              allow_no_c,
                              allow_non_veg)

  #calculate relative frequency--fre/num observations
  df <- data.frame(dplyr::count(entries, !!as.name(name), name = "rel_freq")) %>%
    dplyr::mutate(rel_freq = 100*rel_freq/nrow(entries))

  #return result
  return(df)

}

#-------------------------------------------------------------------------------


#' Calculate Relative Cover
#'
#' `relative_cover()` calculates the total cover per group of interest (species,
#' taxonomic family, or physiognomic group) multiplied by 100 and divided by the total cover for all
#' observations.
#'
#' @inheritParams accepted_entries
#' @param col A character string representing the categorical variable to calculate
#' the relative frequency of. Can be set to "species", "family" or "physiog" (for physiognomy).
#'
#'
#' @return A data frame with categorical variables set by the col argument and their relative cover.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' relative_cover(transect, key = "acronym", db = "michigan_2014", col = "species")
#'
#' #can also include bare ground and unveg water
#' transect_unveg <- data.frame(acronym  = c("GROUND", "ABEESC", "ABIBAL", "AMMBRE",
#' "ANTELE", "WATER", "GROUND", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(60, 50, 4, 20, 30, 20, 20, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
#'
#' relative_cover(transect_unveg, key = "acronym", db = "michigan_2014",
#' col = "species")

relative_cover <- function(x, key = "name", db,
                           col = c("species", "family", "physiog"),
                           cover_metric = "percent_cover", allow_no_c = TRUE,
                           allow_non_veg = TRUE){

  #declaring rel_cov is null
  rel_cov <- NULL

  #col argument must be right
  if( !col %in% c("species", "family", "physiog"))
    stop("'col' argument can only be set to 'species', 'family', or 'physiog'")

  #which column is being called?
  name <- if(col == "species")  {"name"}
  else if(col == "family") {"family"} else if (col == "physiog") {"physiognomy"}

  #bind to regional fqa list to get info about taxonomy, physiognomy
  entries <- accepted_entries(x, key, db, native = FALSE,
                              allow_duplicates = TRUE,
                              cover_weighted = TRUE,
                              cover_metric,
                              allow_no_c,
                              allow_non_veg) %>%
    dplyr::group_by(!!as.name(name)) %>%
    #caclulate cover per group
    dplyr::summarise(sum = sum(.data$cover)) %>%
    as.data.frame() %>%
    dplyr::mutate(rel_cov = 100*sum/sum(sum)) %>%
    dplyr::select(!!as.name(name), rel_cov)

  #return the result
  return(entries)

}

#-------------------------------------------------------------------------------

#' Calculate Relative Importance
#'
#' `relative_importance()` calculates the average of relative frequency and relative cover.
#'
#' @inheritParams accepted_entries
#' @param col A character string representing the categorical variable to calculate
#' the relative frequency of. Can be set to "species", "family" or "physiog" (for physiognomy).
#'
#' @return A data frame with categorical variables set by the col argument and their relative importance.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' relative_importance(transect, key = "acronym", db = "michigan_2014", col = "family")
#'
#' #can also include bare ground and unveg water
#' transect_unveg <- data.frame(acronym  = c("GROUND", "ABEESC", "ABIBAL", "AMMBRE",
#' "ANTELE", "WATER", "GROUND", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(60, 50, 4, 20, 30, 20, 20, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
#'
#' relative_importance(transect_unveg, key = "acronym", db = "michigan_2014",
#' col = "family")
#'

relative_importance <- function(x, key = "name", db,
                                col = c("species", "family", "physiog"),
                                cover_metric = "percent_cover", allow_no_c = TRUE,
                                allow_non_veg = TRUE){

  #declaring var names as null
  rel_import <- NULL

  #which column is being called?
  name <- if(col == "species")  {"name"}
  else if(col == "family") {"family"} else if (col == "physiog") {"physiognomy"}

  #get mean of relative freq and relative cover
  avg <- merge(
    relative_frequency(x, key, db, col, allow_no_c, allow_non_veg),
    relative_cover(x, key, db, col, cover_metric, allow_no_c, allow_non_veg)) %>%
    dplyr::mutate(rel_import = (.data$rel_freq + .data$rel_cov)/2) %>%
    dplyr::select(!!as.name(name), rel_import)

  #return value
  return(avg)

}


#-------------------------------------------------------------------------------

#' Create A Cover-Weighted Summary of Species
#'
#' `species_summary()` produces a table summarizing species' frequency, total cover,
#' relative frequency, relative cover, and relative importance.
#'
#' @inheritParams accepted_entries
#'
#' @return A data frame where each row is a species and each column is information about that species
#' based on the input data frame.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#'species_summary(transect, key = "acronym", db = "michigan_2014")
#'
#' #can also include bare ground and unveg water
#' transect_unveg <- data.frame(acronym  = c("GROUND", "ABEESC", "ABIBAL", "AMMBRE",
#' "ANTELE", "WATER", "GROUND", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(60, 50, 4, 20, 30, 20, 20, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
#'
#' species_summary(transect_unveg, key = "acronym", db = "michigan_2014")
#'

species_summary <- function(x, key = "name", db,
                            cover_metric = "percent_cover",
                            allow_no_c = TRUE,
                            allow_non_veg = TRUE){

  #get accepted entries
  accepted <- accepted_entries(x, key, db, native = FALSE,
                               cover_weighted = TRUE,
                               cover_metric,
                               allow_duplicates = TRUE,
                               allow_no_c,
                               allow_non_veg)

  c_score <- accepted %>%
    dplyr::select("name", "acronym", "nativity", "c", "w") %>%
    dplyr::distinct()

  #getting freq and coverage
  group <- accepted %>%
    dplyr::group_by(!!as.name(key)) %>%
    dplyr::summarise(frequency = dplyr::n(),
                     coverage = sum(.data$cover))

  #relative frequency
  rel_freq <- relative_frequency(x, key, db, col = "species", allow_no_c, allow_non_veg)

  #relative cover
  rel_cov <- relative_cover(x, key, db, col = "species", cover_metric, allow_no_c, allow_non_veg)

  #relative importance
  rel_import <- relative_importance(x, key, db, col = "species", cover_metric, allow_no_c, allow_non_veg)


  #merge together
  df <- merge(c_score, group) %>%
    merge(rel_freq) %>%
    merge(rel_cov) %>%
    merge(rel_import)


  return(df)

}

#-------------------------------------------------------------------------------

#' Create a cover-Weighted Summary of Physiognomic Groups
#'
#' @inheritParams accepted_entries
#'
#' @return A data frame where each row is a physiognomic group and each column is a metric about that species
#' based on the input data frame.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' physiog_summary(transect, key = "acronym", db = "michigan_2014")
#'
#'
#' #can also include bare ground and unveg water
#' transect_unveg <- data.frame(acronym  = c("GROUND", "ABEESC", "ABIBAL", "AMMBRE",
#' "ANTELE", "WATER", "GROUND", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(60, 50, 4, 20, 30, 20, 20, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
#'
#' physiog_summary(transect_unveg, key = "acronym", db = "michigan_2014")
#'

physiog_summary <- function(x, key = "name", db,
                            cover_metric = "percent_cover",
                            allow_no_c = TRUE,
                            allow_non_veg = TRUE){

  #get accepted entries
  accepted <- accepted_entries(x, key, db, native = FALSE,
                               cover_weighted = TRUE,
                               cover_metric,
                               allow_duplicates = TRUE,
                               allow_no_c,
                               allow_non_veg)

  #getting freq and coverage
  group <- accepted %>%
    dplyr::group_by(.data$physiognomy) %>%
    dplyr::summarise(frequency = dplyr::n(),
                     coverage = sum(.data$cover))

  #relative frequency
  rel_freq <- relative_frequency(x, key, db, col = "physiog", allow_no_c, allow_non_veg)

  #relative cover
  rel_cov <- relative_cover(x, key, db, col = "physiog", cover_metric, allow_no_c, allow_non_veg)

  #relative importance
  rel_import <- relative_importance(x, key, db, col = "physiog", cover_metric, allow_no_c, allow_non_veg)

  #merge together
  df <- merge(group, rel_freq) %>%
    merge(rel_cov) %>%
    merge(rel_import)


  return(df)

}
