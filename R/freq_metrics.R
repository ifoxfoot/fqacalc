
#this file contains frequency  metrics (relative_freq(), relative_cover(), relative_importance(), species_summary())

#-------------------------------------------------------------------------------

#' Calculate Relative Frequency
#'
#' `relative_freq()` calculates the frequency of one species, taxonomic family,
#' or physiognomic group multiplied by 100, divided by the frequency of all observations.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym` as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
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
#' relative_freq(transect, key = "acronym", db = "michigan_2014", col = "physiog")

relative_freq <- function(x, key = "scientific_name", db,
                          col = c("species", "family", "physiog")) {


  #col argument must be right
  if( !col %in% c("species", "family", "physiog"))
    stop("'col' argument can only be set to 'species', 'family', or 'physiog'")

  #which column is being called?
  name <- if(col == "species")  {"scientific_name"}
  else if(col == "family") {"family"} else if (col == "physiog") {"physiognomy"}

  #join entries to database in order to get info on family, physiognomy
  entries <- accepted_entries(x, key, db, native = FALSE, allow_duplicates = TRUE)

  #calculate relative frequency--fre/num observations, select right col
  df <- data.frame(100*(table(entries[name])/nrow(entries)))

  colnames(df) <- c({{name}}, "rel_freq")

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
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`, as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use.
#' @param col A character string representing the categorical variable to calculate
#' the relative frequency of. Can be set to "species", "family" or "physiog" (for physiognomy).
#' @param cover_metric a character string representing the cover method used. Acceptable
#' cover methods are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default and is
#' recommended because it is the most accurate.
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

relative_cover <- function(x, key = "scientific_name", db,
                           col = c("species", "family", "physiog"),
                           cover_metric = "percent_cover"){

  #declaring rel_cov is null
  rel_cov <- NULL

  #col argument must be right
  if( !col %in% c("species", "family", "physiog"))
    stop("'col' argument can only be set to 'species', 'family', or 'physiog'")

  #which column is being called?
  name <- if(col == "species")  {"scientific_name"}
  else if(col == "family") {"family"} else if (col == "physiog") {"physiognomy"}

  #bind to regional fqa list to get info about taxonomy, physiognomy
  entries <- accepted_entries(x, key, db, native = FALSE,
                              allow_duplicates = T,
                              cover_weighted = T,
                              cover_metric) %>%
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
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym` as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#' @param col A character string representing the categorical variable to calculate
#' the relative frequency of. Can be set to "species", "family" or "physiog" (for physiognomy).
#' @param cover_metric a character string representing the cover method used. Acceptable
#' cover methods are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default and is
#' recommended because it is the most accurate.
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

relative_importance <- function(x, key = "scientific_name", db,
                                col = c("species", "family", "physiog"),
                                cover_metric = "percent_cover"){

  #declaring var names as null
  rel_import <- NULL

  #which column is being called?
  name <- if(col == "species")  {"scientific_name"}
  else if(col == "family") {"family"} else if (col == "physiog") {"physiognomy"}

  #get mean of relative freq and relative cover
  avg = merge(
    relative_freq(x, key, db, col),
    relative_cover(x, key, db, col, cover_metric)) %>%
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
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym` as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#' @param cover_metric a character string representing the cover method used. Acceptable
#' cover methods are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default and is
#' recommended because it is the most accurate.
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

species_summary <- function(x, key = "scientific_name", db,
                            cover_metric = "percent_cover"){

  #get accepted entries
  accepted <- accepted_entries(x, key, db, native = FALSE,
                               cover_weighted = T,
                               cover_metric,
                               allow_duplicates = T)

  c_score <- accepted %>%
    dplyr::select(.data$scientific_name, .data$acronym, .data$native, .data$c, .data$w) %>%
    dplyr::distinct()

  #getting freq and coverage
  group <- accepted %>%
    dplyr::group_by(!!as.name(key)) %>%
    dplyr::summarise(frequency = dplyr::n(),
                     coverage = sum(.data$cover))

  #relative frequency
  rel_freq <- relative_freq(x, key, db, col = "species")

  #relative cover
  rel_cov <- relative_cover(x, key, db, col = "species", cover_metric)

  #relative importance
  rel_import <- relative_importance(x, key, db, col = "species", cover_metric)


  #merge together
  df <- merge(c_score, group) %>%
    merge(rel_freq) %>%
    merge(rel_cov) %>%
    merge(rel_import)


  return(df)

}
