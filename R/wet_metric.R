
#' Calculate the Mean Wetness Coefficient
#'
#'`mean_w` calculates the mean wetness coefficient for all species in
#' the site assessment. The wetness coefficient is based on USFWS Wetland Indicator
#' Status. Negative wetness coefficients indicate a stronger affinity for wetlands, while
#' positive wetland coefficients indicate an affinity for upland.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"acronym"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#' @param allow_no_c Boolean (TRUE or FALSE). If TRUE, include species that are found in the
#' regional database but have not been assigned a C Values. If FALSE, omit species that have not
#' been assigned C Values.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #mean wetness of all species (native and non-native)
#' mean_w(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #mean wetness of native species
#' mean_w(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

mean_w <- function(x, key = "scientific_name", db, native = FALSE, allow_no_c = TRUE) {

  #calculate mean W vlaue
  mean_w <- mean(accepted_entries(x, key, db, native,
                                  cover_weighted = FALSE,
                                  cover_metric = "percent_cover",
                                  allow_duplicates = FALSE,
                                  allow_no_c)$w)

  #print
  return(mean_w)

}
