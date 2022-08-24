
#' Calculate Quadrat-Level Cover-Weighted Mean C
#'
#' `quadrat_mean_c` calculates the sum of cover times c value per each species,
#' divided by the sum of cover values for all species.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`
#' AND have a column named `cover`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' quadrat_mean_c(x = plant_list, key = "acronym", db = "michigan_2014")

quadrat_mean_c <- function(x, key = "acronym", db) {

  #get accepted entries
  entries <- accepted_entries(x, key, db, cover_weighted = T)

  #calculate mean c score
  mean_c <- sum(entries$c * entries$cover)/sum(entries$cover)

  #return mean
  return(mean_c)

}

#-------------------------------------------------------------------------------

#' Calculate Quadrat-Level Cover-Weighted Mean C for Native Species
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`
#' AND have a column named `cover`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' native_quadrat_mean_c(x = plant_list, key = "acronym", db = "michigan_2014")

native_quadrat_mean_c <- function(x, key = "acronym", db) {

  #get accepted native entries
  entries <- accepted_entries(x, key, db, cover_weighted = T) %>%
    dplyr::filter(native == "native")

  #calculate mean c score
  mean_c <- sum(entries$c * entries$cover)/sum(entries$cover)

  #return mean
  return(mean_c)

}

#-------------------------------------------------------------------------------

