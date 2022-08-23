
#this file contains functions for calculating simple fqa metrics (i.e. without transects or quadrats)

#this prevents a note about native being an undefined global variable
utils::globalVariables("native")

#-------------------------------------------------------------------------------

#' Calculate Number of Species
#'
#' `total_species_richness()` calculates the total number of species in the site
#' assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame must
#' have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#'
#' plant_list <- crooked_island
#' total_species_richness(x = plant_list, key = "acronym", db = "michigan_2014")

total_species_richness <- function(x, key = "acronym", db) {

  #count how many observations are unique and matched
  species_richness <- nrow(accepted_entries(x, key, db))

  #return number of species
  return(species_richness)

}

#-------------------------------------------------------------------------------

#' Calculate Number of Native Species
#'
#' `native_species_richness()` calculates the total number of native species in the
#' site assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame must
#' have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' native_species_richness(x = plant_list, key = "acronym", db = "michigan_2014")

native_species_richness <- function(x, key = "acronym", db) {

  #count how many observations are native, unique, and have c score
  native_richness <- accepted_entries(x, key, db) %>%
    dplyr::filter(native == "native") %>%
    nrow()

  #return number of species
  return(native_richness)

}

#-------------------------------------------------------------------------------

#' Calculate Mean C
#'
#'`total_mean_c()` calculates the mean conservation coefficient for all species in
#'the site assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' total_mean_c(x = plant_list, key = "acronym", db = "michigan_2014")

total_mean_c <- function(x, key = "acronym", db) {

  #calculate mean c score
  mean_c <- mean(accepted_entries(x, key, db)$c)

  #print
  return(mean_c)

}

#-------------------------------------------------------------------------------

#' Calculate Native Mean C
#'
#' `native_mean_c()` calculates the mean conservation coefficient for all native
#' species in the site assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' native_mean_c(x = plant_list, key = "acronym", db = "michigan_2014")

native_mean_c <- function(x, key = "acronym", db) {

  #get native accepted entries
  native_accepted_entries <- accepted_entries(x, key, db) %>%
    dplyr::filter(native == "native")

  #calculate mean C
  mean_c <- mean(native_accepted_entries$c)

  #print
  return(mean_c)

}

#-------------------------------------------------------------------------------

#' Calculate Total FQI
#'
#' `total_FQI()` calculates the Floristic Quality Index (FQI) for the site using
#' all species listed. FQI is found by multiplying the total mean C by the square
#' root of the total species richness.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
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
#' total_FQI(x = plant_list, key = "acronym", db = "michigan_2014")

total_FQI <- function(x, key = "acronym", db) {

  #calculate total fqi
  fqi <- total_mean_c(x, key, db) * suppressMessages(sqrt(total_species_richness(x, key, db)))

  #print
  return(fqi)

}

#-------------------------------------------------------------------------------

#' Calculate Native FQI
#'
#' `native_FQI()` calculates the Floristic Quality Index (FQI) for the site using
#' only native species. Native FQI is found by multiplying the native mean C by
#' the square root of the native species richness.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
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
#' native_FQI(x = plant_list, key = "acronym", db = "michigan_2014")

native_FQI <- function(x, key = "acronym", db) {

  #calculate native fqi
  fqi <- native_mean_c(x, key, db) * suppressMessages(sqrt(native_species_richness(x, key, db)))

  #print
  return(fqi)

}

#-------------------------------------------------------------------------------

#' Calculate Adjusted FQI
#'
#' `adjusted_FQI()` calculates the Adjusted Floristic Quality Index (FQI) for the
#' site using all species. Adjusted FQI is found by multiplying 100 by the native
#' mean C divided by ten and then multiplied by the square root of native species
#' richness divided by the square root of total species richness.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
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
#' adjusted_FQI(x = plant_list, key = "acronym", db = "michigan_2014")

adjusted_FQI <- function(x, key = "acronym", db) {

  #calculate adjusted fqi
  fqi <- 100 * (native_mean_c(x, key, db)/10) *
    suppressMessages(
      sqrt(native_species_richness(x, key, db)/total_species_richness(x, key, db))
      )

  #print
  return(fqi)

}

#-------------------------------------------------------------------------------

#' Calculate All FQA Metrics
#'
#' `all_metrics()` calculates and prints Total Species Richness, Native Species Richness,
#' Mean C, Native Mean C, Total FQI, Native FQI, and Adjusted FQI.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' adjusted_FQI(x = plant_list, key = "acronym", db = "michigan_2014")

all_metrics <- function(x, key = "acronym", db) {

  #create list of all metrics that will be included in the output
  metrics <- c("Total Species Richness",
            "Native Species Richness",
            "Mean C",
            "Native Mean C",
            "Total FQI",
            "Native FQI",
            "Adjusted FQI")

  #create list of values
  values <- c(total_species_richness(x, key, db),
            suppressMessages(native_species_richness(x, key, db)),
            suppressMessages(total_mean_c(x, key, db)),
            suppressMessages(native_mean_c(x, key, db)),
            suppressMessages(total_FQI(x, key, db)),
            suppressMessages(native_FQI(x, key, db)),
            suppressMessages(adjusted_FQI(x, key, db)))


  #bind metrics and values into data frame
  report <- data.frame(metrics, values)

  #return the data frame
  return(report)

}

#-------------------------------------------------------------------------------

