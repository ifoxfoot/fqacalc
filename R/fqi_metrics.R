
#this file contains functions for calculating simple fqa metrics (i.e. without transects or quadrats)

#-------------------------------------------------------------------------------

#' Calculate Number of Species
#'
#' `species_richness()` calculates the total number of species in the site
#' assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame must
#' have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#'
#' plant_list <- crooked_island
#'
#' #number of species (native and exotic)
#' species_richness(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #number of native species
#' species_richness(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

species_richness <- function(x, key = "acronym", db, native) {

  #count how many observations are unique and matched
  species_richness <- nrow(accepted_entries(x, key, db, native))

  #return number of species
  return(species_richness)

}


#-------------------------------------------------------------------------------

#' Calculate Mean C
#'
#'`mean_c()` calculates the mean conservation coefficient for all species in
#'the site assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #mean c of all species (native and exotic)
#' mean_c(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #mean c of native species
#' mean_c(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

mean_c <- function(x, key = "acronym", db, native) {

  #calculate mean c score
  mean_c <- mean(accepted_entries(x, key, db, native)$c)

  #print
  return(mean_c)

}

#-------------------------------------------------------------------------------

#' Calculate FQI
#'
#' `FQI()` calculates the Floristic Quality Index (FQI) for the site using
#' all species listed. FQI is found by multiplying the total mean C by the square
#' root of the total species richness.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #FQI of all species (native and exotic)
#' FQI(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #FQI of native species
#' FQI(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

FQI <- function(x, key = "acronym", db, native) {

  #calculate total fqi
  fqi <- mean_c(x, key, db, native) *
    suppressMessages(sqrt(species_richness(x, key, db, native)))

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
  fqi <- 100 * (suppressMessages(mean_c(x, key, db, native = T))/10) *
      sqrt(suppressMessages(species_richness(x, key, db, native = T))/
             species_richness(x, key, db, native = F)
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
            "Proportion of Species with < 1 C score",
            "Proportion of Species with 1-3.9 C score",
            "Proportion of Species with 4-6.9 C score",
            "Proportion of Species with 7-10 C score",
            "Mean C",
            "Native Mean C",
            "Total FQI",
            "Native FQI",
            "Adjusted FQI")

  #create list of values
  values <- c(species_richness(x, key, db, native = F),
            suppressMessages(species_richness(x, key, db, native = T)),
            sum(accepted$c <= 1 )/length(accepted$c),
            sum(accepted$c >= 1 & accepted$c < 4)/length(accepted$c),
            sum(accepted$c >= 4 & accepted$c < 7)/length(accepted$c),
            sum(accepted$c >= 7 & accepted$c <= 10)/length(accepted$c),
            suppressMessages(mean_c(x, key, db, native = F)),
            suppressMessages(mean_c(x, key, db, native = T)),
            suppressMessages(FQI(x, key, db, native = F)),
            suppressMessages(FQI(x, key, db, native = T)),
            suppressMessages(adjusted_FQI(x, key, db)))


  #bind metrics and values into data frame
  report <- data.frame(metrics, values)

  #return the data frame
  return(report)

}

#-------------------------------------------------------------------------------

