
#this file contains functions for calculating non-cover-weighted fqi metrics
#-------------------------------------------------------------------------------

#' Calculate Number of Species
#'
#' `species_richness` calculates the total number of species in the site
#' assessment.
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
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #number of species (native and non-native)
#' species_richness(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #number of native species
#' species_richness(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

species_richness <- function(x, key = "scientific_name", db, native = FALSE) {

  #count how many observations are unique and matched
  species_richness <- nrow(accepted_entries(x, key, db, native,
                                            cover_weighted = F,
                                            cover_metric = "percent_cover",
                                            allow_duplicates = F))

  #return number of species
  return(species_richness)

}


#-------------------------------------------------------------------------------

#' Calculate Mean C
#'
#' `mean_c` calculates the mean Coefficient of Conservatism for all species in
#' the site assessment.
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
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #mean c of all species (native and non-native)
#' mean_c(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #mean c of native species
#' mean_c(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

mean_c <- function(x, key = "scientific_name", db, native = FALSE) {

  #calculate mean c value
  mean_c <- mean(accepted_entries(x, key, db, native,
                                  cover_weighted = F,
                                  cover_metric = "percent_cover",
                                  allow_duplicates = F)$c)

  #print
  return(mean_c)

}

#-------------------------------------------------------------------------------

#' Calculate FQI
#'
#' `FQI` calculates the Floristic Quality Index (FQI) for the site. FQI is found
#' by multiplying the mean C by the square root of the species richness. If `native = TRUE`,
#' `FQI` will calculate the Native FQI.
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
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #FQI of all species (native and non-native)
#' FQI(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #FQI of native species
#' FQI(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

FQI <- function(x, key = "scientific_name", db, native = FALSE) {

  #calculate total fqi
  fqi <- mean_c(x, key, db, native) *
    suppressMessages(sqrt(species_richness(x, key, db, native)))

  #print
  return(fqi)

}


#-------------------------------------------------------------------------------

#' Calculate Adjusted FQI
#'
#' `adjusted_FQI` calculates the Adjusted Floristic Quality Index for the
#' site. Adjusted FQI is found by multiplying 100 by the Native
#' Mean C divided by 10 and then multiplied by the square root of Native Species
#' Richness over Total Species Richness.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"acronym"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' adjusted_FQI(x = plant_list, key = "acronym", db = "michigan_2014")

adjusted_FQI <- function(x, key = "scientific_name", db) {

  #calculate adjusted fqi
  fqi <- 100 * (suppressMessages(mean_c(x, key, db, native = T))/10) *
      sqrt(suppressMessages(species_richness(x, key, db, native = T))/
             species_richness(x, key, db, native = F)
      )

  #print
  return(fqi)

}

#-------------------------------------------------------------------------------

#' Print a Summary of Non-Weighted FQA Metrics
#'
#' `all_metrics` calculates and prints a summary of all non cover-weighted metrics
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"acronym"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' all_metrics(x = plant_list, key = "acronym", db = "michigan_2014")

all_metrics <- function(x, key = "scientific_name", db) {

  #get list of accepted entries for calculating stats
  accepted <- suppressMessages(accepted_entries(x, key, db, native = F,
                                                cover_weighted = F,
                                                cover_metric = "percent_cover",
                                                allow_duplicates = F))

  #create list of all metrics that will be included in the output
  metrics <- c("Total Species Richness",
            "Native Species Richness",
            "Non-native Species Richness",
            "Proportion of Species with < 1 C Value",
            "Proportion of Species with 1-3.9 C Value",
            "Proportion of Species with 4-6.9 C Value",
            "Proportion of Species with 7-10 C Value",
            "Mean C",
            "Native Mean C",
            "Total FQI",
            "Native FQI",
            "Adjusted FQI",
            "Mean Wetness",
            "Native Mean Wetness")

  #create list of values
  values <- c(species_richness(x, key, db, native = F),
            suppressMessages(species_richness(x, key, db, native = T)),
            nrow(dplyr::filter(accepted, .data$native == "exotic")),
            sum(accepted$c <= 1 )/length(accepted$c),
            sum(accepted$c >= 1 & accepted$c < 4)/length(accepted$c),
            sum(accepted$c >= 4 & accepted$c < 7)/length(accepted$c),
            sum(accepted$c >= 7 & accepted$c <= 10)/length(accepted$c),
            suppressMessages(mean_c(x, key, db, native = F)),
            suppressMessages(mean_c(x, key, db, native = T)),
            suppressMessages(FQI(x, key, db, native = F)),
            suppressMessages(FQI(x, key, db, native = T)),
            suppressMessages(adjusted_FQI(x, key, db)),
            suppressMessages(mean_w(x, key, db, native = FALSE)),
            suppressMessages(mean_w(x, key, db, native = TRUE)))


  #bind metrics and values into data frame
  report <- data.frame(metrics, values)

  #return the data frame
  return(report)

}



