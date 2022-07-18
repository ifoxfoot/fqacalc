
#this prevents a note about native being an undefined global variable
utils::globalVariables("native")

#-------------------------------------------------------------------------------

#' Calculate Number of Species
#'
#' `total_species_richness()` calculates the total number of species in the site
#' assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame must
#' have one of the following columns: `scientific_name`, `acronym`, or `common_name`.
#'
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' total_species_richness(x = plant_list)

total_species_richness <- function(x) {

  #join scores from Michigan FQAI to user's assessment
  user_list_with_scores <- suppressMessages(
    dplyr::left_join(x, michigan_2014_fqai))

  #send warning to user if site assessment contains plant not in FQAI database
  if( any(is.na(user_list_with_scores$c)) )
    message("Species not listed in database, it will be treated as a non-native")

  #count how many unique observations are in species list
  species_richness <- nrow(unique(x))

  #return number of species
  return(species_richness)

}

#-------------------------------------------------------------------------------

#' Calculate Number of Native Species
#'
#' `native_species_richness()` calculates the total number of native species in the
#' site assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name`, `acronym`, or
#' `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' native_species_richness(x = plant_list)

native_species_richness <- function(x) {

  #join scores from Michigan FQAI to user's assessment
  user_list_with_scores <- suppressMessages(
    dplyr::left_join(x, michigan_2014_fqai))

  #send warning to user if site assessment contains plant not in FQAI database
  if( any(is.na(user_list_with_scores$c)) )
    message("Species not listed in database, it will be treated as a non-native")

  #select native plants
  native_species <- user_list_with_scores %>%
    dplyr::filter(native == "native")

  #count unique observations
  native_num <- nrow(unique(native_species))

  #return number of native species
  return(native_num)

}

#-------------------------------------------------------------------------------

#' Calculate Mean C
#'
#'`total_mean_c()` calculates the mean conservation coefficient for all species in
#'the site assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name`, `acronym`, or
#' `common_name`.
#'
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' total_mean_c(x = plant_list)

total_mean_c <- function(x) {

  #join scores from michigan fqai to user's assessment
  user_list_with_scores <- suppressMessages(
    dplyr::left_join(x, michigan_2014_fqai))

  #send warning to user if site assessment contains plant not in FQAI database
  if( any(is.na(user_list_with_scores$c)) )
    message("Species not listed in database, it will be treated as a non-native")

  #make it so non-matching plants have a c value of 0
  user_list_with_scores$c[is.na(user_list_with_scores$c)] <- 0

  #calculate mean C
  mean_c <- mean(user_list_with_scores$c)

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
#' must have one of the following columns: `scientific_name`, `acronym`, or
#' `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' native_mean_c(x = plant_list)

native_mean_c <- function(x) {

  #join scores from michigan fqai to user's assessment
  user_list_with_scores <- suppressMessages(
    dplyr::left_join(x, michigan_2014_fqai))

  #send warning to user if site assessment contains plant not in FQAI database
  if( any(is.na(user_list_with_scores$c)) )
    message("Species not listed in database, it will be treated as a non-native")

  #select only native species
  native_species <- user_list_with_scores %>%
    dplyr::filter(native == "native")

  #calculate mean C
  mean_c <- mean(native_species$c)

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
#' must have one of the following columns: `scientific_name`, `acronym`, or
#' `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' total_FQI(x = plant_list)

total_FQI <- function(x) {

  #calculate total fqi
  fqi <- total_mean_c(x) * suppressMessages(sqrt(total_species_richness(x)))

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
#' must have one of the following columns: `scientific_name`, `acronym`, or
#' `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' native_FQI(x = plant_list)

native_FQI <- function(x) {

  #calculate native fqi
  fqi <- native_mean_c(x) * suppressMessages(sqrt(native_species_richness(x)))

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
#' must have one of the following columns: `scientific_name`, `acronym`, or
#' `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' adjusted_FQI(x = plant_list)

adjusted_FQI <- function(x) {

  #calculate adjusted fqi
  fqi <- 100 * (native_mean_c(x)/10) *
    suppressMessages(
      sqrt(native_species_richness(x)/total_species_richness(x))
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
#' must have one of the following columns: `scientific_name`, `acronym`, or
#' `common_name`.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' adjusted_FQI(x = plant_list)

all_metrics <- function(x) {

#create list of all metrics that will be included in the output
metrics <- c("Total Species Richness",
            "Native Species Richness",
            "Mean C",
            "Native Mean C",
            "Total FQI",
            "Native FQI",
            "Adjusted FQI")

#create list of values
values <- c(total_species_richness(x),
            suppressMessages( native_species_richness(x)),
            suppressMessages(total_mean_c(x)),
            suppressMessages(native_mean_c(x)),
            suppressMessages(total_FQI(x)),
            suppressMessages(native_FQI(x)),
            suppressMessages(adjusted_FQI(x)))

#bind metrics and values into data frame
report <- data.frame(metrics, values)

#return the data frame
return(report)

}
