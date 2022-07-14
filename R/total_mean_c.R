
#' Calculate Number of Species
#'
#' @param x A data frame containing a list of plant species. This data frame must have one of the following columns: `scientific_name`, `acronym`, or `common_name`.
#'
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island_site_assessment
#' total_species_richness(x = plant_list)

total_species_richness <- function(x) {

  #count how many observations are in species list
  species_richness <- nrow(x)

  #return number of species
  return(species_richness)

}

#-------------------------------------------------------------------------------

#' Calculate Number of Native Species
#'
#' @param x A data frame containing a list of plant species. This data frame must have one of the following columns: `scientific_name`, `acronym`, or `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island_site_assessment
#' native_species_richness(x = plant_list)

native_species_richness <- function(x) {

  #join scores from Michigan FQAI to user's assessment
  user_list_with_scores <- dplyr::left_join(x, michigan_2014_fqai)

  #select native plants
  native_species <- user_list_with_scores %>%
    dplyr::filter(native == "native") %>%
    #count observations
    nrow()

  #return number of native species
  return(native_species)

}

#-------------------------------------------------------------------------------

#' Calculate Mean C
#'
#' @param x A data frame containing a list of plant species. This data frame must have one of the following columns: `scientific_name`, `acronym`, or `common_name`.
#'
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island_site_assessment
#' total_mean_c(x = plant_list)

total_mean_c <- function(x) {

  #join scores from michigan fqai to user's assessment
  user_list_with_scores <- dplyr::left_join(x, michigan_2014_fqai)

  #calculate mean C
  mean_c <- mean(user_list_with_scores$c)

  #print
  return(mean_c)

  }

#-------------------------------------------------------------------------------

#' Calculate Native Mean C
#'
#' @param x A data frame containing a list of plant species. This data frame must have one of the following columns: `scientific_name`, `acronym`, or `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island_site_assessment
#' native_mean_c(x = plant_list)

native_mean_c <- function(x) {

  #join scores from michigan fqai to user's assessment
  user_list_with_scores <- dplyr::left_join(x, michigan_2014_fqai) %>%
    dplyr::filter(native == "native")

  #calculate mean C
  mean_c <- mean(user_list_with_scores$c)

  #print
  return(mean_c)

}

#-------------------------------------------------------------------------------

#' Calculate Total FQI
#'
#' @param x A data frame containing a list of plant species. This data frame must have one of the following columns: `scientific_name`, `acronym`, or `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island_site_assessment
#' total_FQI(x = plant_list)

total_FQI <- function(x) {

  #join scores from michigan fqai to user's assessment
  user_list_with_scores <- dplyr::left_join(x, michigan_2014_fqai)

  #calculate mean C
  fqi <- total_mean_c(x) * sqrt(total_species_richness(x))

  #print
  return(fqi)

}

#-------------------------------------------------------------------------------

#' Calculate Native FQI
#'
#' @param x A data frame containing a list of plant species. This data frame must have one of the following columns: `scientific_name`, `acronym`, or `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island_site_assessment
#' native_FQI(x = plant_list)

native_FQI <- function(x) {

  #join scores from michigan fqai to user's assessment
  user_list_with_scores <- dplyr::left_join(x, michigan_2014_fqai) %>%
    dplyr::filter(native == "native")

  #calculate mean C
  fqi <- native_mean_c(x) * sqrt(native_species_richness(x))

  #print
  return(fqi)

}

#-------------------------------------------------------------------------------

#' Calculate Adjusted FQI
#'
#' @param x A data frame containing a list of plant species. This data frame must have one of the following columns: `scientific_name`, `acronym`, or `common_name`.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island_site_assessment
#' adjusted_FQI(x = plant_list)

adjusted_FQI <- function(x) {

  #join scores from michigan fqai to user's assessment
  user_list_with_scores <- dplyr::left_join(x, michigan_2014_fqai)

  #calculate mean C
  fqi <- 100 * (native_mean_c(x)/10) *
    (sqrt(native_species_richness(x))/sqrt(total_species_richness(x)))

  #print
  return(fqi)

}
