
#this file contains functions for calculating non-cover-weighted fqi metrics
#species_richness(), mean_c(), fqi(), adjusted_fqi(), and all_metrics()

#-------------------------------------------------------------------------------

#' Calculate Number of Species
#'
#' `species_richness` calculates the total number of species.
#'
#' @inheritParams accepted_entries
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #number of species (native and introduced)
#' species_richness(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #number of native species
#' species_richness(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

species_richness <- function(x, key = "name", db, native = FALSE, allow_no_c = TRUE) {

  #count how many observations are unique and matched
  species_richness <- nrow(accepted_entries(x, key, db, native, allow_no_c,
                                            cover_weighted = FALSE,
                                            cover_class = "percent_cover",
                                            allow_duplicates = FALSE,
                                            wetland_warning = FALSE))

  #return number of species
  return(species_richness)

}

#-------------------------------------------------------------------------------

#' Calculate Mean C
#'
#' `mean_c` calculates the mean coefficient of conservatism for all species in the inventory
#' or along the transect.
#'
#' @inheritParams accepted_entries
#'
#' @return A non-negative integer
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #mean c of all species (native and introduced)
#' mean_c(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #mean c of native species
#' mean_c(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

mean_c <- function(x, key = "name", db, native = FALSE) {

  #calculate mean C Value
  mean_c <- mean(accepted_entries(x, key, db, native,
                                  cover_weighted = FALSE,
                                  cover_class = "percent_cover",
                                  allow_duplicates = FALSE,
                                  allow_no_c = FALSE,
                                  wetland_warning = FALSE)$c)

  #print
  return(mean_c)

}

#-------------------------------------------------------------------------------

#' Calculate FQI
#'
#' `FQI` calculates the Floristic Quality Index (FQI) for the area of concern. FQI is found
#' by multiplying the mean C by the square root of the species richness. If `native = TRUE`,
#' `FQI` will calculate the Native FQI.
#'
#' @inheritParams accepted_entries
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #FQI of all species (native and introduced)
#' FQI(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #FQI of native species
#' FQI(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

FQI <- function(x, key = "name", db, native = FALSE) {

  # #calculate total fqi
  # fqi <- mean_c(x, key, db, native) *
  #   suppressMessages(sqrt(species_richness(x, key, db, native, allow_no_c = FALSE)))

  #get accepted entries
  entries <- accepted_entries(x, key, db, native,
                              cover_weighted = FALSE,
                              cover_class = "percent_cover",
                              allow_duplicates = FALSE,
                              allow_no_c = FALSE,
                              allow_non_veg = FALSE,
                              wetland_warning = FALSE,
                              plot_id = NULL)

  #calculate FQI
  fqi <- mean(entries$c) * sqrt(nrow(entries))

  return(fqi)

}

#-------------------------------------------------------------------------------

#' Calculate Adjusted FQI
#'
#' `adjusted_FQI` calculates the Adjusted Floristic Quality Index. Adjusted FQI
#' is found by multiplying 100 by the Native Mean C divided by 10 and then multiplied
#' by the square root of Native Species Richness over Total Species Richness.
#'
#' @inheritParams accepted_entries
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' adjusted_FQI(x = plant_list, key = "acronym", db = "michigan_2014")

adjusted_FQI <- function(x, key = "name", db) {

  #get accepted entries
  entries <- accepted_entries(x, key, db,
                              native = FALSE,
                              cover_weighted = FALSE,
                              cover_class = "percent_cover",
                              allow_duplicates = FALSE,
                              allow_no_c = FALSE,
                              allow_non_veg = FALSE,
                              wetland_warning = FALSE,
                              plot_id = NULL)

  #calculate adjusted fqi
  fqi <- 100 * (mean(dplyr::filter(entries, .data$nativity == "native")$c)/10)*
    sqrt(nrow(dplyr::filter(entries, .data$nativity == "native"))/nrow(entries))

  return(fqi)

}

#-------------------------------------------------------------------------------

#' Print a Summary of Non-Weighted FQA Metrics
#'
#' `all_metrics` calculates and prints a summary of all non cover-weighted metrics,
#' including Species Richness, Native Species Richness, Introduced Species Richness,
#' % of species within C value ranges, Mean C, Native Mean C, Total FQI, Native FQI,
#' Adjusted FQI, Mean Wetness, Native Mean Wetness and % Hydrophytes.
#'
#' @inheritParams accepted_entries
#'
#' @return A data frame
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' all_metrics(x = plant_list, key = "acronym", db = "michigan_2014")

all_metrics <- function(x, key = "name", db, allow_no_c = TRUE) {

  #get list of accepted entries for calculating stats
  entries <- accepted_entries(x, key, db, allow_no_c,
                              native = FALSE,
                              cover_weighted = FALSE,
                              cover_class = "percent_cover",
                              allow_duplicates = FALSE,
                              allow_non_veg = FALSE,
                              wetland_warning = TRUE)

  #create list of all metrics that will be included in the output
  metrics <- c("Total Species Richness",
               "Native Species Richness",
               "Introduced Species Richness",
               "% of Species with no C Value",
               "% of Species with 0 C Value",
               "% of Species with 1-3 C Value",
               "% of Species with 4-6 C Value",
               "% of Species with 7-10 C Value",
               "Mean C",
               "Native Mean C",
               "Total FQI",
               "Native FQI",
               "Adjusted FQI",
               "Mean Wetness",
               "Native Mean Wetness",
               "% Hydrophytes")

  values <- c(
    # Total Species Richness
    nrow(entries),
    # Native Species Richness,
    nrow(dplyr::filter(entries, .data$nativity == "native")),
    # Introduced Species Richness
    nrow(dplyr::filter(entries, .data$nativity == "introduced")),
    # % of Species with no C Value
    (sum(is.na(entries$c))/length(entries$c))*100,
    # % of Species with 0 C Value
    (sum(entries$c < 1, na.rm = TRUE )/length(entries$c))*100,
    # % of Species with 1-3 C Value
    (sum(entries$c >= 1 & entries$c < 4, na.rm = TRUE)/length(entries$c))*100,
    # % of Species with 4-6 C Value
    (sum(entries$c >= 4 & entries$c < 7, na.rm = TRUE)/length(entries$c))*100,
    # % of Species with 7-10 C Value
    (sum(entries$c >= 7 & entries$c <= 10, na.rm = TRUE)/length(entries$c))*100,
    # Mean C
    mean(entries$c, na.rm = TRUE),
    # Native Mean C
    mean(dplyr::filter(entries, .data$nativity == "native")$c, na.rm = TRUE),
    # Total FQI
    mean(entries$c, na.rm = TRUE) * sqrt(nrow(dplyr::filter(entries, !is.na(c)))),
    # Native FQI
    mean(dplyr::filter(entries, .data$nativity == "native")$c, na.rm = TRUE) *
      sqrt(nrow(dplyr::filter(entries, !is.na(c), .data$nativity == "native"))),
    # Adjusted FQI
    100 * (mean(dplyr::filter(entries, .data$nativity == "native")$c, na.rm = TRUE)/10)*
      sqrt(
        nrow(dplyr::filter(entries, .data$nativity == "native", !is.na(c)))/
          nrow(dplyr::filter(entries,!is.na(c)))
      ),
    # Mean Wetness
    mean(entries$w, na.rm = TRUE),
    # Native Mean Wetness
    mean(dplyr::filter(entries, .data$nativity == "native")$w, na.rm = TRUE),
    #% hydrophytes
    if(db %in% c("dakotas_excluding_black_hills_2017", "delaware_2013", "illinois_2020",
                 "iowa_2001","louisiana_coastal_prairie_2006", "mid_atlantic_allegheny_plateau_glaciated_2012",
                 "mid_atlantic_allegheny_plateau_nonglaciated_2012", "mid_atlantic_ridge_valley_2012",
                 "minnesota_wetlands_2007", "pennsylvania_piedmont_2013")) {
      (sum(entries$w < -1, na.rm = TRUE)/length(entries$w))*100
    } else {(sum(entries$w < 0, na.rm = TRUE)/length(entries$w))*100}

  )

  #bind metrics and values into data frame
  report <- data.frame(metrics, values)

  #return the data frame
  return(report)

}



