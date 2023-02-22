
#this file contains `mean_w()`

#-------------------------------------------------------------------------------

#' Calculate the Mean Wetness Coefficient
#'
#'`mean_w` calculates the mean wetness coefficient for all species in
#' the site assessment. The wetness coefficient is based on wetland indicator
#' status. Negative wetness coefficients indicate a stronger affinity for wetlands, while
#' positive wetness coefficients indicate an affinity for uplands.
#'
#' @inheritParams accepted_entries
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#'
#' #mean wetness of all species (native and introduced)
#' mean_w(x = plant_list, key = "acronym", db = "michigan_2014", native = FALSE)
#'
#' #mean wetness of native species
#' mean_w(x = plant_list, key = "acronym", db = "michigan_2014", native = TRUE)

mean_w <- function(x, key = "name", db, native = FALSE, allow_no_c = TRUE) {

  #calculate mean W value (remove NA w coeffs)
  mean_w <- mean(accepted_entries(x, key, db, native,
                                  cover_weighted = FALSE,
                                  cover_class = "percent_cover",
                                  allow_duplicates = FALSE,
                                  allow_no_c,
                                  wetland_warning = TRUE)$w, na.rm = TRUE)

  #print
  return(mean_w)

}
