
#this file contains fqi cover-weighted metrics cover_mean_c(), cover_fqi(), transect_summary(), and plot_summary()

#-------------------------------------------------------------------------------

#' Calculate Cover-Weighted Mean C
#'
#' `cover_mean_c()` calculates the sum of cover times the C value per each species,
#' divided by the sum of cover values for all species.
#'
#' @inheritParams accepted_entries
#'
#' @return A non-negative integer
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' plot <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
#' cover = c(50, 4, 20, 30))
#'
#' cover_mean_c(x = plot, key = "acronym", db = "michigan_2014", native = FALSE,
#' allow_duplicates = FALSE)

cover_mean_c <- function(x, key = "scientific_name", db, native = FALSE,
                           cover_metric = "percent_cover", allow_duplicates) {

  #get accepted entries
  entries <- accepted_entries(x, key, db, native,
                              cover_weighted = TRUE, cover_metric, allow_duplicates)

  #calculate mean C Value
  if(allow_duplicates == FALSE){
    mean_c <- sum(entries$c * entries$cover)/sum(entries$cover)}

  else if(allow_duplicates == TRUE){
    entries <- entries %>%
      dplyr::group_by(!!as.name(key)) %>%
      dplyr::mutate(mean = mean(.data$cover)) %>%
      dplyr::distinct(!!as.name(key), mean, c)

    #calculate mean C Value
    mean_c <- sum(entries$c * entries$mean)/
      sum(entries$mean)
  }

  #return mean
  return(mean_c)

}

#-------------------------------------------------------------------------------

#' Calculate Cover-Weighted FQI
#'
#' `cover_FQI()` calculates cover-weighted mean C multiplied by the square root
#' of species richness.
#'
#' @inheritParams accepted_entries
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' transect <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE",
#' "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' cover_FQI(x = transect, key = "acronym", db = "michigan_2014",
#' native = FALSE, allow_duplicates = TRUE)

cover_FQI <- function(x, key = "scientific_name", db, native = FALSE,
                      cover_metric = "percent_cover", allow_duplicates) {

  fqi <- cover_mean_c(x, key, db, native, cover_metric, allow_duplicates) *
    suppressMessages(sqrt(species_richness(x, key, db, native)))

  #return mean
  return(fqi)

}

#-------------------------------------------------------------------------------

#' Print a Summary of Cover-Weighted FQA Metrics
#'
#' `transect_summary()` calculates and prints a summary of both non cover-weighted
#' metrics and cover-weighted metrics.Cover-weighted metrics allow duplicate entries.
#'
#' @inheritParams accepted_entries
#'
#' @return A data frame
#' @export
#'
#' @examples
#' transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' transect_summary(x = transect, key = "acronym", db = "michigan_2014")

transect_summary <- function(x, key = "scientific_name", db, cover_metric = "percent_cover",
                             allow_no_c = TRUE) {


  accepted <- suppressMessages(accepted_entries(x, key, db, native = FALSE,
                                                cover_weighted = FALSE,
                                                cover_metric = "percent_cover",
                                                allow_duplicates = FALSE,
                                                allow_no_c))

  #create list of all metrics that will be included in the output
  metrics <- c("Total Species Richness",
               "Native Species Richness",
               "Non-native Species Richness",
               "% of Species with no C Value",
               "% of Species with 0 C Value",
               "% of Species with 1-3 C Value",
               "% of Species with 4-6 C Value",
               "% of Species with 7-10 C Value",
               "Mean C",
               "Native Mean C",
               "Cover-Weighted Mean C",
               "Cover-Weighted Native Mean C",
               "Total FQI",
               "Native FQI",
               "Cover-Weighted FQI",
               "Cover-Weighted Native FQI",
               "Adjusted FQI",
               "Mean Wetness",
               "Native Mean Wetness"
               )

  #create list of values
  values <- c(suppressMessages(species_richness(x, key, db, native = FALSE, allow_no_c)),
              suppressMessages(species_richness(x, key, db, native = TRUE, allow_no_c)),
              nrow(dplyr::filter(accepted, .data$native == "exotic")),
              (sum(is.na(accepted$c))/length(accepted$c))*100,
              (sum(accepted$c < 1, na.rm = TRUE )/length(accepted$c))*100,
              (sum(accepted$c >= 1 & accepted$c < 4, na.rm = TRUE)/length(accepted$c))*100,
              (sum(accepted$c >= 4 & accepted$c < 7, na.rm = TRUE)/length(accepted$c))*100,
              (sum(accepted$c >= 7 & accepted$c <= 10, na.rm = TRUE)/length(accepted$c))*100,
              suppressMessages(mean_c(x, key, db, native = FALSE)),
              suppressMessages(mean_c(x, key, db, native = TRUE)),
              cover_mean_c(x, key, db, native = FALSE, cover_metric, allow_duplicates = TRUE),
              suppressMessages(cover_mean_c(x, key, db, native = TRUE, cover_metric, allow_duplicates = TRUE)),
              suppressMessages(FQI(x, key, db, native = FALSE)),
              suppressMessages(FQI(x, key, db, native = TRUE)),
              suppressMessages(cover_FQI(x, key, db, native = FALSE, cover_metric, allow_duplicates = TRUE)),
              suppressMessages(cover_FQI(x, key, db, native = TRUE, cover_metric, allow_duplicates = TRUE)),
              suppressMessages(adjusted_FQI(x, key, db)),
              suppressMessages(mean_w(x, key, db, native = FALSE, allow_no_c)),
              suppressMessages(mean_w(x, key, db, native = TRUE, allow_no_c))
              )


  #bind metrics and values into data frame
  report <- data.frame(metrics, values)

  #return the data frame
  return(report)

}

#-------------------------------------------------------------------------------

#' Calculate Plot-level Summary Statistics
#'
#' Input a transect with one or more plots (designated with a unique plot ID) as a
#' single data frame and  the output will be a data frame with plot-level species richness,
#' native species richness, mean c, native mean c, FQI, native FQI, adjusted FQI,
#' cover-weighted FQI, and native cover-weighted FQI.
#'
#' @inheritParams accepted_entries
#'
#' @return A data frame where each row is a plot and columns contain FQI and
#' cover-weighted FQI statistics.
#' @export
#'
#' @examples
#' transect <- transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' plot_summary(transect, key = "acronym", db = "michigan_2014",
#' cover_metric = "percent_cover", plot_id = "quad_id")


plot_summary <- function(x, key = "scientific_name", db,
                         cover_metric = "percent_cover", plot_id,
                         allow_no_c = TRUE, allow_non_veg = TRUE){

  #plot_id argument must be a column name in input data frame x
  if( is.null(plot_id) )
    stop(paste0("'plot_id' must be the name of a column in ", deparse(substitute(x)), " ."))

  #get accepted species
  accepted <- accepted_entries(x, key, db,
                               native = FALSE,
                               cover_weighted = TRUE,
                               cover_metric,
                               allow_duplicates = TRUE,
                               allow_no_c,
                               allow_non_veg,
                               plot_id)

  #get ground columns
  ground <- accepted %>%
    dplyr::filter(!!as.name(key) %in% c("GROUND", "UNVEGETATED GROUND")) %>%
    dplyr::rename(ground_cover = "cover") %>%
    dplyr::select("ground_cover", !!as.name(plot_id))

  #get water columns
  water <- accepted %>%
    dplyr::filter(!!as.name(key) %in% c("WATER", "UNVEGETATED WATER")) %>%
    dplyr::rename(water_cover = "cover") %>%
    dplyr::select("water_cover", !!as.name(plot_id))

  #group by plot ID and calc metrics
  plot_sum <- accepted %>%
    dplyr::group_by(!!as.name(plot_id)) %>%
    dplyr::summarise(

      species_richness
      = suppressMessages(species_richness(dplyr::cur_data(), key, db, native = FALSE, allow_no_c)),

      native_species_richness
      = suppressMessages(species_richness(dplyr::cur_data(), key, db, native = TRUE, allow_no_c)),

      mean_wetness
      = suppressMessages(mean_w(dplyr::cur_data(), key, db, native = FALSE, allow_no_c)),

      mean_c
      = suppressMessages(mean_c(dplyr::cur_data(), key, db, native = FALSE)),

      native_mean_c
      = suppressMessages(mean_c(dplyr::cur_data(), key, db, native = TRUE)),

      cover_mean_c
      = suppressMessages(cover_mean_c(dplyr::cur_data(), key, db, native = FALSE, cover_metric,
                     allow_duplicates = FALSE)),

      FQI
      = suppressMessages(FQI(dplyr::cur_data(), key, db, native = FALSE)),

      native_FQI
      = suppressMessages(FQI(dplyr::cur_data(), key, db, native = TRUE)),

      cover_FQI
      = suppressMessages(cover_FQI(dplyr::cur_data(), key, db, native = FALSE, cover_metric,
                  allow_duplicates = FALSE)),

      native_cover_FQI
      = suppressMessages(cover_FQI(dplyr::cur_data(), key, db, native = TRUE, cover_metric,
                  allow_duplicates = FALSE)),

      adjusted_FQI
      = suppressMessages(adjusted_FQI(dplyr::cur_data(), key, db))
    )

  df <- as.data.frame(dplyr::left_join(plot_sum, ground, by = plot_id)) %>%
    dplyr::left_join(water, by = plot_id)

  return(df)
}
