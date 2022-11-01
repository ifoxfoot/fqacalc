
#this file contains fqi cover-weighted metrics (quadrat_mean_c(), transect_mean_c(), cover_fqi(), all_cover_metrics())

#-------------------------------------------------------------------------------

#' Calculate Cover-Weighted Mean C
#'
#' `cover_mean_c()` calculates the sum of cover times the C value per each species,
#' divided by the sum of cover values for all species.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym` as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#' @param cover_metric a character string representing the cover method used. Acceptable
#' cover methods are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default and is
#' recommended because it is the most accurate.
#' @param allow_duplicates Boolean (TRUE or FALSE). If TRUE, allow duplicate entries of
#' the same species. If FALSE, do not allow species duplication. Setting `allow_duplicates`
#' to TRUE is best for calculating metrics for multiple plots/quadrats which potentially
#' contain the same species. Setting `allow_duplicates` to FALSE is best for calculating
#' metrics for a single plot/quadrat, where each species is entered once along with its
#' total cover value.
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
                              cover_weighted = T, cover_metric, allow_duplicates)

  #calculate mean c value without dups
  if(allow_duplicates == FALSE){
    mean_c <- sum(entries$c * entries$cover)/sum(entries$cover)}

  else if(allow_duplicates == TRUE){
    entries <- entries %>%
      dplyr::group_by(!!as.name(key)) %>%
      dplyr::mutate(mean = mean(.data$cover)) %>%
      dplyr::distinct(!!as.name(key), mean, c)


    #calculate mean c value
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
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym` as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#' @param cover_metric a character string representing the cover method used. Acceptable
#' cover methods are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default and is
#' recommended because it is the most accurate.
#' @param allow_duplicates Boolean (TRUE or FALSE). If TRUE, allow duplicate entries of
#' the same species. If FALSE, do not allow species duplication. Setting `allow_duplicates`
#' to TRUE is best for calculating metrics for multiple plots/quadrats which potentially
#' contain the same species. Setting `allow_duplicates` to FALSE is best for calculating
#' metrics for a single plot/quadrat, where each species is entered once along with its
#' total cover value.
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
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym` as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#' @param cover_metric a character string representing the cover method used. Acceptable
#' cover methods are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default and is
#' recommended because it is the most accurate.
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

transect_summary <- function(x, key = "scientific_name", db, cover_metric = "percent_cover") {


  accepted <- suppressMessages(accepted_entries(x, key, db, native = FALSE,
                                                cover_weighted = FALSE,
                                                cover_metric = "percent_cover",
                                                allow_duplicates = FALSE))

  #create list of all metrics that will be included in the output
  metrics <- c("Total Species Richness",
               "Native Species Richness",
               "Exotic Species Richness",
               "Proportion of Species with < 1 C Value",
               "Proportion of Species with 1-3.9 C Value",
               "Proportion of Species with 4-6.9 C Value",
               "Proportion of Species with 7-10 C Value",
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
  values <- c(suppressMessages(species_richness(x, key, db, native = FALSE)),
              suppressMessages(species_richness(x, key, db, native = TRUE)),
              nrow(dplyr::filter(accepted, .data$native == "exotic")),
              sum(accepted$c <= 1 )/length(accepted$c),
              sum(accepted$c >= 1 & accepted$c < 4)/length(accepted$c),
              sum(accepted$c >= 4 & accepted$c < 7)/length(accepted$c),
              sum(accepted$c >= 7 & accepted$c <= 10)/length(accepted$c),
              suppressMessages(mean_c(x, key, db, native = FALSE)),
              suppressMessages(mean_c(x, key, db, native = TRUE)),
              cover_mean_c(x, key, db, native = FALSE, cover_metric, allow_duplicates = TRUE),
              suppressMessages(cover_mean_c(x, key, db, native = TRUE, cover_metric, allow_duplicates = TRUE)),
              suppressMessages(FQI(x, key, db, native = FALSE)),
              suppressMessages(FQI(x, key, db, native = TRUE)),
              suppressMessages(cover_FQI(x, key, db, native = FALSE, cover_metric, allow_duplicates = TRUE)),
              suppressMessages(cover_FQI(x, key, db, native = TRUE, cover_metric, allow_duplicates = TRUE)),
              suppressMessages(adjusted_FQI(x, key, db)),
              suppressMessages(mean_w(x, key, db, native = FALSE)),
              suppressMessages(mean_w(x, key, db, native = TRUE))
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
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym` as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A character string representing the column that will be used to join
#' the input `x` with the regional FQA database. If a value is not specified the
#' default is `"scientific_name"`. `"scientific_name"` and `"acronym"` are the only acceptable
#' values for key.
#' @param db A character string representing the regional FQA database to use. See
#' `db_names()` for a list of potential values.
#' @param cover_metric a character string representing the cover method used. Acceptable
#' cover methods are: `"percent_cover"`, `"carolina_veg_survey"`, `"braun-blanquet"`,
#' `"daubenmire"`, and `"usfs_ecodata"`. `"percent_cover"` is the default and is
#' recommended because it is the most accurate.
#' @param plot_id A character string representing the column in `x` that contains plot
#' identification values.
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
                         cover_metric = "percent_cover", plot_id){

  if( !plot_id %in% colnames(x) )
    stop(paste("'plot_id' must be the name of a column in", deparse(substitute(x)), "."))

  plot_sum <- x %>%
    dplyr::group_by(!!as.name(plot_id)) %>%
    dplyr::summarise(
      species_richness
      = species_richness(dplyr::cur_data(), key, db, native = FALSE),
      native_species_richness
      = species_richness(dplyr::cur_data(), key, db, native = TRUE),
      mean_c
      = mean_c(dplyr::cur_data(), key, db, native = FALSE),
      native_mean_c
      = mean_c(dplyr::cur_data(), key, db, native = TRUE),
      cover_mean_c
      = cover_mean_c(dplyr::cur_data(), key, db, native = FALSE, cover_metric,
                     allow_duplicates = FALSE),
      FQI
      = FQI(dplyr::cur_data(), key, db, native = FALSE),
      native_FQI
      = FQI(dplyr::cur_data(), key, db, native = TRUE),
      cover_FQI
      = cover_FQI(dplyr::cur_data(), key, db, native = FALSE, cover_metric,
                  allow_duplicates = FALSE),
      native_cover_FQI
      = cover_FQI(dplyr::cur_data(), key, db, native = TRUE, cover_metric,
                  allow_duplicates = FALSE),
      adjusted_FQI
      = adjusted_FQI(dplyr::cur_data(), key, db)
    )

  df <- as.data.frame(plot_sum)

  return(df)
}
