
#this file contains fqi cover-weighted metrics cover_mean_c(), cover_fqi(),
#transect_summary(), and plot_summary()

#-------------------------------------------------------------------------------

#' Calculate Cover-Weighted Mean C
#'
#' `cover_mean_c` calculates the sum of cover multiplied by the C value per each
#' species, divided by the sum of cover values for all species.
#'
#' @inheritParams accepted_entries
#'
#' @return A non-negative number
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' plot <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
#' cover = c(50, 4, 20, 30),
#' plot_id = c(1, 1, 2, 2))
#'
#' cover_mean_c(x = plot, key = "acronym", db = "michigan_2014", native = FALSE,
#' allow_duplicates = FALSE, plot_id = "plot_id")

cover_mean_c <- function(x, key = "name", db, native = FALSE,
                         cover_class = "percent_cover", allow_duplicates,
                         plot_id = NULL) {

  #get accepted entries
  entries <- accepted_entries(x, key, db, native, cover_class, allow_duplicates,
                              cover_weighted = TRUE,
                              allow_no_c = FALSE,
                              allow_non_veg = FALSE,
                              wetland_warning = FALSE,
                              plot_id)

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
#' `cover_FQI` calculates cover-weighted mean C multiplied by the square root
#' of species richness.
#'
#' @inheritParams accepted_entries
#'
#' @return A non-negative number
#' @export
#'
#' @examples
#' transect <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE",
#' "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' plot_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' cover_FQI(x = transect, key = "acronym", db = "michigan_2014",
#' native = FALSE, allow_duplicates = TRUE, plot_id = "plot_id")

cover_FQI <- function(x, key = "name", db, native = FALSE,
                      cover_class = "percent_cover", allow_duplicates,
                      plot_id = NULL) {

  #get accepted entries
  entries <- accepted_entries(x, key, db, native, cover_class, allow_duplicates,
                              cover_weighted = TRUE,
                              allow_no_c = FALSE,
                              allow_non_veg = FALSE,
                              wetland_warning = FALSE,
                              plot_id)

  #calculate mean c
  if(allow_duplicates == FALSE){
    mean_c <- sum(entries$c * entries$cover)/sum(entries$cover)}

  else if(allow_duplicates == TRUE){
    entries <- entries %>%
      dplyr::group_by(!!as.name(key)) %>%
      dplyr::mutate(mean = mean(.data$cover)) %>%
      dplyr::distinct(!!as.name(key), mean, c)
    mean_c <- sum(entries$c * entries$mean)/
      sum(entries$mean)
  }

  #calculate fqi
  fqi <- mean_c * sqrt(nrow(unique(dplyr::select(entries,!!as.name(key)))))

  #return fqi
  return(fqi)

}

#-------------------------------------------------------------------------------

#' Print a Summary of Cover-Weighted FQA Metrics
#'
#' `transect_summary` calculates and prints a summary of both inventory
#' metrics and cover-weighted metrics, including Species Richness, Native Species
#' Richness, Introduced Species Richness, % of species within C value ranges,
#' Mean C, Native Mean C, Cover-weighted Mean C, Native Cover-Weighted Mean C,
#' Total FQI, Native FQI, Cover-Weighted FQI, Native Cover-weighted FQI, Adjusted
#' FQI, Mean Wetness, Native Mean Wetness and % Hydrophytes. Cover-weighted
#' metrics allow duplicate entries for transect level summary metrics.
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
#' plot_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' transect_summary(x = transect, key = "acronym", db = "michigan_2014",
#' plot_id = "plot_id")

transect_summary <- function(x, key = "name", db, cover_class = "percent_cover",
                             allow_no_c = TRUE, plot_id = NULL) {

  #get accepted entries
  entries <- accepted_entries(x, key, db, allow_no_c = allow_no_c,
                              native = FALSE,
                              cover_weighted = TRUE,
                              cover_class,
                              plot_id,
                              allow_duplicates = TRUE,
                              allow_non_veg = FALSE,
                              wetland_warning = TRUE)

  #get unique species (no duplicates)
  unique_entries <- entries %>%
    dplyr::distinct(!!as.name(key), .keep_all = TRUE)

  #get entries where if dups are present the mean cover value is assigned
  entries_c <- entries %>%
    dplyr::group_by(!!as.name(key)) %>%
    dplyr::mutate(mean = mean(.data$cover)) %>%
    dplyr::distinct(!!as.name(key), mean, c)

  #get native entries c
  entries_n <- entries %>%
    dplyr::filter(.data$nativity == "native") %>%
    dplyr::group_by(!!as.name(key)) %>%
    dplyr::mutate(mean = mean(.data$cover)) %>%
    dplyr::distinct(!!as.name(key), mean, c)


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
               "Cover-Weighted Mean C",
               "Cover-Weighted Native Mean C",
               "Total FQI",
               "Native FQI",
               "Cover-Weighted FQI",
               "Cover-Weighted Native FQI",
               "Adjusted FQI",
               "Mean Wetness",
               "Native Mean Wetness",
               "% Hydrophytes"
  )

  values <- c(
    # Total Species Richness
    nrow(unique_entries),
    # Native Species Richness,
    nrow(dplyr::filter(unique_entries, .data$nativity == "native")),
    # Introduced Species Richness
    nrow(dplyr::filter(unique_entries, .data$nativity == "introduced")),
    # % of Species with no C Value
    (sum(is.na(unique_entries$c))/length(unique_entries$c))*100,
    # % of Species with 0 C Value
    (sum(unique_entries$c < 1, na.rm = TRUE )/length(unique_entries$c))*100,
    # % of Species with 1-3 C Value
    (sum(unique_entries$c >= 1 & unique_entries$c < 4, na.rm = TRUE)/
       length(unique_entries$c))*100,
    # % of Species with 4-6 C Value
    (sum(unique_entries$c >= 4 & unique_entries$c < 7, na.rm = TRUE)/
       length(unique_entries$c))*100,
    # % of Species with 7-10 C Value
    (sum(unique_entries$c >= 7 & unique_entries$c <= 10, na.rm = TRUE)/
       length(unique_entries$c))*100,
    # Mean C
    mean(unique_entries$c, na.rm = TRUE),
    # Native Mean C
    mean(dplyr::filter(unique_entries, .data$nativity == "native")$c, na.rm = TRUE),
    # Cover-Weighted Mean C
    sum(entries_c$c * entries_c$mean)/sum(entries_c$mean),
    #Cover-Weighted Native Mean C
    sum(entries_n$c * entries_n$mean)/sum(entries_n$mean),
    # Total FQI
    mean(unique_entries$c, na.rm = TRUE) * sqrt(nrow(dplyr::filter(unique_entries,
                                                                   !is.na(c)))),
    # Native FQI
    mean(dplyr::filter(unique_entries, .data$nativity == "native")$c, na.rm = TRUE) *
      sqrt(nrow(dplyr::filter(unique_entries, !is.na(c), .data$nativity == "native"))),
    # Cover-Weighted FQI
    (sum(entries_c$c * entries_c$mean)/sum(entries_c$mean)) *
      sqrt(nrow(dplyr::filter(unique_entries, !is.na(c)))),
    #Cover-Weighted Native FQI
    (sum(entries_n$c * entries_n$mean)/sum(entries_n$mean)) *
      sqrt(nrow(dplyr::filter(unique_entries, !is.na(c), .data$nativity == "native"))),
    # Adjusted FQI
    100 * (mean(dplyr::filter(unique_entries,
                              .data$nativity == "native")$c, na.rm = TRUE)/10)*
      sqrt(
        nrow(dplyr::filter(unique_entries, .data$nativity == "native", !is.na(c)))/
          nrow(dplyr::filter(unique_entries,!is.na(c)))
      ),
    # Mean Wetness
    mean(unique_entries$w, na.rm = TRUE),
    # Native Mean Wetness
    mean(dplyr::filter(unique_entries, .data$nativity == "native")$w, na.rm = TRUE),
    #% hydrophytes
    if(db %in% c("dakotas_excluding_black_hills_2017",
                 "delaware_2013",
                 "illinois_2020",
                 "iowa_2001","louisiana_coastal_prairie_2006",
                 "mid_atlantic_allegheny_plateau_glaciated_2012",
                 "mid_atlantic_allegheny_plateau_nonglaciated_2012",
                 "mid_atlantic_ridge_valley_2012",
                 "minnesota_wetlands_2007",
                 "pennsylvania_piedmont_2013")) {
      (sum(entries$w < -1, na.rm = TRUE )/length(entries$w))*100
    } else {(sum(entries$w < 0, na.rm = TRUE )/length(entries$w))*100}
  )

  #bind metrics and values into data frame
  report <- data.frame(metrics, values)

  #return the data frame
  return(report)

}

#-------------------------------------------------------------------------------

#' Calculate Plot-level Summary Statistics
#'
#' Input a transect with one or more plots (designated with a unique plot ID) as
#' a single data frame and the output will be a data frame with plot-level
#' species richness, native species richness, mean c, native mean c, FQI, native
#' FQI, adjusted FQI, cover-weighted FQI, and native cover-weighted FQI.
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
#' plot_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' plot_summary(transect, key = "acronym", db = "michigan_2014",
#' cover_class = "percent_cover", plot_id = "plot_id")


plot_summary <- function(x, key = "name", db,
                         cover_class = "percent_cover", plot_id,
                         allow_no_c = TRUE, allow_non_veg = TRUE){


  #get accepted species
  accepted <- accepted_entries(x, key, db,
                               native = FALSE,
                               cover_weighted = TRUE,
                               cover_class,
                               allow_duplicates = TRUE,
                               allow_no_c,
                               allow_non_veg,
                               plot_id,
                               wetland_warning = TRUE)

  #get ground columns
  ground <- accepted %>%
    dplyr::filter(!!as.name(key) %in% c("GROUND", "UNVEGETATED GROUND")) %>%
    dplyr::rename(percent_ground_cover = "cover") %>%
    dplyr::select("percent_ground_cover", !!as.name(plot_id))

  #get water columns
  water <- accepted %>%
    dplyr::filter(!!as.name(key) %in% c("WATER", "UNVEGETATED WATER")) %>%
    dplyr::rename(percent_water_cover = "cover") %>%
    dplyr::select("percent_water_cover", !!as.name(plot_id))

  #get acccepted no veg
  accepted_no_veg <- accepted %>%
    dplyr::filter(!.data$name %in% c("UNVEGETATED GROUND", "UNVEGETATED WATER"),
                  !.data$acronym %in% c("GROUND", "WATER"))


  #group by plot id and calc metrics
  plot_sum <- accepted_no_veg %>%
    dplyr::group_by(!!as.name(plot_id)) %>%
    dplyr::summarise(
      #species richness
      species_richness = nrow(dplyr::pick(dplyr::everything())),

      #native species richness
      native_species_richness = nrow(dplyr::filter(dplyr::pick(dplyr::everything()),
                                                   .data$nativity == "native")),

      #mean wetness
      mean_wetness = mean(dplyr::pick(dplyr::everything())$w, na.rm = TRUE),

      #mean c
      mean_c = mean(dplyr::pick(dplyr::everything())$c, na.rm = TRUE),

      #native mean c
      native_mean_c = mean(dplyr::filter(dplyr::pick(dplyr::everything()),
                                         .data$nativity == "native")$c, na.rm = TRUE),

      #cover mean c
      cover_mean_c = sum(dplyr::filter(dplyr::pick(dplyr::everything()), !is.na(c))$c *
                           dplyr::filter(dplyr::pick(dplyr::everything()), !is.na(c))$cover)/
        sum(dplyr::filter(dplyr::pick(dplyr::everything()), !is.na(c))$cover),

      #FQI
      FQI = mean_c * sqrt(nrow(dplyr::filter(dplyr::pick(dplyr::everything()), !is.na(c)))),

      #native FQI
      native_FQI = mean(dplyr::filter(dplyr::pick(dplyr::everything()),
                                      .data$nativity == "native")$c, na.rm = TRUE) *
        sqrt(nrow(dplyr::filter(dplyr::pick(dplyr::everything()), !is.na(c),
                                .data$nativity == "native"))),

      #cover FQI
      cover_FQI = cover_mean_c *
        sqrt(nrow(dplyr::filter(dplyr::pick(dplyr::everything()), !is.na(c)))),

      #native cover FQI
      native_cover_FQI = (
        sum(dplyr::filter(dplyr::pick(dplyr::everything()),
                          .data$nativity == "native", !is.na(c))$c *
              dplyr::filter(dplyr::pick(dplyr::everything()),
                            .data$nativity == "native", !is.na(c))$cover)/
          sum(dplyr::filter(dplyr::pick(dplyr::everything()),
                            .data$nativity == "native", !is.na(c))$cover)
      ) *
        sqrt(nrow(dplyr::filter(dplyr::pick(dplyr::everything()), !is.na(c),
                                .data$nativity == "native"))),


      #adjusted FQI
      adjusted_FQI = 100 * (mean(dplyr::filter(dplyr::pick(dplyr::everything()),
                                               .data$nativity == "native")$c,
                                 na.rm = TRUE)/10) *
        sqrt(
          nrow(dplyr::filter(dplyr::pick(dplyr::everything()),
                             .data$nativity == "native", !is.na(c)))/
            nrow(dplyr::filter(dplyr::pick(dplyr::everything()),!is.na(c)))
        )
    )

  df <- as.data.frame(dplyr::left_join(plot_sum, ground, by = plot_id)) %>%
    dplyr::left_join(water, by = plot_id)

  return(df)
}
