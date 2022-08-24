
#' Calculate Quadrat-Level Cover-Weighted Mean C
#'
#' `quadrat_mean_c` calculates the sum of cover times c value per each species,
#' divided by the sum of cover values for all species.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`
#' AND have a column named `cover`.
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
#' quadrat <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE"),
#' cover = c(50, 4, 20, 30))
#'
#' quadrat_mean_c(x = quadrat, key = "acronym", db = "michigan_2014", native = FALSE)
#' #4.923077

quadrat_mean_c <- function(x, key = "acronym", db, native) {

  #get accepted entries
  entries <- accepted_entries(x, key, db, native, cover_weighted = T)

  #calculate mean c score
  mean_c <- sum(entries$c * entries$cover)/sum(entries$cover)

  #return mean
  return(mean_c)

}

#-------------------------------------------------------------------------------

transect_mean_c <- function(x, key = "acronym", db, native) {

  #get accepted entries
  entries <- accepted_entries(x, key, db, native,
                              cover_weighted = T,
                              allow_duplicates = T) %>%
    dplyr::group_by(!!as.name(key)) %>%
    dplyr::mutate(mean = mean(cover)) %>%
    dplyr::distinct(!!as.name(key), mean, c)

  #calculate mean c score
  mean_c <- sum(entries$c * entries$mean)/
    sum(entries$mean)

  #return mean
  return(mean_c)

}

#-------------------------------------------------------------------------------

cover_FQI <- function(x, key = "acronym", db, native) {

  fqi <- transect_mean_c(x, key, db, native) *
    suppressMessages(sqrt(species_richness(x, key, db, native)))

  #return mean
  return(fqi)

}

#-------------------------------------------------------------------------------

all_cover_metrics <- function(x, key = "acronym", db) {

  accepted <- suppressMessages(accepted_entries(x, key, db, native = F))

  #create list of all metrics that will be included in the output
  metrics <- c("Total Species Richness",
               "Native Species Richness",
               "Proportion of Species with < 1 C score",
               "Proportion of Species with 1-3.9 C score",
               "Proportion of Species with 4-6.9 C score",
               "Proportion of Species with 7-10 C score",
               "Mean C",
               "Native Mean C",
               "Cover-Weighted Mean C",
               "Cover-Weighted Native Mean C",
               "Total FQI",
               "Native FQI",
               "Cover-Weighted FQI",
               "Cover-Weighted Native FQI",
               "Adjusted FQI"
               )

  #create list of values
  values <- c(suppressMessages(species_richness(x, key, db, native = F)),
              suppressMessages(species_richness(x, key, db, native = T)),
              sum(accepted$c <= 1 )/length(accepted$c),
              sum(accepted$c >= 1 & accepted$c < 4)/length(accepted$c),
              sum(accepted$c >= 4 & accepted$c < 7)/length(accepted$c),
              sum(accepted$c >= 7 & accepted$c <= 10)/length(accepted$c),
              suppressMessages(mean_c(x, key, db, native = F)),
              suppressMessages(mean_c(x, key, db, native = T)),
              transect_mean_c(x, key, db, native = F),
              suppressMessages(transect_mean_c(x, key, db, native = T)),
              suppressMessages(FQI(x, key, db, native = F)),
              suppressMessages(FQI(x, key, db, native = T)),
              suppressMessages(cover_FQI(x, key, db, native = F)),
              suppressMessages(cover_FQI(x, key, db, native = T)),
              suppressMessages(adjusted_FQI(x, key, db))
              )


  #bind metrics and values into data frame
  report <- data.frame(metrics, values)

  #return the data frame
  return(report)

}
