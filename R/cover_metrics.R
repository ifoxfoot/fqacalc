
#this file contains fqi cover-weighted metrics

#-------------------------------------------------------------------------------

#' Calculate Quadrat-Level Cover-Weighted Mean C
#'
#' `quadrat_mean_c` calculates the sum of cover times c value per each species,
#' divided by the sum of cover values for all species. The main difference between
#' `transect_mean_c` and `quadrat_mean_c` is that `transect_mean_c` accepts duplicate entries.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym` as well
#' as a column named `cover` containing percent cover values per each observation.
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

quadrat_mean_c <- function(x, key = "acronym", db, native) {

  #get accepted entries
  entries <- accepted_entries(x, key, db, native, cover_weighted = T)

  #calculate mean c score
  mean_c <- sum(entries$c * entries$cover)/sum(entries$cover)

  #return mean
  return(mean_c)

}

#-------------------------------------------------------------------------------

#' Calculate Transect-Level Cover-Weighted Mean C
#'
#' `transect_mean_c` calculates the sum of species' mean cover times their c value,
#' divided by the sum of mean cover values for all species. The main difference between
#' `transect_mean_c` and `quadrat_mean_c` is that `transect_mean_c` accepts duplicate entries.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`  as well
#' as a column named `cover` containing percent cover values per each observation.
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
#' transect <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE",
#' "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' transect_mean_c(x = transect, key = "acronym", db = "michigan_2014", native = FALSE)

transect_mean_c <- function(x, key = "acronym", db, native) {

  #declaring cover is null
  cover <- NULL

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

#' Calculate Cover-Weighted FQI
#'
#' `cover_FQI` calculates transect-level or quadrat-level (depending on if there
#' are duplicates) multiplied by the square root of species richness.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym` as well
#' as a column named `cover` containing percent cover values per each observation.
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
#' transect <- data.frame(acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE",
#' "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' cover_FQI(x = transect, key = "acronym", db = "michigan_2014", native = FALSE)

cover_FQI <- function(x, key = "acronym", db, native) {

  fqi <- transect_mean_c(x, key, db, native) *
    suppressMessages(sqrt(species_richness(x, key, db, native)))

  #return mean
  return(fqi)

}

#-------------------------------------------------------------------------------

#' Print a Summary of Cover-Weighted FQA Metrics
#'
#' `all_cover_metrics()` calculates and prints a summary of both non cover-weighted
#' metrics and cover-weighted metrics.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`, as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
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
#' all_cover_metrics(x = transect, key = "acronym", db = "michigan_2014")

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

#-------------------------------------------------------------------------------

#' Calculate Relative Frequency
#'
#' `relative_freq()` calculates the frequency of a species, taxonomic family,
#' or physiognomic group, divided by the frequency of all observations.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`, as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#' @param species Optional. A character string equal to the Latin name of a species
#' to calculate relative frequency of that species.
#' @param family Optional. A character string equal to a taxonomic family to
#' calculate the relative frequency of that family.
#' @param physiog Optional. A character string equal to a physiognomic state (i.e.
#' tree, shrub) to calculate the relative frequency of that family.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' relative_freq(transect, key = "acronym", db = "michigan_2014", native = FALSE, physiog = "tree")

relative_freq <- function(x, key = "acronym", db, native,
                          species = NULL, family = NULL, physiog = NULL) {

  #store optional argument
  optional_arg <- c(species, family, physiog)

  #no more than one optional argument can be set
  if( length(optional_arg) > 1)
    stop("Only one optional paremeter ('species', 'family', 'physiog') can be set")

  #at least one optional argument must be set
  if( length(optional_arg) == 0)
    stop("One optional paremeter ('species', 'family', 'physiog') must be set")

  #which argument is being called?
  name <- if(!is.null(species))  {"scientific_name"}
          else if(!is.null(family)) {"family"} else {"physiognomy"}

  #join entries to database in order to get info on family, physiognomy
  entries <- accepted_entries(x, key, db, native, allow_duplicates = TRUE)

  #calculate relative frequency--fre/num observations, select right col
  df <- 100*(table(entries[name])/nrow(entries))[[optional_arg]]

  #return result
  return(df)

}

#-------------------------------------------------------------------------------


#' Calculate Relative Cover
#'
#' `relative_cover()` calculates the total cover per group of interest (species,
#' taxonomic family, or physiognomic group) divided by the total cover for all
#' observations.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`, as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#' @param species Optional. A character string equal to the Latin name of a species
#' to calculate relative frequency of that species.
#' @param family Optional. A character string equal to a taxonomic family to
#' calculate the relative frequency of that family.
#' @param physiog Optional. A character string equal to a physiognomic state (i.e.
#' tree, shrub) to calculate the relative frequency of that family.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' relative_cover(transect, key = "acronym", db = "michigan_2014", native = FALSE,
#' physiog = "tree")

relative_cover <- function(x, key = "acronym", db, native,
                           species = NULL, family = NULL, physiog = NULL){

  #store optional argument
  optional_arg <- c(species, family, physiog)

  #no more than one optional argument can be set
  if( length(optional_arg) > 1)
    stop("Only one optional paremeter ('species', 'family', 'physiog') can be set")

  #at least one optional argument must be set
  if( length(optional_arg) == 0)
    stop("One optional paremeter ('species', 'family', 'physiog') must be set")

  #which argument is being called?
  name <- if(!is.null(species))  {as.name("scientific_name")}
  else if(!is.null(family)) {as.name("family")} else {as.name("physiognomy")}

  #bind to regional fqa list to get info about taxonomy, physiognomy
  entries <- accepted_entries(x, key, db, native, allow_duplicates = T, cover_weighted = T) %>%
    dplyr::group_by(!!name) %>%
    #caclulate cover per group
    dplyr::summarise(sum = sum(cover)) %>%
    as.data.frame()

  #filter for when name is equal to argument
  filtered <- entries %>% dplyr::filter(!!name == optional_arg)

  #calculate cover per that group divided by total cover
  r_cover <- 100*filtered$sum /sum(entries$sum)

  #return the result
  return(r_cover)

}

#-------------------------------------------------------------------------------

#' Calculate Relative Importance
#'
#' `relative_importance()` calculates the average of relative frequecy and relative cover.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`, as well
#' as a column named `cover` containing percent cover values per each observation.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#' @param db A character string representing the regional FQA database to use.
#' @param native Boolean (TRUE or FALSE). If TRUE, calculate metrics using only
#' native species.
#' @param species Optional. A character string equal to the Latin name of a species
#' to calculate relative frequency of that species.
#' @param family Optional. A character string equal to a taxonomic family to
#' calculate the relative frequency of that family.
#' @param physiog Optional. A character string equal to a physiognomic state (i.e.
#' tree, shrub) to calculate the relative frequency of that family.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' transect <- data.frame(
#' acronym  = c("ABEESC", "ABIBAL", "AMMBRE", "ANTELE", "ABEESC", "ABIBAL", "AMMBRE"),
#' cover = c(50, 4, 20, 30, 40, 7, 60),
#' quad_id = c(1, 1, 1, 1, 2, 2, 2))
#'
#' relative_importance(transect, key = "acronym", db = "michigan_2014", native = FALSE,
#' physiog = "tree")

relative_importance <- function(x, key = "acronym", db, native,
                                species = NULL, family = NULL, physiog = NULL){

  #get mean of relative freq and relative cover
  avg = (relative_freq(x, key, db, native,
                      species, family, physiog) +
           relative_cover(x, key, db, native,
                        species, family, physiog))/2

  #return value
  return(avg)

}
