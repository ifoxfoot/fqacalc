
#this prevents a note about native being an undefined global variable
utils::globalVariables("native")

#-------------------------------------------------------------------------------

#' Calculate Number of Species
#'
#' `total_species_richness()` calculates the total number of species in the site
#' assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame must
#' have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#'
#' plant_list <- crooked_island
#' total_species_richness(x = plant_list)

total_species_richness <- function(x, key = "acronym") {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x does not exist
  if( !exists(deparse(substitute(x))) )
    stop(paste("argument ", deparse(substitute(x)), " does not exist."))

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

  #send message to user if site assessment contains duplicate entries
  if( sum(duplicated(x[,key])) > 0 )
    message("Duplicate entries detected. Duplicates will only be counted once.")

  #select only unique entries
  unique_entries <- x %>%
    dplyr::distinct(!!as.name(key))

  #join scores from Michigan FQAI to user's assessment
  unique_entries_joined <-
    fuzzyjoin::regex_left_join(unique_entries,
                               michigan_2014_fqai,
                               by = key,
                               ignore_case = T)

  #send message to user if site assessment contains plant not in FQAI database
  if( any(is.na(unique_entries_joined$c)) )
    message(paste0("species", unique_entries_joined[is.na(unique_entries_joined$c), key],
                  "not listed in database. it will be discarded."))

  #discard entries that have no c score, select native entries
  unique_entries_matched <- unique_entries_joined %>%
    dplyr::filter(!is.na(c))

  #count how many observations are unique and matched
  species_richness <- nrow(unique_entries_matched)

  #return number of species
  return(species_richness)

}

#-------------------------------------------------------------------------------

#' Calculate Number of Native Species
#'
#' `native_species_richness()` calculates the total number of native species in the
#' site assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame must
#' have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' native_species_richness(x = plant_list)

native_species_richness <- function(x, key = "acronym") {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x does not exist
  if( !exists(deparse(substitute(x))) )
    stop(paste("argument ", deparse(substitute(x)), " does not exist."))

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

  #send message to user if site assessment contains duplicate entries
  if( sum(duplicated(x[,key])) > 0 )
    message("Duplicate entries detected. Duplicates will only be counted once.")

  #select only unique entries
  unique_entries <- x %>%
    dplyr::distinct(!!as.name(key))

  #join scores from Michigan FQAI to user's assessment
  unique_entries_joined <-
    fuzzyjoin::regex_left_join(unique_entries,
                               michigan_2014_fqai,
                               by = key,
                               ignore_case = T)

  #send message to user if site assessment contains plant not in FQAI database
  if( any(is.na(unique_entries_joined$c)) )
    message(paste("species", unique_entries_joined[is.na(unique_entries_joined$c), key],
                  "not listed in database. it will be discarded."))

  #discard entries that have no c score, select native entries
  unique_entries_native <- unique_entries_joined %>%
    dplyr::filter(!is.na(c)) %>%
    dplyr::filter(native == "native")

  #count how many observations are native, unique, and have c score
  native_richness <- nrow(unique_entries_native)

  #return number of species
  return(native_richness)

}

#-------------------------------------------------------------------------------

#' Calculate Mean C
#'
#'`total_mean_c()` calculates the mean conservation coefficient for all species in
#'the site assessment.
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' total_mean_c(x = plant_list)

total_mean_c <- function(x, key = "acronym") {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x does not exist
  if( !exists(deparse(substitute(x))) )
    stop(paste("argument ", deparse(substitute(x)), " does not exist."))

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

  #send message to user if site assessment contains duplicate entries
  if( sum(duplicated(x[,key])) > 0 )
    message("Duplicate entries detected. Duplicates will only be counted once.")

  #select only unique entries
  unique_entries <- x %>%
    dplyr::distinct(!!as.name(key))

  #join scores from Michigan FQAI to user's assessment
  unique_entries_joined <-
    fuzzyjoin::regex_left_join(unique_entries,
                               michigan_2014_fqai,
                               by = key,
                               ignore_case = T)

  #send message to user if site assessment contains plant not in FQAI database
  if( any(is.na(unique_entries_joined$c)) )
    message(paste("species", unique_entries_joined[is.na(unique_entries_joined$c), key],
                  "not listed in database. it will be discarded."))

  #discard entries that have no c score
  unique_entries_matched <- unique_entries_joined %>%
    dplyr::filter(!is.na(c))

  #calculate mean c score
  mean_c <- mean(unique_entries_matched $c)

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
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' native_mean_c(x = plant_list)

native_mean_c <- function(x, key = "acronym") {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x does not exist
  if( !exists(deparse(substitute(x))) )
    stop(paste("argument ", deparse(substitute(x)), " does not exist."))

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
                "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

  #send message to user if site assessment contains duplicate entries
  if( sum(duplicated(x[,key])) > 0 )
    message("Duplicate entries detected. Duplicates will only be counted once.")

  #select only unique entries
  unique_entries <- x %>%
    dplyr::distinct(!!as.name(key))

  #join scores from Michigan FQAI to user's assessment
  unique_entries_joined <-
    fuzzyjoin::regex_left_join(unique_entries,
                               michigan_2014_fqai,
                               by = key,
                               ignore_case = T)

  #send message to user if site assessment contains plant not in FQAI database
  if( any(is.na(unique_entries_joined$c)) )
      message(paste("species", unique_entries_joined[is.na(unique_entries_joined$c), key],
                   "not listed in database. it will be discarded."))

  #discard entries that have no c score, select native entries
  unique_entries_native <- unique_entries_joined %>%
    dplyr::filter(!is.na(c)) %>%
    dplyr::filter(native == "native")

  #calculate mean C
  mean_c <- mean(unique_entries_native$c)

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
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' total_FQI(x = plant_list)

total_FQI <- function(x, key = "acronym") {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x does not exist
  if( !exists(deparse(substitute(x))) )
    stop(paste("argument ", deparse(substitute(x)), " does not exist."))

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

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
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' native_FQI(x = plant_list)

native_FQI <- function(x, key = "acronym") {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x does not exist
  if( !exists(deparse(substitute(x))) )
    stop(paste("argument ", deparse(substitute(x)), " does not exist."))

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

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
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#'
#' @return A non-negative integer
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' adjusted_FQI(x = plant_list)

adjusted_FQI <- function(x, key = "acronym") {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x does not exist
  if( !exists(deparse(substitute(x))) )
    stop(paste("argument ", deparse(substitute(x)), " does not exist."))

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

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
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' adjusted_FQI(x = plant_list)

all_metrics <- function(x, key = "acronym") {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x does not exist
  if( !exists(deparse(substitute(x))) )
    stop(paste("argument ", deparse(substitute(x)), " does not exist."))

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

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

#-------------------------------------------------------------------------------

#' Return Data Frame of Successfully matched Plant Species
#'
#' @param x A data frame containing a list of plant species. This data frame
#' must have one of the following columns: `scientific_name` or `acronym`.
#' @param key A column name that will be used to join the input `x` with the 2014
#' Michigan FQAI database. If a value is not specified the default is `acronym`.
#' `scientific_name` and `acronym` are the only acceptable values for key.
#'
#' @return A data frame containing the 'key' column --either `acronym` or
#' `scientific_name` -- as well as columns from the Michigan 2014 fqai database.
#' These columns include `family`, `native`, `c` (which represents the C score),
#' `w` (which represents wetness score), `physiognomy`, `duration`, and `common_name`
#' @export
#'
#' @examples
#' plant_list <- crooked_island
#' adjusted_FQI(x = plant_list)

accepted_entries <- function(x, key = "acronym") {

  #error if x argument is missing
  if( missing(x) )
    stop("argument x is missing, with no default.")

  #error if x does not exist
  if( is.null(x) )
    stop(paste("argument ", x, " does not exist."))

  #error if x is not a data frame
  if( !is.data.frame(x) )
    stop(paste(deparse(substitute(x)), "must be a data frame."))

  #error if x does not have correct col names
  if( !"acronym" %in% colnames(x) & !"scientific_name" %in% colnames(x))
    stop(paste(deparse(substitute(x)),
               "must have a column named 'acronym' and/or 'scientific_name'."))

  #error if key is not acronym or scientific name
  if( !key %in% c("acronym", "scientific_name") )
    stop("key must be equal to 'acronym' or 'scientific_name'.")

  #error if key is not in col names of x
  if( !key %in% colnames(x) )
    stop(paste(deparse(substitute(x)), " does not have a column named ", key, "."))

  #send message to user if site assessment contains duplicate entries
  if( sum(duplicated(x[,key])) > 0 )
    message("Duplicate entries detected. Duplicates will only be counted once.")

  #select only unique entries
  unique_entries <- x %>%
    dplyr::distinct(!!as.name(key))

  #join scores from Michigan FQAI to user's assessment
  unique_entries_joined <-
    dplyr::left_join(unique_entries, michigan_2014_fqai, by = key)

  #send message to user if site assessment contains plant not in FQAI database
  if( any(is.na(unique_entries_joined$c)) )
    message(paste("species", unique_entries_joined[is.na(unique_entries_joined$c), key],
                  "not listed in database. it will be discarded."))

  #discard entries that have no c score, select native entries
  unique_entries_matched <- unique_entries_joined %>%
    dplyr::filter(!is.na(c))

  return(unique_entries_matched)

}
