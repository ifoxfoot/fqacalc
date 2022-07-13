
#this is the function that calculates total mean c values

#' Calculate Mean C
#'
#' @param assessment A dataframe containing a list of plant species. This dataframe must have one of the following columns: `scientific_name`, `acronym`, or `common_name`.
#'
#'
#' @return A value for mean C
#' @export
#'
#' @examples
#' plant_list <- crooked_island_site_assessment
#' total_mean_c(assessment = plant_list)

total_mean_c <- function(assessment) {

  #join scores from michigan fqai to user's assessment
  user_list_with_scores <- dplyr::left_join(assessment, michigan_2014_fqai)

  #calculate mean C
  mean_c <- mean(user_list_with_scores$c)

  #print
  print(mean_c)

  }
