
#this is the function that calculates total mean c values

total_mean_c <- function(assessment) {

  #join scores from michigan fqai to user's assessment
  user_list_with_scores <- dplyr::left_join(assessment, michigan_2014_fqai)

  #calculate mean C
  mean_c <- mean(user_list_with_scores$c)

  #print
  print(mean_c)

  }
