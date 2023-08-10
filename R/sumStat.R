#'@title calculate summary statistics for datasets
#'
#'@name sumStat
#'
#'@author Trevan Flynn
#'
#'@description
#'A simple function to calculate the min, mean, median, max, standard deviation,
#'quantiles (25% and 75% quantiles), skewness, kurtosis and CV.
#'
#'@param data The data to calculate the stats on.
#'@param columns A vector (numeric or string) of the columns to evaluate.
#'
#'@return A data frame with the stats in columns and properties in rows
#'@export

sumStat = function(data, columns){

  #calculate all
  results = data %>%
    tidyr::pivot_longer(., cols = columns, names_to = "variable", values_to = "value")%>%
    dplyr::group_by(variable)%>%
    dplyr::mutate(min = min(value))%>%
    dplyr::mutate(mean = mean(value))%>%
    dplyr::mutate(median = stats::median(value))%>%
    dplyr::mutate(max = max(value))%>%
    dplyr::mutate(sd = stats::sd(value))%>%
    dplyr::mutate(q25 = stats::quantile(value, 0.25))%>%
    dplyr::mutate(q75 = stats::quantile(value, 0.75))%>%
    dplyr::mutate(skewness = e1071::skewness(value))%>%
    dplyr::mutate(kurtosis = e1071::kurtosis(value))%>%
    dplyr::mutate(CV = stats::sd(value)/nrow(.)*100)%>%
    dplyr::distinct(variable, min,mean, median, max, sd, q25, q75, skewness, kurtosis, CV)

  return(results)
}
#END
