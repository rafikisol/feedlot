#'@title Calculate the prediction limit range through bootstrapping
#'
#'@name bootUncert
#'
#'@author Trevan Flynn
#'
#'@description
#'Calculates the lower limits, prediction limit range and upper limits from
#'a stars object of many realisations or from the bootStars() function. This
#'is a parametric approach and bootstrapping does not automatically make the
#'data normal, therefore, check to see that it is to get any reliable results.
#'Additionally, the function uses the average of all bootstraps and for many
#'algorithms, this is not the prediction map and could actually turn it into
#'a different algorithm (e.g., decision trees into a random forest like model).
#'Hence, if predictions = T, then it will return the average as well which can
#'be used as an enemble of models.
#'
#'@param bootStars A stars object with each band being a realisation.
#'@param limits A confidence level to base the prediction intervals on.
#'
#'@return A stars object with bands of the lower, range and upper limits.
#'@export

bootUncert = function(bootStars, limit){

  #get mean
  ave = st_apply(bootStars, 1:2, mean)

  #get se
  sdMap = st_apply(bootStars, 1:2, sd)

  #calculate standard error based on confidence interval (limit)
  se = sdMap * qnorm(limit)

  #get upper
  upper = ave + se

  #get lower
  lower = ave - se

  #get range
  rang = upper - lower

  #if predictions
  if(predictions == TRUE){
    r = c(ave, lower, rang, upper)
    names(r) = c("predictions", "lower", "range", "upper")
    return(merge(r))
  }

  #combine stars objects
  uncert = c(lower, rang, upper)

  #set names
  names(uncert) = c("lower", "range", "upper")

  return(merge(uncert))
}
