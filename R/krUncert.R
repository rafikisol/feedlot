#'@title Calculate the prediction limit range from regression kriging
#'
#'@name krUncert
#'
#'@author Trevan Flynn
#'
#'@description
#'Calculates the the prediction range limit as well as the lower and upper prediction
#'intervals using a specified confidence level.
#'
#'@param preds Is a stars object of the final predictions of the regression kriging model.
#'@param kriged Is a stars object of the kriged residuals
#'@param limit Is the confidence level to calculated the upper and lower limits from.
#'
#'@return A stars object containing the lower limits, prediction range, and upper limits.
#'@export

krUncert = function(preds, kriged, limit = 0.95){

  #standard error
  se = sqrt(kriged[2]) * stats::qnorm(limit)

  #upper
  upper = preds + se

  #lower
  lower = preds - se

  #range limit
  range = upper - lower

  #all together
  uncert = c(lower, range, upper)
  names(uncert) = c("Lower", "Range","Upper")

  return(merge(uncert))
}
