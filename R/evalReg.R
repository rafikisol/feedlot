#'@title Predict a tidy model and return evaluation statistics
#'
#'@name evalReg
#'
#'@author Trevan Flynn
#'
#'@description
#'This function predicts a tidymodel on an evaluation dataset and runs user defined
#'evaluation statistics.
#'
#'@param val Is a data frame of the independent evaluation set.
#'@param property Is a vector of the property predicted (e.g., val$MSA).
#'@param model Is a tidymodel object or results of the regFit() function.
#'@param limit Is the confidence level to use to compute the standard error.
#'@param ... Is the metrics to use for evaluation statistics. See metric_set() in yardstick package.
#'should just right them out in the function (e.g., ccc, rmse, etc.) and not in a list or vector.
#'@param plot Logical, should a plot of the regression line be made.
#'predicting things such as quantiles for uncertainty analysis.
#'
#'@return A list with the regression line plot, evaluation statistics and raw results.
#'@export

evalReg = function(val, property, model, limit = 0.95, plot = TRUE, ...){

  #set metrics to use for evaluation
  mets = yardstick::metric_set(...)

  #predict over validation set
  pred = parsnip::predict.model_fit(model, val)

  #aggregate results
  df = data.frame(property, pred)
  df = stats::setNames(df, c("obvs", "pred"))

  #get residuals
  df$res = df$obvs - df$pred

  #standard error
  df$se = stats::sd(df$res) * stats::qnorm(limit)

  #upper limit
  df$upper = df$pred + df$se

  #lower limit
  df$lower = df$pred - df$se

  #get evaluation statistics
  results = df %>% mets(obvs, pred)

  #plot if plot = TRUE
  if(plot == TRUE){

    p = ggplot2::ggplot(data = df, ggplot2::aes(x = pred, y = obvs))+
      ggplot2::geom_point()+
      ggplot2::stat_smooth(method = "lm", formula = y ~ x, se = F, color = 'blue')+
      ggplot2::geom_abline(slope = 1, intercept = 0 + df$se, color = "red", linetype = 'solid')+
      ggplot2::geom_abline(slope = 1, intercept = 0 - df$se, color = "red", linetype = 'solid')+
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = 'dashed')+
      ggplot2::labs(x = "Predictions", y = "Observations")+
      tune::coord_obs_pred()+
      ggplot2::theme_bw()

    return(stats::setNames(list(p, results, df),c("Plot", "Stats", "Raw_results")))
  }

  else{

    return(stats::setNames(list(results, df),c("Stats", "Raw_results")))
  }
}
