#'@title Calculate the prediction interval coverage probability (PICP) of a model
#'
#'@name picpCalc
#'
#'@author Trevan Flynn
#'
#'@description
#'This function calculates the PICP for a series of confidence intervals from the
#'predictions of a model. The PICP is a type of sensitivity analysis for model
#'uncertainties, which indicates how well the formulated confidence intervals were
#'determined. In other words, do the predictions fall within the confidence intervals.
#'Ideally, the PICP should fall on a 1:1 line with confidence intervals indicating that
#'all predictions fall within the decided limits.
#'
#'@param data Is a data frame containing the measured soil property and the predicted soil property.
#'@param response A vector of the measured soil data (e.g., val$MSA).
#'@param pred A vector of the measured soil data (e.g., val$.pred)
#'
#'@return A list with the first element being a plot of PICP to confidence intervals
#'with Lin's concordance correlation coefficient and the second being the results of the
#'PICP series and confidence levels.
#'
#'@references Brendan P. Malone , Budiman Minasny , Alex B. McBratney (2017). "Using R for Digital Soil Mapping", Springer
#'@export

picpCalc = function(data, response, pred){

  #get residuals
  res = response - pred

  #get standard deviation
  data$stdev = stats::sd(res)

  #confidence interval series
  qp<-stats::qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525))

  #z values
  vMat<-matrix(NA,nrow=nrow(data),ncol =length(qp))

  for(i in 1:length(qp)){
    vMat[, i]<-data$stdev*qp[i]
  }

  #upper limit
  uMat<-matrix(NA, nrow=nrow(data),ncol=length(qp))

  for(i in 1:length(qp)) {
    uMat[, i]<- pred + vMat[, i]
  }

  #lower limit
  lMat<-matrix(NA, nrow=nrow(data),ncol=length(qp))

  for(i in 1:length(qp)) {
    lMat[, i]<- pred - vMat[, i]
  }

  #evaluate for each confidence interval (1s and 0s)
  bMat<-matrix(NA,nrow=nrow(data),ncol=length(qp))

  for(i in 1:ncol(bMat)){
    bMat[, i]<-as.numeric(response <= uMat[, i] &
                            response >= lMat[, i])
  }

  #calculate PICP for series (larger the better)
  picp = colSums(bMat)/nrow(bMat)*100

  #confidence intervals
  cs<-c(99,97.5,95,90,80,60,40,20,10,5)

  #put into data frame
  results = data.frame(picp = picp, cs = cs)

  #Get CCC of 1:1 line
  ccc = as.data.frame(yardstick::ccc_vec(results$picp, results$cs))
  names(ccc) = "CCC" #name
  ccc$x = 10 #x axis
  ccc$y = 90 #y axis

  #plot the line
  p = ggplot2::ggplot(data = results, ggplot2::aes(x= results$cs, y = results$picp))+
    ggplot2::geom_point()+
    ggplot2::geom_text(data = ccc, ggplot2::aes(x = ccc$x, y = ccc$y, label = paste("CCC = ",round(CCC, 2))))+
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = 'red')+
    ggplot2::labs(x = 'Confidence level', y = "PICP", title = "PICP to confidence level")+
    tune::coord_obs_pred()+
    ggplot2::theme_bw()

  #return plot and results
  return(stats::setNames(list(p, results), c("Plot", "Results")))
}

#END
