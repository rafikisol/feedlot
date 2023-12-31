% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evalReg.R
\name{evalReg}
\alias{evalReg}
\title{Predict a tidy model and return evaluation statistics}
\usage{
evalReg(val, property, model, limit = 0.95, plot = TRUE, ...)
}
\arguments{
\item{val}{Is a data frame of the independent evaluation set.}

\item{property}{Is a vector of the property predicted (e.g., val$MSA).}

\item{model}{Is a tidymodel object or results of the regFit() function.}

\item{limit}{Is the confidence level to use to compute the standard error.}

\item{plot}{Logical, should a plot of the regression line be made.
predicting things such as quantiles for uncertainty analysis.}

\item{...}{Is the metrics to use for evaluation statistics. See metric_set() in yardstick package.
should just right them out in the function (e.g., ccc, rmse, etc.) and not in a list or vector.}
}
\value{
A list with the regression line plot, evaluation statistics and raw results. The raw
data is a data frame of the observed, predicted, residuals, standard error, upper and lower limits.
}
\description{
This function predicts a tidymodel on an evaluation dataset and runs user defined
evaluation statistics.
}
\author{
Trevan Flynn
}
