% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regFit.R
\name{regFit}
\alias{regFit}
\title{Build and train a regression tidymodel}
\usage{
regFit(data, x, y, form, method = "rf", engine = "randomForest", ...)
}
\arguments{
\item{data}{A data frame of the training dataset with predictors and response.}

\item{x}{Is a matrix/dataframe of the predictors (e.g., calib[, 1:8]).}

\item{y}{Is a vector of the response to be predicted.}

\item{form}{A formula only used when method = gam. To fit smoothers to the covriates.If gam
is specified than x and y do not need to be.}

\item{method}{Is a character of which algorithm to use. see what_mod() to see which models are
available and character to enter.}

\item{engine}{Which package should be used to run the algorithm. See show_engines()
to get all engines for a certain algorithm (e.g., show_engines("linear_reg"))}

\item{...}{Additional arguments to parameterize the algorithm.}
}
\value{
A tidymodels object.
}
\description{
This function builds a regression model from tidymodels and trains the model on data given.
To see which models are implemented use what_mods() function.
}
\author{
Trevan Flynn
}
