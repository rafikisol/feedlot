% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reduceLm.R
\name{reduceLm}
\alias{reduceLm}
\alias{reduceLM}
\title{Wrapper for the focalReg() function on stars objects}
\usage{
reduceLM(data, w, scale = FALSE, write = F, ...)
}
\arguments{
\item{w}{An integer of the window size to be used in pixels (e.g., w = 5, which = 25 pixels).}

\item{scale}{Logical, should the coefficients be normalized from -1 to +1. More suited to compare
between different images.}

\item{write}{Logical, should you right the file to disk. If true, see writeRaster() in terra package.}

\item{...}{Arguments passed to writeRaster}

\item{images}{A stars image with at least 2 bands. First band is taken as dependent
variable.}
}
\value{
A stars object with the first band being the intercept and the rest being
bands of the coefficients of the independent variable.
}
\description{
This function is to run a regression model on 1 dependent stars object and
as many independent stars objects as one would like. As of now, stars does not
have a suitable technique for focal calculations and so need to wrap around the terra package.
This function is a quick fix and hopefully additional improvements will be made so
do not have to wrap around the terra package.
}
\author{
Trevan Flynn
}
