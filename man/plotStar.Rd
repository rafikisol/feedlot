% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotStar.R
\name{plotStar}
\alias{plotStar}
\title{Plot spatial predictions with publication quality}
\usage{
plotStar(
  image,
  continuous = T,
  scale = "quantile",
  fill = "Property",
  title = "Predictions",
  breaks = 5,
  ...
)
}
\arguments{
\item{image}{The single or multi-band stars object.}

\item{continuous}{Should the fill be continuous or binned.}

\item{fill}{Name of the property to label the colors.}

\item{title}{Title of the plot.}

\item{breaks}{How many breaks in distinct colors to use for visualization.}

\item{...}{Additional parameters to add to face_wrap (e.g., nrow, ncol).}
}
\value{
Returns an figure of the stars object with a scale bar and north arrow.Color is
set to color blind friendly for people like me.
}
\description{
Create a ggplot of spatial predictions from a stars object with publication quality.
It creates a plot of a continuous soil property, which can be binned for better visualisation.
If multi-band image is supplied, the lay out can be specified (e.g., nrow, ncol)
It is best to use multi-band images of the same soil property as the same legend is used.
}
\author{
Trevan Flynn
}
