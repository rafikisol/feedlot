#'@title Wrapper for the focalReg() function on stars objects
#'
#'@name reduceLm
#'
#'@author Trevan Flynn
#'
#'@description
#'This function is to run a regression model on 1 dependent stars object and
#'as many independent stars objects as one would like. As of now, stars does not
#'have a suitable technique for focal calculations and so need to wrap around the terra package.
#'This function is a quick fix and hopefully additional improvements will be made so
#'do not have to wrap around the terra package.
#'
#'@param images A stars image with at least 2 bands. First band is taken as dependent
#'variable.
#'@param w An integer of the window size to be used in pixels (e.g., w = 5, which = 25 pixels).
#'@param scale Logical, should the coefficients be normalized from -1 to +1. More suited to compare
#'between different images.
#'@param write Logical, should you right the file to disk. If true, see writeRaster() in terra package.
#'@param ... Arguments passed to writeRaster
#'
#'@return A stars object with the first band being the intercept and the rest being
#'bands of the coefficients of the independent variable.
#'@export

reduceLM = function(data, w, scale = FALSE, write = F, ...){

  #if you want to write a to file
  if(write == TRUE){

    #run focal regression
    r = terra::focalReg(rast(data), w, ...)

    #run scale
    if(scale == TRUE){
      r = (r - min(r))/(max(r) - min(r))
    }

    #name correctly
    names(r) = c("Intercept", "Slope")

    #back to stars
    st = st_as_stars(r)
  }

  #if to memory only
  else{

    #run focal
    r = terra::focalReg(rast(data), w)

    #normalize
    if(scale == TRUE){
      r = (r - min(r))/(max(r) - min(r))
    }

    #name
    names(r) = c("Intercept", "Slope")

    #back to stars
    st = st_as_stars(r)
  }

  return(st)
}
#END
