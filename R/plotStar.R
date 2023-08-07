#'@title Plot spatial predictions with publication quality
#'
#'@name plotStar
#'
#'@author Trevan Flynn
#'
#'@description
#'Create a ggplot of spatial predictions from a stars object with publication quality.
#'It creates a plot of a continuous soil property, which can be binned for better visualisation.
#'If multi-band image is supplied, the lay out can be specified (e.g., nrow, ncol)
#'It is best to use multi-band images of the same soil property as the same legend is used.
#'
#'@param image The single or multi-band stars object.
#'@param continuous Should the fill be continuous or binned.
#'@param fill Name of the property to label the colors.
#'@param title Title of the plot.
#'@param breaks How many breaks in distinct colors to use for visualization.
#'@param ... Additional parameters to add to face_wrap (e.g., nrow, ncol).
#'
#'@return Returns an figure of the stars object with a scale bar and north arrow.Color is
#'set to color blind friendly for people like me.
#'@export

plotStar = function(image, continuous = T, fill = "Property", title = "Predictions", breaks = 5, ...){

  #get boundary of farm
  bounds = sf::st_union(sf::st_as_sf(image[1], merge = T))

  #must reproject if plot without sf = T
  image = stars::st_warp(image, crs = 4326)

  #make plot (want to save for later)
  if(length(dim(image)) <= 2){

    p = ggplot2::ggplot()+
      stars::geom_stars(data = image)+
      ggplot2::geom_sf(data = bounds, fill = NA, color = 'black', lwd = 1)+
      ggspatial::annotation_scale(location = "br")+
      ggspatial::annotation_north_arrow(location = "tl")+
      ggplot2::labs(x = "Lontitude", y = "Latitude", fill = fill, title = title)+
      ggplot2::coord_sf(crs = 4326)+
      ggplot2::theme_bw()

    #binned
    if(continuous == FALSE){
      p = p +
        ggplot2::scale_fill_viridis_b(option = 'turbo', na.value = NA,
                                      n.breaks = breaks)
    }

    #continuous
    if(continuous == TRUE){
      p = p +
        ggplot2::scale_fill_viridis_c(option = 'turbo', na.value = NA,
                                      n.breaks = breaks)
    }

  }

  #for mult-band images
  if(length(dim(image)) > 2){

    #plot
    p = ggplot2::ggplot()+
      stars::geom_stars(data = image)+
      ggplot2::geom_sf(data = bounds, fill = NA, color = 'black', lwd = 1, inherit.aes = F)+
      ggspatial::annotation_scale(location = "br")+
      ggspatial::annotation_north_arrow(location = "tl")+
      ggplot2::labs(x = "Lontitude", y = "Latitude", fill = fill, title = title)+
      ggplot2::coord_sf(crs = 4326)+
      ggplot2::facet_wrap(~ attributes, ...)+
      ggplot2::theme_bw()

    #binned
    if(continuous == FALSE){
      p = p +
        ggplot2::scale_fill_viridis_b(option = 'turbo', na.value = NA,
                                      n.breaks = breaks)
    }

    #continous
    if(continuous == TRUE){
      p = p +
        ggplot2::scale_fill_viridis_c(option = 'turbo', na.value = NA,
                                      n.breaks = breaks)
    }
  }
  return(p)
}
