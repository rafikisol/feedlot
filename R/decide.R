#'@title Decide which digital data and response
#'
#'@name decide
#'
#'@author Trevan Flynn
#'
#'@description
#'Choose which digital data to use as covariates and which soil property to predict.
#'Essentially, the function cleans the data so that it only contains the necessary
#'information to make a model from.
#'
#'@param covar A string of which covariates to use. Can be gamma ("y"), EM ("em"), lidar ("DEM")
#'or combinations of gamma and EM ("y+em"), gamma and lidar ("y+dem") and EM with lidar ("em+dem").
#'specifying "all", chooses all combinations gamma + EM + lidar.
#'@param response A character of the response to be predicted (e.g., "pH").
#'@param data The data to be used.
#'
#'@return A dataframe of the digital data and response chosen.
#'@export

decide = function(covar, response, data){

  #Just gamma
  if(covar == "y"){
    df = data[, c("G_TC", "K", "U", "Th", response)]
  }

  #Just EM
  if(covar == "em"){
    df = data[, c("Hcon_0.5m", "Pcon_0.5m", "Hcon_1m", "Pcon_1m", response)]
  }

  #Just DEM
  if(covar == "dem"){
    df = data[, c("DEM", "SWI",  "SlOPE", "ASPECT", "RUGGEDNESS", "HILLSHADE", response)]
  }

  #gamma + EM
  if(covar == "y+em"){
    df = data[, c("G_TC", "K", "U", "Th", "Hcon_0.5m", "Pcon_0.5m", "Hcon_1m", "Pcon_1m", response)]
  }

  #gamma + DEM
  if(covar == "y+dem"){
    df = data[, c("G_TC", "K", "U", "Th","DEM", "SWI",  "SlOPE", "ASPECT", "RUGGEDNESS", "HILLSHADE", response)]
  }

  #EM + DEM
  if(covar == "em+dem"){
    df = data[,c("Hcon_0.5m", "Pcon_0.5m", "Hcon_1m", "Pcon_1m", "DEM", "SWI",  "SlOPE", "ASPECT", "RUGGEDNESS", "HILLSHADE", response)]
  }

  #Gamma + EM + DEM
  if(covar == "all"){
    df = data[, c("G_TC", "K", "U", "Th", "Hcon_0.5m", "Pcon_0.5m", "Hcon_1m", "Pcon_1m", "DEM", "SWI",  "SlOPE", "ASPECT", "RUGGEDNESS", "HILLSHADE", response)]
  }

  #return clean data
  return(df)
}

#END
