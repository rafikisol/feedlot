#'@title Build and train a regression tidymodel
#'
#'@name regFit
#'
#'@author Trevan Flynn
#'
#'@description
#'This function builds a regression model from tidymodels and trains the model on data given.
#'To see which models are implemented use what_mods() function.
#'
#'@param data A data frame of the training dataset with predictors and response.
#'@param x Is a matrix/dataframe of the predictors (e.g., calib[, 1:8]).
#'@param y Is a vector of the response to be predicted.
#'@param method Is a character of which algorithm to use. see what_mod() to see which models are
#'available and character to enter.
#'@param engine Which package should be used to run the algorithm. See show_engines()
#'to get all engines for a certain algorithm (e.g., show_engines("linear_reg"))
#'@param ... Additional arguments to parameterize the algorithm.
#'
#'@return A tidymodels object.
#'@export

regFit = function(data, x, y, method = "rf", engine = "randomForest", ...){

  #linear regression
  if(method == "lm"){
    mod = parsnip::linear_reg(...)%>%
      parsnip::set_engine(engine)%>%
      parsnip::set_mode("regression")%>%
      parsnip::fit_xy(x = x, y = y)
  }

  #decision tree
  if(method == "dt"){
    mod = parsnip::decision_tree(...)%>%
      parsnip::set_engine(engine)%>%
      parsnip::set_mode("regression")%>%
      parsnip::fit_xy(x = x, y = y)
  }

  #random forest
  if(method == "rf"){
    mod = parsnip::rand_forest(...)%>%
      parsnip::set_engine(engine)%>%
      parsnip::set_mode("regression")%>%
      parsnip::fit_xy(x = x, y = y)
  }

  #gradient tree boost
  if(method == "gtb"){
    mod = parsnip::boost_tree(...)%>%
      parsnip::set_engine(engine)%>%
      parsnip::set_mode("regression")%>%
      parsnip::fit_xy(x = x, y = y)
  }

  #cubist
  if(method == "cubist"){
    mod = parsnip::cubist_rules()%>%
      parsnip::set_mode("regression")%>%
      rules::cubist_fit(x = x, y = y, ...)
  }

  #linear SVM
  if(method == "svmLin"){
    mod = parsnip::svm_linear(...)%>%
      parsnip::set_engine(engine)%>%
      parsnip::set_mode("regression")%>%
      parsnip::fit_xy(x = x, y = y)
  }

  #radial SVM
  if(method == "svmRad"){
    mod = parsnip::svm_rbf(...)%>%
      parsnip::set_engine(engine)%>%
      parsnip::set_mode("regression")%>%
      parsnip::fit_xy(x = x, y = y)
  }

  #polynomial SVM
  if(method == "svmPol"){
    mod = parsnip::svm_poly(...)%>%
      parsnip::set_engine(engine)%>%
      parsnip::set_mode("regression")%>%
      parsnip::fit_xy(x = x, y = y)
  }

  return(mod)
}
#END
