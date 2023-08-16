#'@title Bootstrap predictions for some tidymodels
#'
#'@name bootStars
#'
#'@author Trevan Flynn
#'
#'@description
#'Performs bootstrap spatial predictions for tidymodels on covariates (stars,
#'terra or raster images). It should be noted this is to create realisations of
#'the same model, not to tune the model. This is good for many statistical analyses
#'and for things like tree ensemble. For tuning, use the internal functions in
#'tidymodels.
#'
#'@param form If gam is specified in method, use formula so can use smoothing.
#'@param x Columns of the dataframe which are used as predictors.
#'@param y Column of the response to be predicted.
#'@param data Data with all information for training in it.
#'@param covar A stars object of the covariates.
#'@param method Which algorithm to use, see what_mod().
#'@param engine Which package to use for the algorithm, see show_engines().
#'@param boots How many bootstraps to perform.
#'
#'@returns A stars object with each band being a realisation from the number of
#' bootstraps.
#' @export

bootStars = function(form, x, y, data, covar, method, engine, boots, ...){

  #Make a list to put maps into
  maps = list()

  for(i in 1:boots){
    #sample same number of points w/ replacement
    rem = sample.int(nrow(data), 1*nrow(data), replace = T)

    #train model for each bootstrap
    #linear regression
    if(method == "lm"){
      mod = parsnip::linear_reg(...)%>%
        parsnip::set_engine(engine)%>%
        parsnip::set_mode("regression")%>%
        parsnip::fit_xy(x = x[rem, ], y = y[rem, ])
    }

    #decision tree
    if(method == "dt"){
      mod = parsnip::decision_tree(...)%>%
        parsnip::set_engine(engine)%>%
        parsnip::set_mode("regression")%>%
        parsnip::fit_xy(x = x[rem, ], y = y[rem, ])
    }

    #random forest
    if(method == "rf"){
      mod = parsnip::rand_forest(...)%>%
        parsnip::set_engine(engine)%>%
        parsnip::set_mode("regression")%>%
        parsnip::fit_xy(x = x[rem, ], y = y[rem, ])
    }

    #gradient tree boost
    if(method == "gtb"){
      mod = parsnip::boost_tree(...)%>%
        parsnip::set_engine(engine)%>%
        parsnip::set_mode("regression")%>%
        parsnip::fit_xy(x = x[rem, ], y = y[rem, ])
    }

    #cubist
    if(method == "cubist"){
      mod = parsnip::cubist_rules()%>%
        parsnip::set_mode("regression")%>%
        rules::cubist_fit(x = x[rem, ], y = y[rem, ], ...)
    }

    #GAM
    if(method == "gam"){
      mod = parsnip::gen_additive_mod()%>%
        parsnip::set_engine("mgcv")%>%
        parsnip::set_mode("regression")%>%
        parsnip::fit(form, data = data[rem, ])

    }

    #linear SVM
    if(method == "svmLin"){
      mod = parsnip::svm_linear(...)%>%
        parsnip::set_engine(engine)%>%
        parsnip::set_mode("regression")%>%
        parsnip::fit_xy(x = x[rem, ], y = y[rem, ])
    }

    #radial SVM
    if(method == "svmRad"){
      mod = parsnip::svm_rbf(...)%>%
        parsnip::set_engine(engine)%>%
        parsnip::set_mode("regression")%>%
        parsnip::fit_xy(x = x[rem, ], y = y[rem, ])
    }

    #polynomial SVM
    if(method == "svmPol"){
      mod = parsnip::svm_poly(...)%>%
        parsnip::set_engine(engine)%>%
        parsnip::set_mode("regression")%>%
        parsnip::fit_xy(x = x[rem, ], y = y[rem, ])
    }

    #predict
    maps[[i]] = predict.model_fit(split(covar), mod)
  }

  #combine into 1 stars object
  m = do.call("c", maps) %>% merge()

  return(m)
}
