#'@title Show which models are available in regFit()
#'
#'@name what_mod
#'
#'@author Trevan Flynn
#'
#'@description
#'Shows which models are available for regFit(), the tidymodel (parsnip) name to look up
#'engines to use and the abbreviation to use in the regFit() function. For engines available, use show_engines() from tidymodels.
#'Note - Will be update constantly and cubist is still experimental.
#'@returns A data frame
#'@export

what_mod = function(){

  data.frame(model = c('linear regressions','decision trees','random forests',
                       'gradient tree boost', 'cubist', "svm linear",
                       'svm radial', 'svm polynomial'),
             parsnip = c("linear_reg", 'decision_tree', "rand_forest",
                         "boost_tree", "cubist_rules", "svm_linear",
                         "svm_rbf", "svm_poly"),
             symbol = c('"lm"', '"dt"', '"rf"', '"gtb"', '"cubist"', '"svmLin"',
                        '"svmRad"', '"svmPol"'))
}
#END
