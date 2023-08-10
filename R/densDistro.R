#'@title Plot summary statistics for training and test sets
#'
#'@name densDistro
#'
#'@author Trevan Flynn
#'
#'@description
#'A function to make a density plot of training and testing datasets. Good for visually
#'depicting the distribution of covariates or the response. 
#'
#'@param training The cleaned data from the decide() function of the training set.
#'@param testing The cleaned data from the decide() function of the training set.
#'@param geom The type of plot either "density" (kernel density distribution) or "boxplot".
#'@param columns The columns in the dataframe to make into long format.
#'@param ... Additional arguments to be passed to facet_wrap() such as number of rows or columns (e.g., nrow, ncol)
#'
#'@return a ggplot density plot wrapped around each variable with colors representing the training and test for each.
#'
#'@note
#'If order in which plots appear for the variables is needed this must be done before running the function
#'for example df$type = factor(df$type, levels = c("gamma data", "EM data")) otherwise will be in alphabetic order.
#'@export

densDistro = function(training, testing, type = "density", columns, ...){
  
    #add a column for testing and training
    tr = training %>% mutate(type = "Training")
    ts = testing %>% mutate(type = "Evaluation")
    
    #combine the two so as to make one plot for both
    df = rbind(tr, ts)
    
    #pivot all variables wanted to a longer format but keep type the same.
    df = df %>%
      pivot_longer(cols = all_of(columns), names_to = "properties", 
                 values_to = "values")
    
    #Make training appear first
    df$type = factor(df$type, c("Training", "Evaluation"))
    
    #make plot
    if(type == "density"){
      p = ggplot2::ggplot(data = df, ggplot2::aes(y = values, fill = type), 
                        color = 'black')+
        ggplot2::geom_density(position = "stack")+
        ggplot2::scale_fill_manual(values = c("yellow", "blue"))+
        ggplot2::coord_flip()+
        ggplot2::facet_wrap("properties", scale = 'free', ...)+
        ggplot2::labs(x = "Property values", y = "Kernel density distribution", fill = "Dataset")+
        ggplot2::theme_bw()+ ggplot2::theme(strip.background  = ggplot2::element_blank(), 
                     strip.text = ggplot2::element_text(hjust = 0, size = 10,face='bold'),
                     axis.title = ggplot2::element_text(size = 10, face="bold"),
                     axis.text = ggplot2::element_text(color = 'black'), panel.background = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
    }
    
    if(type == "boxplot"){
      p = ggplot2::ggplot(data = df, ggplot2::aes(y = values, fill = type), 
                          color = 'black')+
        ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 5), aes(x = values, y = type), show.legend = F)+
        ggplot2::scale_fill_manual(values = c("yellow", "blue"))+
        ggplot2::coord_flip()+
        ggplot2::facet_wrap("properties", scale = 'free', ...)+
        ggplot2::labs(x = "Property values", y = "")+
        ggplot2::theme_bw()+ggplot2::theme(strip.background  = ggplot2::element_blank(), 
                                  strip.text = ggplot2::element_text(hjust = 0, size = 10,face='bold'),
                                  axis.title = ggplot2::element_text(size = 10, face="bold"),
                                  axis.text = ggplot2::element_text(color = 'black'), panel.background = ggplot2::element_blank(),
                                  panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
    }
    return(p)
}
#END

