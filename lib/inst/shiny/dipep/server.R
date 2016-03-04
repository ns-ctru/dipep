# Load libraries
library(dplyr)
library(ggplot2)
library(glmnet)
library(lars)
library(magrittr)
library(rpart)
library(shiny)
library(RColorBrewer)
# Define Server Output
shinyServer(function(input, output){
    ############################################################################
    ## Clinical decision rules                                                ##
    ############################################################################
    ## Table - PERC  (Kline et al. 2004)
    ## Table - Wells (Wells et al. 2001)
    ## Table - Geneva (Wicki et al. 2001)
    ## Table - Shortened Geneva (Klok et al. 2008)
    ## Table - Delphi Consensus
    ############################################################################
    ## Recurrsive Partitioning                                                ##
    ############################################################################
    ## Model
    ## Plot
    ## See https://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html
    ## See https://github.com/thomasp85/ggraph
    ## Table
    
    ############################################################################
    ## Regression - LASSO                                                     ##
    ############################################################################
    ## Model
    data(diabetes)
    attach(diabetes)
    ## x is a matrix of predictor variables
    ## y is the outcome variable
    object <- lars(x,y, type = 'lasso')
    ## Plot 
    output$lasso.plot <- renderPlot(
        ## See this example for plotting coefficients using ggplot2
        ## http://directlabels.r-forge.r-project.org/docs/lineplot/plots/lars.html
        plot(object)  
    )
    ## Table
    output$lasso.coef <- renderTable(
        coef(object)  %>% xtable()
    )
    ############################################################################
    ## Regression - Saturated                                                 ##
    ############################################################################ 
    ## Model
    ## Plot?
    ## Table
    
})
