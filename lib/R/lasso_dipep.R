#' Run LASSO Regression on DiPEP
#'
#' @description Perform LASSO regression on DiPEP study
#'
#' @details
#'
#' Least Absolute Shrinkage and Selection Operator (LASSO) Regression is one of the methods of
#' testing for predictors of pulmonary embolism in pregnant women that are to be tested as
#' part of the DiPEP study.  This function serves as a wrapper, running the model and extracting
#' results and summaries.  This makes it simple to analyse multiple subsets of data (although
#' that approach itself isn't generally recommended).
#'
#' @param df Data frame with all variables
#' @param outcome Outcome variable.
#' @param predictors List of predictor variables to include in the model.
#' @param family Link function for Generalised Linear Model (\code{binomial | gaussian})
#' @param dfmax Limit the maximum number of variables in the model.
#' @param nfolds Number of 'folds' for cross-validation of \code{cv.glmnet} fitted models.  The default (\code{nrow(dipep)}) performs leave-one-out cross validation.
#' @param latex Produce LaTeX summary.
#' @param html Produce HTML summary.
#' @param ascii Produce ASCII summary.
#' @param caption Caption for table summary.
#' @param label Label for table summary.
#'
#' @return List containing the results and diagnostic plots of the LASSO fit.
#'
#' @export
lasso_dipep <- function(df          = dipep,
                        outcome     = 'group',
                        predictors  = c('age'),
                        lasso       = 'glmnet',
                        family      = 'binomial',
                        dfmax       = NULL,
                        nfolds      = nrow(dipep),
                        latex       = TRUE,
                        html        = FALSE,
                        ascii       = FALSE,
                        caption     = 'LASSO regression results.',
                        label       = 'lasso-regress',
                        ...){
    ## Initialise results list
    results <- list()
    ## Parse the equation without clustering
    if(is.na(cluster)){
        .formula <- reformulate(response   = outcome,
                                termlabels = predictors)
    }
    ## Select variables and create vector of outcome and matrix of predictors
    outcome    <- dplyr::select(df, outcome) %>%
                  as.vector()
    predictors <- dplyr::select(df, predictors) %>%
                  as.matrix()
    ## Fit the LASSO by either method
    ## See following sites for useful information/guidance on using these functions...
    ##
    ## https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#log
    ## http://rpackages.ianhowson.com/cran/broom/man/cv.glmnet_tidiers.html
    ## https://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
    ## 
    ## Fit and cross-validate the model
    results$cv.fit <- cv.glmnet(x      = predictors,
                                y      = outcome,
                                family = family,
                                dfmax  = dfmax,
                                nfolds = nfolds)
    ## Tidy and present results
    ## ToDo - Plot (using ggplot2) the LASSO fit
    ## ToDo - Predicted variables
    ## ToDo - Derive sensitivity, specificity, ppv and npv
    return(results)
}
