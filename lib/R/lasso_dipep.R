#' Run LASSO Regression on DiPEP
#'
#' @description Perform LASSO regression on DiPEP study
#'
#' @details
#'
#' Least Absolute Shrinkage and Selection Operator (LASSO) Regression is one of the methods of
#' testing for predictors of pulmonary embolism in pregnant women that are to be tested as
#' part of the DiPEP study.  This function serves as a wrapper, allowing the user to choose which
#' variables to include in the model, between the LASSO method implemented in the \code{lars}
#' package or that in the \code{glmnet} package.
#'
#' @param df Data frame with all variables
#' @param outcome Outcome variable.
#' @param predictors List of predictor variables to include in the model.
#' @param lasso Which LASSO package to use (\code{lars | glmnet}).
#' @param family Link function for Generalised Linear Model (\code{binomial | gaussian})
#' @param dfmax Limit the maximum number of variables in the model (only used by \code{glmnet}).
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
    if(lasso == 'glmnet'){
        model <- glmnet(x      = predictors,
                        y      = outcome,
                        family = family,
                        dfmax  = dfmax)
    }
    else if(lasso == 'lars'){
        model <- lars(x      = predictors,
                      y      = outcome,
                      type   = 'lasso',
                      trace  = FALSE)
    }
    return(results)
}
