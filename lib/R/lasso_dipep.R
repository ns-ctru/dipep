#' Read DiPEP Files
#'
#' @description Read in the raw CSV files exported from Prospect for the DiPEP Study
#'
#' @details
#' Case Report Forms are stored in the Prospect Database (https://www.ctru-prospect.shef.ac.uk/)
#' and are exported as CSV files (see \code{README} in this directory for export options).  These
#' need reading into R for linking and analysing (it would be nice if the relational database
#' that underpins Prospect could be queried directly but thats not going to happen in the
#' foreseeable future).
#'
#' When using this function the _first_ file that should be converted is 'Lookups.csv' as this
#' is the dictionary file that can be used to automatically encode all factor variables across
#' all subsequent files.
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
