#' Wrapper for running glmnet() on Dipep
#'
#' @description Wrapper for running glmnet() on Dipep
#'
#' @details
#'
#' This wrapper runs logistic regression models for a given predictor variable
#' and returns the fitted model (for combining using broom()) along with the
#' predicted values (for calculation of sensitivity and specificity) as well
#' as other summary statistics
#'
#'
#' @param df Data frame to analyse (default is \code{dipep} and shouldn't need changing)
#' @param classification Specify the variable that defines the disease status, for this study there are four classifications of diseases ststus, hence the need for flexibility.
#' @param predictor Predictor variable(s) to test.
#' @param model Name/label of your model.
#' @param relevel Reference level for logistic regression if different from default.
#'
#'
#' @export
dipep_glmnet <- function(df           = dipep,
                         classification  = 'first.st',
                         predictor       = 'age',
                         model           = NULL,
                         relevel         = NULL,
                         exclude         = NULL,
                         ...){
    results <- list()
    ## Remove individuals who are explicitly to be removed
    if(!is.null(exclude)){
        df <- df[!(df$screening %in% exclude),]
    }
    ## Remove non-recruited
    df <- dplyr::filter(df, group %in% c('Diagnosed PE', 'Suspected PE'))
    ## Exclude those who are not classified as PE/No PE by
    ## the specified classification
    ## TODO 2017-02-17 : Why doesn dplyr::filter_(df, !is.na(classification)) not work???
    if(classification == 'first.st'){
        df <- dplyr::filter(df, !is.na(first.st))
        y  <- ifelse(df$first.st == 'PE', 1, 0)
    }
    else if(classification == 'second.st'){
        df <- dplyr::filter(df, !is.na(second.st))
        y  <- ifelse(df$second.st == 'PE', 1, 0)
    }
    else if(classification == 'third.st'){
        df <- dplyr::filter(df, !is.na(third.st))
        y  <- ifelse(df$third.st == 'PE', 1, 0)
    }
    else if(classification == 'fourth.st'){
        df <- dplyr::filter(df, !is.na(fourth.st))
        y  <- ifelse(df$fourth.st == 'PE', 1, 0)
    }
    else if(classification == 'primary.dm'){
        df <- dplyr::filter(df, !is.na(primary.dm))
        y  <- ifelse(df$primary.dm == 'PE', 1, 0)
    }
    else if(classification == 'secondary.dm'){
        df <- dplyr::filter(df, !is.na(secondary.dm))
        y  <- ifelse(df$secondary.dm == 'PE', 1, 0)
    }
    # Matrix of predictors
    x <- subset(df, select = predictor) %>% data.matrix()
    ## Build the formula
    .formula <- reformulate(response = classification,
                            termlabels = predictor)
    ## Run analyses for a range of parameters of alpha (the trade off-between)
    ## LASSO and Ridge Regression.  The regularisation parameter, lambda
    ## is by default run for a range of values
    interim <- list()
    ## for(alpha = seq(from = 0, to = 1, by = 0.05)){
        alpha <- 0
        lasso <- glmnet(y = y,
                        x = x,
                        family = 'binomial',
                        alpha = alpha)
        tidy_lasso   <- tidy(lasso)
        glance_lasso <- glance(lasso)
        cv_lasso <- cv.glmnet(y = y,
                              x = x,
                              family = 'binomial',
                              alpha = alpha)
        tidy_cv_lasso <- tidy(cv_lasso)
        glance_cv_lasso <- glance(cv_lasso)
        ## TODO - Obtain AUC for each model
    ## }
    ## TODO -
}
