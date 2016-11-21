#' Wrapper for running univariable logistic regression
#'
#' @description Wrapper for running univariable logistic regression
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
#' @param predictor Predictor variable(s) to test.
#' @param model Name/label of your model.
#'
#'
#' @export
dipep_glm <- function(df              = .data,
                      predictor       = 'age',
                      model           = 'age',
                      ...){
    results <- list()
    ## Build the formula
    .formula <- reformulate(response = 'pe',
                            termlabels = predictor)
    ## Filter the data frame
    results$df <- dplyr::filter(df, group == 'Suspected PE') %>%
                  dplyr::select_(.dots = c('pe', predictor)) %>%
                  mutate(obs = rownames(.),
                         use = complete.cases(.)) %>%
                  dplyr::filter(use == TRUE) %>%
                  dplyr::select(-use)
    results$df$obs <- rownames(results$df)
    ## Test the model
    results$fitted <- glm(.formula,
                          data   = results$df,
                          family = 'binomial')
    results$tidied    <- broom::tidy(results$fitted)
    results$augmented <- broom::augment(results$fitted)
    results$glance    <- broom::glance(results$fitted)
    ## Add the predictor to each to make it easier combining
    results$tidied$model    <- model
    results$augmented$model <- model
    results$glance$model    <- model
    ## Get predicted probabilities
    results$predicted <- results$fitted %>% predict() %>% as.data.frame()
    ## names(results$predicted) <- paste0(predictor, '.predicted')
    names(results$predicted) <- c('pred')
    results$predicted$obs <- rownames(results$predicted)
    results$predicted <- merge(dplyr::select(results$df, obs, pe),
                               results$predicted,
                               by = c('obs'))
    names(results$predicted) <- c('obs', paste0('pe.', model), paste0('pred.', model))
    ## Calculate : Sensitivity, Specificity, ppv and npv (uses ROCR functions)
    ## results$pred <- prediction(results$predicted$pred, results$predicted$pe)
    ## Standard ROC x = FPR; y = TPR
    ## results$roc.fpr.tpr     <- performance(results$pred, measure = 'tpr', x.measure = 'fpr')
    ## x = Specificity; y = Sensitivity
    ## results$roc.sens.spec   <- performance(results$pred, measure = 'sens', x.measure = 'spec')
    ##
    ## ToDo : Generat ROC plot internally for inclusion in an Appendix
    ## By Group
    return(results)
}
