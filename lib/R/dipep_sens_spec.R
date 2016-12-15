#' Sensitivity, Specificity, PPV, NPV ROC and AUC
#'
#' @description Calculate sensitivity, specificity, positive predictive value and
#'              negative predictive value.
#'
#' @details
#'
#' Given classifiers (rule based or single probabilites from logistic regression) it
#' is desirable to know the sensitivity and specificity, positive and negative predictive
#' value and the Area Under the Curve of the Receiver Operating Characteristic.
#'
#' Individuals who do not have classification by either of the variables should be excluded
#' prior to calling this function using \code{dplyr::filter()}.
#'
#'
#' @param df Data frame.
#' @param ... The two classified variables the first should be the new one, the second the true/existing classifier.
#'
#' @export
dipep_sens_spec <- function(df              = .data,
                            ...){
    results <- list()
    ## Get the variables being specified
    ## classifiers <- list(...)
    ## Extract the non-pe classifier to filter the data
    ## classifiers %>% print()
    ## Extract vectors and count groups
    results$table <- dplyr::select_(df, .dots =lazyeval::lazy_dots(...)) %>%
                     group_by_(.dots = lazyeval::lazy_dots(...)) %>%
                     tally()
    ## Calculate...
    ## True Positive  : tp
    ## True Negative  : tn
    ## False Positive : fp
    ## False Negative : fn
    results$tp <- dplyr::filter(results$table, wells.pe == 'PE' & pe == 'PE') %>%
                  ungroup() %>%
                  dplyr::select(n)
    results$tp <- results$tp[[1]]
    results$tn <- dplyr::filter(results$table, wells.pe == 'No PE' & pe == 'No PE') %>%
                  ungroup() %>%
                  dplyr::select(n)
    results$tn <- results$tn[[1]]
    results$fp <- dplyr::filter(results$table, wells.pe == 'PE' & pe == 'No PE') %>%
                  ungroup() %>%
                  dplyr::select(n)
    results$fp <- results$fp[[1]]
    results$fn <- dplyr::filter(results$table, wells.pe == 'No PE' & pe == 'PE') %>%
                  ungroup() %>%
                  dplyr::select(n)
    results$fn <- results$fn[[1]]
    ## Sensitivity (true positive rate)
    results$sensitivity <- results$tp / (results$tp + results$fn)
    ## Specificity (true negative rate)
    results$specificity <- results$tn / (results$tn + results$fp)
    ## Precision (positive predictive value)
    results$ppv <- results$tp / (results$tp + results$fp)
    ## Negative predictive value
    results$npv <- results$tn / (results$tn + results$fn)
    ## Combine into a data frame
    results$summary <- cbind(results$tp,
                             results$tn,
                             results$fp,
                             results$fn,
                             results$sensitivity,
                             results$specificity,
                             results$ppv,
                             results$npv) %>%
                       as.data.frame()
    names(results$summary) <- c('true_positive', 'true_negative',
                                'false_positive', 'false_negative',
                                'sensitivity', 'specificity',
                                'ppv', 'npv')
    ## Add in the indicator
    results$summary$predictor <- substitute(...) %>% deparse()
    ## By Group
    return(results)
}
