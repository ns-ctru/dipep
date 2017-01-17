#' Tabulate and summarise existing Pulmonary Embolism Scores
#'
#' @description Tabulate and summarise (ROC, sensitivity, specificity) existing PE scores.
#'
#' @details
#'
#'
#' @param df Data frame.
#' @param score The score that is to be summarised, either \code{simplified.pe}, \code{wells.pe} or \code{perc.pe}
#'
#' @export
dipep_existing_sum <- function(df          = dipep,
                               score       = simplified.pe,
                               ...){
    results <- list()
    ## Slim down the data frame only want diagnosed and suspected
    df <- dplyr::select_(df, lazyeval::lazy(pe), lazyeval::lazy(score))
    ## Evaluate the score
    .score <- substitute(score) %>% deparse()
    ## Generate table
    ## print('Debug 1 : Tabulation')
    results$table <- table(df[['pe']], df[[.score]])
    ## Calculate True Positives, False Positives, True Negatives and False Negatives
    ## print('Debug 2 : True/False Positive/Negative')
    results$true.positive  <- results$table[2,2]
    results$false.positive <- results$table[1,2]
    results$true.negative  <- results$table[1,1]
    results$false.negative <- results$table[2,1]
    ## Calculate the Sensitivity (true positive rate; tpr), specificity (true negative
    ## rate; tnr), Positive Predictive Value (PPV) and Negative Predictive Value (NPV)
    ## print('Debug 3 : Sensitivity/Specificity/PPV/NPV')
    results$sensitivity <- results$true.positive / (results$true.positive + results$false.negative)
    results$specificity <- results$true.negative / (results$true.negative + results$false.positive)
    results$ppv         <- results$true.positive / (results$true.positive + results$false.positive)
    results$npv         <- results$true.negative / (results$true.negative + results$false.negative)
    results$fpr <- results$false.positive / (results$true.negative + results$false.positive)
    results$fnr <- results$false.negative / (results$true.positive + results$false.negative)
    results$fdr <- results$false.positive / (results$true.positive + results$false.positive)
    results$accuracy <- (results$true.positive + results$true.negative) /
                        (results$true.positive + results$false.positive + results$true.negative + results$false.negative)
    ## Combine into a summary
    results$summary.table <- cbind(c('True Positive',
                                     'False Positive',
                                     'True Negative',
                                     'False Negative',
                                     'Sensitivity',
                                     'Specificity',
                                     'Positive Predictive Value',
                                     'Negative Predictive Value',
                                     'False Positive Rate',
                                     'False Negative Rate',
                                     'False Discovery Rate',
                                     'Accuracy'),
                                   c(results$true.positive,
                                     results$false.positive,
                                     results$true.negative,
                                     results$false.negative,
                                     results$sensitivity,
                                     results$specificity,
                                     results$ppv,
                                     results$npv,
                                     results$fpr,
                                     results$fnr,
                                     results$fdr,
                                     results$accuracy)) %>% as.data.frame()
    names(results$summary.table) <- c('Performance Metric', 'Value')
    return(results)
}
