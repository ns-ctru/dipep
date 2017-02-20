#' Tabulate and summarise existing Pulmonary Embolism Scores
#'
#' @description Tabulate and summarise (ROC, sensitivity, specificity) existing PE scores.
#'
#' @details
#'
#'
#' @param df Data frame.
#' @param score The score that is to be summarised, either \code{simplified.pe}, \code{wells.pe} or \code{perc.pe}
#' @param classification Specify the variable that defines the disease status, for this study there are four classifications of diseases ststus, hence the need for flexibility.
#' @param exclude List of individuals to explicitly exclude.
#'
#' @export
dipep_existing_sum <- function(df             = dipep,
                               classification = 'first.st'
                               score          = simplified.pe,
                               exclude        = NULL,
                               ...){
    results <- list()
    ## Remove individuals who are explicitly to be removed
    if(!is.null(exclude)){
        df <- df[!(df$screening %in% exclude),]
        ## df <- dplyr::filter_(df, ('screening' %in% !exclude))
    }
    ## Remove non-recruited and exclusions who couldn't be classified
    df <- dplyr::filter(df, group %in% c('Diagnosed PE', 'Suspected PE'))
        ## Exclude those who are not classified as PE/No PE by
    ## the specified classification
    ## TODO 2017-02-17 : Why doesn dplyr::filter_(df, !is.na(classification)) not work???
    if(classification == 'first.st'){
        df <- dplyr::filter(df, !is.na(first.st))
    }
    else if(classification == 'second.st'){
        df <- dplyr::filter(df, !is.na(second.st))
    }
    else if(classification == 'third.st'){
        df <- dplyr::filter(df, !is.na(third.st))
    }
    else if(classification == 'fourth.st'){
        df <- dplyr::filter(df, !is.na(fourth.st))
    }
    else if(classification == 'primary.dm'){
        df <- dplyr::filter(df, !is.na(primary.dm))
    }
    else if(classification == 'secondary.dm'){
        df <- dplyr::filter(df, !is.na(secondary.dm))
    }
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
