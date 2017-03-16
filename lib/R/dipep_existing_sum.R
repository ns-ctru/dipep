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
#' @param exclude.non.recuirted Logical indicator of whether to exclude \code{group == 'Non recruited'}.
#' @param exclude.dvt Logical indicator of whether to exclude \code{group == 'Diagnosed DVT'}.
#'
#' @export
dipep_existing_sum <- function(df      = dipep,
                               title   = 'Simplified Geneva',
                               exclude = NULL,
                               exclude.non.recruited = TRUE,
                               exclude.dvt       = TRUE,
                               ...){
    results <- list()
    ## Remove individuals who are explicitly to be removed
    if(!is.null(exclude)){
        df <- df[!(df$screening %in% exclude),]
        ## df <- dplyr::filter_(df, ('screening' %in% !exclude))
    }
    ## Remove non-recruited and DVT
    if(exclude.non.recruited == TRUE){
        df <- dplyr::filter(df, group != 'Non recruited')
    }
    if(exclude.dvt == TRUE){
        df <- dplyr::filter(df, group != 'Diagnosed DVT')
    }
    ## Subset the data for the two variables of interest, the user specified
    ## classification and the user specified score (as '...' arguments)
    df <- dplyr::select_(df, .dots = lazyeval::lazy_dots(...))
    vars <- names(df)
    ## Rename variables so they are standardised and I don't have to mess around
    ## with Standard Evaluation v's Non Standard Evaulation any more!
    names(df) <- gsub('first.st',              'class',    names(df))
    names(df) <- gsub('second.st',             'class',    names(df))
    names(df) <- gsub('third.st',              'class',    names(df))
    names(df) <- gsub('fourth.st',             'class',    names(df))
    names(df) <- gsub('primary.dm',            'class',    names(df))
    names(df) <- gsub('secondary.dm',          'class',    names(df))
    names(df) <- gsub('simplified.pe',         'class.existing', names(df))
    names(df) <- gsub('simplified',            'score',    names(df))
    names(df) <- gsub('wells.permissive.pe',   'class.existing', names(df))
    names(df) <- gsub('wells.permissive',      'score',    names(df))
    names(df) <- gsub('wells.strict.pe',       'class.existing', names(df))
    names(df) <- gsub('wells.strict',          'score',    names(df))
    names(df) <- gsub('perc.pe',               'class.existing', names(df))
    names(df) <- gsub('perc',                  'score',    names(df))
    names(df) <- gsub('delphi.primary.pe',     'class.existing', names(df))
    names(df) <- gsub('delphi.primary',        'score',    names(df))
    names(df) <- gsub('delphi.sensitivity.pe', 'class.existing', names(df))
    names(df) <- gsub('delphi.sensitivity',    'score',    names(df))
    names(df) <- gsub('delphi.specificity.pe', 'class.existing', names(df))
    names(df) <- gsub('delphi.specificity',    'score',    names(df))
    ## Replace NA classification with 'Exclude'
    df <- mutate(df,
                 class.char = ifelse(is.na(class),
                                     yes = 'Exclude',
                                     no  = as.character(class)))
    ## Bar chart of frequencies by classification
    ## if(title != 'PERC'){
        results$bar.chart <- ggplot(df, aes(x = score)) +
                             geom_bar(aes(fill = class), position = 'dodge') +
                             ggtitle(paste0(title, ' Scores by clinical classification')) +
                             xlab(paste0(title, ' Score')) + ylab('N') +
                             scale_fill_discrete(guide = guide_legend(title = 'Status')) +
                             theme_bw()
    ## }
    ## Likert-style chart
    ## Set variables the centered value for the plots
    if(levels(df$class.existing)[1] == 'No Simplified PE'){
        center = 5
    }
    else if(levels(df$class.existing)[1] == 'No Wells PE'){
        center = 4
    }
    else if(levels(df$class.existing)[1] == 'No PERC PE'){
        center = 2
    }
    else if(levels(df$class.existing)[1] == 'No Delphi Primary PE'){
        center = 3
    }
    else if(levels(df$class.existing)[1] == 'No Delphi Sensitivity PE'){
        center = 2
    }
    else if(levels(df$class.existing)[1] == 'No Delphi Specificity PE'){
        center = 4
    }
    plot.likert         <- dplyr::select(df, score)
    names(plot.likert)  <- paste0(title, ' Score')
    results$likert      <- likert(plot.likert, grouping = df$class.char)
    results$likert.plot <- plot(results$likert, center = center) +
                           labs(caption = paste0('Plots are centered on the Risk category (',
                                                 (center - 1),
                                                 ').\n Percentages indicate the proportion below, within and above this.'))
    results$histogram <- ggplot(df, aes(x = score, fill = class)) +
                         geom_histogram() +
                         facet_wrap(~class, ncol = 3) +
                         ggtitle(paste0(title, ' Scores by clinical classification')) +
                         xlab(paste0(title, ' Score')) + ylab('N') +
                         scale_fill_discrete(guide = guide_legend(title = 'Status')) +
                         theme_bw()
    ## Summarise the scores in tabular format
    all <- mutate(df, score = as.numeric(score)) %>%
           summarise(N        = sum(!is.na(score)),
                     Mean   = mean(score, na.rm = TRUE),
                     SD     = sd(score, na.rm = TRUE),
                     Lower  = quantile(score, probs = 0.25, na.rm = TRUE),
                     Median = quantile(score, probs = 0.5, na.rm = TRUE),
                     Upper  = quantile(score, probs = 0.75, na.rm = TRUE),
                     Min    = min(score, na.rm = TRUE),
                     Max    = max(score, na.rm = TRUE)) %>%
           mutate(class.char = 'All')
    group <- mutate(df, score = as.numeric(score)) %>%
            group_by(class.char) %>%
            summarise(N      = sum(!is.na(score)),
                      Mean   = mean(score, na.rm = TRUE),
                      SD     = sd(score, na.rm = TRUE),
                      Lower  = quantile(score, probs = 0.25, na.rm = TRUE),
                      Median = quantile(score, probs = 0.5, na.rm = TRUE),
                      Upper  = quantile(score, probs = 0.75, na.rm = TRUE),
                      Min    = min(score, na.rm = TRUE),
                      Max    = max(score, na.rm = TRUE))
    results$summary.table <- rbind(all, group) %>%
                             dplyr::select(class.char, N, Mean, SD, Lower, Median, Upper, Min, Max)
    names(results$summary.table) <- gsub('class\\.char', 'Status', names(results$summary.table))
    ## Predicitve assessment
    ## Tabulate first
    results$table <- table(df$class, df$class.existing)
    ## True Positives, False Positives, True Negatives and False Negatives
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
    results$fpr         <- results$false.positive / (results$true.negative + results$false.positive)
    results$fnr         <- results$false.negative / (results$true.positive + results$false.negative)
    results$fdr         <- results$false.positive / (results$true.positive + results$false.positive)
    results$accuracy <- (results$true.positive + results$true.negative) /
                        (results$true.positive + results$false.positive + results$true.negative + results$false.negative)
    ## Combine into a summary
    results$performance.table <- cbind(c('True Positive',
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
    names(results$performance.table) <- c('Performance Metric', 'Value')
    ## Perform simple logistic regression of score on specified outcome
    results$score <- glm(class ~ score,
                         data = df,
                         family = binomial)
    ## Make predictions and bind to observed classification
    predicted <- predict(results$score,
                         type = 'response')
    results$predicted <- cbind(dplyr::filter(df, !is.na(class) & !is.na(score)),
                                   predicted) %>%
                         dplyr::select(class, predicted)
    names(results$predicted) <- c('D', 'M')
    ## Add a term/name based on the arguments and get meaningful title
    if(grepl('first.st', vars)){
        case.review <- 'Primary'
    }
    else if(grepl('second.st', vars)){
        case.review <- 'Secondary'
    }
    if(grepl('perc', vars)){
        title <- 'PERC Score'
    }
    results$predicted$term <- title
    results$predicted$name <- title
    results$score.roc <- dipep_roc(df = results$predicted,
                                   to.plot = title,
                                   title = paste0(title, ' vs ', case.review, ' classification'))
    ## Add the AUC to the plot
    results$roc.plot <- results$score.roc$plot +
                        annotate('text', x = 0.75, y = 0.25,
                                 label = paste0('AUC = ', round(results$score.roc$auc$AUC, 3)))
    return(results)
}
