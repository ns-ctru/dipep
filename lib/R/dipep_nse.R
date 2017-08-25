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
dipep_nse <- function(df             = dipep,
                      title          = 'Simplified Geneva',
                      exclude        = NULL,
                      ...){
    results <- list()
    ## Remove individuals who are explicitly to be removed
    if(!is.null(exclude)){
        df <- df[!(df$screening %in% exclude),]
        ## df <- dplyr::filter_(df, ('screening' %in% !exclude))
    }
    ## Remove non-recruited and DVT
    df <- dplyr::filter(df, group %in% c('Diagnosed PE', 'Suspected PE'))
    ## Subset the data for the two variables of interest, the user specified
    ## classification and the user specified score (as '...' arguments)
    df <- dplyr::select_(df, .dots = lazyeval::lazy_dots(...))
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
    names(df) <- gsub('wells.pe',              'class.existing', names(df))
    names(df) <- gsub('wells',                 'score',    names(df))
    names(df) <- gsub('perc.pe',               'class.existing', names(df))
    names(df) <- gsub('perc',                  'score',    names(df))
    names(df) <- gsub('delphi.primary.pe',     'class.existing', names(df))
    names(df) <- gsub('delphi.primary',        'score',    names(df))
    names(df) <- gsub('delphi.sensitivity.pe', 'class.existing', names(df))
    names(df) <- gsub('delphi.sensitivity',    'score',    names(df))
    names(df) <- gsub('delphi.specificity.pe', 'class.existing', names(df))
    names(df) <- gsub('delphi.specificity',    'score',    names(df))
    ## Replace NA classification with 'Exclude'
    table(df$class, useNA = 'ifany') %>% print()
    df <- mutate(df,
                 class.char = ifelse(is.na(class),
                                     yes = 'Exclude',
                                     no  = as.character(class)))
    table(df$class.char, useNA = 'ifany') %>% print()
    ## Bar chart of frequencies by classification
    results$bar.chart <- ggplot(df, aes(x = score)) +
                         geom_bar(aes(fill = class), position = 'dodge') +
                         ggtitle(paste0(title, ' Scores by clinical classification')) +
                         xlab(paste0(title, ' Score')) + ylab('N') +
                         scale_fill_discrete(guide = guide_legend(title = 'Status')) +
                         theme_bw()
    ## Likert-style chart
    ## Set variables the centered value for the plots
    levels(df$class.existing) %>% print()
    if(levels(df$class.existing)[1] == 'No Simplified PE'){
        center = 4
    }
    else if(levels(df$class.existing)[1] == 'No Wells PE'){
        center = 4
    }
    else if(levels(df$class.existing)[1] == 'No PERC PE'){
        center = 3
    }
    else if(levels(df$class.existing)[2] == 'No Delphi (Primary) PE'){
        center = 2
    }
    else if(levels(df$class.existing)[2] == 'No Delphi (Sensitivity) PE'){
        center = 1
    }
    else if(levels(df$class.existing)[2] == 'No Delphi (Specificity) PE'){
        center = 3
    }
    plot.likert <- dplyr::select(df, score)
    names(plot.likert) <- paste0(title, ' Score')
    results$likert <- likert(plot.likert, grouping = df$class.char)
    results$likert.plot <- plot(results$likert, center = center) +
                           labs(caption = paste0('Plots are centered on the Risk category (',
                                                 center,
                                                 ').\n Percentages indicate the proportion below, within and above this.'))
    return(results)
}
