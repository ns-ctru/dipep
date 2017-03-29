#' Plot Continuous Data
#'
#' @description Plot Continuous data for DiPEP
#'
#' @details
#'
#' Generateas a histogram and box/jittered scatter plot of biomarker assays by the specified
#' classification.
#'
#' @param df Data frame of predicted probabilities for multiple outcomes (in long format).
#' @param exclude Vector of \code{screening}  to exclude.
#' @param exclude.non.recuirted Logical indicator of whether to exclude \code{group == 'Non recruited'}.
#' @param exclude.dvt Logical indicator of whether to exclude \code{group == 'Diagnosed DVT'}.
#' @param exclude.anti.coag Logical indicator of whether to exclude individuals who had received anti-coagulents prior to blood samples being taken (default is \code{FALSE} and it is only relevant to set to \code{TRUE} when analysing certain biomarkers).
#' @param exclude.missing Exclude individuals flagged as having excessive missing data.
#' @param title.to.plot Title for Biomarker axes.
#' @param title.class Title for Classification axes.
#' @param ... Specify the classification (one of \code{first.st} | \code{second.st} | \code{third.st} | \code{fourth.st}) and the biomarker to be plotted.
#'
#' @export
dipep_plot <- function(df        = dipep,
                       exclude   = NULL,
                       exclude.non.recruited = TRUE,
                       exclude.dvt       = TRUE,
                       exclude.anti.coag = FALSE,
                       exclude.missing   = FALSE,
                       title.to.plot     = '',
                       title.class       = '',
                       ...){
    results <- list()
    ## Remove individuals who are explicitly to be removed
    if(!is.null(exclude)){
        df <- df[!(df$screening %in% exclude),]
        ## df <- dplyr::filter_(df, ('screening' %in% !exclude))
    }
    ## Remove non-recruited, DVT and/or missing
    ##
    ## If DVT is FALSE then need to assign the classification variable to be
    ## DVT for plotting.
    if(exclude.non.recruited == TRUE){
        df <- dplyr::filter(df, group != 'Non recruited')
    }
    if(exclude.dvt == TRUE){
        df <- dplyr::filter(df, group != 'Diagnosed DVT')
    }
    if(exclude.missing == TRUE){
        df <- dplyr::filter(df, missing.exclude == FALSE)
    }
    ## Set number of columns for facetting
    if(title.class == 'Recruitment'){
        ncols <- 4
    }
    else{
        ncols <- 3
    }
    ## Remove biomarker data for those on anticoagulents
    if(exclude.anti.coag == TRUE){
        df <- mutate(df,
                     prothombin.time                          = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = prothombin.time),
                     aptt                                     = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = aptt),
                     clauss.fibrinogen                        = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = clauss.fibrinogen),
                     ddimer.innovance                         = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = ddimer.innovance),
                     ddimer.elisa                             = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = ddimer.elisa),
                     d.dimer.cat                              = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = d.dimer.cat),
                     d.dimer                              = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = d.dimer),
                     thrombin.generation.lag.time             = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = thrombin.generation.lag.time),
                     thrombin.generation.endogenous.potential = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = thrombin.generation.endogenous.potential),
                     thrombin.generation.peak                 = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = thrombin.generation.peak),
                     thrombin.generation.time.to.peak         = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = thrombin.generation.time.to.peak),
                     plasmin.antiplasmin                      = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = plasmin.antiplasmin),
                     prothrombin.fragments                    = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = prothrombin.fragments),
                     tissue.factor                            = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = tissue.factor),
                     troponin                                 = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = troponin),
                     nppb                                     = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = nppb),
                     mrproanp                                 = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = mrproanp))
    }
    ## Subset the data for the two variables of interest, the user specified
    ## classification and the user specified score (as '...' arguments)
    ## print('Debug 1')
    ## Extract screening for merging with the rest of the data
    screening <- dplyr::select(df, screening)
    ## Subset variables
    df <- dplyr::select_(df, .dots = lazyeval::lazy_dots(...))
    ## Rename variables so that they are standardised and I don't have to mess
    ## around with Standard Evaluation v's Non Standard Evaluation any more!
    names(df) <- gsub('group',                                    'class',   names(df))
    names(df) <- gsub('first.st',                                 'class',   names(df))
    names(df) <- gsub('second.st',                                'class',   names(df))
    names(df) <- gsub('third.st',                                 'class',   names(df))
    names(df) <- gsub('fourth.st',                                'class',   names(df))
    names(df) <- gsub('primary.dm',                               'class',   names(df))
    names(df) <- gsub('secondary.dm',                             'class',   names(df))
    names(df) <- gsub('aptt',                                     'to.plot', names(df))
    names(df) <- gsub('prothombin.time',                          'to.plot', names(df))
    names(df) <- gsub('clauss.fibrinogen',                        'to.plot', names(df))
    names(df) <- gsub('ddimer.innovance',                         'to.plot', names(df))
    names(df) <- gsub('ddimer.elisa',                             'to.plot', names(df))
    names(df) <- gsub('d.dimer.cat',                              'to.plot', names(df))
    names(df) <- gsub('d.dimer',                                  'to.plot', names(df))
    names(df) <- gsub('thrombin.generation.lag.time',             'to.plot', names(df))
    names(df) <- gsub('thrombin.generation.endogenous.potential', 'to.plot', names(df))
    names(df) <- gsub('thrombin.generation.time.to.peak',         'to.plot', names(df))
    names(df) <- gsub('thrombin.generation.peak',                 'to.plot', names(df))
    names(df) <- gsub('plasmin.antiplasmin',                      'to.plot', names(df))
    names(df) <- gsub('prothrombin.fragments',                    'to.plot', names(df))
    names(df) <- gsub('tissue.factor',                            'to.plot', names(df))
    names(df) <- gsub('troponin',                                 'to.plot', names(df))
    names(df) <- gsub('nppb',                                     'to.plot', names(df))
    names(df) <- gsub('mrproanp',                                 'to.plot', names(df))
    names(df) <- gsub('age',                                      'to.plot', names(df))
    names(df) <- gsub('bmi',                                      'to.plot', names(df))
    names(df) <- gsub('height',                                   'to.plot', names(df))
    names(df) <- gsub('weight',                                   'to.plot', names(df))
    names(df) <- gsub('heart.rate',                               'to.plot', names(df))
    names(df) <- gsub('respiratory.rate',                         'to.plot', names(df))
    names(df) <- gsub('bp.systolic',                              'to.plot', names(df))
    names(df) <- gsub('bp.diastolic',                             'to.plot', names(df))
    names(df) <- gsub('o2.saturation',                            'to.plot', names(df))
    names(df) <- gsub('gestation',                                'to.plot', names(df))
    ## Convert classification to character and replace NA with 'Exclude' so it aligns
    ## with others expectation of what to see.
    if(exclude.dvt == TRUE){
        df <- mutate(df,
                     class = as.character(class),
                     class = ifelse(is.na(class),
                                    yes = 'Exclude',
                                    no  = class))
    }
    else if(exclude.dvt == FALSE){
        df <- cbind(screening,
                    df)
        df <- mutate(df,
                     class = as.character(class))
        df <- df %>%
              mutate(class = case_when(is.na(.$class) & substr(.$screening, 1, 1) == 'D' ~ 'DVT',
                                       is.na(.$class) & substr(.$screening, 1, 1) != 'D' ~ 'Exclude',
                                       !is.na(.$class)                                   ~ .$class))
    }
    ## Generate histogram
    results$histogram <- ggplot(df, aes(x    = to.plot,
                                        fill = class)) +
                         geom_histogram() +
                         xlab(title.to.plot) + ylab('N') +
                         guides(fill = guide_legend(NULL)) +
                         theme(axis.text.x = element_text(angle = 90)) +
                         facet_wrap(~class, ncol = ncols) + theme_bw()
    ## Generate scater plot
    results$scatter <- ggplot(df, aes(x = class,
                                      y = to.plot,
                                      colour = class)) +
                       geom_boxplot(outlier.shape = NA) +
                       geom_point() + geom_jitter() +
                       guides(colour = guide_legend(NULL)) +
                       xlab(title.class) + ylab(title.to.plot) +
                       theme_bw()
    return(results)
}
