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
#' @param exclude Vector of \code{screening}  to exclude.
#' @param exclude.non.recuirted Logical indicator of whether to exclude \code{group == 'Non recruited'}.
#' @param exclude.dvt Logical indicator of whether to exclude \code{group == 'Diagnosed DVT'}.
#' @param exclude.anti.coag Logical indicator of whether to exclude individuals who had received anti-coagulents prior to blood samples being taken (default is \code{FALSE} and it is only relevant to set to \code{TRUE} when analysing certain biomarkers).
#'
#'
#' @export
dipep_glmnet <- function(df           = dipep,
                         classification  = 'first.st',
                         predictor       = c('age'),
                         model           = NULL,
                         exclude         = NULL,
                         exclude.non.recruited = TRUE,
                         exclude.dvt       = TRUE,
                         exclude.anti.coag = FALSE,
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
                     ddimer.elisa                             = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = ddimer.elisa),
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
    ## Remove those who are 'Exclude' for the current classification
    df <- dplyr::filter_(df, !is.na(classification))
    dim(df) %>% print()
    ## Build the formula
    .formula <- reformulate(response = classification,
                            termlabels = predictor)
    return(results)
}
