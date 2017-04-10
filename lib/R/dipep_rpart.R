#' Wrapper for running rpart() on Dipep
#'
#' @description Wrapper for running roart() on Dipep
#'
#' @details
#'
#' This wrapper runs recursive partitioning models for a given predictor variable
#' and returns the fitted model(s) (for combining using broom()) along with the
#' predicted values for each pruning  step and associated ROC curves and summary statistics.
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
#' @param exclude.missing Exclude individuals flagged as having excessive missing data.
#' @param legend Logical indicator of whether to include a legend in the LASSO normalisation plot.
#' @param threshold Threshold for dichotomisation of predicted probabilities (by default \code{0.5} is used for classification so responses match class, this permits alternative thresholds to be passed to \code{dipep_roc()}).
#' @param rpart.opts.method \code{method} option passed to \code{rpart()} (see \code{?rpart} for full details).
#' @param rpart.opts.minsplit \code{minsplit} option passed to \code{rpart()} (see \code{?rpart} for full details).
#' @param rpart.opts.minbucket \code{minbucket} option passed to \code{rpart()} (see \code{?rpart} for full details).
#' @param rpart.opts.cp \code{cp} option passed to \code{rpart()} (see \code{?rpart} for full details).
#'
#'
#' @export
dipep_rpart <- function(df              = dipep,
                        classification  = 'first.st',
                        predictor       = 'age',
                        model           = NULL,
                        relevel         = NULL,
                        exclude         = NULL,
                        exclude.non.recruited = TRUE,
                        exclude.dvt       = TRUE,
                        exclude.anti.coag = FALSE,
                        exclude.missing   = FALSE,
                        legend            = FALSE,
                        threshold         = 0.5,
                        ## Rpart options
                        rpart.opts.method    = 'class',
                        rpart.opts.minsplit  = 4,
                        rpart.opts.minbucket = 2,
                        rpart.opts.cp        = -1,
                        ...){
    results <- list()
    results$threshold <- threshold
    ## Remove individuals who are explicitly to be removed
    if(!is.null(exclude)){
        df <- df[!(df$screening %in% exclude),]
    }
    ## Remove non-recruited, DVT and/or missing
    if(exclude.non.recruited == TRUE){
        df <- dplyr::filter(df, group != 'Non recruited')
    }
    if(exclude.dvt == TRUE){
        df <- dplyr::filter(df, group != 'Diagnosed DVT')
    }
    if(exclude.missing == TRUE){
        df <- dplyr::filter(df, missing.exclude == FALSE)
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
                     bnp                                      = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = bnp),
                     mrproanp                                 = ifelse(exclude.anti.coag == 'Yes',
                                                                       yes = NA,
                                                                       no  = mrproanp))
    }
    ## Obtain the numbre of missing for the current classified
    results$n.exclude <- dplyr::filter(df, is.na(classification)) %>% nrow()
    ## Remove those who are 'Exclude' for the current classification
    if(classification == 'first.st'){
        df <- subset(df, !is.na(first.st))
        results$observed <- df$first.st
    }
    else if(classification == 'second.st'){
        df <- subset(df, !is.na(second.st))
        results$observed <- df$second.st
    }
    else if(classification == 'third.st'){
        df <- subset(df, !is.na(third.st))
        results$observed <- df$third.st
    }
    else if(classification == 'fourth.st'){
        df <- subset(df, !is.na(fourth.st))
        results$observed <- df$fourth.st
    }
    else if(classification == 'primary.dm'){
        df <- subset(df, !is.na(primary.dm))
        results$observed <- df$primary.dm
    }
    else if(classification == 'secondary.dm'){
        df <- subset(df, !is.na(secondary.dm))
        results$observed <- df$secondary.dm
    }
    names(results$observed) <- c('D')
    ## Define the mode
    results$model <- reformulate(response   = classification,
                                 termlabels = predictor)
    results$rpart.full <- rpart(results$model,
                           data    = df,
                           method  = rpart.opts.method,
                           control = rpart.control(minsplit  =- rpart.opts.minsplit,
                                                   minbucket = rpart.opts.minbucket,
                                                   cp        = rpart.opts.cp))
    ## Complexity Parameter selection, extract to a data frame
    results$rpart.full.cp <- results$rpart.full$cptable %>% as.data.frame()
    ## Obtain the complexity parameter and number of splits corresponding to the
    ## minimum value cross-validated error
    results$rpart.full.cp.min <- results$rpart.full.cp[which.min(results$rpart.full.cp[, 'xerror']), 'CP']
    results$rpart.full.splits.min <- results$rpart.full.cp[which.min(results$rpart.full.cp[, 'xerror']), 'CP']
    ## Prune the tree based on this, make predictions and plot the ROC
    results$pruned.min <- prune(results$rpart.full, cp = results$rpart.full.cp.min)
    results$pruned.min.predicted <- predict(results$pruned.min) %>% as.data.frame()
    results$pruned.min.predicted <- cbind(results$observed,
                                          results$pruned.min.predicted)
    results$pruned.min.predicted$term   <- paste0('CP = ', round(results$rpart.full.cp.min, digits = 5))
    results$pruned.min.predicted$name   <- 'min'
    names(results$pruned.min.predicted) <- c('D', 'remove', 'M', 'term', 'name')
    results$pruned.min.predicted <- dplyr::select(results$pruned.min.predicted, -remove)
    results$roc.min <- dipep_roc(df        = results$pruned.min.predicted,
                                 to.plot   = c('min'),
                                 title     = 'Pruned Tree minimising cross-validation error.',
                                 threshold = threshold,
                                 lasso     = FALSE)
    ## Add the AUC to the plot
    results$roc.min$plot <- results$roc.min$plot +
                           annotate('text', x = 0.75, y = 0.25,
                                    label = paste0('AUC = ', round(results$roc.min$auc$AUC, digits = 3)))
    ## Loop over all values of the Complexity Parameter, pruning the full
    results$pruned.all <- list()
    for(i in 1:nrow(results$rpart.full.cp)){
        results$pruned.all[[i]] <- prune(results$rpart.full, cp = results$rpart.full$cptable[i,1])
        ## Build a data frame of predicted probabilities from all steps of the tree
        if(i == 1){
            results$predicted      <- predict(results$pruned.all[[i]]) %>% as.data.frame()
            results$predicted$term <- i
            results$predicted$name <- results$rpart.full$cptable[i,1]
            results$predicted <- cbind(results$observed,
                                       results$predicted)
        }
        else{
            current           <- predict(results$pruned.all[[i]]) %>% as.data.frame()
            current$term      <- i
            current$name      <- results$rpart.full$cptable[i,1]
            current           <- cbind(results$observed,
                                       current)
            results$predicted <- rbind(results$predicted,
                                       current)
        }
    }
    ## Rename and Remove prediction of No PE
    names(results$predicted) <- c('D', 'remove', 'M', 'term', 'name')
    results$predicted <- dplyr::select(results$predicted, -remove)
    ## Plot all terms
    table(results$predicted$term) %>% print()
    table(results$predicted$name) %>% print()
    seq(1:nrow(results$rpart.full.cp)) %>% print()
    results$roc.all <- dipep_roc(df        = results$predicted,
                                 to.plot   = seq(1:nrow(results$rpart.full.cp)),
                                 title     = 'each Pruned Tree',
                                 threshold = threshold,
                                 lasso     = TRUE)
    ## Return results
    return(results)
}
