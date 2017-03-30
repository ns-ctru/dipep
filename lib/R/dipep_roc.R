#' Plot ROC Curves for multiple predictors
#'
#' @description Plot ROC curves for multiple predictors
#'
#' @details
#'
#' Plots multiple ROC curves for supplied predictors, augmenting
#' the plots with labelled AUC statistics for each plot.
#'
#' @param df Data frame of predicted probabilities for multiple outcomes (in long format).
#' @param to.plot Vector of variables to plot.
#' @param title Title for ROC plot.
#' @param threshold Threshold (\code{0 < x < 1}) for classification if not already part of the data frame.
#' @param labels Whether to apply labels to the ROC curve (option passed to \code{geom_roc()}).
#' @param lasso Logical if predicted values are from steps of a LASSO, forces numbering to be numeric
#'
#' @export
dipep_roc <- function(df        = logistic$predicted,
                      to.plot   = c('history.thrombosis', 'history.iv.drug', 'history.veins', 'thrombo', 'surgery', 'thrombosis'),
                      title     = 'Medical History',
                      threshold = 0.5,
                      labels    = FALSE,
                      lasso     = FALSE,
                      ...){
    results <- list()
    if(lasso == TRUE){
        df <- dplyr::filter(df, !(term %in% c('Lambda 1SE', 'Lambda Min'))) %>%
              mutate(term = as.numeric(term))
        df <- dplyr::filter(df, term %in% to.plot)
    }
    else{
        df <- dplyr::filter(df, name %in% to.plot)
    }
    ## Refactor the 'D' variable (as we sometimes have VTE as outcome and sometimes have PE and
    ## geom_roc() only allows two levels).
    df <- mutate(df,
                 D = as.character(D),
                 D = factor(D))
    ## Generate plot
    results$plot <- ggplot(df,
                    aes(d = D, m = M, colour = as.factor(term))) +
                    geom_roc(labels = labels) +
                    ggtitle(paste0('ROC curves for ', title)) +
                    labs(colour = 'Step...') +
                    style_roc() + theme_bw() ## +
    ## guides(guide = guide_legend(title = 'Predictor...'))
    if(lasso == TRUE){
        results$plot <- results$plot +
                        labs(colour = 'Step...')
    }
    else{
        results$plot <- results$plot +
                        labs(colour = 'Predictor...')
    }
    ## Calculate AUC, extract values and label
    results$auc <- calc_auc(results$plot)
    plot.auc <- cbind(to.plot, results$auc$AUC) %>% as.data.frame()
    names(plot.auc) <- c('Predictor', 'AUC')
    results$plot.auc <- mutate(plot.auc,
                               Predictor = gsub('age.cat', 'Age (Categorical)', Predictor),
                               Predictor = gsub('age', 'Age (Continuous)', Predictor),
                               Predictor = gsub('smoking', 'Smoking', Predictor),
                               Predictor = gsub('temperature.cat', 'Temperature (Categorical)', Predictor),
                               Predictor = gsub('temperature', 'Temperature (Continuous)', Predictor),
                               Predictor = gsub('bp.diastolic.cat', 'Diastolic BP (Categorical)', Predictor),
                               Predictor = gsub('bp.diastolic', 'Diastolic BP (Continuous)', Predictor),
                               Predictor = gsub('bp.systolic.cat', 'Systolic BP (Categorical)', Predictor),
                               Predictor = gsub('bp.systolic', 'Systolic BP (Continuous)', Predictor),
                               Predictor = gsub('o2.saturation.cat', 'O2 Saturation (Categorical)', Predictor),
                               Predictor = gsub('o2.saturation', 'O2 Saturation (Continuous)', Predictor),
                               Predictor = gsub('respiratory.rate.cat', 'Respiratory Rate (Categorical)', Predictor),
                               Predictor = gsub('respiratory.rate', 'Respiratory Rate (Continuous)', Predictor),
                               Predictor = gsub('heart.rate.cat', 'Heart Rate (Categorical)', Predictor),
                               Predictor = gsub('heart.rate', 'Heart Rate (Continuous)', Predictor),
                               Predictor = gsub('bmi.cat', 'BMI (Categorical)', Predictor),
                               Predictor = gsub('bmi', 'BMI (Continuous)', Predictor),
                               Predictor = gsub('Ticked', '', Predictor),
                               Predictor = gsub('presenting.features.pleuritic', 'Presenting : Pleuritic', Predictor),
                               Predictor = gsub('presenting.features.non.pleuritic', 'Presenting : Non-Pleuritic', Predictor),
                               Predictor = gsub('presenting.features.sob.exertion', 'Presenting : Shortness of Breath (Exertion)', Predictor),
                               Predictor = gsub('presenting.features.sob.rest', 'Presenting : Shortness of Breath (Rest)', Predictor),
                               Predictor = gsub('presenting.features.haemoptysis', 'Presenting : Haemoptysis', Predictor),
                               Predictor = gsub('presenting.features.cough', 'Presenting : Cough', Predictor),
                               Predictor = gsub('presenting.features.syncope', 'Presenting : Syncope', Predictor),
                               Predictor = gsub('presenting.features.palpitations', 'Presenting : Palpitations', Predictor),
                               Predictor = gsub('presenting.features.other', 'Presenting : Other', Predictor),
                               Predictor = gsub('pregnancies.under.cat', '>= 1 Pregnancy < 24 weeks', Predictor),
                               Predictor = gsub('pregnancies.over.cat', '>= 1 Pregnancy > 24 weeks', Predictor),
                               Predictor = gsub('pregnancies.under', 'Pregnancies < 24 weeks (Continuous)', Predictor),
                               Predictor = gsub('pregnancies.over', 'Pregnancies > 24 weeks (Continuous)', Predictor),
                               Predictor = gsub('prev.preg.problem', 'Previous Pregnancy Problems', Predictor),
                               Predictor = gsub('history.thrombosis', 'Family History of Thrombosis', Predictor),
                               Predictor = gsub('history.veins', 'History of Varicose Veins', Predictor),
                               Predictor = gsub('history.iv.drug', 'History of IV Drug use', Predictor),
                               Predictor = gsub('thrombosis', 'History of Thrombosis', Predictor),
                               Predictor = gsub('dvt.cat', 'Clinical DVT', Predictor),
                               Predictor = gsub('thromboprophylaxis', 'Thromboprophylaxis', Predictor),
                               Predictor = gsub('thromb.event', 'Previous Thrombotic Event', Predictor),
                               Predictor = gsub('trimester', 'Trimester', Predictor),
                               Predictor = gsub('this.pregnancy.problems', 'Problems with this Pregnancy', Predictor),
                               Predictor = gsub('surgery', 'Surgery in previous 4 weeks', Predictor),
                               Predictor = gsub('thrombo', 'Known Thrombophilia', Predictor),
                               Predictor = gsub('multiple.preg', 'Multiple Pregnancy', Predictor),
                               Predictor = gsub('travel', 'Long-haul travel during pregnancy', Predictor),
                               Predictor = gsub('immobil', '>=3 days Immobility/bed rest during pregnancy', Predictor),
                               Predictor = gsub('ecg', 'ECG', Predictor),
                               Predictor = gsub('ecg.cat', 'ECG', Predictor),
                               Predictor = gsub('ecg.pe', 'ECG (PE Related)', Predictor),
                               Predictor = gsub('xray', 'X-ray', Predictor),
                               Predictor = gsub('xray.cat', 'X-ray', Predictor),
                               Predictor = gsub('xray.pe', 'X-ray (PE Related)', Predictor),
                               Predictor = gsub('appt', 'APTT', Predictor),
                               Predictor = gsub('prothombin.time', 'Prothombin (Time)', Predictor),
                               Predictor = gsub('clauss.fibrinogen', 'Clauss Fibrinogen', Predictor),
                               Predictor = gsub('ddimer.innovanance', 'D-Dimer (Innovance)', Predictor),
                               Predictor = gsub('ddimer.elisa', 'D-Dimer (ELISA)', Predictor),
                               Predictor = gsub('d.dimer.cat', 'D-Dimer (Hospital)', Predictor),
                               Predictor = gsub('thrombin.generation.lag.time', 'Thrombin Generation (Lag Time)', Predictor),
                               Predictor = gsub('thrombin.generation.endogenous.potential', 'Thrombin Generation (Endogenous Potential)', Predictor),
                               Predictor = gsub('thrombin.generation.peak', 'Thrombin Generation (Peak)', Predictor),
                               Predictor = gsub('thrombin.generation.time.to.peak', 'Thrombin Generation (Time to Peak)', Predictor),
                               Predictor = gsub('plasmin.antiplasmin', 'Plasmin (Antiplasmin)', Predictor),
                               Predictor = gsub('prothrombin.fragments', 'PF 1 + 2', Predictor),
                               Predictor = gsub('tissue.factor', 'Tissue Factor', Predictor),
                               Predictor = gsub('troponin', 'Troponin', Predictor),
                               Predictor = gsub('bnp', 'BNP', Predictor),
                               Predictor = gsub('mrproanp', 'MRproANP', Predictor),
                               Predictor = gsub('Step min.response', 'Lambda Min', Predictor),
                               Predictor = gsub('Step 1se.response', 'Lambda 1SE', Predictor))
    ## results$plot.auc$x <- 0.75
    ## results$plot.auc$y <- 0.25
    ## ToDo - How to annotate plot with the Predictor and AUC components???
    ## Add AUC to plot
    ## results$plot <- results$plot +
    ##                 geom_text(data = plot.auc, aes(x = 0.75, y = 0.25))
    ## ToDo 2017-02-22 - Calculate...
    ##                   Sensitivity
    ##                   Specificty
    ##                   Positive Predictive Value
    ##                   Negative Predictive Value
    ## Check if threshold (a value which should determine the cut point for
    ## classification) exists in the data, if not then set it
    results$df <- df
    if(!c('threshold') %in% names(results$df)){
        results$df <- mutate(results$df,
                             threshold = threshold)
    }
    ## Classify people based on the threshold
    results$df <- mutate(results$df,
                         m = ifelse(M > threshold,
                                    yes = 1,
                                    no  = 0))
    ## Count the number of individuals in each category (done in dipep_existing_sum() by
    ## tabulation, but thats potentially problematic)
    results$counts <- mutate(results$df,
                             .n = 1) %>%
        ## This puts in missing for combinations NOT seen, trick is that
        ## missing then sum to zero because they are removed
                      complete(term, D, m, fill = list(.n = NA)) %>%
                      group_by(term, D, m) %>%
                      summarise(n = sum(.n, na.rm = TRUE))
    ## Count true/false positive/negative
    results$counts <- ungroup(results$counts) %>%
                      mutate(classification  = case_when(.$D == 'PE'    & .$m == 1 ~ 'true_positive',
                                                         .$D == 'No PE' & .$m == 0 ~ 'true_negative',
                                                         .$D == 'No PE' & .$m == 1 ~ 'false_positive',
                                                         .$D == 'PE'    & .$m == 0 ~ 'false_negative',
                                                         .$D == 'VTE'    & .$m == 1 ~ 'true_positive',
                                                         .$D == 'No VTE' & .$m == 0 ~ 'true_negative',
                                                         .$D == 'No VTE' & .$m == 1 ~ 'false_positive',
                                                         .$D == 'VTE'    & .$m == 0 ~ 'false_negative'))
    ## By term summarise counts of
    results$summary.stats <- dplyr::select(results$counts, term, classification, n) %>%
                             dcast(term ~ classification) %>%
                             mutate(sensitivity = true_positive  / (true_positive + false_negative),
                                    specificity = true_negative  / (true_negative + false_positive),
                                    ppv         = true_positive  / (true_positive + false_positive),
                                    npv         = true_negative  / (true_negative + false_negative),
                                    fpr         = false_positive / (true_negative + false_positive),
                                    fnr         = false_negative / (true_positive + false_negative),
                                    fdr         = false_positive / (true_positive + false_positive),
                                    accuracy    = (true_positive + true_negative) / (true_positive + false_positive + true_negative + false_negative))
    ## Bind AUC in with summary statistics
    t <- results$plot.auc
    names(t) <- gsub('Predictor', 'term', names(t))
    if(lasso == TRUE){
        t <- mutate(t,
                    term = as.numeric(term),
                    AUC  = as.numeric(AUC))
    }
    results$summary.stats <- left_join(results$summary.stats,
                                       t) %>%
                             dplyr::select(term, true_positive, true_negative, false_positive, false_negative,
                                           AUC, sensitivity, specificity, ppv, npv, fpr, fnr, fdr, accuracy)
    names(results$summary.stats) <- c('Term', 'True +ve', 'True -ve', 'False +ve', 'False -ve', 'AUC',
                                      'Sensitivity', 'Specificity', 'PPV', 'NPV', 'FPR', 'FNR', 'FDR', 'Accuracy')
    return(results)
}
