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
#' @param alpha Confidence Interval width.
#' @param ci.method Method for calculating Confidence Interval (default is \code{wilson}, i.e. score-test-based).
#' @param labels Whether to apply labels to the ROC curve (option passed to \code{geom_roc()}).
#' @param lasso Logical if predicted values are from steps of a LASSO, forces numbering to be numeric
#'
#' @export
dipep_roc <- function(df        = logistic$predicted,
                      to.plot   = c('history.thrombosis', 'history.iv.drug', 'history.veins', 'thrombo', 'surgery', 'thrombosis'),
                      title     = 'Medical History',
                      threshold = 0.5,
                      alpha     = 0.05,
                      ci.method = 'exact',
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
                 D = factor(D),
                 name = factor(name))
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
                               Predictor = gsub('this.pregnancy.problems.incl.other', 'Problems with this Pregnancy (including Other)', Predictor),
                               Predictor = gsub('this.pregnancy.problems', 'Problems with this Pregnancy', Predictor),
                               Predictor = gsub('obstetric.complications', 'Other problem with this pregnancy (VTE-related)', Predictor),
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
                               Predictor = gsub('appt.cat', 'APTT : Dichotomised', Predictor),
                               Predictor = gsub('prothombin.time', 'Prothombin (Time)', Predictor),
                               Predictor = gsub('prothombin.time.cat', 'Prothombin (Time) : Dichotomised', Predictor),
                               Predictor = gsub('clauss.fibrinogen', 'Clauss Fibrinogen', Predictor),
                               Predictor = gsub('clauss.fibrinogen.cat', 'Clauss Fibrinogen : Dichotomised', Predictor),
                               Predictor = gsub('ddimer.innovanance', 'D-Dimer (Innovance)', Predictor),
                               Predictor = gsub('ddimer.innovanance.cat', 'D-Dimer (Innovance) : Dichotomised', Predictor),
                               Predictor = gsub('ddimer.elisa', 'D-Dimer (ELISA)', Predictor),
                               Predictor = gsub('ddimer.elisa.cat', 'D-Dimer (ELISA) : Dichotomised', Predictor),
                               Predictor = gsub('d.dimer', 'D-Dimer (Hospital)', Predictor),
                               Predictor = gsub('d.dimer.cat', 'D-Dimer (Hospital) : Dichotomised', Predictor),
                               Predictor = gsub('d.dimer.gestation.cat', 'D-Dimer (Hospital) : Dichotomised (Gestation Specific)', Predictor),
                               Predictor = gsub('thrombin.generation.lag.time', 'Thrombin Generation (Lag Time)', Predictor),
                               Predictor = gsub('thrombin.generation.lag.time.cat', 'Thrombin Generation (Lag Time) : Dichotomised', Predictor),
                               Predictor = gsub('thrombin.generation.endogenous.potential', 'Thrombin Generation (Endogenous Potential)', Predictor),
                               Predictor = gsub('thrombin.generation.endogenous.potential.cat', 'Thrombin Generation (Endogenous Potential) : Dichotomised', Predictor),
                               Predictor = gsub('thrombin.generation.peak', 'Thrombin Generation (Peak)', Predictor),
                               Predictor = gsub('thrombin.generation.peak.cat', 'Thrombin Generation (Peak) : Dichotomised', Predictor),
                               Predictor = gsub('thrombin.generation.time.to.peak', 'Thrombin Generation (Time to Peak)', Predictor),
                               Predictor = gsub('thrombin.generation.time.to.peak.cat', 'Thrombin Generation (Time to Peak) : Dichotomised', Predictor),
                               Predictor = gsub('plasmin.antiplasmin', 'Plasmin-antiplasmin', Predictor),
                               Predictor = gsub('plasmin.antiplasmin.cat', 'Plasmin-antiplasmin : Dichotomised', Predictor),
                               Predictor = gsub('prothrombin.fragments', 'PF 1 + 2', Predictor),
                               Predictor = gsub('prothrombin.fragments.cat', 'PF 1 + 2 : Dichotomised', Predictor),
                               Predictor = gsub('tissue.factor', 'Tissue Factor', Predictor),
                               Predictor = gsub('tissue.factor.cat', 'Tissue Factor : Dichotomised', Predictor),
                               Predictor = gsub('troponin', 'Troponin', Predictor),
                               Predictor = gsub('troponin.cat', 'Troponin : Dichotomised', Predictor),
                               Predictor = gsub('bnp', 'BNP', Predictor),
                               Predictor = gsub('bnp.cat', 'BNP : Dichotomised', Predictor),
                               Predictor = gsub('mrproanp', 'MRproANP', Predictor),
                               Predictor = gsub('mrproanp.cat', 'MRproANP : Dichotomised', Predictor),
                               Predictor = gsub('crp', 'C-Reactive Protein', Predictor),
                               Predictor = gsub('crp.cat', 'C-Reactive Protein : Dichotomised', Predictor),
                               Predictor = gsub('Step min.response', 'Lambda Min', Predictor),
                               Predictor = gsub('Step 1se.response', 'Lambda 1SE', Predictor),
                               Predictor = gsub('simplified.pe', 'Revised Geneva', Predictor),
                               Predictor = gsub('simplified.age', 'Revised Geneva : Age', Predictor),
                               Predictor = gsub('simplified.prev.dvt.pe', 'Revised Geneva : Previous DVT/PE', Predictor),
                               Predictor = gsub('simplified.surgery', 'Revised Geneva : Surgery', Predictor),
                               Predictor = gsub('simplified.neoplasm', 'Revised Geneva : Neoplasm', Predictor),
                               Predictor = gsub('simplified.lower.limb.unilateral.pain', 'Revised Geneva : Unilateral Lower Limb Pain', Predictor),
                               Predictor = gsub('simplified.haemoptysis', 'Revised Geneva : Haemoptysis', Predictor),
                               Predictor = gsub('simplified.heart.rate', 'Revised Geneva : Heart Rate', Predictor),
                               Predictor = gsub('simplified.pain.palpitations', 'Revised Geneva : Pain on limb palpitation', Predictor),
                               Predictor = gsub('perc.pe', 'PERC', Predictor),
                               Predictor = gsub('perc.age', 'PERC : Age', Predictor),
                               Predictor = gsub('perc.heart.rate', 'PERC : Heart Rate', Predictor),
                               Predictor = gsub('perc.o2.saturation', 'PERC : O2 Saturation', Predictor),
                               Predictor = gsub('perc.prev.dvt.pe', 'PERC : Previous DVT/PE', Predictor),
                               Predictor = gsub('perc.surgery', 'PERC : Recent Trauma or Surgery', Predictor),
                               Predictor = gsub('perc.haemoptysis', 'PERC : Haemoptysis', Predictor),
                               Predictor = gsub('perc.hormone', 'PERC : Exogenous Estrogen', Predictor),
                               Predictor = gsub('perc.leg.swelling', 'PERC : Unilateral Leg Swelling', Predictor),
                               Predictor = gsub('wells.permissive.pe', 'Wells : Permissive', Predictor),
                               Predictor = gsub('wells.strict.pe', 'Wells : Strict', Predictor),
                               Predictor = gsub('wells.dvt', 'Wells : DVT', Predictor),
                               Predictor = gsub('wells.alternative.permissive', 'Wells : Alternative (Permissive)', Predictor),
                               Predictor = gsub('wells.alternative.strict', 'Wells : Alternative (Strict)', Predictor),
                               Predictor = gsub('wells.heart.rate', 'Wells : Heart Rate', Predictor),
                               Predictor = gsub('wells.surgery.immobil', 'Wells : Surgery/Immobility', Predictor),
                               Predictor = gsub('wells.dvt.pe', 'Wells : DVT', Predictor),
                               Predictor = gsub('wells.haemoptysis', 'Wells : Haemoptysis', Predictor),
                               Predictor = gsub('wells.neoplasm', 'Wells : Neoplasm', Predictor),
                               Predictor = gsub('delphi.primary.pe', 'Delphi : Primary', Predictor),
                               Predictor = gsub('delphi.sensitivity.pe', 'Delphi : Sensitivity', Predictor),
                               Predictor = gsub('delphi.specificity.pe', 'Delphi : Specificity', Predictor),
                               Predictor = gsub('delphi.haemoptysis', 'Delphi : Haemoptysis', Predictor),
                               Predictor = gsub('delphi.pleuritic', 'Delphi : Pleuritic', Predictor),
                               Predictor = gsub('delphi.history.dvt.pe', 'Delphi : History DVT/PE', Predictor),
                               Predictor = gsub('delphi.family.history', 'Delphi : Family History', Predictor),
                               Predictor = gsub('delphi.medical.history', 'Delphi : Medical History (Hospital Admission, Surgery or Injury)', Predictor),
                               Predictor = gsub('delphi.medical.history.surgery', 'Delphi : Surgery (excl. Cesarean)', Predictor),
                               Predictor = gsub('delphi.obstetric.complication', 'Delphi : Obstetric Complication', Predictor),
                               Predictor = gsub('delphi.medical.complication', 'Delphi : Medical Complication', Predictor),
                               Predictor = gsub('delphi.gestation', 'Delphi : Gestation', Predictor),
                               Predictor = gsub('delphi.bmi', 'Delphi : BMI', Predictor),
                               Predictor = gsub('delphi.dvt', 'Delphi : DVT', Predictor),
                               Predictor = gsub('delphi.o2.saturation', 'Delphi : O2 Saturation', Predictor),
                               Predictor = gsub('delphi.heart.rate', 'Delphi : Heart Rate', Predictor),
                               Predictor = gsub('delphi.respiratory.rate', 'Delphi : Respiratory Rate', Predictor))
    ## Check if threshold (a value which should determine the cut point for
    ## classification) exists in the data, if not then set it
    results$df <- df
    ## outcome.levels <- table(results$df$predictor) %>% length()
    ## if(outcome.levels != 2){
    ##     results$df <- results$df %>%
    ##                   mutate(threshold = threshold,
    ##                          m         = ifelse(M  < threshold,
    ##                                             yes = 1,
    ##                                             no  = 0),
    ##                          .n        = 1)
    ## }
    ## else{
    ##     results$df <- results$df %>%
    ##                   mutate(m  = predictor,
    ##                          .n = 1)
    ## }
    if(!c('threshold') %in% names(results$df)){
        ## ToDo 2017-04-11 - Intelligent threshold for binary variables
        thresholds <- group_by(results$df, name) %>%
                      summarise(n_probs = M %>% as.factor() %>% levels() %>% length(),
                                min     = min(M, na.rm = TRUE),
                                max     = max(M, na.rm = TRUE))
        results$df <- merge(results$df,
                            thresholds,
                            by = c('name'))
        results$df <- results$df %>%
            mutate(threshold = case_when(.$n_probs == 2 & .$M == .$min ~ 1,
                                         .$n_probs == 2 & .$M == .$max ~ 0,
                                         .$n_probs != 2                ~ threshold))
    }
    ## Classify people based on the threshold
    results$df <- results$df %>%
                  mutate(m = ifelse(.$M > .$threshold,
                                    yes = 1,
                                    no  = 0),
                         .n = 1)
    results$counts <- results$df %>%
        ## This puts in missing for combinations NOT seen, trick is that
        ## missing then sum to zero because they are removed
                      complete(term, D, m, fill = list(.n = NA)) %>%
                      group_by(term, D, m) %>%
                      summarise(n = sum(.n, na.rm = TRUE))
    results$counts <- ungroup(results$counts) %>%
                      mutate(classification  = case_when(.$D == 'PE'    & .$m == 1  ~ 'true_positive',
                                                         .$D == 'No PE' & .$m == 0  ~ 'true_negative',
                                                         .$D == 'No PE' & .$m == 1  ~ 'false_positive',
                                                         .$D == 'PE'    & .$m == 0  ~ 'false_negative',
                                                         .$D == 'VTE'    & .$m == 1 ~ 'true_positive',
                                                         .$D == 'No VTE' & .$m == 0 ~ 'true_negative',
                                                         .$D == 'No VTE' & .$m == 1 ~ 'false_positive',
                                                         .$D == 'VTE'    & .$m == 0 ~ 'false_negative'))
    ## Sometimes there are no true_positive/true_negative/false_positive/false_negative which means
    ## the next section won't run.  Conditionally check and add as zeros if not present
    if(!('true_positive' %in% names(results$counts))){
        results$counts$true_positive <- 0
    }
    if(!('true_negative' %in% names(results$counts))){
        results$counts$true_negative <- 0
    }
    if(!('false_positive' %in% names(results$counts))){
        results$counts$false_positive <- 0
    }
    if(!('false_negative' %in% names(results$counts))){
        results$counts$false_negative <- 0
    }
    ## results$counts %>% print()
    ## table(results$counts$classification) %>% print()
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
    ## CIs for sensitivty and specificity
    results$test <- results$summary.stats
    ## head(results$summary.stats) %>% print()
    ci.sensitivity <- binconf(x = results$summary.stats$true_positive,
                              n = results$summary.stats$true_positive + results$summary.stats$false_negative,
                              alpha = alpha,
                              method = ci.method,
                              return.df = TRUE) %>%
                       dplyr::select(-PointEst)
    names(ci.sensitivity) <- c('sensitivity_lci', 'sensitivity_uci')
    ci.specificity <- binconf(x = results$summary.stats$true_negative,
                              n = results$summary.stats$true_negative + results$summary.stats$false_positive,
                              alpha = alpha,
                              method = ci.method,
                              return.df = TRUE) %>%
                      dplyr::select(-PointEst)
    names(ci.specificity) <- c('specificity_lci', 'specificity_uci')
    results$ci <- cbind(ci.sensitivity, ci.specificity) ## %>%
    ## Bind AUC in with summary statistics
    t <- results$plot.auc
    names(t) <- gsub('Predictor', 'term', names(t))
    if(lasso == TRUE){
        t <- mutate(t,
                    term = as.numeric(term),
                    AUC  = as.numeric(AUC))
    }
    results$summary.stats <- left_join(results$summary.stats,
                                       t)
    ## Calculate CI for AUC
    ## Repeat for all predictors passed to the function
    results$auc.ci <- list()
    for(x in unique(results$df$name)){
        if(lasso == TRUE) y <- paste0('step_', x)
        else              y <-  x
        results$auc.ci[[y]] <- roc(D ~ M,
                                   data = results$df,
                                   subset = (name == x)) %>%
                               ci()
    }
    ## Extract to data frame
    results$auc.ci   <- unlist(results$auc.ci) %>%
                        as.data.frame()
    names(results$auc.ci)   <- c('stat')
    results$auc.ci$t <- rownames(results$auc.ci)
    results$auc.ci <- results$auc.ci %>%
                      mutate(component = case_when(grepl('1$', .$t) ~ 'auc_lci',
                                                   grepl('2$', .$t) ~ 'auc',
                                                   grepl('3$', .$t) ~ 'auc_uci'),
                             name      = substr(.$t, 1, nchar(.$t) - 1)) %>%
                      dplyr::select(-t) %>%
                      spread(key = component, value = stat)
    if(lasso == TRUE) results$auc.ci <-arrange(results$auc.ci, desc(name))
    ## Bind with all other statistics and CIs
    results$summary.stats <- cbind(results$summary.stats, results$ci, results$auc.ci) %>%
                             dplyr::select(term, true_positive, true_negative, false_positive, false_negative,
                                           auc, auc_lci, auc_uci,
                                           sensitivity, sensitivity_lci, sensitivity_uci,
                                           specificity, specificity_lci, specificity_uci,
                                           fpr, fnr, fdr, accuracy)
    names(results$summary.stats) <- c('Term', 'True +ve', 'True -ve', 'False +ve', 'False -ve',
                                      'AUC', 'AUC Lower CI', 'AUC Upper CI',
                                      'Sensitivity', 'Sensitivity Lower CI', 'Sensitivity Upper CI',
                                      'Specificity', 'Specificity Lower CI', 'Specificity Upper CI',
                                      'FPR', 'FNR', 'FDR', 'Accuracy')
    return(results)
}
