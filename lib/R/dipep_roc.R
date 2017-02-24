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
#'
#' @export
dipep_roc <- function(df        = logistic$predicted,
                      to.plot   = c('history.thrombosis', 'history.iv.drug', 'history.veins', 'thrombo', 'surgery', 'thrombosis'),
                      title     = 'Medical History',
                      threshold = 0.5,
                      ...){
    results <- list()
    ## Generate plot
    results$plot <- ggplot(dplyr::filter(df, name %in% to.plot),
                    aes(d = D, m = M, colour = term)) +
                    geom_roc() +
                    ggtitle(paste0('ROC curves for ', title, ' Univariable Logistic Regression')) +
                    labs(colour = 'Predictor...') +
                    style_roc() + theme_bw() ## +
                    ## guides(guide = guide_legend(title = 'Predictor...'))
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
                               Predictor = gsub('trimester', 'Trimester', Predictor),
                               Predictor = gsub('this.pregnancy.problems', 'Problems with this Pregnancy', Predictor),
                               Predictor = gsub('surgery', 'Surgery in previous 4 weeks', Predictor),
                               Predictor = gsub('thrombo', 'Known Thrombophilia', Predictor),
                               Predictor = gsub('multiple.preg', 'Multiple Pregnancy', Predictor),
                               Predictor = gsub('travel', 'Long-haul travel during pregnancy', Predictor),
                               Predictor = gsub('immobil', '>=3 days Immobility/bed rest during pregnancy', Predictor),
                               Predictor = gsub('ecg', 'ECG', Predictor),
                               Predictor = gsub('ecg.cat', 'ECG', Predictor),
                               Predictor = gsub('xray', 'X-ray', Predictor),
                               Predictor = gsub('xray.cat', 'X-ray', Predictor),
                               Predictor = gsub('aprothombin', 'Aprothombin', Predictor),
                               Predictor = gsub('prothombin.time', 'Prothombin (Time)', Predictor),
                               Predictor = gsub('clauss.fibrinogen', 'Clauss Fibrinogen', Predictor),
                               Predictor = gsub('ddimer.innovan', 'D-Dimer (Innovan)', Predictor),
                               Predictor = gsub('ddimer.elisa', 'D-Dimer (ELISA)', Predictor),
                               Predictor = gsub('thrombin.generation.lag.time', 'Thrombin Generation (Lag Time)', Predictor),
                               Predictor = gsub('thrombin.generation.endogenous.potential', 'Thrombin Generation (Endogenous Potential)', Predictor),
                               Predictor = gsub('thrombin.generation.peak', 'Thrombin Generation (Peak)', Predictor),
                               Predictor = gsub('thrombin.generation.time.to.peak', 'Thrombin Generation (Time to Peak)', Predictor),
                               Predictor = gsub('plasmin.antiplasmin', 'Plasmin (Antiplasmin)', Predictor),
                               Predictor = gsub('prothrombin.fragments', 'Prothombin Fragments', Predictor),
                               Predictor = gsub('soluble.tissue.factor', 'Soluble Tissue Factor', Predictor),
                               Predictor = gsub('troponin', 'Troponin', Predictor),
                               Predictor = gsub('natriuertic.peptide', 'Natriuertic Peptide', Predictor),
                               Predictor = gsub('mrproanp', 'MRproANP', Predictor))
    results$plot.auc$x <- 0.75
    results$plot.auc$y <- 0.25
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
    if(!c('threshold') %in% names(df)){
        df <- mutate(df,
                     threshold = threshold)
    }
    ## Classify people based on the threshold
    df <- mutate(df,
                 m = ifelse(M > threshold,
                            yes = 1,
                            no  = 0))
    ## By term summarise counts of
    return(results)
}
