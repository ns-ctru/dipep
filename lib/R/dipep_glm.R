#' Wrapper for running univariable logistic regression
#'
#' @description Wrapper for running univariable logistic regression
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
#' @param predictor Predictor variable(s) to test.
#' @param model Name/label of your model.
#'
#'
#' @export
dipep_glm <- function(df              = .data,
                      predictor       = 'age',
                      model           = NULL,
                      ...){
    results <- list()
    ## Build the formula
    .formula <- reformulate(response = 'pe',
                            termlabels = predictor)
    ## Make the model equal the predictor if none is supplied
    if(is.null(model)){
        model <- predictor
    }
    ## Filter the data frame
    results$df <- dplyr::filter(df, group == 'Suspected PE') %>%
                  dplyr::select_(.dots = c('pe', predictor)) %>%
                  mutate(obs = rownames(.),
                         use = complete.cases(.)) %>%
                  dplyr::filter(use == TRUE) %>%
                  dplyr::select(-use)
    results$df$obs <- rownames(results$df)
    ## Test the model
    results$fitted <- glm(.formula,
                          data   = results$df,
                          family = 'binomial')
    ## Use Broom to sweep up/tidy the results
    results$tidied    <- broom::tidy(results$fitted)
    results$tidied <- mutate(results$tidied,
                             term = gsub('age.catOld', 'Age (Old)', term),
                             term = gsub('age', 'Age (Continuous)', term),
                             term = gsub('smokinggave up prior to pregnancy', 'Ex-smoker (Prior)', term),
                             term = gsub('smokinggave up during pregnancy', 'Ex-smoker (During)', term),
                             term = gsub('smokingcurrent', 'Current Smoker', term),
                             term = gsub('temperature.catHigh', 'Temperature (High)', term),
                             term = gsub('temperature', 'Temperature (Continuous)', term),
                             term = gsub('bp.diastolic.catHigh', 'Diastolic (High)', term),
                             term = gsub('bp.diastolic', 'Diastolic (Continuous)', term),
                             term = gsub('bp.systolic.catHigh', 'Systolic (High)', term),
                             term = gsub('bp.systolic', 'Systolic (Continuous)', term),
                             term = gsub('o2.saturation.catHigh', 'O2 Saturation (High)', term),
                             term = gsub('o2.saturation', 'O2 Saturation (Continuous)', term),
                             term = gsub('respiratory.rate.catHigh', 'Respiratory Rate (High)', term),
                             term = gsub('respiratory.rate', 'Respiratory Rate (Continuous)', term),
                             term = gsub('bmi.catHigh', 'BMI (High)', term),
                             term = gsub('bmi', 'BMI (Continuous)', term),
                             term = gsub('Ticked', '', term),
                             term = gsub('presenting.features.pleuritic', 'Presenting : Pleuritic', term),
                             term = gsub('presenting.features.non.pleuritic', 'Presenting : Non-Pleuritic', term),
                             term = gsub('presenting.features.sob.exertion', 'Presenting : Shortness of Breath (Exertion)', term),
                             term = gsub('presenting.features.sob.rest', 'Presenting : Shortness of Breath (Rest)', term),
                             term = gsub('presenting.features.haemoptysis', 'Presenting : Haemoptysis', term),
                             term = gsub('presenting.features.cough', 'Presenting : Cough', term),
                             term = gsub('presenting.features.syncope', 'Presenting : Syncope', term),
                             term = gsub('presenting.features.palpitations', 'Presenting : Palpitations', term),
                             term = gsub('presenting.features.other', 'Presenting : Other', term),
                             term = gsub('pregnancies.under', 'Pregnancies < 24 weeks', term),
                             term = gsub('pregnancies.over', 'Pregnancies > 24 weeks', term),
                             term = gsub('prev.preg.problemNo', 'No Previous Pregnancy Problems', term),
                             term = gsub('history.thrombosisYes', 'Family History of Thrombosis', term),
                             term = gsub('history.veinsYes', 'History of Varicose Veins', term),
                             term = gsub('history.iv.drugYes', 'History of IV Drug use', term),
                             term = gsub('thrombosis', 'History of Thrombosis', term),
                             term = gsub('trimester2nd Trimester', '2nd Trimester', term),
                             term = gsub('trimester3rd Trimester', '3rd Trimester', term),
                             term = gsub('trimesterPost-Partum', 'Post-Partum', term),
                             term = gsub('this.pregnancy.problemsYes', 'Problems with this Pregnancy', term),
                             term = gsub('surgeryYes', 'Surgery in previous 4 weeks', term),
                             term = gsub('thromboYes', 'Known Thrombophilia', term),
                             term = gsub('multiple.pregYes', 'Multiple Pregnancy', term),
                             term = gsub('travelYes', 'Long-haul travel during pregnancy', term),
                             term = gsub('immobilYes', '3 or more days Immobility/bed rest during pregnancy', term),
                             term = gsub('ecgNormal', 'ECG : Normal', term),
                             term = gsub('ecgAbnormal', 'ECG : Abnormal', term),
                             term = gsub('ecgNot performed', 'ECG : Not Performed', term))
    results$augmented <- broom::augment(results$fitted)
    results$glance    <- broom::glance(results$fitted)
    ## Add the predictor to each to make it easier combining
    results$tidied$model    <- model
    results$augmented$model <- model
    results$glance$model    <- model
    ## Get predicted probabilities
    results$predicted <- results$fitted %>% predict() %>% as.data.frame()
    names(results$predicted) <- c('pred')
    results$predicted$obs <- rownames(results$predicted)
    results$predicted <- merge(dplyr::select(results$df, obs, pe),
                               results$predicted,
                               by = c('obs'))
    ## Return ROC curve for this model
    results$roc <- ggplot(results$predicted,
                          aes(d = pe, m = pred)) +
                   geom_roc() +
                   guides(guide = guide_legend(title = '')) +
                   ggtitle(paste0('ROC curve for ', model)) +
                   style_roc() + theme_bw()
    ## Rename predicted df for ease of subsequent use
    names(results$predicted) <- c('obs', paste0('pe.', model), paste0('pred.', model))
    return(results)
}
