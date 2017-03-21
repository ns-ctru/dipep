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
#' @param classification Specify the variable that defines the disease status, for this study there are four classifications of diseases ststus, hence the need for flexibility.
#' @param predictor Predictor variable(s) to test.
#' @param model Name/label of your model.
#' @param relevel Reference level for logistic regression if different from default.
#' @param exclude Vector of \code{screening}  to exclude.
#' @param exclude.non.recuirted Logical indicator of whether to exclude \code{group == 'Non recruited'}.
#' @param exclude.dvt Logical indicator of whether to exclude \code{group == 'Diagnosed DVT'}.
#' @param exclude.anti.coag Logical indicator of whether to exclude individuals who had received anti-coagulents prior to blood samples being taken (default is \code{FALSE} and it is only relevant to set to \code{TRUE} when analysing certain biomarkers).
#'
#' @export
dipep_glm <- function(df              = .data,
                      classification  = 'first.st',
                      predictor       = 'age',
                      model           = NULL,
                      relevel         = NULL,
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
    ## Build the formula
    .formula <- reformulate(response = classification,
                            termlabels = predictor)
    ## Relevel if asked
    ## ToDo - Check this, may not work correctly (usual NSE issues)
    if(!is.null(relevel)){
        df$predictor <- relevel(df$predictor,
                                ref = relevel)
    }
    ## Make the model equal the predictor if none is supplied
    if(is.null(model)){
        model <- predictor
    }
    ## Cross tabulation of counts for combining
    results$table <- dplyr::group_by_(df, classification, predictor) %>%
                     dplyr::summarise(n = n()) %>%
                     dplyr::group_by_(classification) %>%
                     ## dplyr::group_by_(predictor) %>%
                     dplyr::mutate(N    = sum(n, na.rm = TRUE),
                                   prop = (n * 100) / N,
                                   n    = paste0(formatC(n, digits = 0, format = 'f'),
                                                 ' (',
                                                 formatC(prop, digits = 2, format = 'f'),
                                                 '%)'))
    names(results$table) <- c('classification', 'levels', 'n', 'N', 'prop')
    results$table <- results$table[c(1:3)]
    results$table <- dcast(results$table, levels ~ classification) %>%
                     mutate(predictor = predictor,
                            predictor = gsub('age.cat', 'Age (Categorised)', predictor),
                            predictor = gsub('smoking', 'Smoking Status', predictor),
                            predictor = gsub('temperature.cat', 'Temperature (Categorised)', predictor),
                            predictor = gsub('bp.diastolic.cat', 'Diastolic (Categorised)', predictor),
                            predictor = gsub('bp.systolic.cat', 'Systolic (Categorised)', predictor),
                            predictor = gsub('o2.saturation.cat', 'O2 Saturation (Categorised)', predictor),
                            predictor = gsub('respiratory.rate.cat', 'Respiratory Rate (Categorised)', predictor),
                            predictor = gsub('heart.rate.cat', 'Heart Rate (Categorised)', predictor),
                            predictor = gsub('bmi.cat', 'BMI (Categorised)', predictor),
                            predictor = gsub('presenting.features.pleuritic', 'Presenting : Pleuritic', predictor),
                            predictor = gsub('presenting.features.non.pleuritic', 'Presenting : Non-Pleuritic', predictor),
                            predictor = gsub('presenting.features.sob.exertion', 'Presenting : Shortness of Breath (Exertion)', predictor),
                            predictor = gsub('presenting.features.sob.rest', 'Presenting : Shortness of Breath (Rest)', predictor),
                            predictor = gsub('presenting.features.haemoptysis', 'Presenting : Haemoptysis', predictor),
                            predictor = gsub('presenting.features.cough', 'Presenting : Cough', predictor),
                            predictor = gsub('presenting.features.syncope', 'Presenting : Syncope', predictor),
                            predictor = gsub('presenting.features.palpitations', 'Presenting : Palpitations', predictor),
                            predictor = gsub('presenting.features.other', 'Presenting : Other', predictor),
                            predictor = gsub('pregnancies.under.cat', '>= 1 Pregnancy < 24 weeks', predictor),
                            predictor = gsub('pregnancies.over.cat', '>= 1 Pregnancy > 24 weeks', predictor),
                            predictor = gsub('prev.preg.problem', 'Previous Pregnancy Problems', predictor),
                            predictor = gsub('history.thrombosis', 'Family History of Thrombosis', predictor),
                            predictor = gsub('history.veins', 'History of Varicose Veins', predictor),
                            predictor = gsub('history.iv.drug', 'History of IV Drug use', predictor),
                            predictor = gsub('thrombosis', 'History of Thrombosis', predictor),
                            predictor = gsub('trimester', 'Trimester', predictor),
                            predictor = gsub('this.pregnancy.problems', 'Problems with this Pregnancy', predictor),
                            predictor = gsub('surgery', 'Surgery in previous 4 weeks', predictor),
                            predictor = gsub('thrombo', 'Known Thrombophilia', predictor),
                            predictor = gsub('multiple.preg', 'Multiple Pregnancy', predictor),
                            predictor = gsub('travel', 'Long-haul travel during pregnancy', predictor),
                            predictor = gsub('immobil', '<3 days Immobility/bed rest during pregnancy', predictor),
                            predictor = gsub('ecg', 'ECG', predictor),
                            predictor = gsub('xray', 'X-ray', predictor),
                            predictor = gsub('aprothombin', 'Aprothombin', predictor),
                            predictor = gsub('prothombin.time', 'Prothombin (Time)', predictor),
                            predictor = gsub('clauss.fibrinogen', 'Clauss Fibrinogen', predictor),
                            predictor = gsub('ddimer.innovan', 'D-Dimer (Innovan)', predictor),
                            predictor = gsub('ddimer.elisa', 'D-Dimer (ELISA)', predictor),
                            predictor = gsub('thrombin.generation.lag.time', 'Thrombin Generation (Lag Time)', predictor),
                            predictor = gsub('thrombin.generation.endogenous.potential', 'Thrombin Generation (Endogenous Potential)', predictor),
                            predictor = gsub('thrombin.generation.peak', 'Thrombin Generation (Peak)', predictor),
                            predictor = gsub('thrombin.generation.time.to.peak', 'Thrombin Generation (Time to Peak)', predictor),
                            predictor = gsub('plasmin.antiplasmin', 'Plasmin (Antiplasmin)', predictor),
                            predictor = gsub('prothrombin.fragments', 'Prothombin Fragments', predictor),
                            predictor = gsub('soluble.tissue.factor', 'Soluble Tissue Factor', predictor),
                            predictor = gsub('troponin', 'Troponin', predictor),
                            predictor = gsub('natriuertic.peptide', 'Natriuertic Peptide', predictor),
                            predictor = gsub('medical.complication', 'Medical Complication (Delphi reviewed)', predictor),
                            predictor = gsub('obstetric.complication', 'Obstetric Complication (Delphi reviewed)', predictor),
                            predictor = gsub('mrproanp', 'MRproANP', predictor))
    results$table <- results$table[c('predictor', 'levels', 'No PE', 'PE')]
    ## Meaningful label for predictor
    ## Filter the data frame, need to remove all Non-recruited and
    ## those who can not be classified as PE/No PE
    results$df <- dplyr::filter_(df, !is.na(classification)) %>%
                  dplyr::select_(.dots = c(classification, predictor)) %>%
                  mutate(obs = rownames(.),
                         use = complete.cases(.)) %>%
                  dplyr::filter(use == TRUE) %>%
                  dplyr::select(-use)
    results$df$obs <- rownames(results$df)
    ## Test the model
    results$fitted <- glm(.formula,
                          data   = results$df,
                          family = 'binomial')
    ## Obtain Confidence Intervals
    results$ci <- confint(results$fitted) %>%
                  as.data.frame() %>%
                  mutate(term = rownames(.),
                         model = model)
    names(results$ci) <- gsub('2.5 %',  'lci', names(results$ci))
    names(results$ci) <- gsub('97.5 %', 'uci', names(results$ci))
    ## Use Broom to sweep up/tidy the results
    results$tidied    <- broom::tidy(results$fitted)
    ## Merge in the CI
    results$tidied <- left_join(results$tidied,
                                results$ci)
    results$tidied <- mutate(results$tidied,
                             term = gsub('age.catYoung', 'Age (Young)', term),
                             term = gsub('age.catOld', 'Age (Old)', term),
                             term = gsub('age', 'Age (Continuous)', term),
                             term = gsub('smokinggave up prior to pregnancy', 'Ex-smoker (Prior)', term),
                             term = gsub('smokinggave up during pregnancy', 'Ex-smoker (During)', term),
                             term = gsub('smokingcurrent', 'Current Smoker', term),
                             term = gsub('smoking.catNon-smoker', 'Smoking (Binary) : Non-Smoker', term),
                             term = gsub('smoking.Smoker', 'Smoking (Binary) : Smoker', term),
                             term = gsub('temperature.catLow', 'Temperature (Low)', term),
                             term = gsub('temperature.catHigh', 'Temperature (High)', term),
                             term = gsub('temperature', 'Temperature (Continuous)', term),
                             term = gsub('bp.diastolic.catLow', 'Diastolic (Low)', term),
                             term = gsub('bp.diastolic.catHigh', 'Diastolic (High)', term),
                             term = gsub('bp.diastolic', 'Diastolic (Continuous)', term),
                             term = gsub('bp.systolic.catLow', 'Systolic (Low)', term),
                             term = gsub('bp.systolic.catHigh', 'Systolic (High)', term),
                             term = gsub('bp.systolic', 'Systolic (Continuous)', term),
                             term = gsub('o2.saturation.catLow', 'O2 Saturation (Low)', term),
                             term = gsub('o2.saturation.catHigh', 'O2 Saturation (High)', term),
                             term = gsub('o2.saturation', 'O2 Saturation (Continuous)', term),
                             term = gsub('respiratory.rate.catLow', 'Respiratory Rate (Low)', term),
                             term = gsub('respiratory.rate.catHigh', 'Respiratory Rate (High)', term),
                             term = gsub('respiratory.rate', 'Respiratory Rate (Continuous)', term),
                             term = gsub('heart.rate.catLow', 'Heart Rate (Low)', term),
                             term = gsub('heart.rate.catHigh', 'Heart Rate (High)', term),
                             term = gsub('heart.rate', 'Heart Rate (Continuous)', term),
                             term = gsub('bmi.catLow', 'BMI (Low)', term),
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
                             term = gsub('medical.complicationYes', 'Medical Complication (Delphi reviewed)', term),
                             term = gsub('medical.complicationNo', 'Medical Complication (Delphi reviewed)', term),
                             term = gsub('obstetric.complicationYes', 'Obstetric Complication (Delphi reviewed)', term),
                             term = gsub('obstetric.complicationNo', 'Obstetric Complication (Delphi reviewed)', term),
                             term = gsub('pregnancies.under.cat>= 1 previous pregnancy < 24 weeks', '>= 1 Pregnancy < 24 weeks', term,),
                             term = gsub('pregnancies.under.catNo previous pregnancies < 24 weeks', 'No Pregnancy < 24 weeks', term),
                             term = gsub('pregnancies.over.cat>= 1 previous pregnancy > 24 weeks', '>= 1 Pregnancy > 24 weeks', term),
                             term = gsub('pregnancies.over.catNo previous pregnancies > 24 weeks', 'No Pregnancy > 24 weeks', term),
                             term = gsub('pregnancies.under', 'Pregnancies < 24 weeks (Continuous)', term),
                             term = gsub('pregnancies.over', 'Pregnancies > 24 weeks (Continuous)', term),
                             term = gsub('prev.preg.problemNo', 'No Previous Pregnancy Problems', term),
                             term = gsub('prev.preg.problemYes', 'Previous Pregnancy Problems', term),
                             term = gsub('history.thrombosisNo', 'No Family History of Thrombosis', term),
                             term = gsub('history.thrombosisYes', 'Family History of Thrombosis', term),
                             term = gsub('history.veinsNo', 'No History of Varicose Veins', term),
                             term = gsub('history.veinsYes', 'History of Varicose Veins', term),
                             term = gsub('history.iv.drugNo', 'No History of IV Drug use', term),
                             term = gsub('history.iv.drugYes', 'History of IV Drug use', term),
                             term = gsub('thrombosisNo', 'NoHistory of Thrombosis', term),
                             term = gsub('thrombosisYes', 'History of Thrombosis', term),
                             term = gsub('trimester1st Trimester', '1st Trimester', term),
                             term = gsub('trimester2nd Trimester', '2nd Trimester', term),
                             term = gsub('trimester3rd Trimester', '3rd Trimester', term),
                             term = gsub('trimesterPost-Partum', 'Post-Partum', term),
                             term = gsub('cesareanCesarean', 'Cesarean', term),
                             term = gsub('cesareanNo Cesarean', 'No Cesarean', term),
                             term = gsub('preg.postPostpartum', 'Post-partum', term),
                             term = gsub('preg.postPregnant', 'Pregnant', term),
                             term = gsub('existing.medicalNo', 'No Exiting Medical', term),
                             term = gsub('existing.medicalYes', 'Exiting Medical', term),
                             term = gsub('injuryNo', 'No Injury', term),
                             term = gsub('injuryYes', 'Injury', term),
                             term = gsub('this.pregnancy.problemsYes', 'Problems with this Pregnancy', term),
                             term = gsub('this.pregnancy.problemsNo', 'No Problems with this Pregnancy', term),
                             term = gsub('surgeryYes', 'Surgery in previous 4 weeks', term),
                             term = gsub('surgeryNo', 'No Surgery in previous 4 weeks', term),
                             term = gsub('thromboYes', 'Known Thrombophilia', term),
                             term = gsub('thromboNo', 'No Known Thrombophilia', term),
                             term = gsub('multiple.pregYes', 'Multiple Pregnancy', term),
                             term = gsub('multiple.pregNo', 'No Multiple Pregnancy', term),
                             term = gsub('travelYes', 'Long-haul travel during pregnancy', term),
                             term = gsub('travelNo', 'No Long-haul travel during pregnancy', term),
                             term = gsub('immobilYes', '>=3 days Immobility/bed rest during pregnancy', term),
                             term = gsub('immobilNo', '<3 days Immobility/bed rest during pregnancy', term),
                             term = gsub('ecgNormal ECG', 'ECG : Normal', term),
                             term = gsub('ecgAbnormal ECG', 'ECG : Abnormal', term),
                             term = gsub('ecgNot performed ECG', 'ECG : Not Performed', term),
                             term = gsub('ecg.catAbnormal ECG', 'ECG (Binary) : Abnormal', term),
                             term = gsub('ecg.catNormal ECG', 'ECG (Binary) : Abnormal', term),
                             term = gsub('xrayNormal', 'X-ray : Normal', term),
                             term = gsub('xrayAbnormal', 'X-ray : Abnormal', term),
                             term = gsub('xrayNot performed', 'X-ray : Not Performed', term),
                             term = gsub('xray.catAbnormal X-Ray', 'X-ray (Binary) : Abnormal', term),
                             term = gsub('aptt', 'APTT', term),
                             term = gsub('prothombin.time', 'Prothombin (Time)', term),
                             term = gsub('clauss.fibrinogen', 'Clauss Fibrinogen', term),
                             term = gsub('ddimer.innovance', 'D-Dimer (Innovance)', term),
                             term = gsub('ddimer.elisa', 'D-Dimer (ELISA)', term),
                             term = gsub('thrombin.generation.lag.time', 'Thrombin Generation (Lag Time)', term),
                             term = gsub('thrombin.generation.endogenous.potential', 'Thrombin Generation (Endogenous Potential)', term),
                             term = gsub('thrombin.generation.peak', 'Thrombin Generation (Peak)', term),
                             term = gsub('thrombin.generation.time.to.peak', 'Thrombin Generation (Time to Peak)', term),
                             term = gsub('plasmin.antiplasmin', 'Plasmin (Antiplasmin)', term),
                             term = gsub('prothrombin.fragments', 'PF 1 + 2', term),
                             term = gsub('tissue.factor', 'Tissue Factor', term),
                             term = gsub('troponin', 'Troponin', term),
                             term = gsub('nppb', 'NPPB', term),
                             term = gsub('mrproanp', 'MRProANP', term))
    ## Get the predicted response out
    results$augmented <- broom::augment(results$fitted, type.predict = 'response')
    results$glance    <- broom::glance(results$fitted)
    ## Add the predictor to each to make it easier combining
    results$tidied$model    <- model
    results$augmented$model <- model
    results$glance$model    <- model
    ## Get predicted probabilities
    ## results$predicted <- results$fitted %>% predict() %>% as.data.frame()
    ## names(results$predicted) <- c('pred')
    ## results$predicted$obs <- rownames(results$predicted)
    ## results$predicted <- merge(dplyr::select(results$df, obs, pe),
    ##                            results$predicted,
    ##                            by = c('obs'))
    ## results$predicted <- cbind(predict(results$fitted, type = 'response'),
    ##                            results$fitted$y) %>%
    ##                      as.data.frame()
    ## names(results$predicted) <- c('predicted', 'pe')
    ## Extract these from the 'augmented' data frame
    results$predicted <- dplyr::select_(results$augmented, classification, '.fitted')
    if(length(predictor) == 1){
        results$predicted$name <- predictor
    }
    else{
        results$predicted$name <- model
    }
    results$predicted$term <- model
    names(results$predicted) <- gsub('.fitted', 'M', names(results$predicted))
    names(results$predicted) <- gsub(classification, 'D', names(results$predicted))
    ## Return ROC curve for this model
    results$roc <- ggplot(results$predicted,
                          aes(d = D, m = M)) +
                   geom_roc() +
                   guides(guide = guide_legend(title = '')) +
                   ggtitle(paste0('ROC curve for ', model)) +
                   style_roc() + theme_bw()
    ## Add the AUC
    results$auc <- calc_auc(results$roc)
    results$roc <- results$roc +
                   annotate('text', x = 0.75, y = 0.25,
                            label = paste0('AUC = ', round(results$auc$AUC, 3)))
    ## ToDo 20170220 - Calculate sensitivity, specificity, PPV/NPV
    return(results)
}
