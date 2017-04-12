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
#' @param exclude.missing Exclude individuals flagged as having excessive missing data.
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
                      exclude.missing   = FALSE,
                      ...){
    results <- list()
    ## Remove individuals who are explicitly to be removed
    if(!is.null(exclude)){
        df <- df[!(df$screening %in% exclude),]
        ## df <- dplyr::filter_(df, ('screening' %in% !exclude))
    }
    ## Remove non-recruited, DVT or missing
    if(exclude.non.recruited == TRUE){
        df <- dplyr::filter(df, group != 'Non recruited')
    }
    if(exclude.dvt == TRUE){
        df <- dplyr::filter(df, group != 'Diagnosed DVT')
    }
    ## If DVT are NOT being excluded then this will be biomarker data that is
    ## being analysed and we therefore need to INCLUDE DVT WITH PE
    else{
        df <- df %>%
              mutate(first.st  = as.character(first.st),
                     second.st = as.character(second.st),
                     third.st  = as.character(third.st),
                     fourth.st = as.character(fourth.st)) %>%
              mutate(first.st = case_when(substr(.$screening, 1, 1) == 'D'  ~ 'PE',
                                          substr(.$screening, 1, 1) != 'D'  ~ .$first.st),
                     second.st = case_when(substr(.$screening, 1, 1) == 'D' ~ 'PE',
                                           substr(.$screening, 1, 1) != 'D' ~ .$second.st),
                     third.st = case_when(substr(.$screening, 1, 1) == 'D'  ~ 'PE',
                                          substr(.$screening, 1, 1) != 'D'  ~ .$third.st),
                     fourth.st = case_when(substr(.$screening, 1, 1) == 'D' ~ 'PE',
                                           substr(.$screening, 1, 1) != 'D' ~ .$fourth.st)) %>%
              mutate(first.st = factor(first.st,
                                       levels = c('No PE', 'PE')),
                     second.st = factor(second.st,
                                        levels = c('No PE', 'PE')),
                     third.st = factor(third.st,
                                       levels = c('No PE', 'PE')),
                     fourth.st = factor(fourth.st,
                                        levels = c('No PE', 'PE')))
    }
    if(exclude.missing == TRUE){
        df <- dplyr::filter(df, missing.exclude == FALSE)
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
    else if(classification == 'vte'){
        df <- dplyr::filter(df, !is.na(vte))
    }
    ## Remove biomarker data for those on anticoagulents
    if(exclude.anti.coag == TRUE){
        ## df <- mutate(df,
        ##              prothombin.time                          = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = prothombin.time),
        ##              aptt                                     = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = aptt),
        ##              clauss.fibrinogen                        = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = clauss.fibrinogen),
        ##              ddimer.innovance                         = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = ddimer.innovance),
        ##              ddimer.elisa                             = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = ddimer.elisa),
        ##              d.dimer.cat                              = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = d.dimer.cat),
        ##              d.dimer.gestation.cat                    = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = d.dimer.gestation.cat),
        ##              d.dimer                                  = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = d.dimer),
        ##              thrombin.generation.lag.time             = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = thrombin.generation.lag.time),
        ##              thrombin.generation.endogenous.potential = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = thrombin.generation.endogenous.potential),
        ##              thrombin.generation.peak                 = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = thrombin.generation.peak),
        ##              thrombin.generation.time.to.peak         = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = thrombin.generation.time.to.peak),
        ##              plasmin.antiplasmin                      = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = plasmin.antiplasmin),
        ##              prothrombin.fragments                    = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = prothrombin.fragments),
        ##              tissue.factor                            = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = tissue.factor),
        ##              troponin                                 = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = troponin),
        ##              bnp                                     = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = bnp),
        ##              mrproanp                                 = ifelse(exclude.anti.coag == 'Yes',
        ##                                                                yes = NA,
        ##                                                                no  = mrproanp))
        df <- dplyr::filter(df, exclude.anti.coag == 'No')
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
    results$table <- dplyr::filter_(df, !is.na(predictor)) %>%
                     dplyr::group_by_(classification, predictor) %>%
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
                     mutate(predictor = model,
                            levels    = as.character(levels),
                            levels    = ifelse(levels == 'TRUE',
                                               yes = 'Yes',
                                               no  = levels),
                            levels    = ifelse(levels == 'FALSE',
                                               yes = 'No',
                                               no  = levels),
                            levels    = ifelse(levels == '1' | levels == '4',
                                               yes = 'Yes',
                                               no  = levels),
                            levels    = ifelse(levels == '0',
                                               yes = 'No',
                                               no  = levels),
                            levels    = ifelse(is.na(levels),
                                               yes = 'Missing',
                                               no  = levels)
                            ) %>%
     dplyr::filter(levels != 'Missing')
    if(classification != 'vte'){
        results$table <- results$table[c('predictor', 'levels', 'No PE', 'PE')]
    }
    else{
        results$table <- results$table[c('predictor', 'levels', 'No VTE', 'VTE')]
    }
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
    ## Test the model (converting the grouping factor back to numeric)
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

    results$tidied <- results$tidied %>%
        mutate(term = case_when(.$term == '(Intercept)' ~  '(Intercept)',
                                .$term == 'simplified.age' ~  'Geneva : Age > 35',
                                .$term == 'simplified.prev.dvt.pe' ~ 'Geneva : Previous DVT/PE',
                                .$term == 'simplified.surgery' ~ 'Geneva : Surgery',
                                .$term == 'simplified.neoplasm' ~ 'Geneva : Neoplasm',
                                .$term == 'simplified.lower.limb.unilateral.pain' ~ 'Geneva : Unilateral lower limb pain',
                                .$term == 'simplified.haemoptysis' ~ 'Geneva : Haemoptysis',
                                .$term == 'simplified.heart.rate' ~ 'Geneva : Heart Rate',
                                .$term == 'simplified.pain.palpitations' ~ 'Geneva : Pain on limb palpitation',
                                .$term == 'perc.age' ~ 'PERC : Age (> 35)',
                                .$term == 'perc.heart.rate' ~ 'PERC : Heart Rate',
                                .$term == 'perc.o2' ~ 'PERC : O2 Saturation',
                                .$term == 'perc.prev.dvt.pe' ~ 'PERC : Previous DVT/PE',
                                .$term == 'perc.surgery' ~ 'PERC : Surgery',
                                .$term == 'perc.haemoptysis' ~ 'PERC : Haemoptysis',
                                .$term == 'perc.hormone' ~ 'PERC : Exogenous Estrogen',
                                .$term == 'perc.leg.swelling' ~ 'PERC : Unilateral Leg Swelling',
                                .$term == 'wells.dvt' ~ 'Wells : Clinical DVT',
                                .$term == 'wells.alternative.permissive' ~ 'Wells : PE most likely diagnosis (Permissive)',
                                .$term == 'wells.alternative.strict' ~ 'Wells : PE most likely diagnosis (Strict)',
                                .$term == 'wells.heart.rate' ~ 'Wells : Heart Rate',
                                .$term == 'wells.surgery.immobil' ~ 'Wells : Surgery/Immobility',
                                .$term == 'wells.dvt.pe' ~ 'Wells : Previous DVT/PE',
                                .$term == 'wells.haemoptysis' ~ 'Wells : Haemoptysis',
                                .$term == 'wells.neoplasm' ~ 'Wells : Neoplasm',
                                .$term == 'delphi.haemoptysisTRUE' ~ 'Delphi : Haemoptysis',
                                .$term == 'delphi.pleuriticTRUE' ~ 'Delphi : Pleuritic',
                                .$term == 'delphi.history.dvt.peTRUE' ~ 'Delphi : Previous DVT/PE',
                                .$term == 'delphi.family.historyTRUE' ~ 'Delphi : Family History',
                                .$term == 'delphi.medical.historyTRUE' ~ 'Delphi : Medical History (Hospital Admission, Surgery or Injury)',
                                .$term == 'delphi.medical.history.surgeryTRUE' ~ 'Delphi : Surgery (excl. Cesarean)',
                                .$term == 'delphi.obstetric.complicationTRUE' ~ 'Delphi : Obstetric Complications',
                                .$term == 'delphi.medical.complicationTRUE' ~ 'Delphi : Medical Complications',
                                .$term == 'delphi.gestationTRUE' ~ 'Delphi : Gestation',
                                .$term == 'delphi.bmiTRUE' ~ 'Delphi : BMI > 30',
                                .$term == 'delphi.dvtTRUE' ~ 'Delphi : DVT',
                                .$term == 'delphi.o2.saturationTRUE' ~ 'Delphi : O2 Saturation',
                                .$term == 'delphi.heart.rateTRUE' ~ 'Delphi : Heart Rate',
                                .$term == 'delphi.respiratory.rateTRUE' ~ 'Delphi : Respiratory Rate',
                                .$term == 'age.catYoung' ~ 'Age (Young)',
                                .$term == 'age.catOld' ~ 'Age (Old)',
                                .$term == 'age' ~ 'Age (Continuous)',
                                .$term == 'smokinggave up prior to pregnancy' ~ 'Ex-smoker (Prior)',
                                .$term == 'smokinggave up during pregnancy' ~ 'Ex-smoker (During)',
                                .$term == 'smokingcurrent' ~ 'Current Smoker',
                                .$term == 'smoking.catNon-smoker' ~ 'Smoking (Binary) : Non-Smoker',
                                .$term == 'smoking.Smoker' ~ 'Smoking (Binary) : Smoker',
                                .$term == 'temperature.catLow' ~ 'Temperature (Low)',
                                .$term == 'temperature.catHigh' ~ 'Temperature (High)',
                                .$term == 'temperature' ~ 'Temperature (Continuous)',
                                .$term == 'bp.diastolic.catLow' ~ 'Diastolic (Low)',
                                .$term == 'bp.diastolic.catHigh' ~ 'Diastolic (High)',
                                .$term == 'bp.diastolic' ~ 'Diastolic (Continuous)',
                                .$term == 'bp.systolic.catLow' ~ 'Systolic (Low)',
                                .$term == 'bp.systolic.catHigh' ~ 'Systolic (High)',
                                .$term == 'bp.systolic' ~ 'Systolic (Continuous)',
                                .$term == 'o2.saturation.catLow' ~ 'O2 Saturation (Low)',
                                .$term == 'o2.saturation.catHigh' ~ 'O2 Saturation (High)',
                                .$term == 'o2.saturation' ~ 'O2 Saturation (Continuous)',
                                .$term == 'respiratory.rate.catLow' ~ 'Respiratory Rate (Low)',
                                .$term == 'respiratory.rate.catHigh' ~ 'Respiratory Rate (High)',
                                .$term == 'respiratory.rate' ~ 'Respiratory Rate (Continuous)',
                                .$term == 'heart.rate.catLow' ~ 'Heart Rate (Low)',
                                .$term == 'heart.rate.catHigh' ~ 'Heart Rate (High)',
                                .$term == 'heart.rate' ~ 'Heart Rate (Continuous)',
                                .$term == 'bmi.catLow' ~ 'BMI (Low)',
                                .$term == 'bmi.catHigh' ~ 'BMI (High)',
                                .$term == 'bmi' ~ 'BMI (Continuous)',
                                .$term == 'Ticked' ~ '',
                                .$term == 'presenting.features.pleuriticTicked' ~ 'Presenting : Pleuritic',
                                .$term == 'presenting.features.non.pleuriticTicked' ~ 'Presenting : Non-Pleuritic',
                                .$term == 'presenting.features.sob.exertionTicked' ~ 'Presenting : Shortness of Breath (Exertion)',
                                .$term == 'presenting.features.sob.restTicked' ~ 'Presenting : Shortness of Breath (Rest)',
                                .$term == 'presenting.features.haemoptysisTicked' ~ 'Presenting : Haemoptysis',
                                .$term == 'presenting.features.coughTicked' ~ 'Presenting : Cough',
                                .$term == 'presenting.features.syncopeTicked' ~ 'Presenting : Syncope',
                                .$term == 'presenting.features.palpitationsTicked' ~ 'Presenting : Palpitations',
                                .$term == 'presenting.features.otherTicked' ~ 'Presenting : Other',
                                .$term == 'medical.complicationYes' ~ 'VTE-related Medical Problem',
                                .$term == 'medical.complicationNo' ~ 'No VTE-related Medical Problem',
                                .$term == 'obstetric.complicationYes' ~ 'VTE-related Obstetric Problem',
                                .$term == 'obstetric.complicationYes' ~ 'VTE-related Obstetric Problem',
                                .$term == 'obstetric.complications' ~ 'Other problem with this pregnancy (VTE-related)',
                                .$term == 'pregnancies.under.cat>= 1 previous pregnancy < 24 weeks' ~ '>= 1 Pregnancy < 24 weeks',
                                .$term == 'pregnancies.under.catNo previous pregnancies < 24 weeks' ~ 'No Pregnancy < 24 weeks',
                                .$term == 'pregnancies.over.cat>= 1 previous pregnancy > 24 weeks' ~ '>= 1 Pregnancy > 24 weeks',
                                .$term == 'pregnancies.over.catNo previous pregnancies > 24 weeks' ~ 'No Pregnancy > 24 weeks',
                                .$term == 'pregnancies.under' ~ 'Pregnancies < 24 weeks (Continuous)',
                                .$term == 'pregnancies.over' ~ 'Pregnancies > 24 weeks (Continuous)',
                                .$term == 'prev.preg.problemNo' ~ 'No Previous Pregnancy Problems',
                                .$term == 'prev.preg.problemYes' ~ 'Previous Pregnancy Problems',
                                .$term == 'history.thrombosisNo' ~ 'No Family History of Thrombosis',
                                .$term == 'history.thrombosisYes' ~ 'Family History of Thrombosis',
                                .$term == 'history.veinsNo' ~ 'No History of Varicose Veins',
                                .$term == 'history.veinsYes' ~ 'History of Varicose Veins',
                                .$term == 'history.iv.drugNo' ~ 'No History of IV Drug use',
                                .$term == 'history.iv.drugYes' ~ 'History of IV Drug use',
                                .$term == 'thrombosisNo' ~ 'NoHistory of Thrombosis',
                                .$term == 'thrombosisYes' ~ 'History of Thrombosis',
                                .$term == 'trimester1st Trimester' ~ '1st Trimester',
                                .$term == 'trimester2nd Trimester' ~ '2nd Trimester',
                                .$term == 'trimester3rd Trimester' ~ '3rd Trimester',
                                .$term == 'trimesterPost-Partum' ~ 'Post-Partum',
                                .$term == 'cesareanCesarean' ~ 'Cesarean',
                                .$term == 'cesareanNo Cesarean' ~ 'No Cesarean',
                                .$term == 'preg.postPostpartum' ~ 'Post-partum',
                                .$term == 'preg.postPregnant' ~ 'Pregnant',
                                .$term == 'existing.medicalNo' ~ 'No Exiting Medical',
                                .$term == 'existing.medicalYes' ~ 'Exiting Medical',
                                .$term == 'injuryNo' ~ 'No Injury',
                                .$term == 'injuryYes' ~ 'Injury',
                                .$term == 'this.pregnancy.problemsYes' ~ 'Problems with this Pregnancy',
                                .$term == 'this.pregnancy.problemsNo' ~ 'No Problems with this Pregnancy',
                                .$term == 'this.pregnancy.problems.incl.otherYes' ~ 'Problems with this Pregnancy (including Other)',
                                .$term == 'this.pregnancy.problems.incl.otherNo' ~ 'No Problems with this Pregnancy (including Other)',
                                .$term == 'surgeryYes' ~ 'Surgery in previous 4 weeks',
                                .$term == 'surgeryNo' ~ 'No Surgery in previous 4 weeks',
                                .$term == 'dvt.catYes' ~ 'Clinical signs of DVT',
                                .$term == 'dvt.catNo' ~ 'No clinical signs of DVT',
                                .$term == 'thromboYes' ~ 'Known Thrombophilia',
                                .$term == 'thromboNo' ~ 'No Known Thrombophilia',
                                .$term == 'thrombo.eventYes' ~ 'Thrombotic Event this Pregnancy',
                                .$term == 'thrombo.eventNo' ~ 'No Thrombotic Event this Pregnancy',
                                .$term == 'thromb.eventYes' ~ 'Other Thrombotic event during this pregnancy',
                                .$term == 'thromb.eventNo' ~ 'No Other Thrombotic event during this pregnancy',
                                .$term == 'thromboprophylaxisYes' ~ 'Thromboprophylaxis',
                                .$term == 'thromboprophylaxisNo' ~ 'No Thromboprophylaxis',
                                .$term == 'medical.probsYes' ~ 'Pre-existing medical problems',
                                .$term == 'medical.probsNo' ~ 'No Pre-existing medical problems',
                                .$term == 'multiple.pregYes' ~ 'Multiple Pregnancy',
                                .$term == 'multiple.pregNo' ~ 'No Multiple Pregnancy',
                                .$term == 'medical.probsYes' ~ 'Other Previous Medical Problems',
                                .$term == 'medical.probsNo' ~ 'No Other Previous Medical Problems',
                                .$term == 'travelYes' ~ 'Long-haul travel during pregnancy',
                                .$term == 'travelNo' ~ 'No Long-haul travel during pregnancy',
                                .$term == 'immobilYes' ~ '>=3 days Immobility/bed rest during pregnancy',
                                .$term == 'immobilNo' ~ '<3 days Immobility/bed rest during pregnancy',
                                .$term == 'ecgNormal ECG' ~ 'ECG : Normal',
                                .$term == 'ecgAbnormal ECG' ~ 'ECG : Abnormal',
                                .$term == 'ecgNot performed ECG' ~ 'ECG : Not Performed',
                                .$term == 'ecg.catAbnormal ECG' ~ 'ECG (Binary) : Abnormal',
                                .$term == 'ecg.catNormal ECG' ~ 'ECG (Binary) : Abnormal',
                                .$term == 'ecg.peNormal / Not performed / Missing' ~ 'Normal / Not Performed / Missing ECG',
                                .$term == 'ecg.peAbnormal - PE' ~ 'PE related ECG abnormality',
                                .$term == 'xrayNormal' ~ 'X-ray : Normal',
                                .$term == 'xrayAbnormal' ~ 'X-ray : Abnormal',
                                .$term == 'xrayNot performed' ~ 'X-ray : Not Performed',
                                .$term == 'xray.catAbnormal X-Ray' ~ 'X-ray (Binary) : Abnormal',
                                .$term == 'xray.peNormal / Not performed / Missing' ~ 'Normal / Not Performed / Missing CXR',
                                .$term == 'xray.peAbnormal - PE' ~ 'PE Related CXR abnormality',
                                .$term == 'xray.peAbnormal - Other' ~ 'Other CXR abnormality',
                                .$term == 'aptt.catNormal' ~ 'APTT : Normal',
                                .$term == 'aptt.catAbnormal' ~ 'APTT : Abnormal',
                                .$term == 'aptt' ~ 'APTT',
                                .$term == 'prothombin.timeNormal' ~ 'Prothombin (Time)',
                                .$term == 'prothombin.timeAbnormal' ~ 'Prothombin (Time)',
                                .$term == 'prothombin.time' ~ 'Prothombin (Time)',
                                .$term == 'clauss.fibrinogenNormal' ~ 'Clauss Fibrinogen',
                                .$term == 'clauss.fibrinogenAbnormal' ~ 'Clauss Fibrinogen',
                                .$term == 'clauss.fibrinogen' ~ 'Clauss Fibrinogen',
                                .$term == 'ddimer.innovanceNormal' ~ 'D-Dimer (Innovance)',
                                .$term == 'ddimer.innovanceAbnormal' ~ 'D-Dimer (Innovance)',
                                .$term == 'ddimer.innovance' ~ 'D-Dimer (Innovance)',
                                .$term == 'ddimer.elisaNormal' ~ 'D-Dimer (ELISA)',
                                .$term == 'ddimer.elisaAbnormal' ~ 'D-Dimer (ELISA)',
                                .$term == 'ddimer.elisa' ~ 'D-Dimer (ELISA)',
                                .$term == 'd.dimer.catNormal' ~ 'D-Dimer (Hospital) : Normal',
                                .$term == 'd.dimer.catAbnormal' ~ 'D-Dimer (Hospital) : Abnormal',
                                .$term == 'd.dimer.gestation.catNormal' ~ 'D-Dimer (Hospital Gestation Specific) : Normal',
                                .$term == 'd.dimer.gestation.catAbnormal' ~ 'D-Dimer (Hospital Gestation Specific) : Abnormal',
                                .$term == 'd.dimer' ~ 'D-Dimer (Hospital Continuous)',
                                .$term == 'thrombin.generation.lag.timeNormal' ~ 'Thrombin Generation (Lag Time) : Normal',
                                .$term == 'thrombin.generation.lag.timeAbnormal' ~ 'Thrombin Generation (Lag Time) : Abnormal',
                                .$term == 'thrombin.generation.lag.time' ~ 'Thrombin Generation (Lag Time)',
                                .$term == 'thrombin.generation.endogenous.potentialNormal' ~ 'Thrombin Generation (Endogenous Potential) : Normal',
                                .$term == 'thrombin.generation.endogenous.potentialAbnormal' ~ 'Thrombin Generation (Endogenous Potential) : Abnormal',
                                .$term == 'thrombin.generation.endogenous.potential' ~ 'Thrombin Generation (Endogenous Potential)',
                                .$term == 'thrombin.generation.peakNormal' ~ 'Thrombin Generation (Peak) : Normal',
                                .$term == 'thrombin.generation.peakAbnormal' ~ 'Thrombin Generation (Peak) : Abnormal',
                                .$term == 'thrombin.generation.peak' ~ 'Thrombin Generation (Peak)',
                                .$term == 'thrombin.generation.time.to.peakNormal' ~ 'Thrombin Generation (Time to Peak) : Normal',
                                .$term == 'thrombin.generation.time.to.peakAbnormal' ~ 'Thrombin Generation (Time to Peak) : Abnormal',
                                .$term == 'thrombin.generation.time.to.peak' ~ 'Thrombin Generation (Time to Peak)',
                                .$term == 'plasmin.antiplasminNormal' ~ 'Plasmin-antiplasmin : Normal',
                                .$term == 'plasmin.antiplasminAbnormal' ~ 'Plasmin-antiplasmin : Abnormal',
                                .$term == 'plasmin.antiplasmin' ~ 'Plasmin-antiplasmin',
                                .$term == 'prothrombin.fragmentsNormal' ~ 'PF 1 + 2 : Normal',
                                .$term == 'prothrombin.fragmentsAbnormal' ~ 'PF 1 + 2 : Abnormal',
                                .$term == 'prothrombin.fragments' ~ 'PF 1 + 2',
                                .$term == 'tissue.factorNormal' ~ 'Tissue Factor : Normal',
                                .$term == 'tissue.factorAbnormal' ~ 'Tissue Factor : Abnormal',
                                .$term == 'tissue.factor' ~ 'Tissue Factor',
                                .$term == 'troponinNormal' ~ 'Troponin : Normal',
                                .$term == 'troponinAbnormal' ~ 'Troponin : Abnormal',
                                .$term == 'troponin' ~ 'Troponin',
                                .$term == 'bnpNormal' ~ 'BNP : Normal',
                                .$term == 'bnpAbnormal' ~ 'BNP : Abnormal',
                                .$term == 'bnp' ~ 'BNP',
                                .$term == 'mrproanpNormal' ~ 'MRProANP : Normal',
                                .$term == 'mrproanpAbnormal' ~ 'MRProANP : Abnormal',
                                .$term == 'mrproanp' ~ 'MRProANP',
                                .$term == '\\.cat' ~ ''))
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
    results$predicted <- dplyr::select_(results$augmented, classification, predictor, '.fitted')
    if(length(predictor) == 1){
        results$predicted$name <- predictor
    }
    else{
        results$predicted$name <- model
    }
    results$predicted$term <- model
    names(results$predicted) <- gsub('.fitted', 'M', names(results$predicted))
    names(results$predicted) <- gsub(classification, 'D', names(results$predicted))
    names(results$predicted) <- gsub(predictor, 'predictor', names(results$predicted))
    ## ## Convert the observed predictor to a binary 0/1
    ## if(typeof(results$predicted))
    outcome.levels <- results$predicted$predictor %>% table() %>% length()
    if(outcome.levels == 2){
        results$predicted <- results$predicted %>%
                             mutate(predictor = (predictor %>% as.numeric()) - 1,
                                    predictor = case_when(.$predictor == 0 ~ 0,
                                                          .$predictor != 0 ~ 1))
    }
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
