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
#' @param alpha Elasticnet mixing parameter, \code{1} is the LASSO penalty, \code{0} is the Ridge-regression penalty.
#' @param model Name/label of your model.
#' @param relevel Reference level for logistic regression if different from default.
#' @param exclude Vector of \code{screening}  to exclude.
#' @param exclude.non.recuirted Logical indicator of whether to exclude \code{group == 'Non recruited'}.
#' @param exclude.dvt Logical indicator of whether to exclude \code{group == 'Diagnosed DVT'}.
#' @param exclude.anti.coag Logical indicator of whether to exclude individuals who had received anti-coagulents prior to blood samples being taken (default is \code{FALSE} and it is only relevant to set to \code{TRUE} when analysing certain biomarkers).
#' @param legend Logical indicator of whether to include a legend in the LASSO normalisation plot.
#'
#'
#' @export
dipep_glmnet <- function(df           = dipep,
                         classification  = 'first.st',
                         predictor       = c('age'),
                         alpha           = 1,
                         model           = NULL,
                         exclude         = NULL,
                         exclude.non.recruited = TRUE,
                         exclude.dvt       = TRUE,
                         exclude.anti.coag = FALSE,
                         legend            = TRUE,
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
    ## Remove those who are 'Exclude' for the current classification
    df <- dplyr::filter_(df, !is.na(classification))
    ## Exclude those who are not classified as PE/No PE by
    ## the specified classification
    ## TODO 2017-02-17 : Why doesn dplyr::filter_(df, !is.na(classification)) not work???
    ## if(classification == 'first.st'){
    ##     df <- dplyr::filter(df, !is.na(first.st))
    ## }
    ## else if(classification == 'second.st'){
    ##     df <- dplyr::filter(df, !is.na(second.st))
    ## }
    ## else if(classification == 'third.st'){
    ##     df <- dplyr::filter(df, !is.na(third.st))
    ## }
    ## else if(classification == 'fourth.st'){
    ##     df <- dplyr::filter(df, !is.na(fourth.st))
    ## }
    ## else if(classification == 'primary.dm'){
    ##     df <- dplyr::filter(df, !is.na(primary.dm))
    ## }
    ## else if(classification == 'secondary.dm'){
    ##     df <- dplyr::filter(df, !is.na(secondary.dm))
    ## }
    ## Remove biomarker data for those on anticoagulents
    if(exclude.anti.coag == TRUE){
        df <- dplyr::filter(df, exclude.anti.coag == 'No')
    }
    ## Build the formula
    .formula <- reformulate(response = classification,
                            termlabels = predictor)
    ## Fit model
    results$lasso <- glmnetUtils::glmnet(data = df,
                                         .formula,
                                         family = 'binomial',
                                         alpha  = alpha)
    ## Cross validation
    ## Forced to Leave One Out by virtue of setting folds to number of observations (rows)
    results$lasso.cv <- glmnetUtils::cv.glmnet(data = df,
                                               .formula,
                                               family = 'binomial',
                                               alpha  = alpha,
                                               nfolds = nrow(df))
    ## Cross validation maximising AUC
    results$lasso.cv.auc <- glmnetUtils::cv.glmnet(data = df,
                                                   .formula,
                                                   family  = 'binomial',
                                                   alpha  = alpha,
                                                   type.measure = 'auc',
                                                   nfolds = nrow(df))
    ## Obtain thresholds for lambda (1 SE and Min)
    results$lasso.cv.lambda.1se <- which(results$cv.lasso$lambda == results$cv.lasso$lambda.1se)
    results$lasso.cv.lambda.min <- which(results$cv.lasso$lambda == results$cv.lasso$lambda.min)
    ## Plot LASSO
    results$lasso.plot <- autoplot(results$lasso) +
                          geom_vline(xintercept = results$lasso.cv.lambda.1se) +
                          geom_vline(xintercept = results$lasso.cv.lambda.min, lty = 2) +
                          theme_bw()
    if(legend == FALSE){
        results$lasso.plot <- results$lasso.plot + guides(colour = FALSE)
    }
    ## Plot Cross-Validation
    results$lasso.cv.plot      <- autoplot(results$lasso.cv) + theme_bw()
    ## Plot Cross-Validation AUC
    results$lasso.cv.auc.plot <- autoplot(results$lasso.cv.auc) + theme_bw()
    ## Extract Coefficients for different lambda
    coef.extract <- function(x = results$lasso.cv,
                             s = 'lambda.1se'){
        x <- coef(x, s = s) %>% as.matrix() %>% as.data.frame()
        x$term <- rownames(x)
        rownames(x) <- NULL
        x <- mutate(x,
                    term = gsub('age.catYoung', 'Age (Young)', term),
                    term = gsub('age.catOld', 'Age (Old)', term),
                    term = gsub('age', 'Age (Continuous)', term),
                    term = gsub('smokinggave up prior to pregnancy', 'Ex-smoker (Prior)', term),
                    term = gsub('smokinggave up during pregnancy', 'Ex-smoker (During)', term),
                    term = gsub('smokingcurrent', 'Current Smoker', term),
                    term = gsub('smoking.catNon-smoker', 'Smoking (Binary) : Non-Smoker', term),
                    term = gsub('smoking.Smoker', 'Smoking (Binary) : Smoker', term),
                    term = gsub('temperature.catLow', 'Temperature : Low', term),
                    term = gsub('temperature.catHigh', 'Temperature : High', term),
                    term = gsub('temperature', 'Temperature (Continuous)', term),
                    term = gsub('bp.diastolic.catLow', 'Diastolic : Low', term),
                    term = gsub('bp.diastolic.catHigh', 'Diastolic : High', term),
                    term = gsub('bp.diastolic', 'Diastolic (Continuous)', term),
                    term = gsub('bp.systolic.catLow', 'Systolic  : Low', term),
                    term = gsub('bp.systolic.catHigh', 'Systolic : High', term),
                    term = gsub('bp.systolic', 'Systolic (Continuous)', term),
                    term = gsub('o2.saturation.catLow', 'O2 Saturation : Low', term),
                    term = gsub('o2.saturation.catHigh', 'O2 Saturation : High', term),
                    term = gsub('o2.saturation', 'O2 Saturation (Continuous)', term),
                    term = gsub('respiratory.rate.catLow', 'Respiratory Rate : Low', term),
                    term = gsub('respiratory.rate.catHigh', 'Respiratory Rate : High', term),
                    term = gsub('respiratory.rate', 'Respiratory Rate (Continuous)', term),
                    term = gsub('heart.rate.catLow', 'Heart Rate : Low', term),
                    term = gsub('heart.rate.catHigh', 'Heart Rate : High', term),
                    term = gsub('heart.rate', 'Heart Rate (Continuous)', term),
                    term = gsub('bmi.catLow', 'BMI : Low', term),
                    term = gsub('bmi.catHigh', 'BMI : High', term),
                    term = gsub('bmi', 'BMI (Continuous)', term),
                    ## term = gsub('Ticked', '', term),
                    term = gsub('presenting.features.pleuriticNot Ticked', 'Pleuritic : Not Ticked', term),
                    term = gsub('presenting.features.pleuriticTicked', 'Pleuritic : Ticked', term),
                    term = gsub('presenting.features.non.pleuriticNot Ticked', 'Non-Pleuritic : Not Ticked', term),
                    term = gsub('presenting.features.non.pleuriticTicked', 'Non-Pleuritic : Ticked', term),

                    term = gsub('presenting.features.sob.exertionNot Ticked', 'Shortness of Breath (Exertion) : Not Ticked', term),
                    term = gsub('presenting.features.sob.exertionTicked', 'Shortness of Breath (Exertion) : Ticked', term),
                    term = gsub('presenting.features.sob.restNot Ticked', 'Shortness of Breath (Rest) : Not Ticked', term),
                    term = gsub('presenting.features.sob.restTicked', 'Shortness of Breath (Rest) : Ticked', term),
                    term = gsub('presenting.features.haemoptysisNot Ticked', 'Haemoptysis : Not Ticked', term),
                    term = gsub('presenting.features.haemoptysisTicked', 'Haemoptysis : Ticked', term),
                    term = gsub('presenting.features.coughNot Ticked', 'Cough : Not Ticked', term),
                    term = gsub('presenting.features.coughTicked', 'Cough : Ticked', term),
                    term = gsub('presenting.features.syncopeNot Ticked', 'Syncope : Not Ticked', term),
                    term = gsub('presenting.features.syncopeTicked', 'Syncope :Ticked', term),
                    term = gsub('presenting.features.palpitationsNot Ticked', 'Palpitations : Not Ticked', term),
                    term = gsub('presenting.features.palpitationsTicked', 'Palpitations : Ticked', term),
                    term = gsub('presenting.features.otherNot Ticked', 'Other : Not Ticked', term),
                    term = gsub('presenting.features.otherTicked', 'Other : Ticked', term),
                    term = gsub('pregnancies.under.cat', '>= 1 Pregnancy < 24 weeks', term),
                    term = gsub('pregnancies.over.cat', '>= 1 Pregnancy > 24 weeks', term),
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
                    term = gsub('thrombosisNo', 'No History of Thrombosis', term),
                    term = gsub('thrombosisYes', 'History of Thrombosis', term),
                    term = gsub('trimester1st Trimester', '1st Trimester', term),
                    term = gsub('trimester2nd Trimester', '2nd Trimester', term),
                    term = gsub('trimester3rd Trimester', '3rd Trimester', term),
                    term = gsub('trimesterPost-Partum', 'Post-Partum Trimester', term),
                    term = gsub('cesareanCesarean', 'Cesarean', term),
                    term = gsub('cesareanNo Cesarean', 'No Cesarean', term),
                    term = gsub('preg.postPostpartum', 'Post-partum', term),
                    term = gsub('preg.postPregnant', 'Pregnant', term),
                    term = gsub('existing.medicalNo', 'Exiting Medical : No', term),
                    term = gsub('existing.medicalYes', 'Exiting Medical : Yes', term),
                    term = gsub('injuryNo', 'Injury : No', term),
                    term = gsub('injuryYes', 'Injury : Yes', term),
                    term = gsub('this.pregnancy.problemsYes', 'Problems with this Pregnancy', term),
                    term = gsub('this.pregnancy.problemsNo', 'No Problems with this Pregnancy', term),
                    term = gsub('this.pregnancy.problems.incl.otherYes', 'Problems with this Pregnancy (including Other)', term),
                    term = gsub('this.pregnancy.problems.incl.otherNo', 'No Problems with this Pregnancy (including Other)', term),
                    term = gsub('surgeryYes', 'Surgery in previous 4 weeks : Yes', term),
                    term = gsub('surgeryNo', 'Surgery in previous 4 weeks : No', term),
                    term = gsub('thromboYes', 'Known Thrombophilia : Yes', term),
                    term = gsub('thromboNo', 'Known Thrombophilia : No', term),
                    term = gsub('multiple.pregYes', 'Multiple Pregnancy : Yes', term),
                    term = gsub('multiple.pregNo', 'No Multiple Pregnancy : No', term),
                    term = gsub('travelYes', 'Long-haul travel during pregnancy : Yes', term),
                    term = gsub('travelNo', 'Long-haul travel during pregnancy : No', term),
                    term = gsub('immobilYes', '>=3 days Immobility/bed rest during pregnancy : Yes', term),
                    term = gsub('immobilNo', '>= days Immobility/bed rest during pregnancy : No', term),
                    term = gsub('ecgNormal ECG', 'ECG : Normal', term),
                    term = gsub('ecgAbnormal ECG', 'ECG : Abnormal', term),
                    term = gsub('ecgNot performed', 'ECG : Not Performed', term),
                    term = gsub('ecg.catAbnormal ECG', 'ECG (Binary) : Abnormal', term),
                    term = gsub('ecg.catNormal ECG', 'ECG (Binary) : Normal', term),
                    term = gsub('xrayNormal', 'X-ray : Normal', term),
                    term = gsub('xrayAbnormal', 'X-ray : Abnormal', term),
                    term = gsub('xrayNot performed', 'X-ray : Not Performed', term),
                    term = gsub('xray.catAbnormal X-Ray', 'X-ray (Binary) : Abnormal', term),
                    term = gsub('xray.catNormal X-Ray', 'X-ray (Binary) : Normal', term),
                    term = gsub('aptt', 'APTT', term),
                    term = gsub('prothombin.time', 'Prothombin (Time)', term),
                    term = gsub('clauss.fibrinogen', 'Clauss Fibrinogen', term),
                    term = gsub('ddimer.innovance', 'D-Dimer (Innovance)', term),
                    term = gsub('ddimer.elisa', 'D-Dimer (ELISA)', term),
                    term = gsub('thrombin.generation.lag.time', 'Thrombin Generation (Lag Time)', term),
                    term = gsub('thrombin.generation.endogenous.potential', 'Thrombin Generation (Endogenous Potential)', term),
                    term = gsub('thrombin.generation.peak', 'Thrombin Generation (Peak)', term),
                    term = gsub('thrombin.generation.time.to.peak', 'Thrombin Generation (Time to Peak)', term),
                    term = gsub('plasmin.antiplasmin', 'Plasmin-antiplasmin', term),
                    term = gsub('prothrombin.fragments', 'PF 1 + 2', term),
                    term = gsub('tissue.factor', 'Tissue Factor', term),
                    term = gsub('troponin', 'Troponin', term),
                    term = gsub('bnp', 'BNP', term),
                    term = gsub('mrproanp', 'MRProANP', term),
                    term = gsub('crp', 'C-Reactive Protein', term))
        x <- x[,c('term', '1')]
        names(x) <- c('Term', 'Coefficient')
        return(x)
    }
    results$coef.lambda.1se <- coef.extract(x = results$lasso.cv,
                                            s = 'lambda.1se')
    results$coef.lambda.min <- coef.extract(x = results$lasso.cv,
                                            s = 'lambda.min')
    ## Bind the two together
    results$coef.lambda <- merge(results$coef.lambda.1se,
                                 results$coef.lambda.min,
                                 by      = c('Term'),
                                 all     = TRUE)
    names(results$coef.lambda) <- c('Term', '1 SE', 'Min')
    ## Replace the variable names with meaningful descriptions (pinched from dipep_glm())
    ## Obtain predicted probabilities for each level and convert to data frame
    return(results)
}
