#' Wrapper for running glmnet() on Dipep
#'
#' @description Wrapper for running glmnet() on Dipep
#'
#' @details
#'
#' This wrapper runs LASSO regression models for a given predictor variable
#' and returns the fitted model (for combining using broom()) along with the
#' predicted values for each step and associated ROC curves and summary statistics.
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
#' @param exclude.missing Exclude individuals flagged as having excessive missing data.
#' @param legend Logical indicator of whether to include a legend in the LASSO normalisation plot.
#' @param threshold Threshold for dichotomisation of predicted probabilities (by default \code{0.5} is used for classification so responses match class, this permits alternative thresholds to be passed to \code{dipep_roc()}).
#'
#'
#' @export
dipep_glmnet_orig <- function(df              = dipep,
                              classification  = 'first.st',
                              predictor       = 'age',
                              alpha           = 1,
                              model           = NULL,
                              relevel         = NULL,
                              exclude         = NULL,
                              exclude.non.recruited = TRUE,
                              exclude.dvt       = TRUE,
                              exclude.anti.coag = FALSE,
                              exclude.missing   = FALSE,
                              legend            = FALSE,
                              threshold         = 0.5,
                              ...){
    results <- list()
    ## Remove individuals who are explicitly to be removed
    if(!is.null(exclude)){
        df <- df[!(df$screening %in% exclude),]
    }
    ## Remove non-recruited, DVT or missing
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
        df <- dplyr::filter(df, exclude.anti.coag == 'No')
    }
    ## Remove those who are 'Exclude' for the current classification
    df <- dplyr::filter_(df, !is.na(classification))
    ## ANY missing values in the predictors mean that the observation needs
    ## removing as glmnet() doesn't permit missing observations
    ## Vector of outcomes based on status
    if(classification == 'first.st'){
        df <- dplyr::filter(df, !is.na(first.st)) %>%
              subset(select = c('first.st', predictor)) %>%
              dplyr::filter(complete.cases(.))
        y <- ifelse(df$first.st == 'PE', 1, 0)
    }
    else if(classification == 'second.st'){
        df <- dplyr::filter(df, !is.na(second.st)) %>%
              subset(select = c('second.st', predictor)) %>%
              dplyr::filter(complete.cases(.))
        y <- ifelse(df$second.st == 'PE', 1, 0)
    }
    else if(classification == 'third.st'){
        df <- dplyr::filter(df, !is.na(third.st)) %>%
              subset(select = c('third.st', predictor)) %>%
              dplyr::filter(complete.cases(.))
        y <- ifelse(df$third.st == 'PE', 1, 0)
    }
    else if(classification == 'fourth.st'){
        df <- dplyr::filter(df, !is.na(fourth.st)) %>%
              subset(select = c('fourth.st', predictor)) %>%
              dplyr::filter(complete.cases(.))
        y <- ifelse(df$fourth.st == 'PE', 1, 0)
    }
    else if(classification == 'primary.dm'){
        df <- dplyr::filter(df, !is.na(primary.dm)) %>%
              subset(select = c('primary.dm', predictor)) %>%
              dplyr::filter(complete.cases(.))
        y <- ifelse(df$primary.dm == 'PE', 1, 0)
    }
    else if(classification == 'secondary.dm'){
        df <- dplyr::filter(df, !is.na(secondary.dm)) %>%
              subset(select = c('secondary.dm', predictor)) %>%
              dplyr::filter(complete.cases(.))
        y <- ifelse(df$secondary.dm == 'PE', 1, 0)
    }
    # Matrix of predictors
    x <- subset(df, select = predictor) %>% data.matrix()
    ## Bind the response vector and predictor vector into a data frame
    results$x  <- x
    results$y  <- y
    results$df <- rbind(y, x) %>%
                  as.data.frame()
    ## Run LASSO
    ## ToDo 2017-03-08 - How to exclude instances where there is some missing data?
    results$lasso        <- glmnet(y = y,
                                   x = x,
                                   family = 'binomial',
                                   alpha = alpha)
    results$lasso.tidy   <- broom::tidy(results$lasso)
    results$lasso.glance <- broom::glance(results$lasso)
    ## Cross Validate
    results$lasso.cv        <- cv.glmnet(y = y,
                                         x = x,
                                         family = 'binomial',
                                         alpha = alpha)
    results$lasso.cv.tidy   <- broom::tidy(results$lasso.cv)
    results$lasso.cv.glance <- broom::glance(results$lasso.cv)
    ## Cross Validate using AUC
    results$lasso.cv.auc        <- cv.glmnet(y = y,
                                             x = x,
                                             family = 'binomial',
                                             alpha = alpha)
    results$lasso.cv.auc.tidy   <- broom::tidy(results$cv_lasso)
    results$lasso.cv.auc.glance <- broom::glance(results$cv_lasso)
    ## Obtain thresholds for lambda (1 SE and Min)
    results$lasso.cv.lambda.1se <- which(results$cv.lasso$lambda == results$cv.lasso$lambda.1se)
    results$lasso.cv.lambda.min <- which(results$cv.lasso$lambda == results$cv.lasso$lambda.min)
    ## Plot LASSO
    results$lasso.plot <- autoplot(results$lasso) +
                          ## geom_vline(xintercept = results$lasso.cv.glance$lambda.1se) +
                          ## geom_vline(xintercept = results$lasso.cv.lambda.min, lty = 2) +
                          theme_bw()
    if(legend == FALSE){
        results$lasso.plot <- results$lasso.plot + guides(colour = FALSE)
    }
    ## Plot Cross-Validation
    results$lasso.cv.plot     <- autoplot(results$lasso.cv) +
                                 scale_x_reverse() + theme_bw()
    ## Plot Cross-Validation AUC
    results$lasso.cv.auc.plot <- autoplot(results$lasso.cv.auc) +
                                 scale_x_reverse() + theme_bw()
        ## Extract Coefficients for different lambda
    coef.extract <- function(x = results$lasso.cv,
                             s = 'lambda.1se'){
        x <- coef(x, s = s) %>% as.matrix() %>% as.data.frame()
        x$term <- rownames(x)
        rownames(x) <- NULL
        x <- mutate(x,
                    term = gsub('age.cat', 'Age (Categorised)', term),
                    term = gsub('age', 'Age (Continuous)', term),
                    term = gsub('smoking', 'Smoking (As Recorded)', term),
                    term = gsub('smoking.cat', 'Smoking (Binary)', term),
                    term = gsub('temperature.cat', 'Temperature (Binary)', term),
                    term = gsub('temperature', 'Temperature (Continuous)', term),
                    term = gsub('bp.diastolic.cat', 'Diastolic (Binary)', term),
                    term = gsub('bp.diastolic', 'Diastolic (Continuous)', term),
                    term = gsub('bp.systolic.cat', 'Systolic  (Binary)', term),
                    term = gsub('bp.systolic', 'Systolic (Continuous)', term),
                    term = gsub('o2.saturation.cat', 'O2 Saturation (Binary)', term),
                    term = gsub('o2.saturation', 'O2 Saturation (Continuous)', term),
                    term = gsub('respiratory.rate.cat', 'Respiratory Rate (Binary)', term),
                    term = gsub('heart.rate.cat', 'Heart Rate (Binary)', term),
                    term = gsub('heart.rate', 'Heart Rate (Continuous)', term),
                    term = gsub('bmi.cat', 'BMI (Binary)', term),
                    term = gsub('bmi', 'BMI (Continuous)', term),
                    ## term = gsub('Ticked', '', term),
                    term = gsub('presenting.features.pleuritic', 'Pleuritic', term),
                    term = gsub('presenting.features.non.pleuritic', 'Non-Pleuritic', term),
                    term = gsub('presenting.features.sob.exertion', 'Shortness of Breath (Exertion)', term),
                    term = gsub('presenting.features.sob.rest', 'Shortness of Breath (Rest)', term),
                    term = gsub('presenting.features.haemoptysis', 'Haemoptysis', term),
                    term = gsub('presenting.features.cough', 'Cough', term),
                    term = gsub('presenting.features.syncope', 'Syncope', term),
                    term = gsub('presenting.features.palpitations', 'Palpitations', term),
                    term = gsub('presenting.features.other', 'Other', term),
                    term = gsub('pregnancies.under.cat', '>= 1 Pregnancy < 24 weeks', term),
                    term = gsub('pregnancies.over.cat', '>= 1 Pregnancy > 24 weeks', term),
                    term = gsub('pregnancies.under', 'Pregnancies < 24 weeks (Continuous)', term),
                    term = gsub('pregnancies.over', 'Pregnancies > 24 weeks (Continuous)', term),
                    term = gsub('prev.preg.problem', 'Previous Pregnancy Problems', term),
                    term = gsub('history.thrombosis', 'Family History of Thrombosis', term),
                    term = gsub('history.veins', 'History of Varicose Veins', term),
                    term = gsub('history.iv.drug', 'History of IV Drug use', term),
                    term = gsub('thrombosis', 'History of Thrombosis', term),
                    term = gsub('trimester', 'Trimester', term),
                    term = gsub('cesarean', 'Cesarean', term),
                    term = gsub('preg.post', 'Pregnant/Post-partum', term),
                    term = gsub('existing.medical', 'Exiting Medical', term),
                    term = gsub('obstetric.complications', 'Other problem with this pregnancy (VTE-related)', term),
                    term = gsub('injury', 'Injury', term),
                    term = gsub('this.pregnancy.problems.incl.other', 'Problems with this Pregnancy (Including Other)', term),
                    term = gsub('this.pregnancy.problems', 'Problems with this Pregnancy', term),
                    term = gsub('surgery', 'Surgery in previous 4 weeks', term),
                    term = gsub('thrombo', 'Known Thrombophilia', term),
                    term = gsub('multiple.preg', 'Multiple Pregnancy', term),
                    term = gsub('travel', 'Long-haul travel during pregnancy', term),
                    term = gsub('immobil', '>= days Immobility/bed rest during pregnancy', term),
                    term = gsub('ecg', 'ECG', term),
                    term = gsub('ecg.cat', 'ECG (Binary) : Abnormal', term),
                    term = gsub('xray', 'X-ray : Normal', term),
                    term = gsub('xray.cat', 'X-ray (Binary)', term),
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
                    term = gsub('mrproanp', 'MRProANP', term))
        x <- x[,c('term', '1')]
        names(x) <- c('Term', 'Coefficient')
        return(x)
    }
    ## Extract the coefficients for the two Lambda thresholds
    coef.lambda.1se <- coef.extract(x = results$lasso.cv,
                                            s = 'lambda.1se')
    coef.lambda.min <- coef.extract(x = results$lasso.cv,
                                            s = 'lambda.min')
    ## Bind the two together
    results$lasso.cv.coef.lambda <- merge(coef.lambda.1se,
                                          coef.lambda.min,
                                          by      = c('Term'),
                                          all     = TRUE)
    names(results$lasso.cv.coef.lambda) <- c('Term', '1 SE', 'Min')
    ## Generate predictions at both threshold for lambda for Cross-Validated fit
    lasso.cv.lambda.min.response <- predict(results$lasso.cv,
                                                    newx = x,
                                                    s = 'lambda.min',
                                                    type = 'response')
    lasso.cv.lambda.1se.response <- predict(results$lasso.cv,
                                                    newx = x,
                                                    s = 'lambda.1se',
                                                    type = 'response')
    ## Predict overall
    lasso.cv.all <- predict(results$lasso.cv,
                                    newx = x,
                                    s    = results$lasso.cv$lambda,
                            type = 'response')
    ## Bind all predictions together and reshape for plotting
    results$lasso.cv.predicted <- cbind(y,
                                        lasso.cv.lambda.min.response,
                                        lasso.cv.lambda.1se.response) %>%
                                  as.data.frame()
    names(results$lasso.cv.predicted) <- c('D', 'min.response', '1se.response')
    results$lasso.cv.predicted <- cbind(results$lasso.cv.predicted,
                                        lasso.cv.all)
    names(results$lasso.cv.predicted) <- gsub('^*', 'Step ', names(results$lasso.cv.predicted))
    names(results$lasso.cv.predicted) <- gsub('Step D', 'D', names(results$lasso.cv.predicted))
    results$lasso.cv.predicted <- melt(results$lasso.cv.predicted,
                                       id.vars = c('D')) %>%
                                  mutate(name = variable,
                                         variable = gsub('Step min.response', 'Lambda Min', variable),
                                         variable = gsub('Step 1se.response', 'Lambda 1SE', variable),
                                         variable = gsub('Step ', '', variable)
                                         )
    ## Sort names and levels of D for passing to dipep_roc
    names(results$lasso.cv.predicted) <- c('D', 'term', 'M', 'name')
    results$lasso.cv.predicted <- mutate(results$lasso.cv.predicted,
                                         D = factor(D,
                                                    levels = c(0, 1),
                                                    labels = c('No PE', 'PE')),
                                         M = as.numeric(M))
    roc <- dipep_roc(df        = results$lasso.cv.predicted,
                     to.plot   = c('Step min.response',
                                   'Step 1se.response'),
                     title     = 'Cross-Validated LASSO',
                     threshold = threshold,
                     lasso     = FALSE)
    results$lasso.cv.roc           <- roc$plot
    results$lasso.cv.auc           <- roc$plot.auc
    results$lasso.cv.summary.stats <- roc$summary.stats
    results$threshold              <- threshold
    ## Get all predictions
    ## to.plot <- paste0('Step ', seq(1:length(results$lasso.cv$lambda)))
    to.plot <- seq(1:length(results$lasso.cv$lambda))
    roc <- dipep_roc(df        = results$lasso.cv.predicted,
                     to.plot   = to.plot,
                     title     = 'all steps of Cross-Validated LASSO',
                     threshold = threshold,
                     lasso     = TRUE)
    results$lasso.cv.roc.all           <- roc$plot
    results$lasso.cv.auc.all           <- roc$plot.auc
    results$lasso.cv.summary.stats.all <- roc$summary.stats
    return(results)
}
