## 2017-04-27 - Lines on scatter plots and histograms
build()
install()
to.plot <- dipep_plot(df      = dipep,
                      exclude = NULL,
                      exclude.non.recruited = TRUE,
                      exclude.dvt           = FALSE,
                      exclude.anti.coag     = FALSE,
                      exclude.missing       = FALSE,
                      title.to.plot         = 'APTT (min)',
                      title.class           = 'Primary Classification',
                      first.st, height)
to.plot$scatter


## 2017-04-26 - Checking changes in numbers for VTE
print('## Primary')
table(dipep$group, dipep$first.st.vte, useNA = 'ifany')
print('## Secondary')
table(dipep$group, dipep$second.st.vte, useNA = 'ifany')
print('## Tertiary')
table(dipep$group, dipep$third.st.vte, useNA = 'ifany')
print('## Quaternary')
table(dipep$group, dipep$fourth.st.vte, useNA = 'ifany')
print('## First v Second')
table(dipep$first.st.vte, dipep$second.st.vte, useNA = 'ifany')
print('## First v Third')
table(dipep$first.st.vte, dipep$third.st.vte, useNA = 'ifany')
print('## First v Fourth')
table(dipep$first.st.vte, dipep$fourth.st.vte, useNA = 'ifany')
print('## Number of individuals with hospital D-Dimer by group')
t <- dplyr::filter(dipep, !is.na(d.dimer))
table(t$group, t$first.st.vte, useNA = 'ifany')

## 2017-04-23 - Checking existing scores analyses for Primary v Secondary, purportedly "the same"
##              Nothing to see, this was down to not modifying 'first.st' in...
##
##              sections/subsections/existing/logistic.Rmd
##
##              from the script ~/rmarkdown/populate.sh.  Now corrected and scores are different.

## 2017-04-23 - Checking biomarker analyses for Primary v Secondary, purportedly "the same", this could
##              well be down to the fact that classification hasn't changed for the subset who have
##              biomarkers
print('## Primary')
table(dipep$vte, dipep$first.st, useNA = 'ifany')
print('## Secondary')
table(dipep$vte, dipep$second.st, useNA = 'ifany')
print('## Tertiary')
table(dipep$vte, dipep$third.st, useNA = 'ifany')
print('## Quaternary')
table(dipep$vte, dipep$fourth.st, useNA = 'ifany')
print('## First v Third')
table(dipep$first.st, dipep$third.st, useNA = 'ifany')
print('## First v Third by recruitment group')
table(dipep$first.st, dipep$third.st, dipep$group, useNA = 'ifany')

## 2017-04-12 dipep_roc() - getting correct T+ve/T-ve/F-ve/F+ve
test <- dplyr::filter(logistic$predicted, name %in% c('age.cat', 'bmi.cat'))

## 2017-04-11 dipep_roc() with d.dimer.cat/d.dimer.gestation.cat
t <- dplyr::filter(dipep, group %in% c('Diagnosed PE', 'Suspected PE'))
table(t$first.st, t$age.cat)
t <- dplyr::filter(biomarker.all$predicted, name == 'd.dimer.cat')
table(t$D, t$M)
t <- dplyr::filter(dipep, group %in% c('Diagnosed PE', 'Suspected PE'))
table(t$first.st, t$d.dimer.gestation.cat)
t <- dplyr::filter(biomarker.all$predicted, name == 'd.dimer.gestation.cat')
table(t$D, t$M)
summary(t$M)
t$M %>% as.factor() %>% levels() %>% length()
## Ok, need to be more intelligent about categorised individuals and threshold classification
## Have modified dipep_roc() now testing...
testing <- dipep_roc(df = biomarker.all$predicted,
                                       to.plot = c('d.dimer.cat',
                                                   'd.dimer.gestation.cat'),
                                       threshold = 0.3,
                                       title = 'D-Dimer (Hospital) - All')


## 2017-04-10 ROC Area Under the Curve CIs working, but has broken dipep_rpart()
predictor <- c(categorical, common)
rpart.categorical <- dipep_rpart(df             = dipep,
                                 classification = classification,
                                 predictor      = predictor,
                                 exclude.non.recruited = TRUE,
                                 exclude.dvt           = TRUE,
                                 exclude.missing       = TRUE,
                                 legend                = FALSE,
                                 threshold             = 0.5,
                                 rpart.opts.method     = 'class',
                                 rpart.opts.minsplit   = 4,
                                 rpart.opts.minbucket  = 2,
                                 rpart.opts.cp         = -1)
rpart.categorical$predicted
rpart.categorical$rpart.full.cp
dipep_roc(df = rpart.categorical$predicted,
          to.plot = seq(1:nrow(rpart.categorical$rpart.full.cp)),
          threshold = 0.3,
          lasso     = TRUE)

## 2017-04-10 ROC Area Under the Curve Confidence Intervals
test <- dplyr::filter(logistic$predicted, name %in% c('trimester', 'temperature')) %>%
        mutate(term = factor(term),
               name = factor(name))
test %>% group_by(name) %>% print() summarise(auc = roc(.$D ~ .$M))
## Doesn't really work, uses all observations and doesn't do it by
## name, why???
tmp.roc <- with(test, by(test, name, function(x) roc(D ~ M)))
tmp.ci  <- with(test, by(test, name, function(x) roc(D ~ M) %>% ci()))
tmp.roc
tmp.ci
## Extracting values
t <- tmp.ci %>%
     unlist() %>%
     as.data.frame()
names(t) <- 'stat'
t$t <- rownames(t)
t <- t %>%
     mutate(component = case_when(grepl('1', .$t) ~ 'auc_lci',
                                  grepl('2', .$t) ~ 'auc',
                                  grepl('3', .$t) ~ 'auc_uci'),
            name      = substr(.$t, 1, nchar(.$t) - 1)) %>%
     dplyr::select(-t)
## Check both
check <- dipep_roc(df      = test,
                   to.plot = c('trimester', 'temperature'),
                   title   = 'Testing derivation of AUC CI')
## Trimester
check.trimester <- dipep_roc(df      = test,
                             to.plot = c('trimester', 'temperature'),
                             title   = 'Testing derivation of AUC CI')
check.trimester$auc
check.trimester$auc.ci
## ECG
check.temperature <- dipep_roc(df      = test,
                          to.plot = c('temperature'),
                          title   = 'Testing derivation of AUC CI')
check.temperature$auc
check.temperature$auc.ci


## 2017-04-10 Problems during this Pregnancy
sink('problems_this_pregnancy.txt')
print('Overall how many problems have I derived...')
table(dipep$group, dipep$this.pregnancy.problems)
print('Specify 1')
table(dipep$this.preg.problem.specify_1, dipep$group)
print('Specify 2')
table(dipep$this.preg.problem.specify_2, dipep$group)
print('Specify 3')
table(dipep$this.preg.problem.specify_3, dipep$group)
print('Specify 4')
table(dipep$this.preg.problem.specify_4, dipep$group)
print('Specify 5')
table(dipep$this.preg.problem.specify_5, dipep$group)
print('Other 1')
table(dipep$this.preg.problem.other_1)
print('Other 2')
table(dipep$this.preg.problem.other_2)
print('Other 3')
table(dipep$this.preg.problem.other_3)
print('Other 4')
table(dipep$this.preg.problem.other_4)
print('Other 5')
table(dipep$this.preg.problem.other_5)
sink()


## 2017-04-04 Thromboprophylaxis
table(master$thromboprophylaxis$group, master$thromboprophylaxis$thromboprophylaxis,useNA = 'ifany')
## Run 'import.csv' to BEFORE conversion to factor
table(dipep$group, dipep$thromboprophylaxis, useNA = 'ifany')

## 2017-04-04 - Gestation specific d-dimer
table(dipep$first.st, dipep$d.dimer.gestation.cat, useNA = 'ifany')
table(dipep$first.st, dipep$d.dimer.cat, useNA = 'ifany')
t <- dplyr::select(dipep, exclude.anti.coag == 'No')
table(t$first.st, t$d.dimer.gestation.cat, useNA = 'ifany')
table(t$first.st, t$d.dimer.cat, useNA = 'ifany')

## 2017-04-04 - Gestation specific d-dimer
test <- dipep %>%
    mutate(d.dimer.gestation.cat = case_when(.$trimester == '1st Trimester' & (.$d.dimer < .$d.dimer.low | .$d.dimer > .$d.dimer.high) ~ 'Abnormal'
                                             ## .$trimester == 1 & (.$d.dimer < (1.5 * .$d.dimer.low) | .$d.dimer > (1.5 * .$d.dimer.high)) ~ 'Abnormal',
                                             ## .$trimester == 2 & (.$d.dimer < (2 * .$d.dimer.low) | .$d.dimer > (2 * .$d.dimer.high)) ~ 'Abnormal',
                                             ## .$trimester == 3   & (.$d.dimer < (2 * .$d.dimer.low) | .$d.dimer > (2 * .$d.dimer.high)) ~ 'Abnormal'
                                             ),
           d.dimer.gestation.cat = ifelse(is.na(d.dimer.gestation.cat),
                                          yes = 'Normal',
                                          no  = d.dimer.gestation.cat)
          )

## 2017-04-04 - Checking neoplasm differences
simplified.neoplasm <- mutate(dipep,
                              simplified.neoplasm = ifelse(simplified.neoplasm == 2,
                                                           yes = 1,
                                                           no  = simplified.neoplasm)) %>%
                       dipep_glm(classification        = 'first.st',
                                 predictor             = 'simplified.neoplasm',
                                 model                 = 'Geneva - Neoplasm',
                                 exclude               = NULL,
                                 exclude.dvt           = TRUE,
                                 exclude.non.recruited = TRUE,
                                 exclude.missing       = FALSE)
wells.neoplasm <- dipep_glm(df                    = dipep,
                            classification        = 'first.st',
                            predictor             = 'wells.neoplasm',
                            model                 = 'Wells - Neoplasm',
                            exclude               = NULL,
                            exclude.dvt           = TRUE,
                            exclude.non.recruited = TRUE,
                            exclude.missing       = FALSE)

## 2017-03-31 - Confidence Intervals for sensitivity/specificity
build()
install()
test <- dipep_roc(df      = logistic$predicted,
                            to.plot = c('age',
                                        'bmi',
                                        'o2.saturation',
                                        'bp.diastolic',
                                        'bp.systolic',
                                        'temperature'),
                            title   = 'Physiology (Continuous)')
test$summary.stats
names(test$ci)

## 2017-03-30 = Deriving hyperemesis indicator
## Those with hyperemesis requireing admission or detailed in other
t1 <- dplyr::filter(master$pregnancy.problems, grepl('hyperemesis', this.preg.problem.other, ignore.case = TRUE) | this.preg.problem.specify == 'Hyperemesis requiring admission') %>% dplyr::select(screening, this.preg.problem.specify, this.preg.problem.other)
## Those flagged as having Hyperemesis
t2 <- dplyr::filter(dipep, hyperemesis == 1) %>% dplyr::select(screening, hyperemesis)
check <- merge(t1, t2, by = 'screening', all = TRUE)

## 2017-03-29 - Checking scores using old and new
sink('~/work/dipep/tmp/check_scores_new.txt')
## Simplified
print('Simplified')
table(dipep$simplified, dipep$group)
## Wells Permissive
print('Wells (Permissive)')
table(dipep$wells.permissive, dipep$group)
## Wells Strict
print('Wells (Strict)')
table(dipep$wells.strict, dipep$group)
## PERC
print('PERC')
table(dipep$perc, dipep$group)
## Delphi Primary
print('Delphi Primary')
table(dipep$delphi.primary, dipep$group)
## Delphi Sensitivity
print('Delphi Sensitivity')
table(dipep$delphi.sensitivity, dipep$group)
## Delphi Specificity
print('Delphi Specificity')
table(dipep$delphi.specificity, dipep$group)
sink()

## 2017-03-29 - Confidence Intervals for sensitivity and specificity
t <- dplyr::filter(logistic$predicted, name == 'xray.pe') %>%
     dplyr::select(D, M) ## %>%
     ## mutate(M = ifelse(M > 0.380,
     ##                   yes = 'PE',
     ##                   no  = 'No PE'),
     ##        M = factor(M))

test <- ci.se(response = t$D, predictor = t$M, specificities = 0.8) %>% as.data.frame()

## 2017-03-29 - Fixing dipep_roc()
dplyr::filter(biomarker.all$predicted, name %in% c('clauss.fibrinogen',
                                                 'plasmin.antiplasmin',
                                                 'tissue.factor',
                                                 'troponin',
                                               'nppb')) %>%
    ggplot(aes(d = D, m = M, colour = as.factor(term))) +
    geom_roc() +
    ggtitle('ROC curves for ') +
    labs(colour = 'Step...') +
    style_roc() + theme_bw()

## 2017-03-27 - Incorporating DVT with PE for 'vte' analyses of biomarkers
dipep_glm(df = dipep,
                                           exclude.dvt           = FALSE,
                                           exclude.non.recruited = TRUE,
                                           exclude.anti.coag     = FALSE,
                                           exclude.missing       = FALSE,
                                           classification = 'vte',
                                           predictor      = 'prothombin.time',
                                           model          = 'Prothombin (Time)')

## 2017-03-22 - Developing animated plots
for(i in seq(1:length(lasso.categorical$lasso.cv$lambda))){
    print(i)
    t <- dplyr::filter(lasso.categorical$lasso.cv.predicted, term == i) %>%
        ggplot(aes(d = D, m = M)) +
        geom_roc() +
        ggtitle(paste0('ROC Curves for LASSO step ', i)) +
        style_roc() + theme_bw()
    print(t)
}

## 2017-03-15 - Developing dipep_rpart() function to
check <- dipep_rpart(df       = dipep,
                     classification = 'first.st',
                     predictor      = c('age.cat', 'smoking', 'temperature.cat', 'bp.diastolic.cat', 'bp.systolic.cat',
                                        'o2.saturation.cat', 'respiratory.rate.cat', 'bmi.cat',
                                        'pregnancies.under.cat', 'pregnancies.over.cat', 'prev.preg.problem',
                                        'presenting.features.pleuritic',
                                        'presenting.features.non.pleuritic', 'presenting.features.sob.exertion',
                                        'presenting.features.sob.rest', 'presenting.features.haemoptysis',
                                        'presenting.features.cough', 'presenting.features.syncope',
                                        'presenting.features.palpitations', 'presenting.features.other',
                                        'surgery', 'cesarean', 'thromb.event',
                                        'thromboprophylaxis', 'thrombosis', 'preg.post', 'num.fetus'),
                     exclude.non.recruited = TRUE,
                     exclude.dvt           = TRUE,
                     legend                = FALSE,
                     threshold             = 0.5,
                     rpart.opts.method     = 'class',
                     rpart.opts.minsplit   = 4,
                     rpart.opts.minbucket  = 2,
                     rpart.opts.cp         = -1,
                     printcp.opts.digits   = 5,
                     prp.opts.type         = 2,
                     prp.opts.extra        = 'auto',
                     prp.opts.box.palette  = c('green', 'red'),
                     prp.opts.yesno        = 1,
                     prp.opts.branch       = 1,
                     prp.opts.varlen       = 0,
                     prp.opts.faclen       = 0)

## 2017-03-14 - Checking Delphi derivation
print('Syncope')
table(dipep$delphi.primary.syncope, dipep$presenting.features.syncope, useNA = 'ifany')
print('Haemoptysis')
table(dipep$delphi.primary.haemoptysis, dipep$presenting.features.haemoptysis, useNA = 'ifany')
print('Pleuritic')
table(dipep$delphi.primary.pleuritic, dipep$presenting.features.pleuritic, useNA = 'ifany')
print('History DVT/PE')
table(dipep$delphi.primary.history.dvt.pe, dipep$thromb.event, useNA = 'ifany')
table(dipep$delphi.primary.history.dvt.pe, dipep$thrombosis, useNA = 'ifany')
print('History IV Drug')
table(dipep$delphi.primary.history.iv.drug, dipep$history.iv.drug, useNA = 'ifany')
print('Family History')
table(dipep$delphi.primary.family.history, dipep$history.thrombosis, useNA = 'ifany')
print('Medical History')
table(dipep$delphi.primary.medical.history, dipep$injury, useNA = 'ifany')
table(dipep$delphi.primary.medical.history, dipep$surgery, useNA = 'ifany')
table(dipep$delphi.primary.medical.history, dipep$admitted.hospital, useNA = 'ifany')

dplyr::filter(dipep, delphi.primary.medical.history == 1) %>%
    dplyr::select(screening, injury, surgery, admitted.hospital) %>%
    dplyr::filter(injury == 'Yes' & surgery == 'Yes' & admitted.hospital == 'Yes') %>%
    head(n = 15)

print('Obstetric Complications')
table(dipep$delphi.primary.obstetric.complication, dipep$obstetric.complications, useNA = 'ifany')
print('Medical Complications')
table(dipep$delphi.primary.medical.complication, dipep$medical.comorbidity, useNA = 'ifany')
print('Gestation')
table(dipep$delphi.primary.gestation, dipep$trimester, useNA = 'ifany')
print('Clinical DVT')
table(dipep$delphi.primary.clinical.dvt, dipep$dvt, useNA = 'ifany')
print('O2 Saturation')
table(dipep$delphi.primary.o2.saturation, dipep$o2.saturation, useNA = 'ifany')
print('Heart Rate 110bpm')
table(dipep$delphi.primary.heart.rate.110.bpm, dipep$heart.rate, useNA = 'ifany')
print('Heart Rate 100bpm')
table(dipep$delphi.primary.heart.rate.100.bpm, dipep$heart.rate, useNA = 'ifany')
print('Respiratory Rate')
table(dipep$delphi.primary.respiratory.rate, dipep$respiratory.rate, useNA = 'ifany')
print('BMI')
table(dipep$delphi.primary.bmi, dipep$bmi, useNA = 'ifany')

## 2017-03-10 - Predictions for ALL steps in the LASSO
classification <- 'first.st'
predictor <- predictor <- c('age.cat', 'bmi.cat', 'smoking.cat', 'pregnancies.over.cat', 'pregnancies.under.cat',
               'history.thrombosis', 'history.veins', 'history.iv.drug', 'thrombo', 'cesarean',
               'injury', 'thrombosis', 'existing.medical',
               ## 'preg.post',
               'trimester', 'multiple.preg',
               'travel', 'immobil', 'this.pregnancy.problems', 'prev.preg.problem',
               'presenting.features.pleuritic', 'presenting.features.non.pleuritic',
               'presenting.features.sob.exertion', 'presenting.features.sob.rest',
               'presenting.features.haemoptysis', 'presenting.features.cough',
               'presenting.features.syncope',
               ## 'presenting.features.palpitations',
               'presenting.features.other', 'respiratory.rate.cat', 'heart.rate',
               'o2.saturation.cat', 'bp.systolic.cat', 'bp.diastolic.cat', 'ecg.cat', 'xray.cat')
lasso.categorical <- dipep_glmnet_orig(df          = dipep,
                                       classification = classification,
                                       predictor      = predictor,
                                       alpha          = 1,
                                       model          = 'LASSO : Pre-Categorised Variables',
                                       exclude        = NULL,
                                       exclude.non.recruited = TRUE,
                                       exclude.dvt           = TRUE,
                                       exclude.anti.coag     = FALSE,
                                       legend                = TRUE,
                                       threshold             = 0.3)

to.plot <- seq(1:length(lasso.categorical$lasso.cv$lambda))
table(lasso.categorical$lasso.cv.predicted$name)
table(lasso.categorical$lasso.cv.predicted$term)
dipep_roc(df = lasso.categorical$lasso.cv.predicted,
          to.plot = to.plot,
          title = 'all steps of Cross-Validated LASSO',
          threshold = 0.3,
          lasso = TRUE)


lasso.categorical$lasso.cv.roc

## 2017-03-09 - Why can't all variables be used?
predictor <- c('age.cat', 'bmi.cat', 'smoking.cat', 'pregnancies.over.cat', 'pregnancies.under.cat',
               'history.thrombosis', 'history.veins', 'history.iv.drug', 'thrombo', 'cesarean',
               'injury', 'thrombosis', 'existing.medical',
               ## 'preg.post',
               'trimester', 'multiple.preg',
               'travel', 'immobil', 'this.pregnancy.problems', 'prev.preg.problem',
               'presenting.features.pleuritic', 'presenting.features.non.pleuritic',
               'presenting.features.sob.exertion', 'presenting.features.sob.rest',
               'presenting.features.haemoptysis', 'presenting.features.cough',
               'presenting.features.syncope',
               ## 'presenting.features.palpitations',
               'presenting.features.other', 'respiratory.rate.cat', 'heart.rate',
               'o2.saturation.cat', 'bp.systolic.cat', 'bp.diastolic.cat', 'ecg.cat', 'xray.cat')
lasso.categorical <- dipep_glmnet_orig(df          = dipep,
                                       classification = classification,
                                       predictor      = predictor,
                                       alpha          = 1,
                                       model          = 'LASSO : Pre-Categorised Variables',
                                       exclude        = NULL,
                                       exclude.non.recruited = TRUE,
                                       exclude.dvt           = TRUE,
                                       exclude.anti.coag     = FALSE,
                                       legend                = TRUE,
                                       threshold             = 0.3)
lasso.categorical$lasso.cv.auc
lasso.categorical$lasso.cv.roc
lasso.categorical$lasso.cv.summary.stats

## 2017-03-08 - LASSO function testing of dipep_glmnet_orig() which DOESN'T utilise glmnetUtils()
classification <- 'first.st'
predictor <- c('age.cat', 'bmi.cat', 'smoking.cat', 'pregnancies.over.cat', 'pregnancies.under.cat')
lasso.categorical <- dipep_glmnet_orig(df          = dipep,
                                       classification = classification,
                                       predictor      = predictor,
                                       alpha          = 1,
                                       model          = 'LASSO : Pre-Categorised Variables',
                                       exclude        = NULL,
                                       exclude.non.recruited = TRUE,
                                       exclude.dvt           = TRUE,
                                       exclude.anti.coag     = FALSE,
                                       legend                = TRUE)

## 2017-03-08 - LASSO function testing of dipep_glmnet() which utilises glmnetUtils().
classification <- 'first.st'
predictor <- c('age.cat', 'bmi.cat', 'smoking.cat', 'pregnancies.over.cat', 'pregnancies.under.cat',
               'history.thrombosis', 'history.veins', 'history.iv.drug', 'thrombo', 'cesarean',
               'injury', 'thrombosis', 'existing.medical', 'preg.post', 'trimester', 'multiple.preg',
               'travel', 'immobil', 'this.pregnancy.problems', 'prev.preg.problem',
               'presenting.features.pleuritic', 'presenting.features.non.pleuritic',
               'presenting.features.sob.exertion', 'presenting.features.sob.rest',
               'presenting.features.haemoptysis', 'presenting.features.cough',
               'presenting.features.syncope', 'presenting.features.palpitations',
               'presenting.features.other', 'respiratory.rate.cat', 'heart.rate',
               'o2.saturation.cat', 'bp.systolic.cat', 'bp.diastolic.cat', 'ecg.cat', 'xray.cat')
lasso.categorical.check <- dipep_glmnet(df          = dipep,
                                  classification = classification,
                                  predictor      = predictor,
                                  alpha          = 1,
                                  model          = 'LASSO : Pre-Categorised Variables',
                                  exclude        = NULL,
                                  exclude.non.recruited = TRUE,
                                  exclude.dvt           = TRUE,
                                  exclude.anti.coag     = FALSE,
                                  legend                = TRUE)
lasso.categorical$lasso.plot
lasso.categorical.check$lasso.plot

lasso.categorical$coef.lambda

## 2017-03-07 - Checking exclusion of biomarkers
check <- list()
check$summary.include.anti.coag <- dipep_summarise(df           = dipep,
                      	                  grouping     = 'first.st',
                      	                  group.as.col = TRUE,
                                          exclude               = NULL,
                                          exclude.non.recruited = TRUE,
                                          exclude.dvt           = TRUE,
                                          exclude.anti.coag     = FALSE,
                      	                  prothombin.time, aprothombin, clauss.fibrinogen, ddimer.innovan,
                                          thrombin.generation.lag.time, thrombin.generation.endogenous.potential,
                                          thrombin.generation.peak, thrombin.generation.time.to.peak,
                                          ddimer.elisa, plasmin.antiplasmin, prothrombin.fragments,
                                          soluble.tissue.factor, troponin,
                                          natriuertic.peptide, mrproanp)
check$summary.exclude.anti.coag <- dipep_summarise(df           = dipep,
                      	                  grouping     = 'first.st',
                      	                  group.as.col = TRUE,
                                          exclude               = NULL,
                                          exclude.non.recruited = TRUE,
                                          exclude.dvt           = TRUE,
                                          exclude.anti.coag     = TRUE,
                      	                  prothombin.time, aprothombin, clauss.fibrinogen, ddimer.innovan,
                                          thrombin.generation.lag.time, thrombin.generation.endogenous.potential,
                                          thrombin.generation.peak, thrombin.generation.time.to.peak,
                                          ddimer.elisa, plasmin.antiplasmin, prothrombin.fragments,
                                          soluble.tissue.factor, troponin,
                                          natriuertic.peptide, mrproanp)
stopifnot(check$summary.include.anti.coag == check$summary.exclude.anti.coag)
check$plot.include.anti.coag <- dipep_plot(df      = dipep,
                                           exclude = NULL,
                                           exclude.non.recruited = TRUE,
                                           exclude.dvt           = TRUE,
                                           exclude.anti.coag     = FALSE,
                                           title.to.plot         = 'Aprothombin (min)',
                                           title.class           = 'Primary Classification',
                                           first.st, aprothombin)
check$plot.exclude.anti.coag <- dipep_plot(df      = dipep,
                                           exclude = NULL,
                                           exclude.non.recruited = TRUE,
                                           exclude.dvt           = TRUE,
                                           exclude.anti.coag     = TRUE,
                                           title.to.plot         = 'Aprothombin (min)',
                                           title.class           = 'Primary Classification',
                                           first.st, aprothombin)
check$plot.include.anti.coag$scatter
check$plot.exclude.anti.coag$scatter
check$glm.include.anti.coag <- dipep_glm(df = dipep,
                                   exclude.dvt           = TRUE,
                                   exclude.non.recruited = TRUE,
                                   exclude.anti.coag     = FALSE,
                                   classification = 'first.st',
                                   predictor      = 'aprothombin',
                                   model          = 'Aprothombin')
check$glm.exclude.anti.coag <- dipep_glm(df = dipep,
                                         exclude.dvt           = TRUE,
                                         exclude.non.recruited = TRUE,
                                         exclude.anti.coag     = TRUE,
                                         classification = 'first.st',
                                         predictor      = 'aprothombin',
                                         model          = 'Aprothombin')
check$glm.include.anti.coag$tidied
check$glm.exclude.anti.coag$tidied

## 2017-03-06 - Cross tabulation development
build()
install()
checking <- dipep_glm(df = dipep,
                      classification = 'first.st',
                      predictor = 'travel',
                      model     = 'Age (Binary)',
                      exclude   = NULL,
                      exclude.dvt = TRUE,
                      excliude.non.recruited = TRUE)
checking$table

## 2017-03-06 - Checking other problems in 'Diagnosed PE' (/UKOSS) at Kim Horspools request (see
##              email 2017-03-06 @ 11:24 Re : CDR criteria)
checking <- dplyr::select(master$pregnancy.problems,
                     screening,
                     group,
                     site,
                     event.name,
                     this.preg.problem.specify,
                     this.preg.problem.other) %>%
    melt(id.vars = c('screening', 'group', 'site', 'event.name')) %>%
    group_by(screening, variable) %>%
    mutate(n = row_number()) %>%
    dcast(screening + group + site + event.name ~ variable + n) %>%
    mutate(this.pregnancy.problems.dehydration = ifelse(this.preg.problem.specify_1 == 'Dehydration requiring admission' |
                                                        this.preg.problem.specify_2 == 'Dehydration requiring admission' |
                                                        this.preg.problem.specify_3 == 'Dehydration requiring admission' |
                                                        this.preg.problem.specify_4 == 'Dehydration requiring admission' |
                                                        this.preg.problem.specify_5 == 'Dehydration requiring admission',
                                                        yes = 1,
                                                        no  = 0),
           this.pregnancy.problems.eclampsia =  ifelse(this.preg.problem.specify_1 == 'Eclampsia' |
                                                        this.preg.problem.specify_2 == 'Eclampsia' |
                                                        this.preg.problem.specify_3 == 'Eclampsia' |
                                                        this.preg.problem.specify_4 == 'Eclampsia' |
                                                        this.preg.problem.specify_5 == 'Eclampsia',
                                                        yes = 1,
                                                        no  = 0),
           this.pregnancy.problems.gestational.diabetes =  ifelse(this.preg.problem.specify_1 == 'Gestational diabetes' |
                                                                  this.preg.problem.specify_2 == 'Gestational diabetes' |
                                                                  this.preg.problem.specify_3 == 'Gestational diabetes' |
                                                                  this.preg.problem.specify_4 == 'Gestational diabetes' |
                                                                  this.preg.problem.specify_5 == 'Gestational diabetes',
                                                                  yes = 1,
                                                                  no  = 0),
           this.pregnancy.problems.haemorrhage =  ifelse(this.preg.problem.specify_1 == 'Haemorrhage' |
                                                         this.preg.problem.specify_2 == 'Haemorrhage' |
                                                         this.preg.problem.specify_3 == 'Haemorrhage' |
                                                         this.preg.problem.specify_4 == 'Haemorrhage' |
                                                         this.preg.problem.specify_5 == 'Haemorrhage',
                                                         yes = 1,
                                                         no  = 0),
           this.pregnancy.problems.hyperemesis =  ifelse(this.preg.problem.specify_1 == 'Hyperemesis requiring admission' |
                                                         this.preg.problem.specify_2 == 'Hyperemesis requiring admission' |
                                                         this.preg.problem.specify_3 == 'Hyperemesis requiring admission' |
                                                         this.preg.problem.specify_4 == 'Hyperemesis requiring admission' |
                                                         this.preg.problem.specify_5 == 'Hyperemesis requiring admission',
                                                         yes = 1,
                                                         no  = 0),
           this.pregnancy.problems.ovarian.hyperstimulation =  ifelse(this.preg.problem.specify_1 == 'Ovarian hyperstimulation syndrome' |
                                                                      this.preg.problem.specify_2 == 'Ovarian hyperstimulation syndrome' |
                                                                      this.preg.problem.specify_3 == 'Ovarian hyperstimulation syndrome' |
                                                                      this.preg.problem.specify_4 == 'Ovarian hyperstimulation syndrome' |
                                                                      this.preg.problem.specify_5 == 'Ovarian hyperstimulation syndrome',
                                                                      yes = 1,
                                                                      no  = 0),
           this.pregnancy.problems.postpartum.haemorrhage =  ifelse(this.preg.problem.specify_1 == 'Post-partum haemorrhage requiring transfusion' |
                                                                    this.preg.problem.specify_2 == 'Post-partum haemorrhage requiring transfusion' |
                                                                    this.preg.problem.specify_3 == 'Post-partum haemorrhage requiring transfusion' |
                                                                    this.preg.problem.specify_4 == 'Post-partum haemorrhage requiring transfusion' |
                                                                    this.preg.problem.specify_5 == 'Post-partum haemorrhage requiring transfusion',
                                                                    yes = 1,
                                                                    no  = 0),
           this.pregnancy.problems.preeclampsia =  ifelse(this.preg.problem.specify_1 == 'Pre-eclampsia (hypertension and proteinuria)' |
                                                          this.preg.problem.specify_2 == 'Pre-eclampsia (hypertension and proteinuria)' |
                                                          this.preg.problem.specify_3 == 'Pre-eclampsia (hypertension and proteinuria)' |
                                                          this.preg.problem.specify_4 == 'Pre-eclampsia (hypertension and proteinuria)' |
                                                          this.preg.problem.specify_5 == 'Pre-eclampsia (hypertension and proteinuria)',
                                                          yes = 1,
                                                          no  = 0),
           this.pregnancy.problems.preterm =  ifelse(this.preg.problem.specify_1 == 'Preterm birth or mid trimester loss' |
                                                     this.preg.problem.specify_2 == 'Preterm birth or mid trimester loss' |
                                                     this.preg.problem.specify_3 == 'Preterm birth or mid trimester loss' |
                                                     this.preg.problem.specify_4 == 'Preterm birth or mid trimester loss' |
                                                     this.preg.problem.specify_5 == 'Preterm birth or mid trimester loss',
                                                     yes = 1,
                                                     no  = 0),
           this.pregnancy.problems.severe.infection =  ifelse(this.preg.problem.specify_1 == 'Severe infection e.g. pyelonephritis' |
                                                              this.preg.problem.specify_2 == 'Severe infection e.g. pyelonephritis' |
                                                              this.preg.problem.specify_3 == 'Severe infection e.g. pyelonephritis' |
                                                              this.preg.problem.specify_4 == 'Severe infection e.g. pyelonephritis' |
                                                              this.preg.problem.specify_5 == 'Severe infection e.g. pyelonephritis',
                                                              yes = 1,
                                                              no  = 0),
           this.pregnancy.problems.stillbirth =  ifelse(this.preg.problem.specify_1 == 'Stillbirth' |
                                                        this.preg.problem.specify_2 == 'Stillbirth' |
                                                        this.preg.problem.specify_3 == 'Stillbirth' |
                                                        this.preg.problem.specify_4 == 'Stillbirth' |
                                                        this.preg.problem.specify_5 == 'Stillbirth',
                                                        yes = 1,
                                                        no  = 0),
           this.pregnancy.problems = ifelse(this.pregnancy.problems.dehydration == 1 |
                                            this.pregnancy.problems.eclampsia == 1 |
                                            this.pregnancy.problems.gestational.diabetes == 1 |
                                            this.pregnancy.problems.haemorrhage == 1 |
                                            this.pregnancy.problems.hyperemesis == 1 |
                                            this.pregnancy.problems.ovarian.hyperstimulation == 1 |
                                            this.pregnancy.problems.postpartum.haemorrhage == 1 |
                                            this.pregnancy.problems.preeclampsia == 1 |
                                            this.pregnancy.problems.preterm == 1 |
                                            this.pregnancy.problems.severe.infection == 1 |
                                            this.pregnancy.problems.stillbirth == 1,
                                            yes = 1,
                                            no  = 0),
                this.pregnancy.problems = ifelse(is.na(this.pregnancy.problems),
                                                 yes = 0,
                                                 no  = this.pregnancy.problems)) %>%
    dplyr::filter(group == 'Diagnosed PE') %>%
    dplyr::select(screening, group,
                  this.preg.problem.specify_1, this.preg.problem.other_1)
    dplyr::select(screening, medical.specify)
missing <- is.na(checking)
table(missing[,1])
table(missing[,2])
table(missing[,3])
table(missing[,4])
table(missing[,5])
table(missing[,6])
table(missing[,7])
table(missing[,8])
table(missing[,9])
table(missing[,10])
table(missing[,11])
table(missing[,12])

## 2017-03-06 - Checking swelling/pain/pain on palpitation in single legs/both legs for Simplified and PERC
dplyr::filter(dipep, screening %in% c('N01/03',
                                      'N01/27',
                                      'N02/03',
                                      'PE_037 ',
                                      'PE_208',
                                      'PE_260',
                                      'PE_208',
                                      'PE_271',
                                      'PE_276',
                                      'PE_294',
                                      'PE_324',
                                      'S03/34',
                                      'S05/15',
                                      'S05/30',
                                      'S07/24',
                                      'S09/05',
                                      'S11/09')) %>%
    dplyr::select(screening, other.symptoms.specify, simplified.pain.palpitations, simplified.lower.limb.unilateral.pain, perc.leg.swelling)

## 2017-03-03 Duplicates have crept in...where are they from?
dplyr::select(t1, screening) %>% duplicated() %>% table()
dplyr::select(t2, screening) %>% duplicated() %>% table()
dplyr::select(t3, screening) %>% duplicated() %>% table()
dplyr::select(t4, screening) %>% duplicated() %>% table()
dplyr::select(t5, screening) %>% duplicated() %>% table()
dplyr::select(t6, screening) %>% duplicated() %>% table()
dplyr::select(t7, screening) %>% duplicated() %>% table()
dplyr::select(t8, screening) %>% duplicated() %>% table()
dplyr::select(t9, screening) %>% duplicated() %>% table()
dplyr::select(t10, screening) %>% duplicated() %>% table()
dplyr::select(t11, screening) %>% duplicated() %>% table()
dplyr::select(t12, screening) %>% duplicated() %>% table()
dplyr::filter(t12, screening %in% c('PE_047', 'PE_079', 'PE_092', 'PE_223'))

## 2017-03-02 Sorting scoring things Mike has highlighted...
dplyr::filter(dipep, simplified.lower.limb.unilateral.pain == 1) %>%
    dplyr::select(screening, simplified.lower.limb.unilateral.pain, other.symptoms.specify)

dplyr::filter(dipep,
              grepl('swollen left calf', other.symptoms.specify, ignore.case = TRUE) |
              grepl('right calf swelling', other.symptoms.specify, ignore.case = TRUE)) %>%
    dplyr::select(screening, other.symptoms.specify)

dplyr::filter(dipep,
              grepl('right calf swelling', other.symptoms.specify, ignore.case = TRUE) |
              grepl('calf pain', other.symptoms.specify, ignore.case = TRUE) |
              grepl('painful', other.symptoms.specify, ignore.case = TRUE)
              ) %>%
    dplyr::select(screening, other.symptoms.specify)

check <- mutate(dipep, trimester = ifelse(gestation < 98,
                                   yes = 0,
                                   no  = ifelse(gestation >= 98 & gestation < 196,
                                                yes = 1,
                                                ifelse(gestation >= 196 & preg.post == 'Pregnant',
                                                       yes = 2,
                                                       no  = 3))),
                trimester = ifelse(is.na(trimester),
                                   yes = 0,
                                   no  = trimester))
dplyr::filter(check, screening %in% c('PE_001', 'PE_002')) %>%
    dplyr::select(screening, event.date, edd, gestation, trimester, preg.post)
## Now re-write to capture/handle instances like these two...
check <- dipep %>%
    mutate(gestation = 280 - (ymd(edd) - ymd(event.date)),
           preg.post = as.character(preg.post),
           preg.post = ifelse(!is.na(preg.post),
                              yes = preg.post,
                              no  = ifelse(!is.na(delivery.date) & event.date - delivery.date > 0,
                                           yes = 'Postpartum',
                                           no  = 'Pregnant')))
check <- check %>%
    mutate(trimester = case_when(.$gestation < 98   & !is.na(.$preg.post)                     ~ 0,
                                 .$gestation >= 98  & .$gestation < 196   & !is.na(.$preg.post) ~ 1,
                                 .$gestation >= 196 & !is.na(.$preg.post) & .$preg.post == 'Pregnant'   ~ 2,
                                 .$gestation >= 196 & !is.na(.$preg.post) & .$preg.post == 'Postpartum' ~ 3),
           trimester = ifelse(is.na(gestation) & is.na(trimester) & !is.na(preg.post) & preg.post == 'Postpartum',
                              yes = 3,
                              no  = trimester))
dplyr::filter(check, screening %in% c('PE_001', 'PE_002', 'N04/06', 'PE_172')) %>%
    dplyr::select(screening, event.date, delivery.date, edd, gestation, trimester, preg.post)

dplyr::filter(check, gestation >= 196 & is.na(preg.post) & event.date < delivery.date) %>%
    dplyr::select(screening, event.date, delivery.date, edd, gestation, trimester, preg.post)


## 2017-03-01 Why is dipep_existing_sum not giving the right numbers?
table(dipep$group, dipep$first.st, useNA = 'ifany')
check <- dipep_existing_sum(df                    = dipep,
                            title                 = 'Simplified Revised Geneva',
                            exclude               = NULL,
                            exclude.non.recruited = TRUE,
                            exclude.dvt           = TRUE,
                            first.st, simplified.pe, simplified)
check$table
check$summary.table
not.missing.scores <- dplyr::filter(dipep, !is.na(simplified) & group %in% c('Suspected PE', 'Diagnosed PE')) %>%
                      dplyr::select(screening, group, simplified.age, simplified.previous,
                                    simplified.surgery, simplified.neoplasm,
                                    simplified.lower.limb.unilateral.pain, simplified.haemoptysis,
                                    simplified.heart.rate, simplified.lower.limb.pain,
                                    simplified, simplified.risk, simplified.pe)
table(not.missing.scores$group, not.missing.scores$first.st, useNA = 'ifany')
missing.scores <- dplyr::filter(dipep, is.na(simplified) & group %in% c('Suspected PE', 'Diagnosed PE')) %>%
                      dplyr::select(screening, group, simplified.age, simplified.previous,
                                    simplified.surgery, simplified.neoplasm,
                                    simplified.lower.limb.unilateral.pain, simplified.haemoptysis,
                                    simplified.heart.rate, simplified.lower.limb.pain,
                                    simplified, simplified.risk, simplified.pe)
nrow(missing.scores)
table(missing.scores$group)

## 2017-02-28 Turning plotting of biomarker data into a function
build()
install()
dipep_biomarker(df = dipep,
                exclude = NULL,
                exclude.non.recruited = TRUE,
                exclude.non.dvt       = TRUE,
                first.st, aprothombin)

## 2017-02-28 Rescuing N02/02 from being excluded (damn missing data, demonstrates why
##            data management should derive datasets instead of me spending time not
##            doing statistical analysis/reporting)
dplyr::filter(t1, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## PE_156
dplyr::filter(t2, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## PE_156
dplyr::filter(t3, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## PE_156
dplyr::filter(t4, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## PE_156
dplyr::filter(t5, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## PE_156
dplyr::filter(t6, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## Neither present
dplyr::filter(t7, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## PE_156
dplyr::filter(t8, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## PE_156
dplyr::filter(t9, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## Neither present
dplyr::filter(t10, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## Neither present
dplyr::filter(t11, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## PE_156
dplyr::filter(master$biomarker_tidy, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## Neither present
dplyr::filter(master$case.review, screening %in% c('PE_156', 'N02/02')) %>% dplyr::select(screening) ## PE_156
dplyr::filter(case.review1, screening %in% c('PE_156', 'N02/02'))
dplyr::filter(case.review2, screening %in% c('PE_156', 'N02/02'))
dplyr::filter(case.review3, screening %in% c('PE_156', 'N02/02'))
dplyr::filter(master$case.review, screening %in% c('PE_156', 'N02/02'))

## 2017-02-27 Developing dipep_roc() function, should include calculation of sensitivity,
##            specificity, ppv and npv using specified threshold
build()
install()
testing <- dipep_roc(df = biomarker$predicted,
                     to.plot = c('ddimer.elisa'),
                     threshold = 0.250)
testing$counts

testing$summary.stats

table(testing$df$D, testing$df$m)

## 2017-02-24 Identifying who is dropped out of Recursive Partitioning due to missing
##            data.
check <- dplyr::filter(dipep, group %in% c('Diagnosed PE', 'Suspected PE')) %>%
               dplyr::select(screening, first.st,
                             age.cat, smoking, temperature.cat, bp.diastolic.cat, bp.systolic.cat,
                             o2.saturation.cat, respiratory.rate.cat, bmi.cat,
                             pregnancies.under.cat, pregnancies.over.cat, prev.preg.problem,
                             presenting.features.pleuritic,
                             presenting.features.non.pleuritic, presenting.features.sob.exertion,
                             presenting.features.sob.rest, presenting.features.haemoptysis,
                             presenting.features.cough, presenting.features.syncope,
                             presenting.features.palpitations, presenting.features.other,
                             surgery, cesarean, thromb.event,
                             thromboprophylaxis, thrombosis, preg.post, num.fetus)
table(check$first.st, useNA = 'ifany')

## 2017-02-23 Checking biomarker exclusions
names(master$biomarker_exclusions_20170207_clean) <- c('screening', 'early.exclude')
master$biomarker_exclusions_20170223_clean <- dplyr::select(master$biomarker_exclusions_20170223_clean,
                                                   screening, exclude.anti.coag) %>%
                                     mutate(exclude.anti.coag = case_when(.$exclude.anti.coag == 'N' ~ 'No',
                                                                          .$exclude.anti.coag == 'Y' ~ 'Yes'))
names(master$biomarker_exclusions_20170223_clean) <- c('screening', 'late.exclude')
dim(master$biomarker_exclusions_20170207_clean)
dim(master$biomarker_exclusions_20170223_clean)
check <- left_join(master$biomarker_exclusions_20170223_clean,
                   master$biomarker_exclusions_20170207_clean)
table(check$early.exclude, check$late.exclude, useNA = 'ifany')
## 2017-02-23 Existing score function
build()
install()
testing <- dipep_nse(df             = dipep,
                     title          = 'Wells',
                     exclude        = NULL,
                     first.st, perc.pe, perc)

testing$likert.plot

plot(testing$likert, centered = 4)

## 2017-02-18 Simple LASSO testing
build()
install()
t <- dipep_glmnet(df = dipep,
             classification = 'third.st',
             predictor      = c('age', 'history.iv.drug'))
head(t$lasso_predict)

## 2017-02-17 Developing dipep_glmnet() function/wrapper
##
## Test things actually work before testing the function
test.data <- data.frame(
    y=c(1:4, NA, NA, 7:10),
    x=c(letters[c(1:8, 8, 8)]),
    z=c(letters[c(1:8, 8, 8)]),
    stringsAsFactors = F)
test.f <- function(df = test.data,
                   classification = y,
                   predictor      = 'x'){
    subset(df, !is.na(classification), select = predictor)
}
test.f()

.formula <- reformulate(response = 'first.st',
                        termlabels = c('age', 'gestation'))
lasso.categorical <- glmnetUtils::glmnet(data = dplyr::filter(dipep, !is.na(first.st)),
                                         .formula,
                                         family  = 'binomial',
                                         alpha = 1)
tidy(lasso.categorical)
predict(lasso.categorical, newdata = dipep) %>% head()
cv.lasso.categorical <- glmnetUtils::cv.glmnet(data = dplyr::filter(dipep, !is.na(first.st)),
                                            .formula,
                                            family  = 'binomial',
                                            alpha = 1)
predict(cv.lasso.categorical, newdata = dipep)

## As a function call
t <- dipep_glmnet(df = dipep,
             classification = 'third.st',
             predictor      = c('age', 'history.iv.drug'))

## 2017-02-17 Modifying dipep_glm() to allow individuals (non-recruited who are also UKOSS)
##            to be explicitly excluded.
logistic$age <- dipep_glm(df = dipep,
                          classification = 'first.st',
                          predictor      = 'age',
                          model          = 'Age',
                          exclude        = c('N04/06', 'N04/07'))
## WORKS \o/

## 2017-02-17 Developing ROC plotting function (eases maintainability of code)
t <- dipep_roc()
t$plot + geom_text(data = t$plot.auc, aes(x = x, y = y, label = AUC))
## NO JOY :(

## 2017-02-16 CHecking primary and secondary classifications
sink(file = '~/work/dipep/tmp/classification_check.txt')
print('Secondary Classification (DM has Classifcation, I do not)')
dplyr::filter(master$case.review, is.na(secondary.class) & !is.na(secondary.class.dm))
dplyr::filter(master$case.review, is.na(secondary.class) & !is.na(secondary.class.dm)) %>%
    write.table('~/work/dipep/tmp/check1.txt', sep = ',')
print('Secondary Classification (DM has Classifcation, I do not)')
dplyr::filter(master$case.review, !is.na(secondary.class) & is.na(secondary.class.dm))
dplyr::filter(master$case.review, !is.na(secondary.class) & is.na(secondary.class.dm)) %>%
    write.table('~/work/dipep/tmp/check2.txt', sep = ',')
sink()

## 2017-02-15 Checking how classification is actually done, whether case reviewers overall
##            classifications have to match or whether its done on the components, i.e.
##            agreement on imaging, then treatment and then follow-up
sink(file = '~/work/dipep/tmp/case_review_check.txt')
dplyr::filter(master$case.review, !is.na(primary.class3)) %>%
    dplyr::select(screening,
                  img.class1, img.class2, img.class3,
                  trt.class1, trt.class2, trt.class3,
                  fup.class1, fup.class2, fup.class3,
                  primary.class1, primary.class2, primary.class3)
check <- mutate(master$case.review,
                img.match        = img.class1 == img.class2,
                trt.match        = trt.class1 == trt.class2,
                fup.match        = fup.class1 == fup.class2,
                primary.match    = primary.class1 == primary.class2,
                secondary.match  = secondary.class1 == secondary.class2)
table(check$img.match, useNA = 'ifany')
dplyr::filter(check, img.match == FALSE) %>%
    dplyr::select(screening,
                  img.class1, img.class2, img.class3,
                  trt.class1, trt.class2, trt.class3,
                  fup.class1, fup.class2, fup.class3,
                  primary.class1, primary.class2, primary.class3)
table(check$trt.match, useNA = 'ifany')
dplyr::filter(check, trt.match == FALSE) %>%
    dplyr::select(screening,
                  img.class1, img.class2, img.class3,
                  trt.class1, trt.class2, trt.class3,
                  fup.class1, fup.class2, fup.class3,
                  primary.class1, primary.class2, primary.class3)
table(check$fup.match, useNA = 'ifany')
dplyr::filter(check, fup.match == FALSE) %>%
    dplyr::select(screening,
                  img.class1, img.class2, img.class3,
                  trt.class1, trt.class2, trt.class3,
                  fup.class1, fup.class2, fup.class3,
                  primary.class1, primary.class2, primary.class3)
table(check$primary.match, useNA = 'ifany')
dplyr::filter(check, primary.match == FALSE) %>%
    dplyr::select(screening,
                  img.class1, img.class2, img.class3,
                  trt.class1, trt.class2, trt.class3,
                  fup.class1, fup.class2, fup.class3,
                  primary.class1, primary.class2, primary.class3)
table(check$secondary.match, useNA = 'ifany')
dplyr::filter(check, secondary.match == FALSE) %>%
    dplyr::select(screening,
                  img.class1, img.class2, img.class3,
                  trt.class1, trt.class2, trt.class3,
                  fup.class1, fup.class2, fup.class3,
                  primary.class1, primary.class2, primary.class3)
sink()

## 2016-12-14 Deriving the number of missing observations for a subset
test <- dipep_incomplete(df = dipep,
                         id = screening,
                         heart.rate, respiratory.rate, bp.systolic, bp.diastolic) %>%
        group_by(group, n_missing) %>%
        tally()
test <- dipep_incomplete(df = dipep,
                         heart.rate, respiratory.rate, bp.systolic, bp.diastolic,
                         summarise = TRUE)

## 2016-12-07 Resolving multivariable model in light of tweaks to dipep_glm()
##            restructuring function and obtaining predicted probabilities
dipep_glm(df = dipep,
          predictor = c('age', 'bmi', 'temperature', 'bp.diastolic', 'bp.systolic',
                        'o2.saturation', 'respiratory.rate', 'heart.rate', 'smoking',
                        'prev.preg.problem', 'pregnancies.under', 'pregnancies.over',
                        'presenting.features.pleuritic', 'presenting.features.non.pleuritic',
                        'presenting.features.sob.rest', 'presenting.features.sob.exertion',
                        'presenting.features.cough', 'presenting.features.haemoptysis',
                        'presenting.features.syncope', 'presenting.features.palpitations',
                        'presenting.features.other', 'surgery',
                        'history.thrombosis', 'history.veins', ## 'history.iv.drug',
                        'thrombosis', 'trimester', 'this.pregnancy.problems',
                        'thrombo', 'multiple.preg', 'travel', 'immobil',
                        'ecg', 'xray'),
          model = 'Saturated Multivariable Logistic Model')

## 2016-12-02 Testing AUC annotation
logistic <- list()
## Age
logistic$age <- dipep_glm(df = dipep,
                          predictor = 'age')

## 2016-11-22 Checking conversion of NA to values for categorical variables
#######################################################################
## Combine required variables into one coherent data frame (would be ##
## nice if data management provided this functionality in Prospect   ##
## but that is highly unlikely to happen).                           ##
#######################################################################
## Subset variables from each dataframe
##
## Demographics
t1 <- dplyr::select(master$womans.details,
                    screening,
                    group,
                    site,
                    event.name,
                    year.of.birth,
                    ethnicity,
                    marital.status,
                    employment,
                    height,
                    weight,
                    smoking)
## Previous Pregnancies
t2 <- dplyr::select(master$previous.pregnancies,
                    screening,
                    group,
                    site,
                    event.name,
                    pregnancies.over,
                    pregnancies.under,
                    prev.preg.problem)
## Presenting Features
t3 <- dplyr::select(master$presenting.features,
                    screening,
                    group,
                    site,
                    event.name,
                    presenting.features.pleuritic,
                    presenting.features.non.pleuritic,
                    presenting.features.sob.exertion,
                    presenting.features.sob.rest,
                    presenting.features.haemoptysis,
                    presenting.features.cough,
                    presenting.features.syncope,
                    presenting.features.palpitations,
                    presenting.features.other,
                    other.symptoms.specify,
                    incidental,
                    heart.rate,
                    respiratory.rate,
                    o2.saturation,
                    bp.systolic,
                    bp.diastolic,
                    temperature,
                    dvt,
                    ecg,
                    ecg.specify,
                    xray,
                    xray.specify,
                    life.support.presentation,
                    diagnosis.post,
                    d.dimer,
                    d.dimer.high)
## Thrombotic Event
t4 <- dplyr::select(master$thrombotic.events,
                    screening,
                    group,
                    site,
                    event.name,
                    thromb.event)
## Thromboprophylaxis
t5 <- dplyr::select(master$thromboprophylaxis,
                    screening,
                    group,
                    site,
                    event.name,
                    thromboprophylaxis)
## EQ5D
t6 <- dplyr::select(master$eq5d,
                    screening,
                    group,
                    site,
                    ## event.name,
                    mobility,
                    self.care,
                    usual.activity,
                    pain.discomfort,
                    anxiety.depression,
                    health.scale.number)
## Medical History
t7 <- dplyr::select(master$med.hist,
                    screening,
                    group,
                    site,
                    event.name,
                    history.thrombosis,
                    history.veins,
                    history.iv.drug,
                    injury,
                    thrombo,
                    thrombosis,
                    medical.probs,
                    surgery,
                    surgery.other,
                    injury)
## This Pregnancy
t8 <- dplyr::select(master$pregnancy,
                    screening,
                    group,
                    site,
                    event.name,
                    preg.post,
                    edd,
                    multiple.preg,
                    num.fetus,
                    travel,
                    immobil)
## Details of medical history problems
t9 <- dplyr::select(master$med.hist.problems,
                    screening,
                    group,
                    site,
                    event.name,
                    medical.specify,
                    medical.other)
## Details of medical history problems
t10 <- dplyr::select(master$pregnancy.problems,
                     screening,
                     group,
                     site,
                     event.name,
                     this.preg.problem.specify,
                     this.preg.problem.other)
## Details of medication (required for derivation of PERC score)
t11 <- dplyr::select(master$therapy,
                     screening,
                     group,
                     site,
                     event.name,
                     other.medication,
                     other.medication.specify)
## Extract the event date from the screening froms as
## for some reason the event.date is not recorded in any
## form and instead the consent.date is to be used as a
## proxy for baseline
event.date.dvt <- dplyr::select(master$screening.dvt,
                                screening,
                                group,
                                site,
                                consent.date)
event.date.suspected.pe <- dplyr::select(master$screening.suspected.pe,
                                         screening,
                                         group,
                                         site,
                                         consent.date)
event.date <- rbind(event.date.dvt,
                    event.date.suspected.pe)
rm(event.date.dvt, event.date.suspected.pe)
names(event.date) <- gsub('consent', 'event', names(event.date))
## Merge the subsets
merge.by <- c('screening', 'group', 'site', 'event.name')
t <- merge(t1,
           t2,
           by    = merge.by,
           all   = TRUE) %>%
    merge(.,
          t3,
          by    = merge.by,
          all   = TRUE) %>%
    merge(.,
          t4,
          by    = merge.by,
          all   = TRUE) %>%
    merge(.,
          t5,
          by    = merge.by,
          all   = TRUE) %>%
    merge(.,
          t6,
          by    = c('screening', 'group', 'site'),
          all   = TRUE) %>%
    merge(.,
          t7,
          by    = merge.by,
          all   = TRUE) %>%
    merge(.,
          t8,
          by    = merge.by,
          all   = TRUE) %>%
    merge(.,
          t9,
          by    = merge.by,
          all   = TRUE) %>%
    merge(.,
          t10,
          by    = merge.by,
          all   = TRUE) %>%
    merge(.,
          t11,
          by    = merge.by,
          all   = TRUE)
## Now do two merges with the event.date, one to get a master dataset...
dipep <- merge(t,
               event.date,
               by    = c('screening', 'group', 'site'),
               all.x = TRUE)
## ...and one to get the IDs of those who have an event.date but nothing else...
master$missing <- merge(t,
                        event.date,
                        by  = c('screening', 'group', 'site'),
                        all.y = TRUE) %>%
                  filter(is.na(year.of.birth)) %>%
                  dplyr::select(screening, event.date, group, site, year.of.birth)
## Table variables for checking
print('BMI')
table(dipep$bmi, useNA = 'ifany')
print('Pregnancies Over')
table(dipep$pregnancies.over, useNA = 'ifany')
print('Pregnancies Under')
table(dipep$pregnancies.under, useNA = 'ifany')
print('Temperature')
table(dipep$temperature, useNA = 'ifany')
print('BP Diastolic')
table(dipep$bp.diastolic, useNA = 'ifany')
print('BP Systolic')
table(dipep$bp.systolic, useNA = 'ifany')
print('O2 Saturation')
table(dipep$o2.saturation, useNA = 'ifany')
print('Respiratory Rate')
table(dipep$respiratory.rate, useNA = 'ifany')
print('Smoking')
table(dipep$smoking, useNA = 'ifany')
print('ECG')
table(dipep$ecg, useNA = 'ifany')
print('XRay')
table(dipep$xray, useNA = 'ifany')
print('Medical Problem')
table(dipep$medical.specify, useNA = 'ifany')
print('Problems this pregnancy')
table(dipep$this.preg.problem.specify, useNA = 'ifany')
print('Pleuritic')
table(dipep$presenting.features.pleuritic, useNA = 'ifany')
print('Non-Pleuritic')
table(dipep$presenting.features.non.pleuritic, useNA = 'ifany')
print('SOB Exertion')
table(dipep$presenting.features.sob.exertion, useNA = 'ifany')
print('SOB Rest')
table(dipep$presenting.features.sob.rest, useNA = 'ifany')
print('Cough')
table(dipep$presenting.features.cough, useNA = 'ifany')
print('Haemoptysis')
table(dipep$presenting.features.haemoptysis, useNA = 'ifany')
print('Syncope')
table(dipep$presenting.features.syncope, useNA = 'ifany')
print('Palpitations')
table(dipep$presenting.features.palpitations, useNA = 'ifany')
print('Other')
table(dipep$presenting.features.other, useNA = 'ifany')
print('Thrombosis')
table(dipep$history.thrombosis, useNA = 'ifany')
print('History Veins')
table(dipep$history.veins, useNA = 'ifany')
print('History IV Drugs')
table(dipep$history.iv.drug, useNA = 'ifany')
print('Thrombo')
table(dipep$thrombo, useNA = 'ifany')
print('Multiple Pregnancies')
table(dipep$multiple.preg, useNA = 'ifany')
print('Travel')
table(dipep$travel, useNA = 'ifany')
print('Immobil')
table(dipep$immobil, useNA = 'ifany')
#######################################################################
## Derive variables (something it would be nice if Data Management   ##
## could do in Prospect as then BMI, age, etc. would all be          ##
## standardised and reusable and whoever does the QA will not have to##
## duplicate the work of deriving variables).                        ##
#######################################################################
dipep <- mutate(dipep,
                bmi = weight / (height / 100)^2,
                age = 2016 - year.of.birth,
                pregnancies.over.cat = ifelse(pregnancies.over == 0,
                                             yes = 0,
                                             no  = 1),
                pregnancies.over.cat = ifelse(is.na(pregnancies.over.cat),
                                              yes = 0,
                                              no  = pregnancies.over.cat),
                pregnancies.under.cat = ifelse(pregnancies.under == 0,
                                              yes = 0,
                                              no  = 1),
                pregnancies.under.cat = ifelse(is.na(pregnancies.under.cat),
                                              yes = 0,
                                              no  = pregnancies.under.cat),
                temperature.cat = ifelse(temperature <= 37.4,
                                         yes = 0,
                                         no  = 1),
                temperature.cat = ifelse(is.na(temperature.cat),
                                         yes = 0,
                                         no  = temperature.cat),
                bp.diastolic.cat = ifelse(bp.diastolic >= 60,
                                          yes = 0,
                                          no  = 1),
                bp.diastolic.cat = ifelse(is.na(bp.diastolic.cat),
                                          yes = 0,
                                          no  = bp.diastolic.cat),
                bp.systolic.cat = ifelse(bp.systolic >= 80,
                                         yes = 0,
                                         no  = 1),
                bp.systolic.cat = ifelse(is.na(bp.systolic.cat),
                                          yes = 0,
                                          no  = bp.systolic.cat),
                o2.saturation.cat = ifelse(o2.saturation >= 95,
                                           yes = 0,
                                           no  = 1),
                o2.saturation.cat = ifelse(is.na(o2.saturation),
                                           yes = 0,
                                           no  = o2.saturation.cat),
                respiratory.rate.cat = ifelse(respiratory.rate <= 24,
                                              yes = 0,
                                              no  = 1),
                respiratory.rate.cat = ifelse(is.na(respiratory.rate),
                                              yes = 0,
                                              no  = respiratory.rate.cat),
                bmi.cat = ifelse(bmi >= 30,
                                 yes = 1,
                                 no  = 0),
                bmi.cat = ifelse(is.na(bmi),
                                 yes = 0,
                                 no  = bmi.cat),
                gestation = 280 - (ymd(edd) - ymd(event.date)),
                trimester = ifelse(gestation < 98,
                                   yes = 0,
                                   no  = ifelse(gestation >= 98 & gestation < 196,
                                                yes = 1,
                                                ifelse(gestation >= 196 & preg.post == 'Pregnant',
                                                       yes = 2,
                                                       no  = 3))),
                trimester = ifelse(is.na(trimester),
                                   yes = 0,
                                   no  = trimester),
                heart.rate.cat = ifelse(trimester != 2 & heart.rate > 100,
                                        yes = 1,
                                        no  = ifelse(trimester == 2 & heart.rate > 110,
                                                     yes = 1,
                                                     no  = 0)),
                heart.rate.cat = ifelse(is.na(heart.rate.cat),
                                        yes = 0,
                                        no  = heart.rate.cat),
                ## See email 2016-10-27 @ 10:46 from s.goodacre@sheffield.ac.uk
                cesarean = ifelse(grepl('c*esarian|c*section|caesarean|emcs|lscs|c/s', surgery.other, ignore.case = TRUE),
                                  yes = 1,
                                  no  = 0),
                cesarean = ifelse(is.na(cesarean),
                                  yes = 0,
                                  no  = cesarean),
                smoking.cat = ifelse(smoking == 'current' | smoking == 'gave up during pregnancy',
                                     yes = 1,
                                     no  = 0),
                smoking.cat = ifelse(is.na(smoking.cat),
                                     yes = 0,
                                     no  = smoking.cat),
                age.cat = ifelse(age > 35,
                                 yes = 1,
                                 no  = 0),
                age.cat = ifelse(is.na(age.cat),
                                 yes = 0,
                                 no  = age.cat),
                ecg.cat = ifelse(ecg == 'Abnormal',
                                 yes = 1,
                                 no  = 0),
                ecg.cat = ifelse(is.na(ecg.cat),
                                 yes = 0,
                                 no  = ecg.cat),
                xray.cat = ifelse(xray == 'Abnormal',
                                  yes = 1,
                                  no  = 0),
                xray.cat = ifelse(is.na(xray.cat),
                                  yes = 0,
                                  no  = xray.cat),
                existing.medical = ifelse(medical.specify == 'Autoimmune diseases' |
                                          medical.specify == 'Cancer' |
                                          medical.specify == 'Cardiac disease (congenital or acquired)' |
                                          medical.specify == 'Diabetes' |
                                          medical.specify == 'Gross varicose veins' |
                                          medical.specify == 'Haematological disorders e.g. sickle cell disease' |
                                          medical.specify == 'Inflammatory disorders e.g. inflammatory bowel disease' |
                                          medical.specify == 'Malignancy within 6 months' |
                                          medical.specify == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera'  |
                                          medical.specify == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease',
                                          yes = 1,
                                          no  = 0),
                existing.medical = ifelse(is.na(existing.medical),
                                          yes = 0,
                                          no  = existing.medical),
                this.pregnancy.problems = ifelse(this.preg.problem.specify == 'Dehydration requiring admission' |
                                                 this.preg.problem.specify == 'Eclampsia' |
                                                 this.preg.problem.specify == 'Gestational diabetes' |
                                                 this.preg.problem.specify == 'Haemorrhage' |
                                                 this.preg.problem.specify == 'Hyperemesis requiring admission' |
                                                 this.preg.problem.specify == 'Ovarian hyperstimulation syndrome' |
                                                 this.preg.problem.specify == 'Post-partum haemorrhage requiring transfusion' |
                                                 this.preg.problem.specify == 'Pre-eclampsia (hypertension and proteinuria)' |
                                                 this.preg.problem.specify == 'Preterm birth or mid trimester loss' |
                                                 this.preg.problem.specify == 'Severe infection e.g. pyelonephritis' |
                                                 this.preg.problem.specify == 'Stillbirth',
                                                 yes = 1,
                                                 no  = 0),
                this.pregnancy.problems = ifelse(is.na(this.pregnancy.problems),
                                                 yes = 0,
                                                 no  = this.pregnancy.problems),
                diagnosis.post.pe = ifelse(grepl('pe', diagnosis.post, ignore.case = TRUE) |
                                           grepl('pulmonary embo', diagnosis.post, ignore.case = TRUE) |
                                           grepl('p\\.e\\.', diagnosis.post, ignore.case = TRUE),
                                           yes = 1,
                                           no  = 0),
                ## ## Some really unhelpful entry that needs correcting
                diagnosis.post.pe = ifelse(grepl('UNLIKELY P\\.E\\.', diagnosis.post, ignore.case = TRUE) |
                                           grepl('rule out PE', diagnosis.post, ignore.case = TRUE),
                                           yes = 0,
                                           no  = 1),
                ## NEW
                presenting.features.pleuritic = ifelse(is.na(presenting.features.pleuritic),
                                                       yes = 'Not Ticked',
                                                       no  = as.character(presenting.features.pleuritic)),
                presenting.features.non.pleuritic = ifelse(is.na(presenting.features.non.pleuritic),
                                                       yes = 'Not Ticked',
                                                       no  = as.character(presenting.features.non.pleuritic)),
                presenting.features.sob.exertion = ifelse(is.na(presenting.features.sob.exertion),
                                                       yes = 'Not Ticked',
                                                       no  = as.character(presenting.features.sob.exertion)),
                presenting.features.sob.rest = ifelse(is.na(presenting.features.sob.rest),
                                                       yes = 'Not Ticked',
                                                       no  = as.character(presenting.features.sob.rest)),
                presenting.features.haemoptysis = ifelse(is.na(presenting.features.haemoptysis),
                                                       yes = 'Not Ticked',
                                                       no  = as.character(presenting.features.haemoptysis)),
                presenting.features.cough = ifelse(is.na(presenting.features.cough),
                                                       yes = 'Not Ticked',
                                                       no  = as.character(presenting.features.cough)),
                presenting.features.syncope = ifelse(is.na(presenting.features.syncope),
                                                       yes = 'Not Ticked',
                                                       no  = as.character(presenting.features.syncope)),
                presenting.features.palpitations = ifelse(is.na(presenting.features.palpitations),
                                                       yes = 'Not Ticked',
                                                       no  = as.character(presenting.features.palpitations)),
                presenting.features.other = ifelse(is.na(presenting.features.other),
                                                       yes = 'Not Ticked',
                                                       no  = as.character(presenting.features.other)),
                history.thrombosis = ifelse(is.na(history.thrombosis),
                                            yes = 'No',
                                            no  = as.character(history.thrombosis)),
                history.veins = ifelse(is.na(history.veins),
                                            yes = 'No',
                                            no  = as.character(history.veins)),
                history.iv.drug = ifelse(is.na(history.iv.drug),
                                            yes = 'No',
                                            no  = as.character(history.iv.drug)),
                thrombo = ifelse(is.na(thrombo),
                                            yes = 'No',
                                            no  = as.character(thrombo)),
                multiple.preg = ifelse(is.na(multiple.preg),
                                            yes = 'No',
                                            no  = as.character(multiple.preg)),
                travel = ifelse(is.na(travel),
                                            yes = 'No',
                                            no  = as.character(travel)),
                immobil = ifelse(is.na(immobil),
                                            yes = 'No',
                                            no  = as.character(immobil)),
                ecg = ifelse(is.na(ecg),
                                            yes = 'Not performed',
                                            no  = as.character(ecg))
                ## D-Dimer
                ## d.dimer.high = ifelse(d.dimer > ,
                ##                       yes = 1,
                ##                       no  = 0),
                ## d.dimer.very.high = ifelse(d.dimer > ,
                ##                            yes = 1,
                ##                            no  = 0)
                )

## Table variables for checking
print('BMI')
table(dipep$bmi.cat, useNA = 'ifany')
print('Pregnancies Over')
table(dipep$pregnancies.over.cat, useNA = 'ifany')
print('Pregnancies Under')
table(dipep$pregnancies.under.cat, useNA = 'ifany')
print('Temperature')
table(dipep$temperature.cat, useNA = 'ifany')
print('BP Diastolic')
table(dipep$bp.diastolic.cat, useNA = 'ifany')
print('BP Systolic')
table(dipep$bp.systolic.cat, useNA = 'ifany')
print('O2 Saturation')
table(dipep$o2.saturation.cat, useNA = 'ifany')
print('Respiratory Rate')
table(dipep$respiratory.rate.cat, useNA = 'ifany')
print('Smoking')
table(dipep$smoking.cat, useNA = 'ifany')
print('ECG')
table(dipep$ecg.cat, useNA = 'ifany')
print('XRay')
table(dipep$xray.cat, useNA = 'ifany')
print('Medical Problem')
table(dipep$existing.medical, useNA = 'ifany')
print('Problems this pregnancy')
table(dipep$this.pregnancy.problems, useNA = 'ifany')
print('Pleuritic')
table(dipep$presenting.features.pleuritic, useNA = 'ifany')
print('Non-Pleuritic')
table(dipep$presenting.features.non.pleuritic, useNA = 'ifany')
print('SOB Exertion')
table(dipep$presenting.features.sob.exertion, useNA = 'ifany')
print('SOB Rest')
table(dipep$presenting.features.sob.rest, useNA = 'ifany')
print('Cough')
table(dipep$presenting.features.cough, useNA = 'ifany')
print('Haemoptysis')
table(dipep$presenting.features.haemoptysis, useNA = 'ifany')
print('Syncope')
table(dipep$presenting.features.syncope, useNA = 'ifany')
print('Palpitations')
table(dipep$presenting.features.palpitations, useNA = 'ifany')
print('Other')
table(dipep$presenting.features.other, useNA = 'ifany')
print('Thrombosis')
table(dipep$history.thrombosis, useNA = 'ifany')
print('History Veins')
table(dipep$history.veins, useNA = 'ifany')
print('History IV Drugs')
table(dipep$history.iv.drug, useNA = 'ifany')
print('Thrombo')
table(dipep$thrombo, useNA = 'ifany')
print('Multiple Pregnancies')
table(dipep$multiple.preg, useNA = 'ifany')
print('Travel')
table(dipep$travel, useNA = 'ifany')
print('Immobil')
table(dipep$immobil, useNA = 'ifany')

## Ensure everything is a factor
dipep <- mutate(dipep,
                bmi.cat = factor(bmi.cat,
                                 levels = c(0, 1),
                                 labels = c('Low', 'High')),
                pregnancies.under.cat = factor(pregnancies.under.cat,
                                                levels = c(0, 1),
                                                labels = c('No previous pregnancies < 24 weeks',
                                                           '>= 1 previous pregnancy < 24 weeks')),
                pregnancies.over.cat = factor(pregnancies.over.cat,
                                               levels = c(0, 1),
                                               labels = c('No previous pregnancies < 24 weeks',
                                                          '>o= 1 previous pregnancy < 24 weeks')),
                temperature.cat = factor(temperature.cat,
                                         levels = c(0, 1),
                                         labels = c('Low', 'High')),
                ## heart.rate.cat = factor(heart.rate.cat,
                ##                         levels = c(0, 1),
                ##                         labels = c('Low', 'High')),
                bp.diastolic.cat = factor(bp.diastolic.cat,
                                          levels = c(0, 1),
                                          labels = c('Low', 'High')),
                bp.systolic.cat = factor(bp.systolic.cat,
                                         levels = c(0, 1),
                                         labels = c('Low', 'High')),
                heart.rate.cat = factor(heart.rate.cat,
                                        levels = c(0, 1),
                                        labels = c('Low', 'High')),
                o2.saturation.cat = factor(o2.saturation.cat,
                                           levels = c(0, 1),
                                           labels = c('Low', 'High')),
                respiratory.rate.cat = factor(respiratory.rate.cat,
                                              levels = c(0, 1),
                                              labels = c('Low', 'High')),
                cesarean = factor(cesarean,
                                  levels = c(0, 1),
                                  labels = c('No Cesarean', 'Cesarean')),
                smoking.cat = factor(smoking.cat,
                                 levels = c(0, 1),
                                 labels = c('Non-smoker', 'Smoker')),
                age.cat = factor(age.cat,
                                 levels = c(0, 1),
                                 labels = c('Young', 'Old')),
                ecg.cat = factor(ecg.cat,
                                 levels = c(0, 1),
                                 labels = c('Normal ECG', 'Abnormal ECG')),
                xray.cat = factor(xray.cat,
                                  levels = c(0, 1),
                                  labels = c('Normal ECG', 'Abnormal ECG')),
                trimester = factor(trimester,
                                   levels = c(0, 1, 2, 3),
                                   labels = c('1st Trimester', '2nd Trimester', '3rd Trimester', 'Post-Partum')),
                existing.medical = factor(existing.medical,
                                          levels = c(0, 1),
                                          labels = c('No', 'Yes')),
                this.pregnancy.problems = factor(this.pregnancy.problems,
                                                 levels = c(0, 1),
                                                 labels = c('No', 'Yes')),
                diagnosis.post.pe = factor(diagnosis.post.pe,
                                           levels = c(0, 1),
                                           labels = c('No PE', 'PE')),
                presenting.features.pleuritic  = factor(presenting.features.pleuritic),
                presenting.features.non.pleuritic  = factor(presenting.features.non.pleuritic,
                                                        levels = c(0, 1),
                                                        labels = c('Not Ticked', 'Ticked')),
                presenting.features.sob.exertion  = factor(presenting.features.sob.exertion,
                                                        levels = c(0, 1),
                                                        labels = c('Not Ticked', 'Ticked')),
                presenting.features.sob.rest  = factor(presenting.features.sob.rest,
                                                        levels = c(0, 1),
                                                        labels = c('Not Ticked', 'Ticked')),
                presenting.features.cough  = factor(presenting.features.cough,
                                                        levels = c(0, 1),
                                                        labels = c('Not Ticked', 'Ticked')),
                presenting.features.haemoptysis  = factor(presenting.features.haemoptysis,
                                                        levels = c(0, 1),
                                                        labels = c('Not Ticked', 'Ticked')),
                history.thrombosis = factor(history.thrombosis,
                                            levels = c(0, 1),
                                            labels = c('No', 'Yes')),
                history.veins = factor(history.veins,
                                            levels = c(0, 1),
                                            labels = c('No', 'Yes')),
                history.iv.drug = factor(history.iv.drug,
                                            levels = c(0, 1),
                                            labels = c('No', 'Yes')),
                thrombo = factor(thrombo,
                                            levels = c(0, 1),
                                            labels = c('No', 'Yes')),
                multiple.preg = factor(multiple.preg,
                                            levels = c(0, 1),
                                            labels = c('No', 'Yes')),
                travel = factor(travel,
                                            levels = c(0, 1),
                                            labels = c('No', 'Yes')),
                immobil = factor(immobil,
                                            levels = c(0, 1),
                                            labels = c('No', 'Yes')),
                ecg = factor(ecg,
                                            levels = c(0, 1, 2),
                                            labels = c('Not performed', 'Normal', 'Abnormal'))
                ## ToDo - Thresholds
                ## d.dimer.high = factor(d.dimer.high,
                ##                       levels = c(0, 1),
                ##                       labels = c('Normal', 'High')),
                ## d.dimer.very.high = ifelse(d.dimer.very.high,
                ##                            levels = c(0, 1),
                ##                            labels = c('Normal', 'Very High'))
                )

## Table variables for checking
print('BMI')
table(dipep$bmi.cat, useNA = 'ifany')
print('Pregnancies Over')
table(dipep$pregnancies.over.cat, useNA = 'ifany')
print('Pregnancies Under')
table(dipep$pregnancies.under.cat, useNA = 'ifany')
print('Temperature')
table(dipep$temperature.cat, useNA = 'ifany')
print('BP Diastolic')
table(dipep$bp.diastolic.cat, useNA = 'ifany')
print('BP Systolic')
table(dipep$bp.systolic.cat, useNA = 'ifany')
print('O2 Saturation')
table(dipep$o2.saturation.cat, useNA = 'ifany')
print('Respiratory Rate')
table(dipep$respiratory.rate.cat, useNA = 'ifany')
print('Smoking')
table(dipep$smoking.cat, useNA = 'ifany')
print('ECG')
table(dipep$ecg.cat, useNA = 'ifany')
print('XRay')
table(dipep$xray.cat, useNA = 'ifany')
print('Medical Problem')
table(dipep$existing.medical, useNA = 'ifany')
print('Problems this pregnancy')
table(dipep$this.pregnancy.problems, useNA = 'ifany')
print('Pleuritic')
table(dipep$presenting.features.pleuritic, useNA = 'ifany')
print('Non-Pleuritic')
table(dipep$presenting.features.non.pleuritic, useNA = 'ifany')
print('SOB Exertion')
table(dipep$presenting.features.sob.exertion, useNA = 'ifany')
print('SOB Rest')
table(dipep$presenting.features.sob.rest, useNA = 'ifany')
print('Cough')
table(dipep$presenting.features.cough, useNA = 'ifany')
print('Haemoptysis')
table(dipep$presenting.features.haemoptysis, useNA = 'ifany')
print('Syncope')
table(dipep$presenting.features.syncope, useNA = 'ifany')
print('Palpitations')
table(dipep$presenting.features.palpitations, useNA = 'ifany')
print('Other')
table(dipep$presenting.features.other, useNA = 'ifany')
print('Thrombosis')
table(dipep$history.thrombosis, useNA = 'ifany')
print('History Veins')
table(dipep$history.veins, useNA = 'ifany')
print('History IV Drugs')
table(dipep$history.iv.drug, useNA = 'ifany')
print('Thrombo')
table(dipep$thrombo, useNA = 'ifany')
print('Multiple Pregnancies')
table(dipep$multiple.preg, useNA = 'ifany')
print('Travel')
table(dipep$travel, useNA = 'ifany')
print('Immobil')
table(dipep$immobil, useNA = 'ifany')


## 2016-10-13 Sample data/function to ask question
set.seed(43290)
temp <- cbind(rnorm(n = 100, mean = 2, sd = 4),
              rnorm(n = 100, mean = 3, sd = 6)) %>% as.data.frame()
names(temp) <- c('eg1', 'eg2')
mean(temp$eg1)
mean(temp$eg2)
my_summarise <- function(df = temp,
                         to.sum = c('eg1', 'eg2'),
                         ...){
    ## ## Select columns
    df <- dplyr::select_(df, .dots = c(to.sum))
    ## Summarise
    results <- summarise_(df,
                          n = ~n(),
                          mean = lazyeval::interp(~mean(x, na.rm = TRUE),
                                                  x = as.name(to.sum)))
    return(results)
}
my_summarise(df = dipep, to.sum = 'respiratory.rate')


## 2016-10-12 Developing function to summarise by arbitrary classifier
## Testing function dipep_summarise()
dipep_summarise(df = master$presenting.features,
                grouping = group,
                to.sum = respiratory.rate)

## Could do everything using this approach perhaps, as will have a data.frame
## used in the model with all variables in then filter the predictors and pass
## to a summarise_each() within a function.
summarise_each(master$presenting.features,
               funs(mean(., na.rm = TRUE),
                    sd  (., na.rm = TRUE)))

## Overall can use...
summarise(master$presenting.features,
          mean = mean(respiratory.rate, na.rm = TRUE),
          sd = sd(respiratory.rate, na.rm = TRUE),
          p25 = quantile(respiratory.rate,
                         probs = c(0.25),
                         na.rm = TRUE),
          median = quantile(respiratory.rate,
                            probs = c(0.5),
                            na.rm = TRUE),
          p75 = quantile(respiratory.rate,
                         probs = c(0.75),
                         na.rm = TRUE),
          min = min(respiratory.rate, na.rm = TRUE),
          max = max(respiratory.rate, na.rm = TRUE),
          n = n(),
          missing = sum(is.na(respiratory.rate)))

## By group, preceed with grouping
group_by(master$presenting.features, group) %>%
    summarise(mean = mean(respiratory.rate, na.rm = TRUE),
              sd = sd(respiratory.rate, na.rm = TRUE),
              p25 = quantile(respiratory.rate,
                             probs = c(0.25),
                             na.rm = TRUE),
              median = quantile(respiratory.rate,
                                probs = c(0.5),
                                na.rm = TRUE),
              p75 = quantile(respiratory.rate,
                             probs = c(0.75),
                             na.rm = TRUE),
              min = min(respiratory.rate, na.rm = TRUE),
              max = max(respiratory.rate, na.rm = TRUE),
              n = n(),
              missing = sum(is.na(respiratory.rate)))
