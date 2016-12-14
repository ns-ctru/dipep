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
