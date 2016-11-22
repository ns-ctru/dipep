## Filename         import.R
## Created          2016-02-29
## Author           n.shephard@sheffield.ac.uk
##
## Description      Imports the data for the DiPEP study that was exported
##                  from Prospect database

## Load libraries
library(dplyr)
library(lubridate)
library(readr)
library(reshape2)

#######################################################################
## Read in each file to a unique data frame that is part of a list   ##
#######################################################################
master <- list()
#######################################################################
## Lookups.csv                                                       ##
#######################################################################
## Read in the data dictionary (or 'lookups') to ease converting
## factor variables
master$data.dictionary <- read_dipep(file       = "Lookups.csv",
                                     header     = TRUE,
                                     sep        = ',',
                                     dictionary = NULL)

#######################################################################
## 30 day follow-up.csv                                              ##
#######################################################################
master$follow.up.30.day <- read_dipep(file             = "30 day follow-up.csv",
                                      header           = TRUE,
                                      sep              = ',',
                                      convert.dates    = TRUE,
                                      dictionary       = master$data.dictionary)
#######################################################################
## Annotations.csv                                                   ##
#######################################################################
master$annotations <- read_dipep(file   = "Annotations.csv",
                                 header           = TRUE,
                                 sep              = ',',
                                 convert.dates    = TRUE,
                                 dictionary       = master$data.dictionary)
#######################################################################
## Blood sample.csv                                                  ##
#######################################################################
master$blood.sample <- read_dipep(file       = "Blood sample.csv",
                                  header     = TRUE,
                                  sep        = ',',
                                  dictionary = master$data.dictionary)
#######################################################################
## Client Service Receipt Inventory.csv                              ##
#######################################################################
master$service.receipt <- read_dipep(file   = "Client Service Receipt Inventory.csv",
                                     header           = TRUE,
                                     sep              = ',',
                                     convert.dates    = TRUE,
                                     dictionary       = master$data.dictionary)
#######################################################################
## Client Service Receipt Inventory - Hospital.csv                   ##
#######################################################################
master$service.receipt.hospital <- read_dipep(file   = "Client Service Receipt Inventory - Hospital.csv",
                                              header           = TRUE,
                                              sep              = ',',
                                              convert.dates    = TRUE,
                                              dictionary       = master$data.dictionary)
#######################################################################
## Completion.csv                                                    ##
#######################################################################
master$completion <- read_dipep(file   = "Completion.csv",
                                header           = TRUE,
                                sep              = ',',
                                convert.dates    = TRUE,
                                dictionary       = master$data.dictionary)
#######################################################################
## Contact.csv                                                       ##
#######################################################################
master$contact <- read_dipep(file   = "Contact.csv",
                             header           = TRUE,
                             sep              = ',',
                             convert.dates    = TRUE,
                             dictionary       = master$data.dictionary)
#######################################################################
## Delivery.csv                                                      ##
#######################################################################
master$delivery <- read_dipep(file   = "Delivery.csv",
                              header           = TRUE,
                              sep              = ',',
                              convert.dates    = TRUE,
                              dictionary       = master$data.dictionary)
#######################################################################
## Discrepancies.csv                                                 ##
#######################################################################
master$discrepancies <- read_dipep(file   = "Discrepancies.csv",
                                   header           = TRUE,
                                   sep              = ',',
                                   convert.dates    = TRUE,
                                   dictionary       = master$data.dictionary)
#######################################################################
## EQ-5D-5L.csv                                                      ##
#######################################################################
master$eq5d <- read_dipep(file   = "EQ-5D-5L.csv",
                          header           = TRUE,
                          sep              = ',',
                          convert.dates    = TRUE,
                          dictionary       = master$data.dictionary)
## Non-standard responses have been used, convert these to the standard
## responses
master$eq5d <- mutate(master$eq5d,
                      mobility_ = ifelse(mobility == 'I have no problems in walking about', 'None',
                                   ifelse(mobility == 'I have slight problems in walking about', 'Slight',
                                    ifelse(mobility == 'I have moderate problems in walking about', 'Moderate',
                                     ifelse(mobility == 'I have severe problems in walking about', 'Severe',
                                      ifelse(mobility == 'I am unable to walk about', 'Extreme', NA))))),
                      self.care_ = ifelse(self.care == 'I have no problems washing or dressing myself', 'None',
                                    ifelse(self.care == 'I have slight problems washing or dressing myself', 'Slight',
                                     ifelse(self.care == 'I have moderate problems washing or dressing myself', 'Moderate',
                                      ifelse(self.care == 'I have severe problems washing or dressing myself', 'Severe',
                                       ifelse(self.care == 'I am unable to wash or dress myself', 'Extreme', NA))))),
                      usual.activity_ = ifelse(usual.activity == 'I have no problems doing my usual activities', 'None',
                                         ifelse(usual.activity == 'I have slight problems doing my usual activities', 'Slight',
                                          ifelse(usual.activity == 'I have moderate problems doing my usual activities', 'Moderate',
                                           ifelse(usual.activity == 'I have severe problems doing my usual activities', 'Severe',
                                            ifelse(usual.activity == 'I am unable to do my usual activities', 'Extreme', NA))))),
                      pain.discomfort_ = ifelse(pain.discomfort == 'I have no pain or discomfort', 'None',
                                         ifelse(pain.discomfort == 'I have slight pain or discomfort', 'Slight',
                                          ifelse(pain.discomfort == 'I have moderate pain or discomfort', 'Moderate',
                                           ifelse(pain.discomfort == 'I have severe pain or discomfort', 'Severe',
                                            ifelse(pain.discomfort == 'I have extreme pain or discomfort', 'Extreme', NA))))),
                      anxiety.depression_ = ifelse(anxiety.depression == 'I am not anxious or depressed', 'None',
                                         ifelse(anxiety.depression == 'I am slightly anxious or depressed', 'Slight',
                                          ifelse(anxiety.depression == 'I am moderately anxious or depressed', 'Moderate',
                                           ifelse(anxiety.depression == 'I am severely anxious or depressed', 'Severe',
                                           ifelse(anxiety.depression == 'I am extremely anxious or depressed', 'Extreme', NA))))))
master$eq5d <- dplyr::select(master$eq5d, -mobility, -self.care, -usual.activity, -pain.discomfort, -anxiety.depression)
names(master$eq5d) <- gsub('_', '', names(master$eq5d))
## Make sure they all have the same levels
master$eq5d$mobility           <- ordered(master$eq5d$mobility,
                                          levels = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'))
master$eq5d$self.care          <- ordered(master$eq5d$self.care,
                                          levels = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'))
master$eq5d$usual.activity     <- ordered(master$eq5d$usual.activity,
                                          levels = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'))
master$eq5d$pain.discomfort    <- ordered(master$eq5d$pain.discomfort,
                                          levels = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'))
master$eq5d$anxiety.depression <- ordered(master$eq5d$anxiety.depression,
                                          levels = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'))
## Derive EQ5D-5L Scores
## ToDo - Why isn't eq5d_score() functioning?
## master$eq5d <- eq5d_score(master$eq5d,
##                           mobility.response = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'),
##                           self.response     = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'),
##                           activity.response = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'),
##                           pain.response     = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'),
##                           anxiety.response  = c('None', 'Slight', 'Moderate', 'Severe', 'Extreme'))


#######################################################################
## Events.csv                                                        ##
#######################################################################
master$events <- read_dipep(file   = "Events.csv",
                            header           = TRUE,
                            sep              = ',',
                            convert.dates    = TRUE,
                            dictionary       = master$data.dictionary)
#######################################################################
## Follow-up sent.csv                                                ##
#######################################################################
master$follow.up.sent <- read_dipep(file   = "Follow-up sent.csv",
                                    header           = TRUE,
                                    sep              = ',',
                                    convert.dates    = TRUE,
                                    dictionary       = master$data.dictionary)
#######################################################################
## Forms.csv                                                         ##
#######################################################################
master$forms <- read_dipep(file   = "Forms.csv",
                           header           = TRUE,
                           sep              = ',',
                           convert.dates    = TRUE,
                           dictionary       = master$data.dictionary)
#######################################################################
## Individuals.csv                                                   ##
#######################################################################
master$individuals <- read_dipep("Individuals.csv",
                                 header           = TRUE,
                                 sep              = ',',
                                 convert.dates    = TRUE,
                                 dictionary       = master$data.dictionary)
#######################################################################
## Investigations.csv                                                ##
#######################################################################
master$investigations <- read_dipep("Investigations.csv",
                                    header           = TRUE,
                                    sep              = ',',
                                    convert.dates    = TRUE,
                                    dictionary       = master$data.dictionary)
#######################################################################
## Investigations - Investigation.csv                                ##
#######################################################################
master$investigations.investigation <- read_dipep("Investigations - Investigation.csv",
                                                  header           = TRUE,
                                                  sep              = ',',
                                                  convert.dates    = TRUE,
                                                  dictionary       = master$data.dictionary)
#######################################################################
## Medical History.csv                                               ##
#######################################################################
master$med.hist <- read_dipep("Medical History.csv",
                              header           = TRUE,
                              sep              = ',',
                              convert.dates    = TRUE,
                              dictionary       = master$data.dictionary)
#######################################################################
## Medical History - Medical problems.csv                            ##
#######################################################################
master$med.hist.problems <- read_dipep("Medical History - Medical problems.csv",
                                       header           = TRUE,
                                       sep              = ',',
                                       convert.dates    = TRUE,
                                       dictionary       = master$data.dictionary)
#######################################################################
## Medical History - Thrombophilia.csv                               ##
#######################################################################
master$med.hist.thrombophilia <- read_dipep("Medical History - Thrombophilia.csv",
                                            header           = TRUE,
                                            sep              = ',',
                                            convert.dates    = TRUE,
                                            dictionary       = master$data.dictionary)
#######################################################################
## Outcomes - infant.csv                                             ##
#######################################################################
master$outcome.infant <- read_dipep("Outcomes - infant.csv",
                                    header           = TRUE,
                                    sep              = ',',
                                    convert.dates    = TRUE,
                                    dictionary       = master$data.dictionary)
#######################################################################
## Outcomes - infant - Infant.csv                                    ##
#######################################################################
master$outcome.infant.infant <- read_dipep("Outcomes - infant - Infant.csv",
                                           header           = TRUE,
                                           sep              = ',',
                                           convert.dates    = TRUE,
                                           dictionary       = master$data.dictionary)
#######################################################################
## Outcomes - woman.csv                                              ##
#######################################################################
master$outcome.woman <- read_dipep("Outcomes - woman.csv",
                                   header           = TRUE,
                                   sep              = ',',
                                   convert.dates    = TRUE,
                                   dictionary       = master$data.dictionary)
#######################################################################
## Outcomes - woman - Maternal morbidity.csv                         ##
#######################################################################
master$outcome.woman.morbidity <- read_dipep("Outcomes - woman - Maternal morbidity.csv",
                                             header           = TRUE,
                                             sep              = ',',
                                             convert.dates    = TRUE,
                                             dictionary       = master$data.dictionary)
#######################################################################
## Presenting features.csv                                           ##
#######################################################################
master$presenting.features <- read_dipep("Presenting features.csv",
                                         header           = TRUE,
                                         sep              = ',',
                                         convert.dates    = TRUE,
                                         dictionary       = master$data.dictionary)
#######################################################################
## Previous pregnancies.csv                                          ##
#######################################################################
master$previous.pregnancies <- read_dipep("Previous pregnancies.csv",
                                          header           = TRUE,
                                          sep              = ',',
                                          convert.dates    = TRUE,
                                          dictionary       = master$data.dictionary)
#######################################################################
## Previous pregnancies - Previous pregnancy problems.csv            ##
#######################################################################
master$previous.pregnancies.problems <- read_dipep("Previous pregnancies - Previous pregnancy problems.csv",
                                                   header           = TRUE,
                                                   sep              = ',',
                                                   convert.dates    = TRUE,
                                                   dictionary       = master$data.dictionary)
#######################################################################
## Questionnaire Contact.csv                                         ##
#######################################################################
master$questionnaire.contact <- read_dipep("Questionnaire Contact.csv",
                                           header           = TRUE,
                                           sep              = ',',
                                           convert.dates    = TRUE,
                                           dictionary       = master$data.dictionary)
#######################################################################
## Screening DVT.csv                                                 ##
#######################################################################
master$screening.dvt <- read_dipep("Screening DVT.csv",
                                   header           = TRUE,
                                   sep              = ',',
                                   convert.dates    = TRUE,
                                   dictionary       = master$data.dictionary)
#######################################################################
## Screening non recruited.csv                                       ##
#######################################################################
master$screening.non.recruited <- read_dipep("Screening non recruited.csv",
                                             header           = TRUE,
                                             sep              = ',',
                                             convert.dates    = TRUE,
                                             dictionary       = master$data.dictionary)
#######################################################################
## Screening Suspected PE.csv                                        ##
#######################################################################
master$screening.suspected.pe <- read_dipep("Screening Suspected PE.csv",
                                            header           = TRUE,
                                            sep              = ',',
                                            convert.dates    = TRUE,
                                            dictionary       = master$data.dictionary)
#######################################################################
## Sign off.csv                                                      ##
#######################################################################
master$sign.off <- read_dipep("Sign off.csv",
                              header           = TRUE,
                              sep              = ',',
                              convert.dates    = TRUE,
                              dictionary       = master$data.dictionary)
#######################################################################
## Sites.csv                                                         ##
#######################################################################
master$sites <- read_dipep("Sites.csv",
                           header           = TRUE,
                           sep              = ',',
                           convert.dates    = TRUE,
                           dictionary       = master$data.dictionary)
#######################################################################
## Therapy.csv                                                       ##
#######################################################################
master$therapy <- read_dipep("Therapy.csv",
                             header           = TRUE,
                             sep              = ',',
                             convert.dates    = TRUE,
                             dictionary       = master$data.dictionary)
#######################################################################
## This Pregnancy continued.csv                                      ##
#######################################################################
master$pregnancy.continued <- read_dipep("This Pregnancy continued.csv",
                                         header           = TRUE,
                                         sep              = ',',
                                         convert.dates    = TRUE,
                                         dictionary       = master$data.dictionary)
#######################################################################
## This Pregnancy continued - Problems during this pregnancy.csv     ##
#######################################################################
master$pregnancy.problems <- read_dipep("This Pregnancy continued - Problems during this pregnancy.csv",
                                        header           = TRUE,
                                        sep              = ',',
                                        convert.dates    = TRUE,
                                        dictionary       = master$data.dictionary)
#######################################################################
## This Pregnancy.csv                                                ##
#######################################################################
master$pregnancy <- read_dipep("This Pregnancy.csv",
                               header           = TRUE,
                               sep              = ',',
                               convert.dates    = TRUE,
                               dictionary       = master$data.dictionary)
#######################################################################
## This Pregnancy - Immobility.csv                                   ##
#######################################################################
master$pregnancy.immobility <- read_dipep("This Pregnancy - Immobility.csv",
                                          header           = TRUE,
                                          sep              = ',',
                                          convert.dates    = TRUE,
                                          dictionary       = master$data.dictionary)
#######################################################################
## This Pregnancy - Long-haul travel.csv                             ##
#######################################################################
master$pregnancy.long.haul <- read_dipep("This Pregnancy - Long-haul travel.csv",
                                          header           = TRUE,
                                          sep              = ',',
                                          convert.dates    = TRUE,
                                          dictionary       = master$data.dictionary)
#######################################################################
## Thromboprophylaxis.csv                                            ##
#######################################################################
master$thromboprophylaxis <- read_dipep("Thromboprophylaxis.csv",
                                        header           = TRUE,
                                        sep              = ',',
                                        convert.dates    = TRUE,
                                        dictionary       = master$data.dictionary)
#######################################################################
## Thrombotic event.csv                                              ##
#######################################################################
master$thrombotic.events <- read_dipep("Thrombotic event.csv",
                                       header           = TRUE,
                                       sep              = ',',
                                       convert.dates    = TRUE,
                                       dictionary       = master$data.dictionary)
#######################################################################
## Unavailable Forms.csv                                             ##
#######################################################################
master$unavailable.forms <- read_dipep("Unavailable Forms.csv",
                                       header           = TRUE,
                                       sep              = ',',
                                       convert.dates    = TRUE,
                                       dictionary       = master$data.dictionary)
#######################################################################
## Womans details.csv                                                ##
#######################################################################
master$womans.details <- read_dipep("Womans details.csv",
                                    header           = TRUE,
                                    sep              = ',',
                                    convert.dates    = TRUE,
                                    dictionary       = master$data.dictionary)
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
rm(t, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, event.date)

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
                                               labels = c('No previous pregnancies > 24 weeks',
                                                          '>= 1 previous pregnancy > 24 weeks')),
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
                presenting.features.non.pleuritic  = factor(presenting.features.non.pleuritic),
                presenting.features.sob.exertion  = factor(presenting.features.sob.exertion),
                presenting.features.sob.rest  = factor(presenting.features.sob.rest),
                presenting.features.cough  = factor(presenting.features.cough),
                presenting.features.syncope  = factor(presenting.features.syncope),
                presenting.features.haemoptysis  = factor(presenting.features.haemoptysis),
                history.thrombosis = factor(history.thrombosis,
                                            levels = c(0, 1),
                                            labels = c('No', 'Yes')),
                history.veins = factor(history.veins),
                history.iv.drug = factor(history.iv.drug),
                thrombo = factor(thrombo),
                multiple.preg = factor(multiple.preg),
                travel = factor(travel),
                immobil = factor(immobil),
                ecg.cat = factor(ecg.cat,
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

## Add a dummy for PE for now
dipep$pe <- ifelse(runif(n = nrow(dipep)) > 0.7, 1, 0)
dipep$pe <- factor(dipep$pe,
                   levels = c(0, 1),
                   labels = c('No PE', 'PE'))
#######################################################################
## Derive clinical rules based on...                                 ##
##                                                                   ##
## Geneva (Wicki et al. 2001)                                        ##
## Simplified Geneva (http://dx.doi.org/10.1001/archinte.168.19.2131)##
## PERC (http://dx.doi.org/10.1111/j.1538-7836.2004.00790.x)         ##
## Wells (http://dx.doi.org/10.7326/0003-4819-135-2-200107170-00010) ##
#######################################################################
## Simplified Geneva
dipep <- mutate(dipep,
                simplified.age = ifelse(age > 65,
                                        yes = 1,
                                        no  = 0),
                ## ToDo - Check how to define previous PE
                simplified.previous = ifelse(dvt == 'Yes',
                                             yes = 1,
                                             no  = 0),
                simplified.surgery = ifelse(surgery == 'Yes',
                                            yes = 1,
                                            no  = 0),
                simplified.neoplasm = ifelse(medical.specify == 'Cancer',
                                             yes = 1,
                                             no  = 0),
                simplified.lower.limb.unilateral.pain = ifelse(grepl('leg pain', other.symptoms.specify, ignore.case = TRUE),
                                                    yes = 1,
                                                    no  = 0),
                ## There are however entries of 'Bilateral lower leg pain' which need correcting
                simplified.lower.limb.unilateral.pain = ifelse(grepl('leg pain', other.symptoms.specify, ignore.case = TRUE),
                                                    yes = 0,
                                                    no  = simplified.lower.limb.unilateral.pain),
                simplified.haemoptysis = ifelse(presenting.features.haemoptysis == 'Ticked',
                                                yes = 1,
                                                no  = 0),
                simplified.heart.rate = ifelse(heart.rate > 75,
                                               yes = 1,
                                               no  = 0),
                simplified.lower.limb.pain = ifelse(grepl('bilateral lower leg pain', other.symptoms.specify, ignore.case = TRUE) |
                                                    grepl('pain in legs', other.symptoms.specify, ignore.case = TRUE),
                                                    yes = 1,
                                                    no  = 0),
                simplified = simplified.age +
                             simplified.previous +
                             simplified.neoplasm +
                             simplified.lower.limb.unilateral.pain +
                             simplified.haemoptysis +
                             simplified.heart.rate +
                             simplified.lower.limb.pain)
## PERC
dipep <- mutate(dipep,
                perc.age = ifelse(age > 50,
                                  yes = 1,
                                  no  = 0),
                perc.heart.rate = ifelse(heart.rate > 100,
                                         yes = 1,
                                         no  = 0),
                perc.o2 = ifelse(o2.saturation < 95,
                                         yes = 1,
                                         no  = 0),
                perc.cough = ifelse(presenting.features.cough == 'Ticked',
                                    yes = 1,
                                    no  = 0),
                perc.haemoptysis = ifelse(presenting.features.haemoptysis == 'Ticked',
                                         yes = 1,
                                         no  = 0),
                perc.leg.swelling = ifelse(grepl('leg swelling', other.symptoms.specify, ignore.case = TRUE) |
                                           grepl('legs swelling', other.symptoms.specify, ignore.case = TRUE),
                                           yes = 1,
                                           no  = 0),
                perc.surgery = ifelse(surgery == 'Yes',
                                      yes = 1,
                                      no  = 0),
                ## perc.embolism = ifelse(,
                ##                        yes = 1,
                ##                        no  = 0),
                perc.hormone = ifelse(other.medication.specify == 'hrt',
                                      yes = 1,
                                      no  = 0),
                perc.dvt = ifelse(dvt == 'Yes',
                                  yes = 1,
                                  no  = 0),
                perc = perc.age +
                       perc.heart.rate +
                       perc.o2 +
                       perc.cough +
                       perc.haemoptysis +
                       perc.leg.swelling +
                       perc.surgery +
                       ## perc.embolism +
                       perc.hormone +
                       perc.dvt)
## Wells
dipep <- mutate(dipep,
                wells.dvt = ifelse(dvt == 'Yes',
                                   yes = 3,
                                   no  = 0),
                ## wells.alternative = ifelse(,
                ##                            yes = 3,
                ##                            no  = 0),
                wells.heart.rate = ifelse(heart.rate > 100,
                                          yes = 1.5,
                                          no  = 0),
                wells.surgery.immobil = ifelse(surgery == 'Yes' | immobil == 'Yes',
                                               yes = 1.5,
                                               no  = 0),
                wells.previous.dvt.pe = ifelse(thrombosis == 'Yes',
                                               yes = 1.5,
                                               no  = 0),
                wells.hemoptysis = ifelse(presenting.features.haemoptysis == 'Ticked',
                                          yes = 3,
                                          no  = 0),
                wells.neoplasm = ifelse(medical.specify == 'Cancer',
                                        yes = 3,
                                        no  = 0),
                wells = wells.dvt +
                        ## wells.alternative +
                        wells.heart.rate +
                        wells.surgery.immobil +
                        wells.previous.dvt.pe +
                        wells.hemoptysis +
                        wells.neoplasm)
#######################################################################
## Derive an imputed data set                                        ##
## ToDo 2016-10-14 - Obtain mean values to impute when missing so far##
##                   only works on the dichotomised values           ##
#######################################################################
missing.data <- do.call(rbind, sapply(dipep, function(i) is.na(i) %>% table())) %>%
    as.data.frame()
missing.data$variable <- rownames(missing.data)
names(missing.data) <- c('Present', 'Missing', 'variable')
missing.data <- mutate(missing.data,
                       Missing = ifelse(Missing == 482,
                                        yes = 0,
                                        no  = Missing)) %>%
                arrange(variable, Present, Missing)
write.csv(missing.data, file = '../../tmp/missing.data.csv')

dipep.imputed <- mutate(dipep,
                        surgery   = ifelse(is.na(surgery), yes = 'No', no = surgery),
                        travel    = ifelse(is.na(travel), yes = 'No', no = travel),
                        immobil   = ifelse(is.na(immobil), yes = 'No', no = travel),
                        pregnancies.over.cat = ifelse(is.na(pregnancies.over.cat),
                                                      yes = 'No previous pregnancies < 24 weeks',
                                                      no = pregnancies.over.cat),
                        pregnancies.under.cat = ifelse(is.na(pregnancies.under.cat),
                                                       yes = 'No previous pregnancies < 24 weeks',
                                                       no = pregnancies.under.cat),
                        bp.diastolic.cat = ifelse(is.na(bp.diastolic.cat), yes = 'Low', no = bp.diastolic.cat),
                        bp.systolic.cat = ifelse(is.na(bp.systolic.cat), yes = 'Low', no = bp.systolic.cat),
                        heart.rate.cat = ifelse(is.na(heart.rate.cat), yes = 'Low', no = heart.rate.cat),
                        bmi.cat = ifelse(is.na(bmi.cat), yes = 'Low', no = bmi.cat),
                        smoking.cat = ifelse(is.na(smoking.cat), yes = 'Non-smoker', no = smoking.cat),
                        age.cat = ifelse(is.na(age.cat), yes = 'Young', no = age.cat)
                        ## heart.rate.cat = ifelse(is.na(), yes = , no = ),
                        ## heart.rate.cat = ifelse(is.na(), yes = , no = )
                        )

#######################################################################
## Database Specification                                            ##
#######################################################################
## The 'Forms' Worksheet from the Data Management document           ##
##                                                                   ##
## 'DiPEP database specification'                                    ##
##                                                                   ##
## See https://goo.gl/Yw9I22                                         ##
##                                                                   ##
## Downloaded : 2016-03-01                                           ##
#######################################################################
forms <- read.csv('forms.csv')
names(forms) <- gsub("\\.$", "", names(forms)) %>%
                tolower()
forms <- dplyr::select(forms,
                       name, identifier)

#######################################################################
## The 'Fields' Worksheet from the Data Management document          ##
##                                                                   ##
## 'DiPEP database specification'                                    ##
##                                                                   ##
## See https://goo.gl/Yw9I22                                         ##
##                                                                   ##
## Downloaded : 2016-03-01                                           ##
#######################################################################
fields <- read.csv('fields.csv')
fields <- dplyr::select(fields, Identifier, Label)
names(fields) <- c('variable', 'description')
fields$variable <- gsub('_', '.', fields$variable)


#######################################################################
## Data Dictionary for R objects                                    ##
#######################################################################
dipep.README <- names(master) %>%
          data.frame()
dipep.README$description <- ''
names(dipep.README) <- c('data.frame', 'description')
dipep.README <- within(dipep.README,{
                 description[data.frame == 'data.dictionary']                <- 'Data Dictionary - Factor variables'
                 description[data.frame == 'follow.up.30.day']               <- '30-day Follow-Up'
                 description[data.frame == 'annotations']                    <- ''
                 description[data.frame == 'blood.sample']                   <- 'Details of Blood Sample'
                 description[data.frame == 'service.receipt']                <- ''
                 description[data.frame == 'service.receipt.hospital']       <- ''
                 description[data.frame == 'completion']                     <- ''
                 description[data.frame == 'contact']                        <- 'Study Completion'
                 description[data.frame == 'delivery']                       <- 'Details of Birth'
                 description[data.frame == 'discrepancies']                  <- ''
                 description[data.frame == 'eq5d']                           <- 'EQ5D Quality of Life'
                 description[data.frame == 'events']                         <- 'Date of Events (Redundant - does not contain dates)'
                 description[data.frame == 'follow.up.sent']                 <- 'Follow-Up Forms Sent'
                 description[data.frame == 'forms']                          <- 'Study Forms/Prospect Tables'
                 description[data.frame == 'individuals']                    <- 'Recruited Patients'
                 description[data.frame == 'investigations']                 <- 'Investigations (Broad)'
                 description[data.frame == 'investigations.investigation']   <- 'Investigations (Detailed)'
                 description[data.frame == 'med.hist']                       <- 'Medical History (Broad)'
                 description[data.frame == 'med.hist.problems']              <- 'Medical History (Detailed)'
                 description[data.frame == 'med.hist.thrombophilia']         <- 'Medical History (Thrombophilia)'
                 description[data.frame == 'outcome.infant']                 <- 'Outcome for Infant (Broad)'
                 description[data.frame == 'outcome.infant.infant']          <- 'Outcome for Infant (Broad)'
                 description[data.frame == 'outcome.woman']                  <- 'Outcome for Woman (Broad)'
                 description[data.frame == 'outcome.woman.morbidity']        <- 'Outcome for Woman (Detailed)'
                 description[data.frame == 'presenting.features']            <- 'Presenting Features'
                 description[data.frame == 'previous.pregnancies']           <- 'Previous Pregnancies'
                 description[data.frame == 'previous.pregnancies.problems']  <- 'Previous Pregnancies - Problems'
                 description[data.frame == 'questionnaire.contact']          <- ''
                 description[data.frame == 'screening.dvt']                  <- 'Screening - Deep Vein Thrombosis Group'
                 description[data.frame == 'screening.non.recruited']        <- 'Screening - Non Recruited Group'
                 description[data.frame == 'screening.suspected.pe']         <- 'Screening - Suspected Pulmonary Embolism Group'
                 description[data.frame == 'sign.off']                       <- ''
                 description[data.frame == 'sites']                          <- 'Deatils of Study Sites'
                 description[data.frame == 'therapy']                        <- 'Therapy Details'
                 description[data.frame == 'pregnancy']                      <- 'Pregnancy (Broad)'
                 description[data.frame == 'pregnancy.continued']            <- 'Pregnancy - Continued or Not'
                 description[data.frame == 'pregnancy.problems']             <- 'Pregnancy - Problems'
                 description[data.frame == 'pregnancy.immobility']           <- 'Pregnancy - Immobility Details'
                 description[data.frame == 'pregnancy.long.haul']            <- 'Pregnancy - Long Haul Flights'
                 description[data.frame == 'thromboprophylaxis']             <- 'Thromboprophylaxis'
                 description[data.frame == 'thrombotic.events']              <- 'Details of '
                 description[data.frame == 'unavailable.forms']              <- 'Unavailable Forms'
                 description[data.frame == 'womans.details']                 <- 'Womands Details'
                 description[data.frame == 'dipep']                          <- 'Combined data set for analysis (no imputation)'
                 description[data.frame == 'dipep.imputed']                  <- 'Combined data set for analysis (imputed missing values)'
                 description[data.frame == 'missing.data'] <- 'Summary of data completeness (FALSE == Not Missing; TRUE == Missing)'
})

#######################################################################
## Data Dictaionary for each data frame                              ##
#######################################################################
dipep.README.variables <- list()
dipep.README.variables$follow.up.30.day               <- fields_dipep(df     = master$follow.up.30.day,
                                                                fields = fields)
dipep.README.variables$annotations                    <- fields_dipep(df     = master$annotations,
                                                                fields = fields)
dipep.README.variables$blood.sample                   <- fields_dipep(df     = master$blood.sample,
                                                                fields = fields)
dipep.README.variables$service.receipt                <- fields_dipep(df     = master$service.receipt,
                                                                fields = fields)
dipep.README.variables$service.receipt.hospital       <- fields_dipep(df     = master$service.receipt.hospital,
                                                                fields = fields)
dipep.README.variables$completion                     <- fields_dipep(df     = master$completion,
                                                                fields = fields)
dipep.README.variables$contact                        <- fields_dipep(df     = master$contact,
                                                                fields = fields)
dipep.README.variables$delivery                       <- fields_dipep(df     = master$delivery,
                                                                fields = fields)
dipep.README.variables$discrepancies                  <- fields_dipep(df     = master$discrepancies,
                                                                fields = fields)
dipep.README.variables$eq5d                           <- fields_dipep(df     = master$eq5d,
                                                                fields = fields)
dipep.README.variables$events                         <- fields_dipep(df     = master$events,
                                                                fields = fields)
dipep.README.variables$follow.up.sent                 <- fields_dipep(df     = master$follow.up.sent,
                                                                fields = fields)
dipep.README.variables$forms                          <- fields_dipep(df     = master$forms,
                                                                fields = fields)
dipep.README.variables$individuals                    <- fields_dipep(df     = master$individuals,
                                                                fields = fields)
dipep.README.variables$investigations                 <- fields_dipep(df     = master$investigations,
                                                                fields = fields)
dipep.README.variables$investigations.investigation   <- fields_dipep(df     = master$investigations.investigation,
                                                                fields = fields)
dipep.README.variables$med.hist                       <- fields_dipep(df     = master$med.hist,
                                                                fields = fields)
dipep.README.variables$med.hist.problems              <- fields_dipep(df     = master$med.hist.problems,
                                                                fields = fields)
dipep.README.variables$med.hist.thrombophilia         <- fields_dipep(df     = master$med.hist.thrombophilia,
                                                                fields = fields)
dipep.README.variables$outcome.infant                 <- fields_dipep(df     = master$outcome.infant,
                                                                fields = fields)
dipep.README.variables$outcome.infant.infant          <- fields_dipep(df     = master$outcome.infant.infant,
                                                                fields = fields)
dipep.README.variables$outcome.woman                  <- fields_dipep(df     = master$outcome.woman,
                                                                fields = fields)
dipep.README.variables$outcome.woman.morbidity        <- fields_dipep(df     = master$outcome.woman.morbidity,
                                                                fields = fields)
dipep.README.variables$presenting.features            <- fields_dipep(df     = master$presenting.features,
                                                                fields = fields)
dipep.README.variables$previous.pregnancies           <- fields_dipep(df     = master$previous.pregnancies,
                                                                fields = fields)
dipep.README.variables$previous.pregnancies.problems  <- fields_dipep(df     = master$previous.pregnancies.problems,
                                                                fields = fields)
dipep.README.variables$questionnaire.contact          <- fields_dipep(df     = master$questionnaire.contact,
                                                                fields = fields)
dipep.README.variables$screening.dvt                  <- fields_dipep(df     = master$screening.dvt,
                                                                fields = fields)
dipep.README.variables$screening.non.recruited        <- fields_dipep(df     = master$screening.non.recruited,
                                                                fields = fields)
dipep.README.variables$screening.suspected.pe         <- fields_dipep(df     = master$screening.suspected.pe,
                                                                fields = fields)
dipep.README.variables$sign.off                       <- fields_dipep(df     = master$sign.off,
                                                                fields = fields)
dipep.README.variables$sites                          <- fields_dipep(df     = master$sites,
                                                                fields = fields)
dipep.README.variables$therapy                        <- fields_dipep(df     = master$therapy,
                                                                fields = fields)
dipep.README.variables$pregnancy.continued            <- fields_dipep(df     = master$pregnancy.continued,
                                                                fields = fields)
dipep.README.variables$pregnancy.problems             <- fields_dipep(df     = master$pregnancy.problems,
                                                                fields = fields)
dipep.README.variables$pregnancy                      <- fields_dipep(df     = master$pregnancy,
                                                                fields = fields)
dipep.README.variables$pregnancy.immobility           <- fields_dipep(df     = master$pregnancy.immobility,
                                                                fields = fields)
dipep.README.variables$pregnancy.long.haul            <- fields_dipep(master$pregnancy.long.haul,
                                                                fields = fields)
dipep.README.variables$thromboprophylaxis             <- fields_dipep(df     = master$thromboprophylaxis,
                                                                fields = fields)
dipep.README.variables$thrombotic.events              <- fields_dipep(df     = master$thrombotic.events,
                                                                fields = fields)
dipep.README.variables$unavailable.forms              <- fields_dipep(df     = master$unavailable.forms,
                                                                fields = fields)
dipep.README.variables$womans.details                 <- fields_dipep(df     = master$womans.details,
                                                                fields = fields)

#######################################################################
## Save all data frames                                              ##
#######################################################################
save(master,
     dipep,
     ## follow.up.30.day,
     ## blood.sample,
     ## service.receipt,
     ## service.receipt.hospital,
     ## completion,
     ## contact,
     ## delivery,
     ## discrepancies,
     ## eq5d,
     ## events,
     ## follow.up.sent,
     ## forms,
     ## individuals,
     ## investigations,
     ## investigations.investigation,
     ## med.hist,
     ## med.hist.problems,
     ## med.hist.thrombophilia,
     ## outcome.infant,
     ## outcome.infant.infant,
     ## outcome.woman,
     ## outcome.woman.morbidity,
     ## presenting.features,
     ## previous.pregnancies,
     ## previous.pregnancies.problems,
     ## questionnaire.contact,
     ## screening.dvt,
     ## screening.non.recruited,
     ## screening.suspected.pe,
     ## sign.off,
     ## sites,
     ## therapy,
     ## pregnancy.continued,
     ## pregnancy.problems,
     ## pregnancy,
     ## pregnancy.immobility,
     ## preganancy.long.haul,
     ## thromboprophylaxis,
     ## thrombotic.events,
     ## unavailable.forms,
     ## womans.details,
     file   = '../data/dipep.RData')

## Write a dataset in Stata format for Mike Bradburn to QC
## dplyr::select(dipep, -life.support.presentation, -incidental) %>%
##     write.dta(file = 'dipep.dta')
names(dipep) <- gsub("\\.", "_", names(dipep))
names(dipep) <- gsub("presenting_features", "presenting", names(dipep))
names(dipep) <- gsub("simplified_", "simp_", names(dipep))
write_dta(dipep, version = 14, path = 'dipep.dta')
names(dipep) <- gsub("_", ".", names(dipep))
names(dipep) <- gsub("presenting", "presenting.features", names(dipep))
names(dipep) <- gsub("simp_", "simplified_", names(dipep))
