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
## Derive BMI
master$womans.details$bmi <- master$womans.details$weight / (master$womans.details$height / 100)^2

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
                    site,
                    group,
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
                    pregnancies.over,
                    pregnancies.under,
                    prev.preg.problem)
## Presenting Features
t3 <- dplyr::select(master$presenting.features,
                    screening,
                    group,
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
                    d.dimer.performed,
                    d.dimer.not.recorded,
                    d.dimer.unt,
                    d.dimer.ugl,
                    d.dimer,
                    d.dimer.low,
                    d.dimer.high)
## Thrombotic Event
t4 <- dplyr::select(master$thrombotic.events,
                    screening,
                    group,
                    thromb.event)
## Thromboprophylaxis
t5 <- dplyr::select(master$thromboprophylaxis,
                    screening,
                    group,
                    thromboprophylaxis)
## EQ5D
t6 <- dplyr::select(master$eq5d,
                    screening,
                    group,
                    site,
                    event.name,
                    mobility,
                    self.care,
                    usual.activity,
                    pain.discomfort,
                    anxiety.depression,
                    health.scale.number) ## ,
                    ## eq5d)
## Merge the subsets
merge.by <- c('screening', 'group')
dipep <- merge(t1,
               t2,
               by    = merge.by,
               all   = TRUE) %>%
         merge(.,
               t3,
               by    = merge.by,
               all   = TRUE)
rm(t1, t2, t3)


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
README <- names(master) %>%
          data.frame()
README$description <- ''
names(README) <- c('data.frame', 'description')
README <- within(README,{
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
})

#######################################################################
## Data Dictaionary for each data frame                              ##
#######################################################################
README.variables <- list()
README.variables$follow.up.30.day               <- fields_dipep(df     = master$follow.up.30.day,
                                                                fields = fields)
README.variables$annotations                    <- fields_dipep(df     = master$annotations,
                                                                fields = fields)
README.variables$blood.sample                   <- fields_dipep(df     = master$blood.sample,
                                                                fields = fields)
README.variables$service.receipt                <- fields_dipep(df     = master$service.receipt,
                                                                fields = fields)
README.variables$service.receipt.hospital       <- fields_dipep(df     = master$service.receipt.hospital,
                                                                fields = fields)
README.variables$completion                     <- fields_dipep(df     = master$completion,
                                                                fields = fields)
README.variables$contact                        <- fields_dipep(df     = master$contact,
                                                                fields = fields)
README.variables$delivery                       <- fields_dipep(df     = master$delivery,
                                                                fields = fields)
README.variables$discrepancies                  <- fields_dipep(df     = master$discrepancies,
                                                                fields = fields)
README.variables$eq5d                           <- fields_dipep(df     = master$eq5d,
                                                                fields = fields)
README.variables$events                         <- fields_dipep(df     = master$events,
                                                                fields = fields)
README.variables$follow.up.sent                 <- fields_dipep(df     = master$follow.up.sent,
                                                                fields = fields)
README.variables$forms                          <- fields_dipep(df     = master$forms,
                                                                fields = fields)
README.variables$individuals                    <- fields_dipep(df     = master$individuals,
                                                                fields = fields)
README.variables$investigations                 <- fields_dipep(df     = master$investigations,
                                                                fields = fields)
README.variables$investigations.investigation   <- fields_dipep(df     = master$investigations.investigation,
                                                                fields = fields)
README.variables$med.hist                       <- fields_dipep(df     = master$med.hist,
                                                                fields = fields)
README.variables$med.hist.problems              <- fields_dipep(df     = master$med.hist.problems,
                                                                fields = fields)
README.variables$med.hist.thrombophilia         <- fields_dipep(df     = master$med.hist.thrombophilia,
                                                                fields = fields)
README.variables$outcome.infant                 <- fields_dipep(df     = master$outcome.infant,
                                                                fields = fields)
README.variables$outcome.infant.infant          <- fields_dipep(df     = master$outcome.infant.infant,
                                                                fields = fields)
README.variables$outcome.woman                  <- fields_dipep(df     = master$outcome.woman,
                                                                fields = fields)
README.variables$outcome.woman.morbidity        <- fields_dipep(df     = master$outcome.woman.morbidity,
                                                                fields = fields)
README.variables$presenting.features            <- fields_dipep(df     = master$presenting.features,
                                                                fields = fields)
README.variables$previous.pregnancies           <- fields_dipep(df     = master$previous.pregnancies,
                                                                fields = fields)
README.variables$previous.pregnancies.problems  <- fields_dipep(df     = master$previous.pregnancies.problems,
                                                                fields = fields)
README.variables$questionnaire.contact          <- fields_dipep(df     = master$questionnaire.contact,
                                                                fields = fields)
README.variables$screening.dvt                  <- fields_dipep(df     = master$screening.dvt,
                                                                fields = fields)
README.variables$screening.non.recruited        <- fields_dipep(df     = master$screening.non.recruited,
                                                                fields = fields)
README.variables$screening.suspected.pe         <- fields_dipep(df     = master$screening.suspected.pe,
                                                                fields = fields)
README.variables$sign.off                       <- fields_dipep(df     = master$sign.off,
                                                                fields = fields)
README.variables$sites                          <- fields_dipep(df     = master$sites,
                                                                fields = fields)
README.variables$therapy                        <- fields_dipep(df     = master$therapy,
                                                                fields = fields)
README.variables$pregnancy.continued            <- fields_dipep(df     = master$pregnancy.continued,
                                                                fields = fields)
README.variables$pregnancy.problems             <- fields_dipep(df     = master$pregnancy.problems,
                                                                fields = fields)
README.variables$pregnancy                      <- fields_dipep(df     = master$pregnancy,
                                                                fields = fields)
README.variables$pregnancy.immobility           <- fields_dipep(df     = master$pregnancy.immobility,
                                                                fields = fields)
README.variables$pregnancy.long.haul            <- fields_dipep(df     = master$preganancy.long.haul,
                                                                fields = fields)
README.variables$thromboprophylaxis             <- fields_dipep(df     = master$thromboprophylaxis,
                                                                fields = fields)
README.variables$thrombotic.events              <- fields_dipep(df     = master$thrombotic.events,
                                                                fields = fields)
README.variables$unavailable.forms              <- fields_dipep(df     = master$unavailable.forms,
                                                                fields = fields)
README.variables$womans.details                 <- fields_dipep(df     = master$womans.details,
                                                                fields = fields)

#######################################################################
## Save all data frames                                              ##
#######################################################################
save(master,
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
