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
## ToDo - Convert to case_when() when time permits
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
master$med.hist.problems.scoring <- dplyr::filter(master$med.hist.problems,
                                                  grepl('thrombosis', medical.other, ignore.case = TRUE) |
                                                  grepl('embolism', medical.other, ignore.case = TRUE) |
                                                  grepl('pulmonary', medical.other, ignore.case = FALSE)) %>%
                                    dplyr::select(screening, group, site, event.name) %>%
                                    unique() %>%
                                    mutate(medical.other.dvt.pe = 'Yes')
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
## 2017-03-03 - The most likely diagnosis, required for one of the existing
##              scores has been reviewed outside of Prospect and provided as
##              an Excel (!?!?!?) spreadsheet 'lib/data-raw/xls/PE\ likely\ diagnosis.xlsx
##              by Steve Goodacre (2017-03-03 @ 14:00; Subject Re: Review of PE).  This has
##              been saved to text (semi-colon delimited because it includes a free text
##              field that has commas in it) and is imported here and combined
##              with master$presenting.features because the classification is to be used
##              instead of the 'diagnosis.post' variable from presenting features
master$likely.diagnosis <- read.table("likely_diagnosis.csv",
                                      header = TRUE,
                                      sep    = '\t')
master$likely.diagnosis <- mutate(master$likely.diagnosis,
                                  likely.diagnosis = factor(likely.diagnosis,
                                                            levels = c(0, 1, 2),
                                                            labels = c('Other', 'Possible PE', 'PE')))
master$presenting.features <- left_join(master$presenting.features,
                                        master$likely.diagnosis)
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
## 2017-02-28 - Screening ID 'N02/02' is missing from this file (and many others,
##              see master$unavailable.forms), as a consequence they MUST be
##              added, as it was assumed when writing this code that womans.details
##              is a master list, and if NOT added then this person is NOT included
##              in any derived dataset (i.e. dipep dataframe which is used in all
##              analyses and summaries)
system('cat Womans*.csv > Womans_details_all.csv')
master$womans.details <- read_dipep("Womans_details_all.csv",
                                    header           = TRUE,
                                    sep              = ',',
                                    convert.dates    = TRUE,
                                    dictionary       = master$data.dictionary)
system('rm Womans_details_all.csv')
#######################################################################
## Case Review 1.csv                                                 ##
#######################################################################
master$case.review1 <- read_dipep("Case Review 1.csv",
                                  header           = TRUE,
                                  sep              = ',',
                                  convert.dates    = TRUE,
                                  dictionary       = master$data.dictionary)
master$case.review1$case.review <- 1
master$case.review1.investigation <- read_dipep("Case Review 1 - Investigation.csv",
                                                header           = TRUE,
                                                sep              = ',',
                                                convert.dates    = TRUE,
                                                dictionary       = master$data.dictionary)
#######################################################################
## Case Review 2.csv                                                 ##
#######################################################################
master$case.review2 <- read_dipep("Case Review 2.csv",
                                  header           = TRUE,
                                  sep              = ',',
                                  convert.dates    = TRUE,
                                  dictionary       = master$data.dictionary)
master$case.review2$case.review <- 2
master$case.review2.investigation <- read_dipep("Case Review 2 - Investigation.csv",
                                                header           = TRUE,
                                                sep              = ',',
                                                convert.dates    = TRUE,
                                                dictionary       = master$data.dictionary)
#######################################################################
## Case Review 3.csv                                                 ##
#######################################################################
master$case.review3 <- read_dipep("Case Review 3.csv",
                                  header           = TRUE,
                                  sep              = ',',
                                  convert.dates    = TRUE,
                                  dictionary       = master$data.dictionary)
master$case.review3$case.review <- 3
master$case.review3.investigation <- read_dipep("Case Review 3 - Investigation.csv",
                                                header           = TRUE,
                                                sep              = ',',
                                                convert.dates    = TRUE,
                                                dictionary       = master$data.dictionary)
#######################################################################
## Combine Case Reviews into one                                     ##
#######################################################################
## Short function to subset and clean (convert to string) the case review
## variables that are important.
case.review1 <- dipep_case_review(df       = master$case.review1,
                                  reviewer = 1)
case.review2 <- dipep_case_review(df       = master$case.review2,
                                  reviewer = 2)
case.review3 <- dipep_case_review(df       = master$case.review3,
                                  reviewer = 3)
master$case.review <- left_join(case.review1,
                                case.review2) %>%
                      left_join(.,
                                case.review3) %>%
    ## TODO - This needs careful revision when classification queries have been resolved
    mutate(first.st     = ifelse(first.st1 == first.st2,
                                 yes = first.st1,
                                 no  = first.st3),
           first.st     = ifelse(first.st == 'Exclude',
                                 yes = NA,
                                 no  = first.st),
           second.st    = ifelse(second.st1 == second.st2,
                                 yes = second.st1,
                                 no  = second.st3),
           second.st    = ifelse(second.st == 'Exclude',
                                 yes = NA,
                                 no  = second.st),
           third.st     = ifelse(third.st1 == third.st2,
                                 yes = third.st1,
                                 no  = third.st3),
           third.st     = ifelse(third.st == 'Exclude',
                                 yes = NA,
                                 no  = third.st),
           fourth.st    = ifelse(fourth.st1 == fourth.st2,
                                 yes = fourth.st1,
                                 no  = fourth.st3),
           fourth.st    = ifelse(fourth.st == 'Exclude',
                                 yes = NA,
                                 no  = fourth.st),
           primary.dm   = ifelse(primary.class1 == primary.class2,
                                 yes = primary.class1,
                                 no  = primary.class3),
           primary.dm   = ifelse(primary.dm == 'Exclude',
                                 yes = NA,
                                 no  = primary.dm),
           secondary.dm = ifelse(secondary.class1 == secondary.class2,
                                 yes = secondary.class1,
                                 no  = secondary.class3),
           secondary.dm = ifelse(secondary.dm == 'Exclude',
                                 yes = NA,
                                 no  = secondary.dm),
           ## There are three individuals who have only been reviewed by one person
           ## use that classification here.
           first.st     = ifelse(screening %in% c('PE_268', 'PE_269', 'PE_271'),
                                 yes = first.st1,
                                 no  = first.st),
           second.st    = ifelse(screening %in% c('PE_268', 'PE_269', 'PE_271'),
                                 yes = second.st1,
                                 no  = second.st),
           third.st     = ifelse(screening %in% c('PE_268', 'PE_269', 'PE_271'),
                                 yes = third.st1,
                                 no  = third.st),
           fourth.st    = ifelse(screening %in% c('PE_268', 'PE_269', 'PE_271'),
                                 yes = fourth.st1,
                                 no  = fourth.st),
           ## Ensure all are factors
           first.st     = factor(first.st,     levels = c('No PE', 'PE')),
           second.st    = factor(second.st,    levels = c('No PE', 'PE')),
           third.st     = factor(third.st,     levels = c('No PE', 'PE')),
           fourth.st    = factor(fourth.st,    levels = c('No PE', 'PE')),
           primary.dm   = factor(primary.dm,   levels = c('No PE', 'PE')),
           secondary.dm = factor(secondary.dm, levels = c('No PE', 'PE')))
## Add in the group so that desired table structure can be created
master$case.review <- left_join(dplyr::select(master$womans.details,
                                              screening,
                                              group),
                                master$case.review)
## TODO 20170213 - How to define other classifications, and whether to actually do so or not
##                 Personally I see no utility as the subsequent classifications are using
##                 individuals for whom there is uncertainty about their classification.
##                 This will only serve to reduce the predictive value of any model (or change
##                 the variables that are selected as being useful for prediction).
##
##                 Regardless its still unclear how to do this, the document by Steve Goodacre
##                 attached to email from Kim Horspool (2017-02-10 @ 10:13 Re: DiPEP case
##                 review update) hhas classifications of Imaging, Treatment and Follow-up,
##                 and indicates how Primary and Secondary classifications (made by reviewers)
##                 are made.  It does not however indicate how to reconcile when reviewers 1
##                 and 2 do not concur on these three categories (which are the underlying
##                 reasons why reviewer 3 has been used) because it is the image classification
##                 that may be discordant, and it then becomes a question of which one to
##                 use for that, same rules, as the overall classification?
##                              tertiary.class    = ifelse(),
##                              quaternary.class  = ifelse(),
##                              quinternary.class = ifelse())
## Remove helper funciton and temporary dataframes
rm(clean_case_review, case.review1, case.review2, case.review3)
#######################################################################
## biomarker_clean.csv                                               ##
#######################################################################
## First tidy the CSV file exported from Excel to remove the redundant
## and uninformative header rows.  Done with a short Bash script that
## uses grep -v to print only lines that don't contain the headers, and
## a few calls to sed to remove entries such as 'no plasma', 'ce' and 'nc' and
## inequality symbols (primarily '<') which for some inexplicable reason are
## recorded in fields which are meant to be numerical.
##
## If you don't understand this search StackOverflow for things like...
##
## bash
## grep
## regular expressions
## sed
##
## Note also that this code will run fine on a GNU/Linux system but you
## will have to install a UNIX-like shell (e.g. Cygwin) and tweak the
## system() call to ensure that it is invoked should you wish to run
## this code in M$-Win.
system('./clean_biomarker.sh')
master$biomarker_raw <- read.table(file   = 'biomarker_clean.csv',
                                   header = TRUE,
                                   sep    = ';')
## 2017-03-01 - Sample 'D09/01' should be 'S09/02' see email from k.horspool@sheffield.ac.uk
##              2017-03-01 @ 16:09 subject : Re: [DIPEP] Figures
master$biomarker_raw$Sample.name <- gsub('D09/01', 'S09/02', master$biomarker_raw$Sample.name)
master$biomarker_tidy <- master$biomarker_raw
## Tidy up the names
names(master$biomarker_tidy) <- names(master$biomarker_tidy) %>%
                                tolower()
names(master$biomarker_tidy) <- gsub('sample.number', 'sample', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('sample.name', 'screening', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('pt', 'prothombin.time', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('aprothombin.timet.sp.liquid', 'aptt', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('clauss.fibrinogen', 'clauss.fibrinogen', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('d.dimer.innovan.latex.test', 'ddimer.innovance', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('lagtime', 'thrombin.generation.lag.time', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('etp', 'thrombin.generation.endogenous.potential', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('ttpeak', 'thrombin.generation.time.to.peak', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('^peak', 'thrombin.generation.peak', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('error.message..comment', 'error.message', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('d.dimer.eliza.zymutest..hyphen.', 'ddimer.elisa', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('pap', 'plasmin.antiplasmin', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('pf.1.2', 'prothrombin.fragments', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('tf', 'tissue.factor', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('troponin.1', 'troponin', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('nppb', 'nppb', names(master$biomarker_tidy))
names(master$biomarker_tidy) <- gsub('mproanp', 'mrproanp', names(master$biomarker_tidy))
## Remove extrenuous columns
master$biomarker_tidy <- dplyr::select(master$biomarker_tidy, -c(sample, error.message, comments, key, x))
## UNCONFIRMED - But it looks like a value of '-1' has been used to indicate missing data
##               Convert those to true missing now.
## NB - Requested confirmation from Kiran via email (2017-02-02 @ 13:26; Subject :
##      Fwd: DiPep - expert elicitation exercise) and despite a response from Kiran
##      (2017-02-02 @ 18:14) this aspect was not addressed.
master$biomarker_tidy[master$biomarker_tidy == -1] <- NA
#######################################################################
## biomarker_anticoag_exclusions_clean.csv                           ##
#######################################################################
## Now read in the exclusions which are those who have received anti-coagulents
## prior to blood sample being taken for assay.  This list is based on an XLS
## file which lists those identified based on the assays and has then been augmented
## by review of case notes to confirm whether anti-coagulants were received.
## The file was sent by email 2017-02-07 by Ellen Bradley (e.bradley@sheffield.ac.uk) and
## forwarded by Kim Horspool (k.horspool@sheffield.ac.uk) the same day (subject Fwd : Data Cleansing)
## and has been saved to ../lib/data-raw/xls/biomarker_anticoag_exclusions_20170207.xls
## The worksheet has been saved to CSV ../lib/data-raw/biomarker_anticoag_exclusions_20170207.csv
## however because of the use of commas in free text fields the delimiter has been
## set to a semi-colon so that importing does not split fields (albeit that most of
## those text fields are redundant)
##
## First file from 20170207 (NOW COMPLETELY REDUNDANT SO COMMENTED OUT)
## system('./clean_biomarker_exclusions.sh')
## master$biomarker_exclusions_20170207_raw <- read.table(file   = 'biomarker_anticoag_exclusions_20170207_clean.csv',
##                                               header = TRUE,
##                                               sep    = ';')
## ## Only really need two colums, ID and indicator of anti-coagulant
## master$biomarker_exclusions_20170207_clean <- dplyr::select(master$biomarker_exclusions_20170207_raw,
##                                                    Sample.name,
##                                                    Received.anti.coag.before.blood.sample.)
## names(master$biomarker_exclusions_20170207_clean) <- c('screening', 'exclude.anti.coag')
## ## BUT...there are four individuals who _didn't_ receive anti-coagulants so
## ## now filter those out to a list for use in cleaning the actual data
## master$biomarker_exclusions_20170207_clean <- dplyr::filter(master$biomarker_exclusions_20170207_clean,
##                                                             exclude.anti.coag == 'Yes')
## Then there is a revised data set from 20170223
master$biomarker_exclusions_20170223_raw <- read.table(file   = 'biomarker_anticoag_exclusions_20170223.csv',
                                                       header = TRUE,
                                                       sep    = ';')
master$biomarker_exclusions_20170223_clean <- dplyr::select(master$biomarker_exclusions_20170223_raw, -X)
names(master$biomarker_exclusions_20170223_clean) <- c('screening', 'exclude.anti.coag', 'exclude.thrombin')
master$biomarker_exclusions_clean <- dplyr::select(master$biomarker_exclusions_20170223_clean,
                                                   screening, exclude.anti.coag) %>%
                                     mutate(exclude.anti.coag = case_when(.$exclude.anti.coag == 'N' ~ 'No',
                                                                          .$exclude.anti.coag == 'Y' ~ 'Yes'))
## Now scrub the data out of the tidied master$biomarker_tidy
master$biomarker_tidy <- left_join(master$biomarker_tidy,
                                   master$biomarker_exclusions_clean) %>%
                         mutate(exclude.anti.coag = ifelse(!is.na(exclude.anti.coag),
                                                           yes = exclude.anti.coag,
                                                           no  = 'No'))
## master$biomarker_tidy <- mutate(master$biomarker_tidy,
##                                 aprothombin = ifelse(exclude.anti.coag == 'Yes',
##                                                      yes = NA,
##                                                      no  = aprothombin),
##                                 thrombin.generation.lag.time = ifelse(exclude.anti.coag == 'Yes',
##                                                                       yes = NA,
##                                                                       no  = thrombin.generation.lag.time),
##                                 thrombin.generation.endogenous.potential = ifelse(exclude.anti.coag == 'Yes',
##                                                                       yes = NA,
##                                                                       no  = thrombin.generation.endogenous.potential),
##                                 thrombin.generation.time.to.peak = ifelse(exclude.anti.coag == 'Yes',
##                                                                           yes = NA,
##                                                                           no  = thrombin.generation.time.to.peak),
##                                 thrombin.generation.peak = ifelse(exclude.anti.coag == 'Yes',
##                                                                   yes = NA,
##                                                                   no  = thrombin.generation.peak))
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
                    likely.diagnosis,
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
                    surgery.other)
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
## This data frame is in long format and needs converting to wide
t9 <- dplyr::select(master$med.hist.problems,
                    screening,
                    group,
                    site,
                    event.name,
                    medical.specify,
                    medical.other) %>%
      melt(id.vars = c('screening', 'group', 'site', 'event.name')) %>%
      group_by(screening, variable) %>%
      mutate(n = row_number()) %>%
      dcast(screening + group + site + event.name ~ variable + n) %>%
## There are multiple records for historical medical problems and problems with
## this pregnancy to which end these need resolving into a single variable...
      mutate(existing.medical.autoimmune = ifelse(medical.specify_1 == 'Autoimmune diseases' |
                                                  medical.specify_2 == 'Autoimmune diseases' |
                                                  medical.specify_3 == 'Autoimmune diseases' |
                                                  medical.specify_4 == 'Autoimmune diseases' |
                                                  medical.specify_5 == 'Autoimmune diseases' |
                                                  medical.specify_6 == 'Autoimmune diseases' |
                                                  medical.specify_7 == 'Autoimmune diseases' |
                                                  medical.specify_8 == 'Autoimmune diseases' |
                                                  medical.specify_9 == 'Autoimmune diseases',
                                                  yes = 1,
                                                  no  = 0),
             existing.medical.autoimmune = ifelse(is.na(existing.medical.autoimmune),
                                                  yes = 0,
                                                  no  = existing.medical.autoimmune),
             existing.medical.cancer = ifelse(medical.specify_1 == 'Cancer' |
                                              medical.specify_2 == 'Cancer' |
                                              medical.specify_3 == 'Cancer' |
                                              medical.specify_4 == 'Cancer' |
                                              medical.specify_5 == 'Cancer' |
                                              medical.specify_6 == 'Cancer' |
                                              medical.specify_7 == 'Cancer' |
                                              medical.specify_8 == 'Cancer' |
                                              medical.specify_9 == 'Cancer',
                                              yes = 1,
                                              no  = 0),
             existing.medical.cancer = ifelse(is.na(existing.medical.cancer),
                                              yes = 0,
                                              no  = existing.medical.cancer),
             existing.medical.cardiac = ifelse(medical.specify_1 == 'Cardiac disease (congenital or acquired)' |
                                               medical.specify_2 == 'Cardiac disease (congenital or acquired)' |
                                               medical.specify_3 == 'Cardiac disease (congenital or acquired)' |
                                               medical.specify_4 == 'Cardiac disease (congenital or acquired)' |
                                               medical.specify_5 == 'Cardiac disease (congenital or acquired)' |
                                               medical.specify_6 == 'Cardiac disease (congenital or acquired)' |
                                               medical.specify_7 == 'Cardiac disease (congenital or acquired)' |
                                               medical.specify_8 == 'Cardiac disease (congenital or acquired)' |
                                               medical.specify_9 == 'Cardiac disease (congenital or acquired)',
                                               yes = 1,
                                               no  = 0),
             existing.medical.cardiac = ifelse(is.na(existing.medical.cardiac),
                                               yes = 0,
                                               no  = existing.medical.cardiac),
             existing.medical.diabetes = ifelse(medical.specify_1 == 'Diabetes' |
                                                medical.specify_2 == 'Diabetes' |
                                                medical.specify_3 == 'Diabetes' |
                                                medical.specify_4 == 'Diabetes' |
                                                medical.specify_5 == 'Diabetes' |
                                                medical.specify_6 == 'Diabetes' |
                                                medical.specify_7 == 'Diabetes' |
                                                medical.specify_8 == 'Diabetes' |
                                                medical.specify_9 == 'Diabetes',
                                                yes = 1,
                                                no  = 0),
             existing.medical.diabetes = ifelse(is.na(existing.medical.diabetes),
                                                yes = 0,
                                                no  = existing.medical.diabetes),
             existing.medical.varicose = ifelse(medical.specify_1 == 'Gross varicose veins' |
                                                medical.specify_2 == 'Gross varicose veins' |
                                                medical.specify_3 == 'Gross varicose veins' |
                                                medical.specify_4 == 'Gross varicose veins' |
                                                medical.specify_5 == 'Gross varicose veins' |
                                                medical.specify_6 == 'Gross varicose veins' |
                                                medical.specify_7 == 'Gross varicose veins' |
                                                medical.specify_8 == 'Gross varicose veins' |
                                                medical.specify_9 == 'Gross varicose veins',
                                                yes = 1,
                                                no  = 0),
             existing.medical.varicose = ifelse(is.na(existing.medical.varicose),
                                                yes = 0,
                                                no  = existing.medical.varicose),
             existing.medical.haematological = ifelse(medical.specify_1 == 'Haematological disorders e.g. sickle cell disease' |
                                                      medical.specify_2 == 'Haematological disorders e.g. sickle cell disease' |
                                                      medical.specify_3 == 'Haematological disorders e.g. sickle cell disease' |
                                                      medical.specify_4 == 'Haematological disorders e.g. sickle cell disease' |
                                                      medical.specify_5 == 'Haematological disorders e.g. sickle cell disease' |
                                                      medical.specify_6 == 'Haematological disorders e.g. sickle cell disease' |
                                                      medical.specify_7 == 'Haematological disorders e.g. sickle cell disease' |
                                                      medical.specify_8 == 'Haematological disorders e.g. sickle cell disease' |
                                                      medical.specify_9 == 'Haematological disorders e.g. sickle cell disease',
                                                      yes = 1,
                                                      no  = 0),
             existing.medical.haematological = ifelse(is.na(existing.medical.haematological),
                                                      yes = 0,
                                                      no  = existing.medical.haematological),
             existing.medical.inflammatory = ifelse(medical.specify_1 == 'Inflammatory disorders e.g. inflammatory bowel disease' |
                                                    medical.specify_2 == 'Inflammatory disorders e.g. inflammatory bowel disease' |
                                                    medical.specify_3 == 'Inflammatory disorders e.g. inflammatory bowel disease' |
                                                    medical.specify_4 == 'Inflammatory disorders e.g. inflammatory bowel disease' |
                                                    medical.specify_5 == 'Inflammatory disorders e.g. inflammatory bowel disease' |
                                                    medical.specify_6 == 'Inflammatory disorders e.g. inflammatory bowel disease' |
                                                    medical.specify_7 == 'Inflammatory disorders e.g. inflammatory bowel disease' |
                                                    medical.specify_8 == 'Inflammatory disorders e.g. inflammatory bowel disease' |
                                                    medical.specify_9 == 'Inflammatory disorders e.g. inflammatory bowel disease',
                                                    yes = 1,
                                                    no  = 0),
             existing.medical.inflammatory = ifelse(is.na(existing.medical.inflammatory),
                                                    yes = 0,
                                                    no  = existing.medical.inflammatory),
             existing.medical.malignancy.6.month = ifelse(medical.specify_1 == 'Malignancy within 6 months' |
                                                          medical.specify_2 == 'Malignancy within 6 months' |
                                                          medical.specify_3 == 'Malignancy within 6 months' |
                                                          medical.specify_4 == 'Malignancy within 6 months' |
                                                          medical.specify_5 == 'Malignancy within 6 months' |
                                                          medical.specify_6 == 'Malignancy within 6 months' |
                                                          medical.specify_7 == 'Malignancy within 6 months' |
                                                          medical.specify_8 == 'Malignancy within 6 months' |
                                                          medical.specify_9 == 'Malignancy within 6 months',
                                                          yes = 1,
                                                          no  = 0),
             existing.medical.malignancy.6.month = ifelse(is.na(existing.medical.malignancy.6.month),
                                                          yes = 0,
                                                          no  = existing.medical.malignancy.6.month),
             existing.medical.myeloproliferative = ifelse(medical.specify_1 == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera' |
                                                          medical.specify_2 == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera' |
                                                          medical.specify_3 == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera' |
                                                          medical.specify_4 == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera' |
                                                          medical.specify_5 == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera' |
                                                          medical.specify_6 == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera' |
                                                          medical.specify_7 == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera' |
                                                          medical.specify_8 == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera' |
                                                          medical.specify_9 == 'Myeloproliferative disorders e.g. essential thrombocythaemia, polycythaemia vera',
                                                          yes = 1,
                                                          no  = 0),
             existing.medical.myeloproliferative = ifelse(is.na(existing.medical.myeloproliferative),
                                                          yes = 0,
                                                          no  = existing.medical.myeloproliferative),
             existing.medical.other = ifelse(medical.specify_1 == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease' |
                                             medical.specify_2 == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease' |
                                             medical.specify_3 == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease' |
                                             medical.specify_4 == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease' |
                                             medical.specify_5 == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease' |
                                             medical.specify_6 == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease' |
                                             medical.specify_7 == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease' |
                                             medical.specify_8 == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease' |
                                             medical.specify_9 == 'Other medical disorders e.g. nephrotic syndrome, cardiac disease',
                                             yes = 1,
                                             no  = 0),
             existing.medical.other = ifelse(is.na(existing.medical.other),
                                             yes = 0,
                                             no  = existing.medical.other),
             existing.medical = ifelse(existing.medical.autoimmune == 1 |
                                       existing.medical.cancer == 1 |
                                       existing.medical.cardiac == 1 |
                                       existing.medical.diabetes == 1 |
                                       existing.medical.autoimmune == 1 |
                                       existing.medical.varicose == 1 |
                                       existing.medical.haematological == 1 |
                                       existing.medical.inflammatory == 1 |
                                       existing.medical.malignancy.6.month == 1 |
                                       existing.medical.myeloproliferative == 1 |
                                       existing.medical.other == 1,
                                       yes = 1,
                                       no  = 0),
             existing.medical = ifelse(is.na(existing.medical),
                                       yes = 0,
                                       no  = existing.medical)) %>%
             ## existing.medical = factor(existing.medical,
             ##                           levels = c(0, 1),
             ##                           labels = c('No', 'Yes'))) %>%
    dplyr::select(-existing.medical.autoimmune,
                  ## LEAVE CANCER INDICATOR IN, REQUIRED FOR GENEVA SCORE
                  ## -existing.medical.cancer,
                  -existing.medical.cardiac,
                  -existing.medical.diabetes,
                  -existing.medical.autoimmune,
                  -existing.medical.varicose,
                  -existing.medical.haematological,
                  -existing.medical.inflammatory,
                  -existing.medical.malignancy.6.month,
                  -existing.medical.myeloproliferative,
                  -existing.medical.other,
                  -medical.specify_1, -medical.other_1,
                  -medical.specify_2, -medical.other_2,
                  -medical.specify_3, -medical.other_3,
                  -medical.specify_4, -medical.other_4,
                  -medical.specify_5, -medical.other_5,
                  -medical.specify_6, -medical.other_6,
                  -medical.specify_7, -medical.other_7,
                  -medical.specify_8, -medical.other_8,
                  -medical.specify_9, -medical.other_9)
## Merge the indicators of previous PE or DVT (derived above 2017-03-02)
t9 <- merge(t9,
            master$med.hist.problems.scoring,
            by = c('screening', 'group', 'site', 'event.name'),
            all = TRUE) %>%
    mutate(existing.medical.cancer = ifelse(!is.na(existing.medical.cancer),
                                            yes = existing.medical.cancer,
                                            no  = 0),
           existing.medical = ifelse(!is.na(existing.medical),
                                            yes = existing.medical,
                                            no  = 0))
## Details of medical history problems
t10 <- dplyr::select(master$pregnancy.problems,
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
    dplyr::select(-this.pregnancy.problems.dehydration,
                  -this.pregnancy.problems.eclampsia,
                  -this.pregnancy.problems.gestational.diabetes,
                  -this.pregnancy.problems.haemorrhage,
                  -this.pregnancy.problems.hyperemesis,
                  -this.pregnancy.problems.ovarian.hyperstimulation,
                  -this.pregnancy.problems.postpartum.haemorrhage,
                  -this.pregnancy.problems.preeclampsia,
                  -this.pregnancy.problems.preterm,
                  -this.pregnancy.problems.severe.infection,
                  -this.pregnancy.problems.stillbirth,
                  -this.preg.problem.specify_1, -this.preg.problem.other_1,
                  -this.preg.problem.specify_2, -this.preg.problem.other_2,
                  -this.preg.problem.specify_3, -this.preg.problem.other_3,
                  -this.preg.problem.specify_4, -this.preg.problem.other_4,
                  -this.preg.problem.specify_5, -this.preg.problem.other_5)
## Details of medication (required for derivation of PERC score)
t11 <- dplyr::select(master$therapy,
                     screening,
                     group,
                     site,
                     event.name,
                     other.medication,
                     other.medication.specify)
## Delivery date
t12 <- dplyr::select(master$outcome.infant,
                     screening,
                     group,
                     site,
                     delivery.date) %>%
        unique()
## Yet MORE data from outside the relational database, this time
## definitions of whether individuals have 'active clinical comorbidities'
## and/or 'obstetric complications' which are two of the criteria for
## the Delphi Consensus Clinical Decision Rule.
##
## These were NOT part of Prospect and have been derived seperately and
## attachd to an email from s.goodacre@sheffield.ac.uk 2017-03-06 @ 09:38
## Subject : CDR criteria (filenames have been changed for brevity)
master$cdr.active.medical.comorbidities.raw <- read.csv(file = 'cdr_active_medical_comorbidities.csv')
master$cdr.active.medical.comorbidities.clean <- dplyr::select(master$cdr.active.medical.comorbidities.raw,
                                                               Participant.No., group, Medical.co.morbidity.0.no.1.yes)
names(master$cdr.active.medical.comorbidities.clean) <- c('screening', 'group', 'medical.comorbidity')
master$cdr.obstetric.complications.raw <- read.csv(file = 'cdr_obstetric_complications.csv')
master$cdr.obstetric.complications.clean <- dplyr::select(master$cdr.obstetric.complications.raw,
                                                          Participant.No., group, Obstetric.complications.0.no.1.yes)
names(master$cdr.obstetric.complications.clean) <- c('screening', 'group', 'obstetric.complications')
## Combine, retaining all from both
master$cdr.supplementary <- merge(master$cdr.active.medical.comorbidities.clean,
                                  master$cdr.obstetric.complications.clean,
                                  by    = c('screening', 'group'),
                                  all   = TRUE)
## Ah, but there are DUPLICATES in these files, likely because some
## people have multiple medical co-morbidities, how inconvenient, we now
## de-duplicate the data
master$cdr.supplementary[is.na(master$cdr.supplementary)] <- 0
master$cdr.supplementary <- group_by(master$cdr.supplementary, screening) %>%
                            mutate(any.medical = sum(medical.comorbidity),
                                   any.obstetric = sum(obstetric.complications)) %>%
                            dplyr::select(-medical.comorbidity, -obstetric.complications) %>%
                            unique() %>%
                            mutate(medical.comorbidity = ifelse(any.medical > 0,
                                                                yes = 1,
                                                                no  = 0),
                                   obstetric.complications = ifelse(any.obstetric > 0,
                                                                    yes = 1,
                                                                    no  = 0)) %>%
                            dplyr::select(-any.medical, -any.obstetric) %>%
                            unique()
## Extract the event date from the screening froms as
## for some reason the event.date is not recorded in any
## form and instead the consent.date is to be used as a
## proxy for baseline
event.date.dvt <- dplyr::select(master$screening.dvt,
                                screening,
                                group,
                                site,
                                completing.date)
names(event.date.dvt) <- gsub('completing', 'consent', names(event.date.dvt))
event.date.suspected.pe <- dplyr::select(master$screening.suspected.pe,
                                         screening,
                                         group,
                                         site,
                                         consent.date)
event.date.non.recruited <- dplyr::select(master$screening.non.recruited,
                                          screening,
                                          group,
                                          site,
                                          completing.date)
names(event.date.non.recruited) <- gsub('completing', 'consent', names(event.date.non.recruited))
event.date.diagnosed.pe <- dplyr::filter(master$investigations, group == 'Diagnosed PE') %>%
                           dplyr::select(screening,
                                         group,
                                         site,
                                         pe.date)
names(event.date.diagnosed.pe) <- gsub('pe', 'consent', names(event.date.diagnosed.pe))
event.date <- rbind(event.date.dvt,
                    event.date.suspected.pe,
                    event.date.non.recruited,
                    event.date.diagnosed.pe)
rm(event.date.dvt, event.date.suspected.pe, event.date.non.recruited)
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
          all   = TRUE) %>%
    merge(.,
          t12,
          by    = c('screening', 'group', 'site'),
          all   = TRUE) %>%
    merge(.,
          master$biomarker_tidy,
          by    = 'screening',
          all.x = TRUE) %>%
    merge(.,
          dplyr::select(master$case.review, screening, group, primary.dm, secondary.dm, first.st, second.st, third.st, fourth.st),
          by    = c('screening', 'group'),
          all.x = TRUE) %>%
    merge(.,
          dplyr::select(master$service.receipt, screening, group, admitted.hospital),
          by    = c('screening', 'group'),
          all.x = TRUE) %>%
    merge(.,
          master$cdr.supplementary,
          by    = c('screening', 'group'),
          all.x = TRUE)
## Replace the cdr.supplementary from missing to 0 for medical.comorbidity and obstetric.complications
t <- mutate(t,
            medical.comorbidity = ifelse(is.na(medical.comorbidity),
                                         yes = 0,
                                         no  = medical.comorbidity),
            obstetric.complications = ifelse(is.na(obstetric.complications),
                                             yes = 0,
                                             no  = obstetric.complications))
## Now do three merges with the event.date, one to get a master dataset (excluding those who were Non recruited)...
dipep <- merge(t,
               event.date,
               by    = c('screening', 'group', 'site'),
               all.x = TRUE)
## Make a copy of this for assessing missing data
dipep.raw <- dipep
## ...and one to get the IDs of those who have an event.date but nothing else...
master$missing <- merge(t,
                        event.date,
                        by  = c('screening', 'group', 'site'),
                        all.y = TRUE) %>%
                  filter(is.na(year.of.birth)) %>%
                  dplyr::select(screening, event.date, group, site, year.of.birth)
## rm(t, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, event.date)
#######################################################################
## Derive variables (something it would be nice if Data Management   ##
## could do in Prospect as then BMI, age, etc. would all be          ##
## standardised and reusable and whoever does the QA will not have to##
## duplicate the work of deriving variables).                        ##
##                                                                   ##
## IMPORTANT : Where responses are missing the CI (Steve Goodacre)   ##
##             wishes to have 'normal' used instead.  This is        ##
##             straight forward for categorical variables, or those  ##
##             that are dichotomised, less so for continuous         ##
##             variables, where 'normal' values are meant to be      ##
##             provided by Steve Goodacre and Gordon Fuller (but are ##
##             not yet available).                                   ##
#######################################################################
dipep.raw <- mutate(dipep.raw,
                    bmi = weight / (height / 100)^2,
                    age = year(event.date) - year.of.birth)
## 2017-03-02 - Some preg.post are missing which messes up derivation of Trimester.
##              For some reason doesn't work if this is in the same mutate(), hence
##              its extraction.
dipep <- mutate(dipep,
                edd       = ifelse(edd == '',
                                   yes = NA,
                                   no  = edd),
                preg.post = as.character(preg.post),
                preg.post = ifelse(!is.na(preg.post),
                                   yes = preg.post,
                                   no  = ifelse(!is.na(delivery.date) & event.date - delivery.date > 0,
                                                yes = 'Postpartum',
                                                no  = 'Pregnant')),
                preg.post = ifelse(!is.na(preg.post),
                                   yes = preg.post,
                                   no  = ifelse(is.na(delivery.date) & ymd(event.date) - ymd(edd) > 0,
                                                yes = 'Postpartum',
                                                no  = 'Pregnant')),
                gestation = 280 - (ymd(edd) - ymd(event.date)))

dipep <- dipep %>%
         mutate(bmi = weight / (height / 100)^2,
                age = year(event.date) - year.of.birth,
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
                temperature.cat = ifelse(temperature <= 37.5,
                                         yes = 0,
                                         no  = 1),
                temperature.cat = ifelse(is.na(temperature.cat),
                                         yes = 0,
                                         no  = temperature.cat),
                bp.diastolic.cat = ifelse(bp.diastolic >= 50,
                                          yes = 0,
                                          no  = 1),
                bp.diastolic.cat = ifelse(is.na(bp.diastolic.cat),
                                          yes = 0,
                                          no  = bp.diastolic.cat),
                bp.systolic.cat = ifelse(bp.systolic >= 90,
                                         yes = 0,
                                         no  = 1),
                bp.systolic.cat = ifelse(is.na(bp.systolic.cat),
                                          yes = 0,
                                          no  = bp.systolic.cat),
                o2.saturation.cat = ifelse(o2.saturation < 95,
                                           yes = 1,
                                           no  = 0),
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
                ## gestation = 280 - (ymd(edd) - ymd(event.date)),
                ## trimester = ifelse(gestation < 98,
                ##                    yes = 0,
                ##                    no  = ifelse(gestation >= 98 & gestation < 196,
                ##                                 yes = 1,
                ##                                 ifelse(gestation >= 196 & preg.post == 'Pregnant',
                ##                                        yes = 2,
                ##                                        no  = 3))),
                ## trimester = ifelse(is.na(trimester),
                ##                    yes = 0,
                ##                    no  = trimester),
                ## 2017-03-01 - For some reason there are instances where preg.post is missing
                ##              These need resolving _before_ trimester can be derived, therefore
                ##              use the delivery date to do so.
                trimester = case_when(.$gestation < 98   & !is.na(.$preg.post)                     ~ 0,
                                      .$gestation >= 98  & .$gestation < 196   & !is.na(.$preg.post) ~ 1,
                                      .$gestation >= 196 & !is.na(.$preg.post) & .$preg.post == 'Pregnant'   ~ 2,
                                      .$gestation >= 196 & !is.na(.$preg.post) & .$preg.post == 'Postpartum' ~ 3),
                trimester = ifelse(is.na(gestation) & is.na(trimester) &
                                   !is.na(preg.post) & preg.post == 'Postpartum',
                                   yes = 3,
                                   no  = trimester),
                heart.rate.cat = ifelse(trimester != 2 & heart.rate > 100,
                                        yes = 1,
                                        no  = ifelse(trimester == 2 & heart.rate > 110,
                                                     yes = 1,
                                                     no  = 0)),
                heart.rate.cat = ifelse(is.na(heart.rate.cat),
                                        yes = 0,
                                        no  = heart.rate.cat),
                surgery = ifelse(is.na(surgery),
                                 yes = 'No',
                                 no  = as.character(surgery)),
                surgery = factor(surgery,
                                 levels = c('No', 'Yes')),
                prev.preg.problem = ifelse(is.na(prev.preg.problem),
                                           yes = 'No',
                                           no  = as.character(prev.preg.problem)),
                prev.preg.problem = factor(prev.preg.problem,
                                           levels = c('No', 'Yes')),
                ## See email 2016-10-27 @ 10:46 from s.goodacre@sheffield.ac.uk
                ## ToDo 2017-02-22 - Possible some of these SHOULDN'T be derived for
                ##                   individuals who are 'Non recruited' since they
                ##                   won't have responded to the surveys.
                cesarean = ifelse(grepl('c*esarian|c*section|caesarean|emcs|lscs|c/s', surgery.other, ignore.case = TRUE),
                                  yes = 1,
                                  no  = 0),
                cesarean = ifelse(is.na(cesarean),
                                  yes = 0,
                                  no  = cesarean),
                smoking = ifelse(is.na(smoking),
                                 yes = 'never',
                                 no  = as.character(smoking)),
                smoking = factor(smoking,
                                 levels = c('never', 'gave up prior to pregnancy', 'gave up during pregnancy', 'current')),
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
                existing.medical = ifelse(is.na(existing.medical),
                                          yes = 0,
                                          no  = existing.medical),
                existing.medical.cancer = ifelse(is.na(existing.medical.cancer),
                                                 yes = 0,
                                                 no  = existing.medical.cancer),
                this.pregnancy.problems = ifelse(is.na(this.pregnancy.problems),
                                                 yes = 0,
                                                 no  = this.pregnancy.problems),
                diagnosis.post.pe = ifelse(grepl('pe', diagnosis.post, ignore.case = TRUE) |
                                           grepl('pulmonary embo', diagnosis.post, ignore.case = TRUE) |
                                           grepl('p\\.e\\.', diagnosis.post, ignore.case = TRUE),
                                           yes = 1,
                                           no  = 0),
                ## Some really unhelpful entries that need tidying
                diagnosis.post.pe = ifelse(grepl('^suspected pulmonary emb', diagnosis.post, ignore.case = TRUE) |
                                           grepl('^suspected pe', diagnosis.post, ignore.case = TRUE) |
                                           grepl('^suspected p\\.e', diagnosis.post, ignore.case = TRUE) |
                                           grepl('^query pe$', diagnosis.post, ignore.case = TRUE),
                                           yes = 1,
                                           no  = 0),
                ## Now convert matches that include secondary possibility to no
                diagonsis.post.pe = ifelse(grepl('embolus or', diagnosis.post, ignore.case = TRUE) |
                                           grepl('^suspected p\\.e or', diagnosis.post, ignore.case = TRUE) |
                                           grepl('^suspected p\\.e, or', diagnosis.post, ignore.case = TRUE),
                                           yes = 0,
                                           no  = diagnosis.post),
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
                injury = ifelse(is.na(injury),
                                 yes = 'No',
                                no  = as.character(injury)),
                thrombosis = ifelse(is.na(thrombosis),
                                 yes = 'No',
                                no  = as.character(thrombosis)),
                ecg = ifelse(is.na(ecg),
                             yes = 'Not performed',
                             no  = as.character(ecg)),
                medical.other.dvt.pe = ifelse(is.na(medical.other.dvt.pe),
                                              yes = 'No',
                                              no  = medical.other.dvt.pe),
                admitted.hospital = ifelse(is.na(admitted.hospital),
                                           yes = 'No',
                                           no  = as.character(admitted.hospital)),
                ddimer.elisa.pooled = ifelse(!is.na(ddimer.elisa),
                                             yes = ddimer.elisa,
                                             no  = d.dimer),
                ddimer.innovance.pooled = ifelse(!is.na(ddimer.innovance),
                                                 yes = ddimer.innovance,
                                                 no  = d.dimer)))
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
                heart.rate.cat = factor(heart.rate.cat,
                                        ## levels = c(0, 1),
                                        labels = c('Low', 'High')),
                bp.diastolic.cat = factor(bp.diastolic.cat,
                                          levels = c(0, 1),
                                          labels = c('High', 'Low')),
                bp.systolic.cat = factor(bp.systolic.cat,
                                         levels = c(0, 1),
                                         labels = c('High', 'Low')),
                o2.saturation.cat = factor(o2.saturation.cat,
                                           levels = c(0, 1),
                                           labels = c('High', 'Low')),
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
                ecg     = factor(ecg),
                ecg.cat = factor(ecg.cat,
                                 levels = c(0, 1),
                                 labels = c('Normal ECG', 'Abnormal ECG')),
                xray     = factor(xray),
                xray.cat = factor(xray.cat,
                                  levels = c(0, 1),
                                  labels = c('Normal X-Ray', 'Abnormal X-Ray')),
                trimester = factor(trimester,
                                   levels = c(0, 1, 2, 3),
                                   labels = c('1st Trimester', '2nd Trimester', '3rd Trimester', 'Post-Partum')),
                existing.medical = factor(existing.medical,
                                          levels = c(0, 1),
                                          labels = c('No', 'Yes')),
                existing.medical.cancer = factor(existing.medical.cancer,
                                                 levels = c(0, 1),
                                                 labels = c('No', 'Yes')),
                this.pregnancy.problems = factor(this.pregnancy.problems,
                                                 levels = c(0, 1),
                                                 labels = c('No', 'Yes')),
                diagnosis.post.pe = factor(diagnosis.post.pe,
                                           levels = c(0, 1),
                                           labels = c('No PE', 'PE')),
                medical.other.dvt.pe = factor(medical.other.dvt.pe,
                                              labels = c('No', 'Yes')),
                thromboprophylaxis   = factor(thromboprophylaxis,
                                              labels = c('No', 'Yes')),
                admitted.hospital    = factor(admitted.hospital,
                                              levels = c('No', 'Yes')),
                presenting.features.pleuritic      = factor(presenting.features.pleuritic),
                presenting.features.non.pleuritic  = factor(presenting.features.non.pleuritic),
                presenting.features.sob.exertion   = factor(presenting.features.sob.exertion),
                presenting.features.sob.rest       = factor(presenting.features.sob.rest),
                presenting.features.cough          = factor(presenting.features.cough),
                presenting.features.syncope        = factor(presenting.features.syncope),
                presenting.features.haemoptysis    = factor(presenting.features.haemoptysis),
                presenting.features.other          = factor(presenting.features.other),
                history.thrombosis                 = factor(history.thrombosis),
                history.veins                      = factor(history.veins),
                history.iv.drug                    = factor(history.iv.drug),
                thrombo                            = factor(thrombo),
                multiple.preg                      = factor(multiple.preg),
                thrombosis                         = factor(thrombosis),
                injury                             = factor(injury),
                travel                             = factor(travel),
                immobil                            = factor(immobil))
## Explicitly set the reference level for all factor variables
dipep <- mutate(dipep,
                bmi.cat                           = relevel(bmi.cat,
                                                            ref = 'Low'),
                pregnancies.under.cat             = relevel(pregnancies.under.cat,
                                                            ref = 'No previous pregnancies < 24 weeks'),
                pregnancies.over.cat              = relevel(pregnancies.over.cat,
                                                            ref = 'No previous pregnancies > 24 weeks'),
                prev.preg.problem                 = relevel(prev.preg.problem,
                                                            ref = 'No'),
                temperature.cat                   = relevel(temperature.cat,
                                                            ref = 'Low'),
                heart.rate.cat                    = relevel(heart.rate.cat,
                                                            ref = 'Low'),
                bp.diastolic.cat                  = relevel(bp.diastolic.cat,
                                                            ref = 'Low'),
                bp.systolic.cat                   = relevel(bp.systolic.cat,
                                                            ref = 'Low'),
                heart.rate.cat                    = relevel(heart.rate.cat,
                                                            ref = 'Low'),
                o2.saturation.cat                 = relevel(o2.saturation.cat,
                                                            ref = 'High'),
                respiratory.rate.cat              = relevel(respiratory.rate.cat,
                                                            ref = 'Low'),
                cesarean                          = relevel(cesarean,
                                                            ref = 'No Cesarean'),
                surgery                           = relevel(surgery,
                                                            ref = 'No'),
                admitted.hospital                 = relevel(admitted.hospital,
                                                            ref = 'No'),
                smoking.cat                       = relevel(smoking.cat,
                                                            ref = 'Non-smoker'),
                age.cat                           = relevel(age.cat,
                                                            ref = 'Young'),
                ecg                               = relevel(ecg,
                                                            ref = 'Not performed'),
                ecg.cat                           = relevel(ecg.cat,
                                                            ref = 'Normal ECG'),
                xray                              = relevel(xray,
                                                            ref = 'Not performed'),
                xray.cat                          = relevel(xray.cat,
                                                            ref = 'Normal X-Ray'),
                trimester                         = relevel(trimester,
                                                            ref = '1st Trimester'),
                existing.medical                  = relevel(existing.medical,
                                                            ref = 'No'),
                this.pregnancy.problems           = relevel(this.pregnancy.problems,
                                                            ref = 'No'),
                diagnosis.post.pe                 = relevel(diagnosis.post.pe,
                                                            ref = 'No PE'),
                presenting.features.pleuritic     = relevel(presenting.features.pleuritic,
                                                            ref = 'Not Ticked'),
                presenting.features.non.pleuritic = relevel(presenting.features.non.pleuritic,
                                                            ref = 'Not Ticked'),
                presenting.features.sob.exertion  = relevel(presenting.features.sob.exertion,
                                                            ref = 'Not Ticked'),
                presenting.features.sob.rest      = relevel(presenting.features.sob.rest,
                                                            ref = 'Not Ticked'),
                presenting.features.cough         = relevel(presenting.features.cough,
                                                            ref = 'Not Ticked'),
                presenting.features.syncope       = relevel(presenting.features.syncope,
                                                            ref = 'Not Ticked'),
                presenting.features.haemoptysis   = relevel(presenting.features.haemoptysis,
                                                            ref = 'Not Ticked'),
                presenting.features.other         = relevel(presenting.features.other,
                                                            ref = 'Not Ticked'),
                history.thrombosis                = relevel(history.thrombosis,
                                                            ref = 'No'),
                history.veins                     = relevel(history.veins,
                                                            ref = 'No'),
                history.iv.drug                   = relevel(history.iv.drug,
                                                            ref = 'No'),
                thrombo                           = relevel(thrombo,
                                                            ref = 'No'),
                thrombosis                        = relevel(thrombosis,
                                                            ref = 'No'),
                multiple.preg                     = relevel(multiple.preg,
                                                            ref = 'No'),
                travel                            = relevel(travel,
                                                            ref = 'No'),
                immobil                           = relevel(immobil,
                                                            ref = 'No'),
                injury                            = relevel(injury,
                                                            ref = 'No'),
                medical.other.dvt.pe              = relevel(medical.other.dvt.pe,
                                                            ref = 'No'),
                thromboprophylaxis                = relevel(thromboprophylaxis,
                                                            ref = 'No'))
## Derive binary indicator for Suspected v's Diagnosed PE
dipep <- dipep %>%
         mutate(pe = case_when(.$group == 'Suspected PE' ~ 'Suspected PE',
                               .$group == 'Diagnosed PE' ~ 'Diagnosed PE'),
                pe = factor(pe, levels = c('Suspected PE', 'Diagnosed PE')))
#######################################################################
## Derive clinical rules based on...                                 ##
##                                                                   ##
## Simplified Geneva (http://dx.doi.org/10.1001/archinte.168.19.2131)##
## PERC (http://dx.doi.org/10.1111/j.1538-7836.2004.00790.x)         ##
## Wells (http://dx.doi.org/10.7326/0003-4819-135-2-200107170-00010) ##
#######################################################################
## Simplified Geneva
dipep <- dipep %>%
         mutate(simplified.age = ifelse(age <= 65 | is.na(age),
                                        yes = 0,
                                        no  = 1),
                simplified.prev.dvt.pe = ifelse(thrombosis == 'No' | is.na(thrombosis),
                                                yes = 0,
                                                no  = 3),
                simplified.prev.dvt.pe = ifelse(thromb.event == 'Yes',
                                                yes = 3,
                                                no  = simplified.prev.dvt.pe),
                simplified.prev.dvt.pe = ifelse(is.na(thromb.event),
                                                yes = 0,
                                                no  = simplified.prev.dvt.pe),
                simplified.surgery = ifelse(surgery == 'No' | is.na(surgery),
                                            yes = 0,
                                            no  = 2),
                simplified.neoplasm = ifelse(existing.medical.cancer == 'No' | is.na(existing.medical.cancer),
                                             yes = 0,
                                             no  = 2),
                simplified.lower.limb.unilateral.pain = ifelse(grepl('leg pain', other.symptoms.specify, ignore.case = TRUE) |
                                                               grepl('calf pain', other.symptoms.specify, ignore.case = TRUE) |
                                                               grepl('pain in left leg', other.symptoms.specify, ignore.case = TRUE) |
                                                               grepl('right calf swelling and pain', other.symptoms.specify, ignore.case = TRUE) |
                                                               grepl('painful \\(r\\) leg', other.symptoms.specify, ignore.case = TRUE),
                                                               yes = 3,
                                                               no  = 0),
                ## There are however entries of 'Bilateral lower leg pain' which need correcting
                simplified.lower.limb.unilateral.pain = ifelse(grepl('bilateral lower leg pain', other.symptoms.specify, ignore.case = TRUE) |
                                                               grepl('bilateral calf pain', other.symptoms.specify, ignore.case = TRUE),
                                                    yes = 0,
                                                    no  = simplified.lower.limb.unilateral.pain),
                simplified.haemoptysis = ifelse(presenting.features.haemoptysis == 'Not Ticked' | is.na(presenting.features.haemoptysis),
                                                yes = 0,
                                                no  = 2),
                simplified.heart.rate = case_when(.$heart.rate < 75                       ~ 0,
                                                  .$heart.rate >= 75 & .$heart.rate < 94  ~ 3,
                                                  .$heart.rate >= 94                      ~ 5),
                simplified.heart.rate = ifelse(is.na(simplified.heart.rate),
                                               yes = 0,
                                               no  = simplified.heart.rate),
                ## 2017-03-07 - Use Clinical signs of DVT to assess pain on palpitations
                ##              See emails from s.goodacre@sheffield.ac.uk 2017-03-07 @ 09:04
                ##                              s.goodacre@sheffield.ac.uk 2017-03-07 @ 09:07
                simplified.pain.palpitations = ifelse(dvt == 'No' | dvt == 'Not answered' | dvt == 'Not recorded' | is.na(dvt),
                                                      yes = 0,
                                                      no  = 4),
                simplified = simplified.age +
                                   simplified.prev.dvt.pe +
                                   simplified.surgery +
                                   simplified.neoplasm +
                                   simplified.lower.limb.unilateral.pain +
                                   simplified.haemoptysis +
                                   simplified.heart.rate +
                                   simplified.pain.palpitations,
                simplified.risk = ifelse(simplified < 4,
                                         yes = 'Low',
                                         no  = ifelse(simplified >= 11,
                                                      yes = 'High',
                                                      no  = 'Moderate')),
                simplified.risk = factor(simplified.risk,
                                         levels = c('Low', 'Moderate', 'High')),
                simplified.pe = ifelse(simplified >= 4,
                                       yes = 'Simplified PE',
                                       no  = 'No Simplified PE'),
                simplified.pe = factor(simplified.pe,
                                       levels = c('No Simplified PE', 'Simplified PE')),
                simplified    = factor(simplified, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)))
## PERC
dipep <- dipep %>%
         mutate(perc.age = ifelse(age < 50 | is.na(age),
                                  yes = 0,
                                  no  = 1),
                perc.heart.rate = ifelse(heart.rate < 100 | is.na(heart.rate),
                                         yes = 0,
                                         no  = 1),
                perc.o2 = ifelse(o2.saturation >= 95,
                                 yes = 0,
                                 no  = 1),
                perc.o2 = ifelse(is.na(o2.saturation),
                                 yes = 0,
                                 no  = perc.o2),
                perc.prev.dvt.pe = ifelse(thrombosis == 'No' | is.na(thrombosis),
                                          yes = 0,
                                          no  = 1),
                perc.prev.dvt.pe = ifelse(thromb.event == 'Yes',
                                          yes = 1,
                                          no  = perc.prev.dvt.pe),
                perc.prev.dvt.pe = ifelse(is.na(thromb.event),
                                          yes = 0,
                                          no  = perc.prev.dvt.pe),
                perc.surgery = ifelse(surgery == 'No' | is.na(surgery),
                                      yes = 0,
                                      no  = 1),
                perc.haemoptysis = ifelse(presenting.features.haemoptysis == 'Ticked',
                                          yes = 1,
                                          no  = 0),
                perc.hormone = ifelse(other.medication.specify != 'hrt',
                                      yes = 0,
                                      no  = 1),
                perc.hormone = ifelse(is.na(other.medication.specify),
                                      yes = 0,
                                      no  = perc.hormone),
                perc.leg.swelling = ifelse(grepl('leg swelling', other.symptoms.specify, ignore.case = TRUE) |
                                           grepl('legs swelling', other.symptoms.specify, ignore.case = TRUE) |
                                           grepl('swollen leg', other.symptoms.specify, ignore.case = TRUE) |
                                           grepl('swollen left calf', other.symptoms.specify, ignore.case = TRUE) |
                                           grepl('right calf swelling', other.symptoms.specify, ignore.case = TRUE),
                                           yes = 1,
                                           no  = 0),
                ## Recode bilateral instances
                perc.leg.swelling = ifelse(grepl('both legs swelling', other.symptoms.specify, ignore.case = TRUE) |
                                           grepl('swollen legs', other.symptoms.specify, ignore.case = TRUE),
                                           yes = 0,
                                           no  = perc.leg.swelling),
                perc      = perc.age +
                                  perc.heart.rate +
                                  perc.o2 +
                                  perc.prev.dvt.pe +
                                  perc.surgery +
                                  perc.haemoptysis +
                                  perc.hormone +
                                  perc.leg.swelling,
                perc.pe = ifelse(perc >= 1,
                                 yes = 'PERC PE',
                                 no  = 'No PERC PE'),
                perc          = factor(perc, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8)),
                perc.pe = factor(perc.pe,
                                 levels = c('No PERC PE', 'PERC PE')))
## Wells
dipep <- mutate(dipep,
                wells.dvt = ifelse(dvt == 'Yes',
                                   yes = 3,
                                   no  = 0),
                wells.dvt = ifelse(is.na(dvt),
                                   yes = 0,
                                   no  = wells.dvt),
                ## Two definitions of PE is primary diagnosis OR equally likely...
                wells.alternative.permissive = ifelse(likely.diagnosis == 'Other',
                                                      yes = 0,
                                                      no  = 3),
                wells.alternative.strict = ifelse(likely.diagnosis == 'PE',
                                                  yes = 3,
                                                  no  = 0),
                wells.heart.rate = ifelse(heart.rate <= 100 | is.na(heart.rate),
                                          yes = 0,
                                          no  = 1.5),
                wells.immobil = ifelse(immobil == 'No' | is.na(immobil),
                                       yes = 0,
                                       no  = 1.5),
                wells.surgery = ifelse(surgery == 'No' | is.na(surgery),
                                       yes = 0,
                                       no  = 1.5),
                wells.surgery.immobil = ifelse(wells.surgery != 0 | wells.immobil != 0,
                                               yes = 1.5,
                                               no  = 0),
                wells.prev.dvt.pe.thrombosis = ifelse(thrombosis == 'No' | is.na(thrombosis),
                                                      yes = 0,
                                                      no  = 1.5),
                ## 2017-03-07 - Email from s.goodacre@sheffield.ac.uk 2017-03-07 @ 09:04
                ##              states that previous thrombosis (thrombosis) AND
                ##              thrombosis during pregnancy are to be used
                wells.current.dvt.pe.thromb.event  = ifelse(thromb.event == 'No' | is.na(thromb.event),
                                                            yes = 0,
                                                            no = 1.5),
                wells.dvt.pe = ifelse(wells.prev.dvt.pe.thrombosis      == 0 &
                                      wells.current.dvt.pe.thromb.event == 0,
                                      yes = 0,
                                      no  = 1.5),
                wells.haemoptysis = ifelse(presenting.features.haemoptysis == 'Not Ticked' | is.na(presenting.features.haemoptysis),
                                          yes = 0,
                                          no  = 1),
                wells.neoplasm = ifelse(existing.medical.cancer == 'No' | is.na(existing.medical.cancer),
                                        yes = 0,
                                        no  = 1),
                wells.permissive = wells.dvt +
                                         wells.alternative.permissive +
                                         wells.heart.rate +
                                         wells.surgery.immobil +
                                         wells.dvt.pe +
                                         wells.haemoptysis +
                                         wells.neoplasm,
                wells.permissive.risk = ifelse(wells.permissive > 6,
                                               yes = 'High',
                                               no = ifelse(wells.permissive > 2,
                                                           yes = 'Moderate',
                                                           no  = 'Low')),
                wells.permissive.pe      = ifelse(wells.permissive > 4,
                                                  yes = 'Wells PE',
                                                  no  = 'No Wells PE'),
                wells.permissive         = factor(wells.permissive,
                                                  levels = c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12.5)),
                wells.permissive.pe      = factor(wells.permissive.pe,
                                                  levels = c('No Wells PE', 'Wells PE')),
                wells.strict = wells.dvt +
                                     wells.alternative.strict +
                                     wells.heart.rate +
                                     wells.surgery.immobil +
                                     wells.dvt.pe +
                                     wells.haemoptysis +
                                     wells.neoplasm,
                wells.strict.risk = ifelse(wells.strict > 6,
                                       yes = 'High',
                                       no = ifelse(wells.strict > 2,
                                                   yes = 'Moderate',
                                                   no  = 'Low')),
                wells.strict.pe      = ifelse(wells.strict > 4,
                                                  yes = 'Wells PE',
                                                  no  = 'No Wells PE'),
                wells.strict         = factor(wells.strict,
                                                  levels = c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12.5)),
                wells.strict.pe      = factor(wells.strict.pe,
                                              levels = c('No Wells PE', 'Wells PE')))
## Remove extrenuous variables
dipep <- dplyr::select(dipep,
                       -wells.prev.dvt.pe.thrombosis,
                       -wells.current.dvt.pe.thromb.event)
## Delphi Consensus rule.
## Nothings ever simple and they've derived three scores, which will need testing in
## however many different cohorts they wish to test sensitivity in!!!
##
## Details are in the following document
##
## ../projects/DiPEP/08. Study Management/PMG/07 Jun 16 - PMG + Delphi Meeting/Documents for Delphi Consensus Meeting/DIPEP CLINICAL DECISION RULE FEEDBACK_7June2016.docx
##
## 2017-03-20 Oh dear, the above rules were outdated, transpires the correct ones are detailed in...
##
## ../projects/DiPEP/14. Study Closure/Record of Dissemination/Conferences/Submissions/Consensus Derived Clinical Decision Rules to Guide Advanced Imaging Decisions for Pulmonary Embolism in Pregnancy and the Post-partum Period_SG_final.docx

dipep <- dipep %>%
         mutate(## Three forms of the Delphi Consensus score are required.  Start
                ## with the Primary (REVISED 2017-03-20)
                ##
                ## NB - Changing method, derive binary (FALSE/TRUE) indicators of whether an individual
                ##      has a component, THEN derive the three scores multiplying each indicator by its
                ##      weight
                delphi.haemoptysis = case_when(is.na(.$presenting.features.haemoptysis)          ~ FALSE,
                                               .$presenting.features.haemoptysis == 'Not Ticked' ~ FALSE,
                                               .$presenting.features.haemoptysis == 'Ticked'     ~ TRUE),
                delphi.pleuritic   = case_when(is.na(.$presenting.features.pleuritic)          ~ FALSE,
                                               .$presenting.features.pleuritic == 'Not Ticked' ~ FALSE,
                                               .$presenting.features.pleuritic == 'Ticked'     ~ TRUE),
                delphi.thromb.event = case_when(is.na(.$thromb.event)   ~ FALSE,
                                                .$thromb.event == 'No'  ~ FALSE,
                                                .$thromb.event == 'Yes' ~ TRUE),
                delphi.thrombosis = case_when(is.na(.$thrombosis)   ~ FALSE,
                                                .$thrombosis == 'No'  ~ FALSE,
                                              .$thrombosis == 'Yes' ~ TRUE),
                delphi.history.dvt.pe = ifelse(delphi.thromb.event == TRUE |
                                               delphi.thrombosis == TRUE,
                                               yes = TRUE,
                                               no  = FALSE),
                delphi.family.history = case_when(is.na(.$history.thrombosis)    ~ FALSE,
                                                  .$history.thrombosis == 'No'   ~ FALSE,
                                                  .$history.thrombosis == 'Yes'  ~ TRUE),
                delphi.medical.history.injury = case_when(is.na(.$injury)   ~ FALSE,
                                                          .$injury == 'No'  ~ FALSE,
                                                          .$injury == 'Yes' ~ TRUE),
                delphi.medical.history.surgery = case_when(is.na(.$surgery)   ~ FALSE,
                                                           .$surgery == 'No'  ~ FALSE,
                                                           .$surgery == 'Yes' ~ TRUE),
                delphi.medical.history = ifelse(delphi.medical.history.injury == TRUE |
                                               delphi.medical.history.surgery == TRUE,
                                               yes = TRUE,
                                               no  = FALSE),
                delphi.obstetric.complication = case_when(is.na(.$obstetric.complication) ~ FALSE,
                                                         .$obstetric.complication == 0   ~ FALSE,
                                                         .$obstetric.complication == 1   ~ TRUE),
                delphi.obstetric.complication = ifelse(.$preg.post == 'Postpartum' & .$surgery == 'Yes',
                                                       yes = TRUE,
                                                       no  = delphi.obstetric.complication),
                delphi.medical.complication = case_when(is.na(.$medical.comorbidity)   ~ FALSE,
                                                       .$medical.comorbidity == 0  ~ FALSE,
                                                       .$medical.comorbidity == 1 ~ TRUE),
                delphi.gestation = case_when(is.na(.$trimester)             ~ FALSE,
                                            .$trimester == '1st Trimester' ~ FALSE,
                                            .$trimester == '2nd Trimester' ~ FALSE,
                                            .$trimester == '3rd Trimester' ~ TRUE,
                                            .$trimester == 'Post-Partum'   ~ TRUE),
                delphi.bmi = case_when(is.na(.$bmi) ~ FALSE,
                                      .$bmi < 30   ~ FALSE,
                                      .$bmi >= 30  ~ TRUE),
                delphi.dvt = case_when(is.na(.$dvt)            ~ FALSE,
                                      .$dvt == 'No'           ~ FALSE,
                                      .$dvt == 'Not answered' ~ FALSE,
                                      .$dvt == 'Not recorded' ~ FALSE,
                                      .$dvt == 'Yes'          ~ TRUE),
                delphi.o2.saturation = case_when(is.na(.$o2.saturation) ~ FALSE,
                                                .$o2.saturation >= 94  ~ FALSE,
                                                .$o2.saturation < 94   ~ TRUE),
                delphi.heart.rate = case_when(is.na(.$heart.rate) ~ FALSE,
                                             .$trimester != 2 & .$heart.rate <= 100 ~ FALSE,
                                             .$trimester != 2 & .$heart.rate > 100  ~ TRUE,
                                             .$trimester == 2 & .$heart.rate <= 110 ~ FALSE,
                                             .$trimester == 2 & .$heart.rate > 110  ~ TRUE),
                delphi.respiratory.rate = case_when(is.na(.$respiratory.rate) ~ FALSE,
                                                   .$respiratory.rate <= 24   ~ FALSE,
                                                   .$respiratory.rate > 24    ~ TRUE),
                ## Now that there are binary indicators for each component derive an overall
                ## score for each of the rules scaling as required
                delphi.primary       = (delphi.haemoptysis             * 3) +
                                       (delphi.pleuritic               * 0) +
                                       (delphi.history.dvt.pe          * 3) +
                                       (delphi.family.history          * 0) +
                                       (delphi.medical.history         * 2) +
                                       (delphi.obstetric.complication  * 1) +
                                       (delphi.medical.complication    * 2) +
                                       (delphi.gestation               * 1) +
                                       (delphi.bmi                     * 1) +
                                       (delphi.dvt                     * 3) +
                                       (delphi.o2.saturation           * 3) +
                                       (delphi.heart.rate              * 2) +
                                       (delphi.respiratory.rate        * 2),
                delphi.primary.pe = ifelse(delphi.primary >= 3,
                                           yes = 'Delphi Primary PE',
                                           no  = 'No Delphi Primary PE'),
                delphi.primary.pe = factor(delphi.primary.pe,
                                           levels = c('No Delphi Primary PE', 'Delphi Primary PE')),
                delphi.primary    = factor(delphi.primary,
                                           levels = c('0',  '1',   '2',  '3',  '4',  '5',  '6',  '7',  '8',  '9',
                                                      '10', '11', '12', '13', '14', '15', '16', '17', '18', '19',
                                                      '20', '21', '22', '23')),
                delphi.sensitivity       = (delphi.haemoptysis             * 1) +
                                           (delphi.pleuritic               * 1) +
                                           (delphi.history.dvt.pe          * 1) +
                                           (delphi.family.history          * 1) +
                                           (delphi.medical.history         * 1) +
                                           (delphi.obstetric.complication  * 1) +
                                           (delphi.medical.complication    * 1) +
                                           (delphi.gestation               * 1) +
                                           (delphi.bmi                     * 1) +
                                           (delphi.dvt                     * 1) +
                                           (delphi.o2.saturation           * 1) +
                                           (delphi.heart.rate              * 1) +
                                           (delphi.respiratory.rate        * 1),
                delphi.sensitivity.pe = ifelse(delphi.sensitivity >= 1,
                                               yes = 'Delphi Sensitivity PE',
                                               no  = 'No Delphi Sensitivity PE'),
                delphi.sensitivity.pe = factor(delphi.sensitivity.pe,
                                               levels = c('No Delphi Sensitivity PE', 'Delphi Sensitivity PE')),
                delphi.sensitivity    = factor(delphi.sensitivity,
                                               levels = c('0',   '1',  '2',  '3', '4', '5', '6', '7', '8', '9',
                                                       '10', '11', '12', '13')),
                delphi.specificity    = (delphi.haemoptysis             * 4) +
                                        (delphi.pleuritic               * 0) +
                                        (delphi.history.dvt.pe          * 4) +
                                        (delphi.family.history          * 0) +
                                        (delphi.medical.history         * 1) +
                                        (delphi.obstetric.complication  * 0) +
                                        (delphi.medical.complication    * 1) +
                                        (delphi.gestation               * 0) +
                                        (delphi.bmi                     * 0) +
                                        (delphi.dvt                     * 4) +
                                        (delphi.o2.saturation           * 3) +
                                        (delphi.heart.rate              * 2) +
                                        (delphi.respiratory.rate        * 2),
                delphi.specificity.pe = ifelse(delphi.specificity >= 4,
                                               yes = 'Delphi Specificity PE',
                                               no  = 'No Delphi Specificity PE'),
                delphi.specificity.pe = factor(delphi.specificity.pe,
                                               levels = c('No Delphi Specificity PE', 'Delphi Specificity PE')),
                delphi.specificity    = factor(delphi.specificity,
                                               levels = c('0',   '1',  '2',  '3',  '4',  '5',  '6',  '7',  '8',  '9',
                                                          '10', '11', '12', '13', '14', '15', '16', '17', '18', '19',
                                                          '20', '21'))) %>%
        dplyr::select(-delphi.thromb.event, -delphi.thrombosis, -delphi.medical.history.injury, -delphi.medical.history.surgery)


#######################################################################
## Date    : 2017-03-20 @ 11:32                                      ##
## From    : s.goodacre@sheffield.ac.uk                              ##
## Subject : DiPEP analysis                                          ##
## Details : Use the Delphi Obstetric/Medical complications in       ##
##           univariable logistic regression.                        ##
#######################################################################
dipep <- dipep %>%
         mutate(medical.complication = ifelse(delphi.medical.complication == 0 | is.na(delphi.medical.complication),
                                         yes = 'No',
                                         no  = 'Yes'),
                medical.complication = factor(medical.complication,
                                              labels = c('No', 'Yes')),
                medical.complication = relevel(medical.complication,
                                               ref = 'No'),
                obstetric.complication = ifelse(delphi.obstetric.complication == 0 | is.na(delphi.obstetric.complication),
                                         yes = 'No',
                                         no  = 'Yes'),
                obstetric.complication = factor(obstetric.complication,
                                                labels = c('No', 'Yes')),
                obstetric.complication = relevel(obstetric.complication,
                                                 ref = 'No'))

#######################################################################
## Date    : 2017-03-21 @ 11:32                                      ##
## From    : s.goodacre@sheffield.ac.uk                              ##
## Subject : DiPEP analysis                                          ##
## Details : Clarification of what files to use to get classification##
##           of ECG and X-Ray in relation to PE                      ##
#######################################################################
master$ecg.pe.classification.raw <- read.csv(file = 'ecg_pe_classification.csv',
                                             sep  = ';')
names(master$ecg.pe.classification.raw) <- gsub('Participant\\.No\\.',
                                                'screening',
                                                names(master$ecg.pe.classification.raw))
names(master$ecg.pe.classification.raw) <- gsub('PE\\.related\\.abnormality',
                                                'ecg.pe',
                                                names(master$ecg.pe.classification.raw))
## dplyr::select(master$ecg.pe.classification.raw,
##               screening,
##               ecg.pe)
master$xray.pe.classification.raw <- read.csv(file = 'xray_pe_classification.csv',
                                              sep  = ';')
names(master$xray.pe.classification.raw) <- gsub('Participant\\.No\\.',
                                                 'screening',
                                                 names(master$xray.pe.classification.raw))
names(master$xray.pe.classification.raw) <- gsub('PE\\.related\\.abnormality',
                                                 'xray.pe',
                                                 names(master$xray.pe.classification.raw))
## dplyr::select(master$xray.pe.classification.raw,
##               screening,
##               xray.pe)
## Merge with the dipep data frame and derive new variables for Xray reconciling with
## the existing abnormal classification.
dipep <- left_join(dipep,
                   dplyr::select(master$xray.pe.classification.raw,
                                 screening,
                                 xray.pe)) %>%
         mutate(xray.pe = ifelse(is.na(xray.pe) | xray.pe == '',
                                 yes = 'No',
                                 no  = xray.pe)) %>%
         left_join(.,
                   dplyr::select(master$ecg.pe.classification.raw,
                                 screening,
                                 ecg.pe)) %>%
         mutate(ecg.pe = ifelse(is.na(ecg.pe) | ecg.pe == '',
                                yes = 'No',
                                no  = ecg.pe)) %>%
         ## Finally derive new variables with informative levels and ensure factors
         ## with correct levels
         mutate(xray.pe = case_when((is.na(.$xray) | .$xray== 'Not performed' | .$xray == 'Normal') & .$xray.pe == 'No' ~ 'Normal / Not Performed / Missing',
                                    .$xray == 'Abnormal' & .$xray.pe == 'No'  ~ 'Abnormal - Other',
                                    .$xray == 'Abnormal' & .$xray.pe == 'Yes' ~ 'Abnormal - PE'),
                xray.pe = factor(xray.pe,
                                 levels = c('Normal / Not Performed / Missing', 'Abnormal - Other', 'Abnormal - PE')),
                xray.pe = relevel(xray.pe,
                                  ref = 'Normal / Not Performed / Missing'),
                ecg.pe = case_when((is.na(.$ecg) | .$ecg== 'Not performed' | .$ecg == 'Normal') & .$ecg.pe == 'No' ~ 'Normal / Not Performed / Missing',
                                   .$ecg == 'Abnormal' & .$ecg.pe == 'No'  ~ 'Normal / Not Performed / Missing',
                                   .$ecg == 'Abnormal' & .$ecg.pe == 'Yes' ~ 'Abnormal - PE'),
                ecg.pe = factor(ecg.pe,
                                levels = c('Normal / Not Performed / Missing', 'Abnormal - PE')),
                ecg.pe = relevel(ecg.pe,
                                 ref = 'Normal / Not Performed / Missing'))
## Tabulate to check (commented out)
## print('Original X-Ray (Row) v X-Ray PE')
## table(dipep$xray, dipep$xray.pe, useNA = 'ifany')
## print('Combined X-Ray')
## table(dipep$xray.pe2, useNA = 'ifany')
## print('Original v Combined X-Ray')
## table(dipep$xray, dipep$xray.pe2, useNA = 'ifany')
## print('Original ECG')
## table(dipep$ecg, useNA = 'ifany')
## print('Original ECG v ECG PE')
## table(dipep$ecg, dipep$ecg.pe, useNA = 'ifany')
## print('Combined ECG')
## table(dipep$ecg.pe2, useNA = 'ifany')
## print('Original v Combined ECG')
## table(dipep$ecg, dipep$ecg.pe2, useNA = 'ifany')

#######################################################################
## UKOSS Exclusions                                                  ##
#######################################################################
master$ukoss.exclusions <- read.table('exclusions_20170210.csv',
                                      header = FALSE,
                                      sep    = ';')
## Remove redundant and uninformative header
master$ukoss.exclusions <- master$ukoss.exclusions[4:nrow(master$ukoss.exclusions),]
names(master$ukoss.exclusions) <- c('screening', 'reason')
## Clean and tidy the identifiers 24 of which have been mutated and would no
## longer match anything
master$ukoss.exclusions <- mutate(master$ukoss.exclusions,
                                  screening = gsub(',', '', screening),
                                  screening = ifelse(nchar(screening) == 3,
                                                 yes = paste0('PE_', screening),
                                                 no  = screening),
                                  screening = ifelse(nchar(screening) == 4,
                                                 yes = gsub('E', 'PE_', screening),
                                                 no  = screening))

#######################################################################
## Other exclusions...                                               ##
#######################################################################
## Who      : k.horspool@sheffield.ac.uk                             ##
## Date     : 2017-03-20 @ 08:20                                     ##
## Subject  : Fwd: DiPEP data_problems with this pregnancy and XRAY/ ##
##                 ECG                                               ##
## Details  : Remove X-Ray for...                                    ##
##                                                                   ##
##              N01/16                                               ##
##              S08/07                                               ##
##              PE_280                                               ##
##              PE_269                                               ##
#######################################################################
xray <- c('N01/16', 'S08/07', 'PE_280', 'PE_269')
dipep <- dipep %>%
         mutate(xray     = ifelse(screening %in% xray,
                                  yes = NA,
                                  no  = xray),
                xray.cat = ifelse(screening %in% xray,
                                  yes = NA,
                                  no  = xray))

#######################################################################
## Missing data exclusions                                           ##
#######################################################################
## Restricting analyses to those who have > X% of data for each of   ##
## the chosen groupings (thresholds below)                           ##
##                                                                   ##
## Group          | Variables                    | Threshold         ##
## ---------------|------------------------------|-----------------  ##
## Physiology     | heart rate                   | 1 or more         ##
##                | respiratory rate             |                   ##
##                | O2 Saturation                |                   ##
## ---------------|------------------------------|-----------------  ##
## This Pregnancy | Multiple Pregnancy           | > 50%             ##
##                | Travel                       |                   ##
##                | Immobility                   |
## ---------------|------------------------------|-----------------  ##
## Medical History| Family history of thrombosis | > 50%             ##
##                | History of Varicose Veins    |                   ##
##                | History of IV Drug use       |                   ##
##                | Known thrombophilia          |                   ##
##                | Surgery                      |                   ##
##                | Injury                       |                   ##
##                | Medical problems             |                   ##
#######################################################################
physiology.miss.n <- dipep %>%
                     dplyr::select(heart.rate, respiratory.rate, o2.saturation) %>%
                     is.na() %>%
                     rowSums()
pregnancy.miss.n <- dipep %>%
                    dplyr::select(multiple.preg, travel, immobil) %>%
                    is.na() %>%
                    rowSums()
med.hist.miss.n  <- dipep %>%
                   dplyr::select(history.thrombosis,
                                 history.veins,
                                 history.iv.drug,
                                 thrombo,
                                 surgery,
                                 injury,
                                 medical.probs) %>%
                    is.na() %>%
                    rowSums()
missing <- cbind(dipep$screening,
                 physiology.miss.n,
                 pregnancy.miss.n,
                 med.hist.miss.n) %>%
           as.data.frame()
names(missing) <- gsub('V1', 'screening', names(missing))
missing <- mutate(missing,
                  physiology.miss.n = as.numeric(physiology.miss.n),
                  pregnancy.miss.n  = as.numeric(pregnancy.miss.n),
                  med.hist.miss.n   = as.numeric(med.hist.miss.n))
rm(physiology.miss.n, pregnancy.miss.n, med.hist.miss.n)
dipep <- left_join(dipep,
                   missing) %>%
         mutate(physiology.exclude = ifelse(physiology.miss.n > 1,
                                            yes = TRUE,
                                            no  = FALSE),
                pregnancy.exclude  = ifelse(pregnancy.miss.n / 3 > 0.5,
                                            yes = TRUE,
                                            no  = FALSE),
                med.hist.exclude   = ifelse(med.hist.miss.n / 7 > 0.5,
                                            yes = TRUE,
                                            no  = FALSE),
                missing.exclude    = ifelse(physiology.exclude == TRUE |
                                            pregnancy.exclude  == TRUE |
                                            med.hist.exclude   == TRUE,
                                            yes = TRUE,
                                            no  = FALSE))

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
fields <- rbind(fields,
                read.csv('fields_derived.csv'))

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
                 description[data.frame == 'biomarker_raw'] <- 'Raw Biomarker data after some (external) cleaning and import'
                 description[data.frame == 'biomarker_tidy'] <- 'Tidied Biomarker data after some (external) cleaning and import'
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
dipep.README.variables$dipep <- fields_dipep(df = dipep,
                                             fields = fields)

#######################################################################
## Save all data frames                                              ##
#######################################################################
save(master,
     dipep,
     dipep.raw,
     file   = '../data/dipep.RData')
## Subset the classification for Dan Pollard
classification <- dplyr::select(dipep,
                                screening, first.st, second.st, third.st, fourth.st, primary.dm, secondary.dm)
save(classification,
     file = '../data/classification.RData')
write.table(classification,
            file = 'classification.csv',
            row.names = FALSE,
            col.names  = TRUE,
            sep        = ",")
## Write a dataset in Stata format for Mike Bradburn to QC
## dplyr::select(dipep, -life.support.presentation, -incidental) %>%
##     write.dta(file = 'dipep.dta')
dipep_ <- dipep
names(dipep_) <- gsub('\\.', '_', names(dipep_))
names(dipep_) <- gsub('presenting_features', 'presenting', names(dipep_))
names(dipep_) <- gsub('simplified_', 'simp_', names(dipep_))
names(dipep_) <- gsub('thrombin_generation_', 'tg_', names(dipep_))
names(dipep_) <- gsub('delphig_primary_', 'dp_pri_', names(dipep_))
names(dipep_) <- gsub('delphi_sensitivity_', 'dp_sen_', names(dipep_))
names(dipep_) <- gsub('delphi_specificity_', 'dp_spe_', names(dipep_))
write_dta(dipep_, version = 14, path = 'stata/dipep.dta')
write_dta(dipep.README.variables$dipep, version = 14, path = 'stata/dipep_description.dta')
rm(dipep_)
