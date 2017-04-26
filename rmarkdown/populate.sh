#!/bin/bash
##
## Created     : 2017-03-16
## Author      : n.shephard@sheffield.ac.uk
## Description : Takes the original set of files designed to work with the primary
##               classification of PE ('first.st') and uses sed to replace all
##               references to this primary classifier with the secondary ('second.st'),
##               tertiary ('third.st') and quaternary ('fourth.st') classifiers.
##
## NB - This is a Bash Shell script and is designed to work on *NIX systems.  If you
##      wish to use it under M$-Win then install and use the Cygwin Shell...
##
##      https://www.cygwin.com/

########################################################################################
## Secondary                                                                          ##
########################################################################################
## Start by copying the master file
sed -e 's/first\.st/second\.st/g' dipep.Rmd \
    -e 's/Primary Analyses/Secondary Analyses/g' \
    -e 's/set\.seed(69027181)/set\.seed(47848971)/g' > secondary/secondary.Rmd
## Copy the whole section/ directory so ALL child files are present
cp -r sections secondary/.
## Now replace 'first.st' within secondary/section/subsections/* files
## Demographics
sed -e 's/\$first\.st/\$second\.st/g' sections/subsections/demographics.Rmd \
    -e 's/first\.st,/second\.st,/g'> secondary/sections/subsections/demographics.Rmd
## Missing Data
sed 's/first\.st/second\.st/g' sections/subsections/missing_data.Rmd > secondary/sections/subsections/missing_data.Rmd
## Existing Scores
sed 's/first\.st/second\.st/g' sections/subsections/results_existing.Rmd > secondary/sections/subsections/results_existing.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/existing/logistic.Rmd > secondary/sections/subsections/existing/logistic.Rmd
sed 's/first\.st/second.st/g' sections/subsections/existing/simplified.Rmd > secondary/sections/subsections/existing/simplified.Rmd
sed 's/first\.st/second.st/g' sections/subsections/existing/perc.Rmd > secondary/sections/subsections/existing/perc.Rmd
sed 's/first\.st/second.st/g' sections/subsections/existing/wells_permissive.Rmd > secondary/sections/subsections/existing/wells_permissive.Rmd
sed 's/first\.st/second.st/g' sections/subsections/existing/wells_strict.Rmd > secondary/sections/subsections/existing/wells_strict.Rmd
sed 's/first\.st/second.st/g' sections/subsections/existing/delphi_primary.Rmd > secondary/sections/subsections/existing/delphi_primary.Rmd
sed 's/first\.st/second.st/g' sections/subsections/existing/delphi_sensitivity.Rmd > secondary/sections/subsections/existing/delphi_sensitivity.Rmd
sed 's/first\.st/second.st/g' sections/subsections/existing/delphi_specificity.Rmd > secondary/sections/subsections/existing/delphi_specificity.Rmd
## Logistic Regression
sed 's/first\.st/second\.st/g' sections/subsections/results_logistic.Rmd > secondary/sections/subsections/results_logistic.Rmd
## Biomarkers
sed -e "s/classification = 'first\.st/classification = 'second\.st/g" sections/subsections/biomarkers/prep.Rmd \
    -e 's/D = first\.st/D = second\.st/g'> secondary/sections/subsections/biomarkers/prep.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/biomarker_summary_all.Rmd > secondary/sections/subsections/biomarkers/biomarker_summary_all.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/biomarker_summary_excl_anti_coag.Rmd > secondary/sections/subsections/biomarkers/biomarker_summary_excl_anti_coag.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/biomarker_logistic_all.Rmd > secondary/sections/subsections/biomarkers/biomarker_logistic_all.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/biomarker_logistic_excl_anti_coag.Rmd > secondary/sections/subsections/biomarkers/biomarker_logistic_excl_anti_coag.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/gestation_all.Rmd > secondary/sections/subsections/biomarkers/gestation_all.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/gestation_excl_anti_coag.Rmd > secondary/sections/subsections/biomarkers/gestation_excl_anti_coag.Rmd
## Biomarkers - individual files for ALL
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_aptt.Rmd > secondary/sections/subsections/biomarkers/all_aptt.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_clauss_fibrinogen.Rmd > secondary/sections/subsections/biomarkers/all_clauss_fibrinogen.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_prothombin_time.Rmd > secondary/sections/subsections/biomarkers/all_prothombin_time.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_ddimer_innovance.Rmd > secondary/sections/subsections/biomarkers/all_ddimer_innovance.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_ddimer_elisa.Rmd > secondary/sections/subsections/biomarkers/all_ddimer_elisa.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_thrombin_generation_lag_time.Rmd > secondary/sections/subsections/biomarkers/all_thrombin_generation_lag_time.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_thrombin_generation_endogenous_potential.Rmd > secondary/sections/subsections/biomarkers/all_thrombin_generation_endogenous_potential.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_thrombin_generation_peak.Rmd > secondary/sections/subsections/biomarkers/all_thrombin_generation_peak.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_thrombin_generation_time_to_peak.Rmd > secondary/sections/subsections/biomarkers/all_thrombin_generation_time_to_peak.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_plasmin_antiplasmin.Rmd > secondary/sections/subsections/biomarkers/all_plasmin_antiplasmin.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_mrproanp.Rmd > secondary/sections/subsections/biomarkers/all_mrproanp.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_crp.Rmd > secondary/sections/subsections/biomarkers/all_crp.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_troponin.Rmd > secondary/sections/subsections/biomarkers/all_troponin.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_tissue_factor.Rmd > secondary/sections/subsections/biomarkers/all_tissue_factor.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_prothrombin_fragments.Rmd > secondary/sections/subsections/biomarkers/all_prothrombin_fragments.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_ddimer_cat.Rmd > secondary/sections/subsections/biomarkers/all_ddimer_cat.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_ddimer.Rmd > secondary/sections/subsections/biomarkers/all_ddimer.Rmd
## Biomarkers - individual files for EXCLUDING ANTI COAGS
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_aptt.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_aptt.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_clauss_fibrinogen.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_clauss_fibrinogen.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_prothombin_time.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_prothombin_time.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer_innovance.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_ddimer_innovance.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer_elisa.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_ddimer_elisa.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_lag_time.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_lag_time.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_endogenous_potential.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_endogenous_potential.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_peak.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_peak.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_time_to_peak.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_time_to_peak.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_plasmin_antiplasmin.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_plasmin_antiplasmin.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_mrproanp.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_mrproanp.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_crp.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_crp.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_troponin.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_troponin.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_tissue_factor.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_tissue_factor.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_prothrombin_fragments.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_prothrombin_fragments.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer_cat.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_ddimer_cat.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_ddimer.Rmd
## Recursive Partitioning
sed 's/first\.st/second\.st/g' sections/subsections/rpart/categorised.Rmd > secondary/sections/subsections/rpart/categorised.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/rpart/continuous.Rmd > secondary/sections/subsections/rpart/continuous.Rmd
## LASSO
sed 's/first\.st/second\.st/g' sections/subsections/lasso/categorised.Rmd > secondary/sections/subsections/lasso/categorised.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/lasso/continuous.Rmd > secondary/sections/subsections/lasso/continuous.Rmd


########################################################################################
## Tertiary                                                                          ##
########################################################################################
## Start by copying the master file
sed -e 's/first\.st/third\.st/g' dipep.Rmd \
    -e 's/Primary Analyses/Tertiary Analyses/g' \
    -e 's/set\.seed(69027181)/set\.seed(1653142)/g'  > tertiary/tertiary.Rmd
## Copy the whole section/ directory so ALL child files are present
cp -r sections tertiary/.
## Now replace 'first.st' within tertiary/section/subsections/* files
## Demographics
sed -e 's/\$first\.st/\$third\.st/g' sections/subsections/demographics.Rmd \
    -e 's/first\.st,/third\.st,/g' > tertiary/sections/subsections/demographics.Rmd
## Missing Data
sed 's/first\.st/third\.st/g' sections/subsections/missing_data.Rmd > tertiary/sections/subsections/missing_data.Rmd
## Existing Scores
sed 's/first\.st/third\.st/g' sections/subsections/results_existing.Rmd > tertiary/sections/subsections/results_existing.Rmd
sed 's/first\.st/third.st/g' sections/subsections/existing/logistic.Rmd > tertiary/sections/subsections/existing/logistic.Rmd
sed 's/first\.st/third.st/g' sections/subsections/existing/simplified.Rmd > tertiary/sections/subsections/existing/simplified.Rmd
sed 's/first\.st/third.st/g' sections/subsections/existing/perc.Rmd > tertiary/sections/subsections/existing/perc.Rmd
sed 's/first\.st/third.st/g' sections/subsections/existing/wells_permissive.Rmd > tertiary/sections/subsections/existing/wells_permissive.Rmd
sed 's/first\.st/third.st/g' sections/subsections/existing/wells_strict.Rmd > tertiary/sections/subsections/existing/wells_strict.Rmd
sed 's/first\.st/third.st/g' sections/subsections/existing/delphi_primary.Rmd > tertiary/sections/subsections/existing/delphi_primary.Rmd
sed 's/first\.st/third.st/g' sections/subsections/existing/delphi_sensitivity.Rmd > tertiary/sections/subsections/existing/delphi_sensitivity.Rmd
sed 's/first\.st/third.st/g' sections/subsections/existing/delphi_specificity.Rmd > tertiary/sections/subsections/existing/delphi_specificity.Rmd
## Logistic Regression
sed 's/first\.st/third\.st/g' sections/subsections/results_logistic.Rmd > tertiary/sections/subsections/results_logistic.Rmd
## Biomarkers
sed -e "s/classification = 'first\.st/classification = 'third\.st/g" sections/subsections/biomarkers/prep.Rmd \
    -e 's/D = first\.st/D = third\.st/g' > tertiary/sections/subsections/biomarkers/prep.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/biomarker_summary_all.Rmd > tertiary/sections/subsections/biomarkers/biomarker_summary_all.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/biomarker_summary_excl_anti_coag.Rmd > tertiary/sections/subsections/biomarkers/biomarker_summary_excl_anti_coag.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/biomarker_logistic_all.Rmd > tertiary/sections/subsections/biomarkers/biomarker_logistic_all.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/biomarker_logistic_excl_anti_coag.Rmd > tertiary/sections/subsections/biomarkers/biomarker_logistic_excl_anti_coag.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/gestation_all.Rmd > tertiary/sections/subsections/biomarkers/gestation_all.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/gestation_excl_anti_coag.Rmd > tertiary/sections/subsections/biomarkers/gestation_excl_anti_coag.Rmd

## Biomarkers - individual files for ALL
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_aptt.Rmd > thirdary/sections/subsections/biomarkers/all_aptt.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_clauss_fibrinogen.Rmd > thirdary/sections/subsections/biomarkers/all_clauss_fibrinogen.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_prothombin_time.Rmd > thirdary/sections/subsections/biomarkers/all_prothombin_time.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_ddimer_innovance.Rmd > thirdary/sections/subsections/biomarkers/all_ddimer_innovance.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_ddimer_elisa.Rmd > thirdary/sections/subsections/biomarkers/all_ddimer_elisa.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_thrombin_generation_lag_time.Rmd > thirdary/sections/subsections/biomarkers/all_thrombin_generation_lag_time.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_thrombin_generation_endogenous_potential.Rmd > thirdary/sections/subsections/biomarkers/all_thrombin_generation_endogenous_potential.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_thrombin_generation_peak.Rmd > thirdary/sections/subsections/biomarkers/all_thrombin_generation_peak.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_thrombin_generation_time_to_peak.Rmd > thirdary/sections/subsections/biomarkers/all_thrombin_generation_time_to_peak.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_plasmin_antiplasmin.Rmd > thirdary/sections/subsections/biomarkers/all_plasmin_antiplasmin.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_mrproanp.Rmd > thirdary/sections/subsections/biomarkers/all_mrproanp.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_crp.Rmd > thirdary/sections/subsections/biomarkers/all_crp.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_troponin.Rmd > thirdary/sections/subsections/biomarkers/all_troponin.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_tissue_factor.Rmd > thirdary/sections/subsections/biomarkers/all_tissue_factor.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_prothrombin_fragments.Rmd > thirdary/sections/subsections/biomarkers/all_prothrombin_fragments.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_ddimer_cat.Rmd > thirdary/sections/subsections/biomarkers/all_ddimer_cat.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_ddimer.Rmd > thirdary/sections/subsections/biomarkers/all_ddimer.Rmd
## Biomarkers - individual files for EXCLUDING ANTI COAGS
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_aptt.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_aptt.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_clauss_fibrinogen.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_clauss_fibrinogen.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_prothombin_time.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_prothombin_time.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer_innovance.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_ddimer_innovance.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer_elisa.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_ddimer_elisa.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_lag_time.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_lag_time.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_endogenous_potential.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_endogenous_potential.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_peak.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_peak.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_time_to_peak.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_time_to_peak.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_plasmin_antiplasmin.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_plasmin_antiplasmin.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_mrproanp.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_mrproanp.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_crp.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_crp.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_troponin.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_troponin.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_tissue_factor.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_tissue_factor.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_prothrombin_fragments.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_prothrombin_fragments.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer_cat.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_ddimer_cat.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer.Rmd > thirdary/sections/subsections/biomarkers/excl_anti_coag_ddimer.Rmd

## Recursive Partitioning
sed 's/first\.st/third\.st/g' sections/subsections/rpart/categorised.Rmd > tertiary/sections/subsections/rpart/categorised.Rmd
sed -e 's/first\.st/third\.st/g' sections/subsections/rpart/continuous.Rmd \
    -e 's/threshold             = 0\.5/threshold             = 0.3/g' > tertiary/sections/subsections/rpart/continuous.Rmd
## LASSO
sed 's/first\.st/third\.st/g' sections/subsections/lasso/categorised.Rmd > tertiary/sections/subsections/lasso/categorised.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/lasso/continuous.Rmd > tertiary/sections/subsections/lasso/continuous.Rmd

########################################################################################
## Quaternary                                                                          ##
########################################################################################
## Start by copying the master file
sed -e 's/first\.st/fourth\.st/g' dipep.Rmd \
    -e 's/Primary Analyses/Quaternary Analyses/g'  \
    -e 's/set\.seed(69027181)/set\.seed(241347971)/g'  > quaternary/quaternary.Rmd
## Copy the whole section/ directory so ALL child files are present
cp -r sections quaternary/.
## Now replace 'first.st' within quaternary/section/subsections/* files
## Demographics
sed -e 's/\$first\.st/\$fourth\.st/g' sections/subsections/demographics.Rmd \
    -e 's/first\.st,/fourth\.st,/g'> quaternary/sections/subsections/demographics.Rmd
## Missing Data
sed 's/first\.st/fourth\.st/g' sections/subsections/missing_data.Rmd > quaternary/sections/subsections/missing_data.Rmd
## Existing Scores
sed 's/first\.st/fourth\.st/g' sections/subsections/results_existing.Rmd > quaternary/sections/subsections/results_existing.Rmd
sed 's/first\.st/fourth.st/g' sections/subsections/existing/logistic.Rmd > quaternary/sections/subsections/existing/logistic.Rmd
sed 's/first\.st/fourth.st/g' sections/subsections/existing/simplified.Rmd > quaternary/sections/subsections/existing/simplified.Rmd
sed 's/first\.st/fourth.st/g' sections/subsections/existing/perc.Rmd > quaternary/sections/subsections/existing/perc.Rmd
sed 's/first\.st/fourth.st/g' sections/subsections/existing/wells_permissive.Rmd > quaternary/sections/subsections/existing/wells_permissive.Rmd
sed 's/first\.st/fourth.st/g' sections/subsections/existing/wells_strict.Rmd > quaternary/sections/subsections/existing/wells_strict.Rmd
sed 's/first\.st/fourth.st/g' sections/subsections/existing/delphi_primary.Rmd > quaternary/sections/subsections/existing/delphi_primary.Rmd
sed 's/first\.st/fourth.st/g' sections/subsections/existing/delphi_sensitivity.Rmd > quaternary/sections/subsections/existing/delphi_sensitivity.Rmd
sed 's/first\.st/fourth.st/g' sections/subsections/existing/delphi_specificity.Rmd > quaternary/sections/subsections/existing/delphi_specificity.Rmd
## Logistic Regression
sed 's/first\.st/fourth\.st/g' sections/subsections/results_logistic.Rmd > quaternary/sections/subsections/results_logistic.Rmd
## Biomarkers
sed -e "s/classification = 'first\.st/classification = 'fourth\.st/g" sections/subsections/biomarkers/prep.Rmd \
    -e 's/D = first\.st/D = fourth\.st/g' > quaternary/sections/subsections/biomarkers/prep.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/biomarker_summary_all.Rmd > quaternary/sections/subsections/biomarkers/biomarker_summary_all.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/biomarker_summary_excl_anti_coag.Rmd > quaternary/sections/subsections/biomarkers/biomarker_summary_excl_anti_coag.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/biomarker_logistic_all.Rmd > quaternary/sections/subsections/biomarkers/biomarker_logistic_all.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/biomarker_logistic_excl_anti_coag.Rmd > quaternary/sections/subsections/biomarkers/biomarker_logistic_excl_anti_coag.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/gestation_all.Rmd > quaternary/sections/subsections/biomarkers/gestation_all.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/gestation_excl_anti_coag.Rmd > quaternary/sections/subsections/biomarkers/gestation_excl_anti_coag.Rmd

## Biomarkers - individual files for ALL
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_aptt.Rmd > fourthary/sections/subsections/biomarkers/all_aptt.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_clauss_fibrinogen.Rmd > fourthary/sections/subsections/biomarkers/all_clauss_fibrinogen.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_prothombin_time.Rmd > fourthary/sections/subsections/biomarkers/all_prothombin_time.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_ddimer_innovance.Rmd > fourthary/sections/subsections/biomarkers/all_ddimer_innovance.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_ddimer_elisa.Rmd > fourthary/sections/subsections/biomarkers/all_ddimer_elisa.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_thrombin_generation_lag_time.Rmd > fourthary/sections/subsections/biomarkers/all_thrombin_generation_lag_time.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_thrombin_generation_endogenous_potential.Rmd > fourthary/sections/subsections/biomarkers/all_thrombin_generation_endogenous_potential.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_thrombin_generation_peak.Rmd > fourthary/sections/subsections/biomarkers/all_thrombin_generation_peak.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_thrombin_generation_time_to_peak.Rmd > fourthary/sections/subsections/biomarkers/all_thrombin_generation_time_to_peak.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_plasmin_antiplasmin.Rmd > fourthary/sections/subsections/biomarkers/all_plasmin_antiplasmin.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_mrproanp.Rmd > fourthary/sections/subsections/biomarkers/all_mrproanp.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_crp.Rmd > fourthary/sections/subsections/biomarkers/all_crp.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_troponin.Rmd > fourthary/sections/subsections/biomarkers/all_troponin.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_tissue_factor.Rmd > fourthary/sections/subsections/biomarkers/all_tissue_factor.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_prothrombin_fragments.Rmd > fourthary/sections/subsections/biomarkers/all_prothrombin_fragments.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_ddimer_cat.Rmd > fourthary/sections/subsections/biomarkers/all_ddimer_cat.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_ddimer.Rmd > fourthary/sections/subsections/biomarkers/all_ddimer.Rmd
## Biomarkers - individual files for EXCLUDING ANTI COAGS
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_aptt.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_aptt.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_clauss_fibrinogen.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_clauss_fibrinogen.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_prothombin_time.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_prothombin_time.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer_innovance.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_ddimer_innovance.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer_elisa.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_ddimer_elisa.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_lag_time.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_lag_time.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_endogenous_potential.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_endogenous_potential.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_peak.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_peak.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_time_to_peak.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_thrombin_generation_time_to_peak.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_plasmin_antiplasmin.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_plasmin_antiplasmin.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_mrproanp.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_mrproanp.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_crp.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_crp.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_troponin.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_troponin.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_tissue_factor.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_tissue_factor.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_prothrombin_fragments.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_prothrombin_fragments.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer_cat.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_ddimer_cat.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_ddimer.Rmd > fourthary/sections/subsections/biomarkers/excl_anti_coag_ddimer.Rmd

## Recursive Partitioning
sed 's/first\.st/fourth\.st/g' sections/subsections/rpart/categorised.Rmd > quaternary/sections/subsections/rpart/categorised.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/rpart/continuous.Rmd > quaternary/sections/subsections/rpart/continuous.Rmd
## LASSO
sed 's/first\.st/fourth\.st/g' sections/subsections/lasso/categorised.Rmd > quaternary/sections/subsections/lasso/categorised.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/lasso/continuous.Rmd > quaternary/sections/subsections/lasso/continuous.Rmd
