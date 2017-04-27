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
## Alternative to sed'ing each file is to sed the populate.sh files and run those...
##
## Biomarkers
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/results_all.Rmd > secondary/sections/subsections/biomarkers/results_all.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/results_excl_anti_coag.Rmd > secondary/sections/subsections/biomarkers/results_excl_anti_coag.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/all_template.Rmd > secondary/sections/subsections/biomarkers/all_template.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/excl_anti_coag_template.Rmd > secondary/sections/subsections/biomarkers/excl_anti_coag_template.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/results_excl_anti_coag.Rmd > secondary/sections/subsections/biomarkers/results_excl_anti_coag.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/biomarkers/populate.sh > secondary/sections/subsections/biomarkers/populate.sh
cd secondary/sections/subsections/biomarkers/
./populate.sh
cd ../../../../
## Existing scores
sed 's/first\.st/second\.st/g' sections/subsections/existing/logistic.Rmd > secondary/sections/subsections/existing/logistic.Rmd
sed 's/first\.st/second\.st/g' sections/subsections/existing/populate.sh > secondary/sections/subsections/existing/populate.sh
sed 's/first\.st/second\.st/g' sections/subsections/existing/template.Rmd > secondary/sections/subsections/existing/template.Rmd
cd secondary/sections/subsections/existing/
./populate.sh
cd ../../../../
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
## Alternative to sed'ing each file is to sed the populate.sh files and run those...
##
## Biomarkers
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/results_all.Rmd > tertiary/sections/subsections/biomarkers/results_all.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/results_excl_anti_coag.Rmd > tertiary/sections/subsections/biomarkers/results_excl_anti_coag.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/all_template.Rmd > tertiary/sections/subsections/biomarkers/all_template.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/excl_anti_coag_template.Rmd > tertiary/sections/subsections/biomarkers/excl_anti_coag_template.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/results_excl_anti_coag.Rmd > tertiary/sections/subsections/biomarkers/results_excl_anti_coag.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/biomarkers/populate.sh > tertiary/sections/subsections/biomarkers/populate.sh
cd tertiary/sections/subsections/biomarkers/
./populate.sh
cd ../../../../
## Existing scores
sed 's/first\.st/third\.st/g' sections/subsections/existing/logistic.Rmd > tertiary/sections/subsections/existing/logistic.Rmd
sed 's/first\.st/third\.st/g' sections/subsections/existing/populate.sh > tertiary/sections/subsections/existing/populate.sh
sed 's/first\.st/third\.st/g' sections/subsections/existing/template.Rmd > tertiary/sections/subsections/existing/template.Rmd
cd tertiary/sections/subsections/existing/
./populate.sh
cd ../../../../
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
## Alternative to sed'ing each file is to sed the populate.sh files and run those...
##
## Biomarkers
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/results_all.Rmd > quaternary/sections/subsections/biomarkers/results_all.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/results_excl_anti_coag.Rmd > quaternary/sections/subsections/biomarkers/results_excl_anti_coag.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/all_template.Rmd > quaternary/sections/subsections/biomarkers/all_template.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/excl_anti_coag_template.Rmd > quaternary/sections/subsections/biomarkers/excl_anti_coag_template.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/results_excl_anti_coag.Rmd > quaternary/sections/subsections/biomarkers/results_excl_anti_coag.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/biomarkers/populate.sh > quaternary/sections/subsections/biomarkers/populate.sh
cd quaternary/sections/subsections/biomarkers/
./populate.sh
cd ../../../../
## Existing scores
sed 's/first\.st/fourth\.st/g' sections/subsections/existing/logistic.Rmd > quaternary/sections/subsections/existing/logistic.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/existing/populate.sh > quaternary/sections/subsections/existing/populate.sh
sed 's/first\.st/fourth\.st/g' sections/subsections/existing/template.Rmd > quaternary/sections/subsections/existing/template.Rmd
cd quaternary/sections/subsections/existing/
./populate.sh
cd ../../../../
## Recursive Partitioning
sed 's/first\.st/fourth\.st/g' sections/subsections/rpart/categorised.Rmd > quaternary/sections/subsections/rpart/categorised.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/rpart/continuous.Rmd > quaternary/sections/subsections/rpart/continuous.Rmd
## LASSO
sed 's/first\.st/fourth\.st/g' sections/subsections/lasso/categorised.Rmd > quaternary/sections/subsections/lasso/categorised.Rmd
sed 's/first\.st/fourth\.st/g' sections/subsections/lasso/continuous.Rmd > quaternary/sections/subsections/lasso/continuous.Rmd
