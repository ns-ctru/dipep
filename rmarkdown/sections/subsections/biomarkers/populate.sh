#!/bin/bash
##
## DESCRIPTION   Short script to generate output files (would like to figure out how to do
##               this all in R but do not have time so sticking with this quick and dirty
##               approach)
##
## USAGE         Works on *NIX systems, you would need to install a bash shell (e.g. Cygwin)
##               to run this on M$-Windows machines.  Modify the 'template.Rmd' then run...
##
##                  $ ./populate.sh
##
##               ...and all files will be updated and reflect the changes.

## APTT
sed -e 's/template/aptt/g' all_template.Rmd \
    -e 's/TEMPLATE/APTT (min)/g' \
    -e 's/MEASUREMENT/APTT/g' > all_aptt.Rmd
sed -e 's/template/aptt/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/APTT (min)/g' \
    -e 's/MEASUREMENT/APTT/g'  > excl_anti_coag_aptt.Rmd

## Clauss Fibrinogen
sed -e 's/template/clauss\.fibrinogen/g' all_template.Rmd \
    -e 's/TEMPLATE/Clauss Fibrinogen (g\/l)/g' \
    -e 's/MEASUREMENT/Clauss Fibrinogen/g'  > all_clauss_fibrinogen.Rmd
sed -e 's/template/clauss\.fibrinogen/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/Clauss Fibrinogen (g\/l)/g' \
    -e 's/MEASUREMENT/Clauss Fibrinogen/g' > excl_anti_coag_clauss_fibrinogen.Rmd

## Prothrombin (Time)
sed -e 's/template/prothombin.time/g' all_template.Rmd \
    -e 's/TEMPLATE/Prothombin Time (min)/g' \
    -e 's/MEASUREMENT/Prothombin (Time)/g' > all_prothombin_time.Rmd
sed -e 's/template/prothombin.time/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/Prothombin Time (min)/g' \
    -e 's/MEASUREMENT/Prothombin (Time)/g' > excl_anti_coag_prothombin_time.Rmd

## D-Dimer Innovance
sed -e 's/template/ddimer.innovance/g' all_template.Rmd \
    -e 's/TEMPLATE/D-Dimer Innovance (mg\/l)/g' \
    -e 's/MEASUREMENT/D-Dimer (Innovance)/g' > all_ddimer_innovance.Rmd
sed -e 's/template/ddimer.innovance/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/D-Dimer Innovance (mg\/l)/g' \
    -e 's/MEASUREMENT/D-Dimer (Innovance)/g' > excl_anti_coag_ddimer_innovance.Rmd

## D-Dimer ELISA
sed -e 's/template/ddimer.elisa/g' all_template.Rmd \
    -e 's/TEMPLATE/D-Dimer ELISA (ng\/l)/g' \
    -e 's/MEASUREMENT/D-Dimer (ELISA)/g' > all_ddimer_elisa.Rmd
sed -e 's/template/ddimer.elisa/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/D-Dimer ELISA (ng\/l)/g' \
    -e 's/MEASUREMENT/D-Dimer (ELISA)/g' > excl_anti_coag_ddimer_elisa.Rmd

## Thrombin Generation (Lag Time)
sed -e 's/template/thrombin.generation.lag.time/g' all_template.Rmd \
    -e 's/TEMPLATE/Thrombin Generation - Lag Time (min)/g' \
    -e 's/MEASUREMENT/Thrombin Generation (Lag Time)/g' > all_thrombin_generation_lag_time.Rmd
sed -e 's/template/thrombin.generation.lag.time/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/Thrombin Generation - Lag Time (min)/g' \
    -e 's/MEASUREMENT/Thrombin Generation (Lag Time)/g' > excl_anti_coag_thrombin_generation_lag_time.Rmd

## Thrombin Generation (Endogenous Potential)
sed -e 's/template/thrombin.generation.endogenous.potential/g' all_template.Rmd \
    -e 's/TEMPLATE/Thrombin Generation - Endogenous Potential (min)/g' \
    -e 's/MEASUREMENT/Thrombin Generation (Endogenous Potential)/g' > all_thrombin_generation_endogenous_potential.Rmd
sed -e 's/template/thrombin.generation.endogenous.potential/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/Thrombin Generation - Endogenous Potential (min)/g' \
    -e 's/MEASUREMENT/Thrombin Generation (Endogenous Potential)/g'  > excl_anti_coag_thrombin_generation_endogenous_potential.Rmd

## Thrombin Generation (Peak)
sed -e 's/template/thrombin.generation.peak/g' all_template.Rmd \
    -e 's/TEMPLATE/Thrombin Generation - Peak (nM)/g' \
    -e 's/MEASUREMENT/Thrombin Generation (Peak)/g'  > all_thrombin_generation_peak.Rmd
sed -e 's/template/thrombin.generation.peak/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/Thrombin Generation - Peak (nM)/g' \
    -e 's/MEASUREMENT/Thrombin Generation (Peak)/g' > excl_anti_coag_thrombin_generation_peak.Rmd

## Thrombin Generation (Time To Peak)
sed -e 's/template/thrombin.generation.time.to.peak/g' all_template.Rmd \
    -e 's/TEMPLATE/Thrombin Generation - Time To Peak (min)/g' \
    -e 's/MEASUREMENT/Thrombin Generation (Time to Peak)/g' > all_thrombin_generation_time_to_peak.Rmd
sed -e 's/template/thrombin.generation.time.to.peak/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/Thrombin Generation - Time To Peak (min)/g' \
    -e 's/MEASUREMENT/Thrombin Generation (Time to Peak)/g' > excl_anti_coag_thrombin_generation_time_to_peak.Rmd

## Plasmin Antiplasmin
sed -e 's/template/plasmin.antiplasmin/g' all_template.Rmd \
    -e 's/TEMPLATE/Plasmin Antiplasmin (sec)/g' \
    -e 's/MEASUREMENT/Plasmin (Antiplasmin)/g' > all_plasmin_antiplasmin.Rmd
sed -e 's/template/plasmin.antiplasmin/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/Plasmin Antiplasmin (sec)/g' \
    -e 's/MEASUREMENT/Plasmin (Antiplasmin)/g' > excl_anti_coag_plasmin_antiplasmin.Rmd

## NPPB
sed -e 's/template/nppb/g' all_template.Rmd \
    -e 's/TEMPLATE/NPPB (pg\/ml)/g' \
    -e 's/MEASUREMENT/NPPB/g' > all_nppb.Rmd
sed -e 's/template/nppb/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/NPBB (pg\/ml)/g' \
    -e 's/MEASUREMENT/NPPB/g'  > excl_anti_coag_nppb.Rmd

## Tissure Factor
sed -e 's/template/tissue.factor/g' all_template.Rmd \
    -e 's/TEMPLATE/Tissue Factor ()/g' \
    -e 's/MEASUREMENT/Tissue Factor/g'  > all_tissue_factor.Rmd
sed -e 's/template/tissue.factor/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/Tissue Factor ()/g' \
    -e 's/MEASUREMENT/Tissue Factor/g' > excl_anti_coag_tissue_factor.Rmd

## PF 1+2
sed -e 's/template/prothrombin.fragments/g' all_template.Rmd \
    -e 's/TEMPLATE/PF 1 + 2 ()/g' \
    -e 's/MEASUREMENT/PF 1 + 2/g' > all_prothrombin_fragments.Rmd
sed -e 's/template/prothrombin.fragments/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/PF 1 + 2 ()/g' \
    -e 's/MEASUREMENT/PF 1 + 2/g' > excl_anti_coag_prothrombin_fragments.Rmd

## D-Dimer (Hospital) - Binary
sed -e 's/template/d.dimer.cat/g' all_template.Rmd \
    -e 's/TEMPLATE/D-Dimer (Hospital) : Binary/g' \
    -e 's/MEASUREMENT/D-Dimer (Hospital)/g' \
    -e 's/##### D-Dimer (Hospital) : Binary/##### All/g' > all_ddimer_cat.Rmd
sed -e 's/template/d.dimer.cat/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/D-Dimer (Hospital) : Binary/g' \
    -e 's/MEASUREMENT/D-Dimer (Hospital)/g' \
    -e 's/##### D-Dimer (Hospital) : Binary/##### Excluding those on Anticoagulants/g'> excl_anti_coag_ddimer_cat.Rmd

## D-Dimer (Hospital) - Continuous
sed -e 's/template/d.dimer/g' all_template.Rmd \
    -e 's/TEMPLATE/D-Dimer (Hospital) : Continuous/g' \
    -e 's/MEASUREMENT/D-Dimer (Hospital)/g' \
    -e 's/##### D-Dimer (Hospital) : Continuous/##### All/g' > all_ddimer.Rmd
sed -e 's/template/d.dimer/g' excl_anti_coag_template.Rmd \
    -e 's/TEMPLATE/D-Dimer (Hospital) : Continuous/g'  \
    -e 's/MEASUREMENT/D-Dimer (Hospital)/g' \
    -e 's/##### D-Dimer (Hospital) : Continuous/##### Excluding those on Anticoagulants/g' > excl_anti_coag_ddimer.Rmd
