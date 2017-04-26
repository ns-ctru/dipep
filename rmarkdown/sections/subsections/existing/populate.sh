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

## Simplified Revised Geneva
sed -e 's/SCORE/Revised Geneva/g' template.Rmd \
    -e 's/score/simplified/g' \
    -e 's/CHUNK/simplified_revised_geneva/g' \
    -e 's/RESULTS/simplified/g' > foot_simplified.Rmd
cat head_simplified.Rmd foot_simplified.Rmd > simplified.Rmd

## Wells Permissive
sed -e 's/SCORE/Wells Score (Permissive)/g' template.Rmd \
    -e 's/score/wells.permissive/g' \
    -e 's/CHUNK/wells_permissive/g' \
    -e 's/RESULTS/wells.permissive/g' > foot_wells_permissive.Rmd
cat head_wells_permissive.Rmd foot_wells_permissive.Rmd > wells_permissive.Rmd

## Wells Strict
sed -e 's/SCORE/Wells Score (Strict)/g' template.Rmd \
    -e 's/score/wells.strict/g' \
    -e 's/CHUNK/wells_strict/g' \
    -e 's/RESULTS/wells.strict/g' > foot_wells_strict.Rmd
cat head_wells_strict.Rmd foot_wells_strict.Rmd > wells_strict.Rmd

## PERC
sed -e 's/SCORE/PERC Score/g' template.Rmd \
    -e 's/score/perc/g' \
    -e 's/CHUNK/perc/g' \
    -e 's/RESULTS/perc/g' > foot_perc.Rmd
cat head_perc.Rmd foot_perc.Rmd > perc.Rmd

## Delphi Primary
sed -e 's/SCORE/Delphi Primary Score/g' template.Rmd \
    -e 's/score/delphi.primary/g' \
    -e 's/CHUNK/delphi.primary/g' \
    -e 's/RESULTS/delphi.primary/g' > foot_delphi_primary.Rmd
cat head_delphi_primary.Rmd foot_delphi_primary.Rmd > delphi_primary.Rmd

## Delphi Sensitivity
sed -e 's/SCORE/Delphi Sensitivity Score/g' template.Rmd \
    -e 's/score/delphi.sensitivity/g' \
    -e 's/CHUNK/delphi.sensitivity/g' \
    -e 's/RESULTS/delphi.sensitivity/g' > foot_delphi_sensitivity.Rmd
cat head_delphi_sensitivity.Rmd foot_delphi_sensitivity.Rmd > delphi_sensitivity.Rmd

## Delphi Specificity
sed -e 's/SCORE/Delphi Specificity Score/g' template.Rmd \
    -e 's/score/delphi.specificity/g' \
    -e 's/CHUNK/delphi.specificity/g' \
    -e 's/RESULTS/delphi.specificity/g' > foot_delphi_specificity.Rmd
cat head_delphi_specificity.Rmd foot_delphi_specificity.Rmd > delphi_specificity.Rmd
