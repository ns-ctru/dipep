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


sed 's/template/age/g' template.Rmd > age.Rmd
sed 's/template/age.cat/g' template.Rmd > age.cat.Rmd
sed 's/template/bmi/g' template.Rmd > bmi.Rmd
sed 's/template/bmi.cat/g' template.Rmd > bmi.cat.Rmd
sed 's/template/bp.diastolic/g' template.Rmd > bp.diastolic.Rmd
sed 's/template/bp.diastolic.cat/g' template.Rmd > bp.diastolic.cat.Rmd
sed 's/template/bp.systolic/g' template.Rmd > bp.systolic.Rmd
sed 's/template/bp.systolic.cat/g' template.Rmd > bp.systolic.cat.Rmd
sed 's/template/heart.rate/g' template.Rmd > heart.rate.Rmd
sed 's/template/heart.rate.cat/g' template.Rmd > heart.rate.cat.Rmd
sed 's/template/o2.saturation/g' template.Rmd > o2.saturation.Rmd
sed 's/template/o2.saturation.cat/g' template.Rmd > o2.saturation.cat.Rmd
sed 's/template/respiratory.rate/g' template.Rmd > respiratory.rate.Rmd
sed 's/template/respiratory.rate.cat/g' template.Rmd > respiratory.rate.cat.Rmd
sed 's/template/pregnancies.under/g' template.Rmd > pregnancies.under.Rmd
sed 's/template/pregnancies.over/g' template.Rmd > pregnancies.over.Rmd
sed 's/template/prev.preg.problem/g' template.Rmd > prev.preg.problem.Rmd
sed 's/template/this.pregnancy.problems/g' template.Rmd > this.pregnancy.problems.Rmd
sed 's/template/multiple.preg/g' template.Rmd > multiple.preg.Rmd
sed 's/template/history.thrombosis/g' template.Rmd > history.thrombosis.Rmd
sed 's/template/history.veins/g' template.Rmd > history.veins.Rmd
sed 's/template/history.thrombosis/g' template.Rmd > history.thrombosis.Rmd
sed 's/template/history.iv.drug/g' template.Rmd > history.iv.drug.Rmd
sed 's/template/thrombosis/g' template.Rmd > thrombosis.Rmd
sed 's/template/thrombo/g' template.Rmd > thrombo.Rmd
sed 's/template/trimester/g' template.Rmd > trimester.Rmd
