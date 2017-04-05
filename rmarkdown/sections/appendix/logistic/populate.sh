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
sed 's/template/presenting.features.pleuritic/g' template.Rmd > presenting.features.pleuritic.Rmd
sed 's/template/presenting.features.non.pleuritic/g' template.Rmd > presenting.features.non.pleuritic.Rmd
sed 's/template/presenting.features.sob.exertion/g' template.Rmd > presenting.features.sob.exertion.Rmd
sed 's/template/presenting.features.sob.rest/g' template.Rmd > presenting.features.sob.rest.Rmd
sed 's/template/presenting.features.haemoptysis/g' template.Rmd > presenting.features.haemoptysis.Rmd
sed 's/template/presenting.features.cough/g' template.Rmd > presenting.features.cough.Rmd
sed 's/template/presenting.features.syncope/g' template.Rmd > presenting.features.syncope.Rmd
sed 's/template/presenting.features.palpitations/g' template.Rmd > presenting.features.palpitations.Rmd
sed 's/template/presenting.features.other/g' template.Rmd > presenting.features.other.Rmd
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
sed 's/template/thromboprophylaxis/g' template.Rmd > thromboprophylaxis.Rmd
sed 's/template/trimester/g' template.Rmd > trimester.Rmd
sed 's/template/injury/g' template.Rmd > injury.Rmd
sed 's/template/surgery/g' template.Rmd > surgery.Rmd
sed 's/template/thrombo/g' template.Rmd > thrombo.Rmd
sed 's/template/medical.complication/g' template.Rmd > medical.complication.Rmd
sed 's/template/obstetric.complication/g' template.Rmd > obstetric.complication.Rmd
sed 's/template/xray.pe/g' template.Rmd > xray.pe.Rmd
sed 's/template/ecg.pe/g' template.Rmd > ecg.pe.Rmd
sed 's/template/dvt.cat/g' template.Rmd > ecg.pe.Rmd
sed -e 's/template/simplified.lower.limb.unilateral.pain/g' template.Rmd \
    -e 's/logistic/existing/g' > simplified.lower.limb.unilateral.pain.Rmd
sed -e 's/template/simplified.pain.palpitations/g' template.Rmd \
    -e 's/logistic/existing/g' > simplified.pain.palpitations.Rmd
sed -e 's/template/wells.alternative.permissive/g' template.Rmd \
    -e 's/logistic/existing/g' > wells.alternative.permissive.Rmd
sed -e 's/template/wells.alternative.strict/g' template.Rmd \
    -e 's/logistic/existing/g' > wells.alternative.strict.Rmd
sed -e 's/template/perc.hormone/g' template.Rmd \
    -e 's/logistic/existing/g' > perc.hormone.Rmd
sed -e 's/template/perc.leg.swelling/g' template.Rmd \
    -e 's/logistic/existing/g' > perc.leg.swelling.Rmd
sed -e 's/template/delphi.medical.history.surgery/g' template.Rmd \
    -e 's/logistic/existing/g' > delphi.medical.history.surgery.Rmd
sed -e 's/template/delphi.medical.history/g' template.Rmd \
    -e 's/logistic/existing/g' > delphi.medical.history.Rmd
sed -e 's/template/delphi.medical.complication/g' template.Rmd \
    -e 's/logistic/existing/g' > delphi.medical.complication.Rmd
sed -e 's/template/delphi.obstetric.complication/g' template.Rmd \
    -e 's/logistic/existing/g' > delphi.obstetric.complication.Rmd
sed -e 's/template/delphi.gestation/g' template.Rmd \
    -e 's/logistic/existing/g' > delphi.gestation.Rmd
