## Filename     run_all.sh
## Author       n.shephard@sheffield.ac.uk
## Created      2017-04-27
##
## Description  Runs all analyses (and some more) for DiPEP

## Generate Secondary/Tertiary/Quaternary file structure using the following
## Bash script (comments within on how it works, but note it calls nested
## Bash scripts (also called populate.sh) to generate files too).
system('./populate.sh')
## Run Primary analyses twice, once to produce HTML Report...
render('dipep.Rmd')
## ...and once to produce a PDF for those who have archaic computers that
## can not open/render the HTML version
render('dipep_pdf.Rmd')
## Run Secondary analyses
setwd('secondary')
render('secondary.Rmd')
setwd('../')
## Run Tertiary analyses
setwd('tertiary')
## NB For some reason the Recursive partitioning of continuous variables does not run
##    from the markdown.  You have to manually run the chunk...
##
##    results_rpart_continuous
##
##    ...in the file...
##
##        ../rmarkdown/tertiary/section/subsection/rpart/continuous.Rmd
##
##    ..and then run the last section.
render('tertiary.Rmd')
setwd('../')
## Run Quaternary analyses
setwd('quaternary')
render('quaternary.Rmd')
setwd('../')
