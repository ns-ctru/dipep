#!/bin/bash
## Clean the biomarker file of unnecessary headers and then
## clean non-numeric values and correct corrupted identifiers
grep -v 'citrated plasma samples' biomarker_20170412.csv | \
    grep -v 'ETP Measured values' | \
    grep -v 'mg/l' | \
    grep -v 'Reference range' |
    sed -e 's/no plasma//g' \
	-e 's/ce//g' \
	-e 's/s/S/g' \
	-e 's/SO/S0/g' \
	-e 's/nc//g' \
	-e 's/NC//g' \
	-e 's/<//g' \
	-e 's/S\//S/g' \
	-e 's/D\//D/g' \
	-e 's/    (HIV +)//g' \
	-e 's/s\//S/g' > biomarker_clean.csv
