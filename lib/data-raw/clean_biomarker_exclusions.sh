#!/bin/bash
## Clean the biomarker file of unnecessary headers and then
## clean non-numeric values and correct corrupted identifiers
grep -v ';;;;;;;' biomarker_anticoag_exclusions_20170207.csv  | \
    sed	-e 's/S\//S/g' \
	-e 's/s\//S/g' \
	-e 's/D\//D/g' > biomarker_anticoag_exclusions_clean.csv
