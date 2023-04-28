# Code to reproduce "The development of canonical proportion continues past toddlerhood" (ICPhS2023 paper)

## Information on how to reproduce our analyses

To run the analyses from our ICPhS 2023 paper, you will need [RStudio](https://posit.co/download/rstudio-desktop/) ([more information here](https://libscie.github.io/rmarkdown-workshop/handout.html)). To run the analyses, open scripts/analysis.Rmd in RStudio (make sure the scripts/ directory is your working directory) and click the "knit" button near the top of the RStudio window. This reads in data/combined-data-for-analysis.csv and outputs the statistical analyses and plots we report on in the paper. 

Note: If you want to reproduce the full pipeline, contact Kasia Hitczenko, as you'll need access to the following OSF directory (https://osf.io/tegsx/) and relevant gin datasets (phonSES, png2019, solomon, tsimane2017). Place the files from OSF in the data/ directory, then run: (1) get-child-gender-zooniverse.R, (2-5) get-babblecorpus-cps.R, get-solomon-data.R, get-semenzin-yeli-data.R, get-zooniverse-cps.R, (6) analysis.Rmd.


## Description of other contents 

/data

output data:
* combined-data-for-analysis.csv: full data used as input to our analyses. This is created by analysis.R from the following four sources of data:
* babblecorpus_cp.csv: data for children from babblecorpus; output from get-babblecorpus-cps.R script
* solomon_cp.csv: data for hand-coded solomon children; output from get-solomon-data.R script
* yeli-semenzin_cp.csv: data for yeli children that come from work by semenzin; output from get-semenzin-yeli-data.R
* zooniverse_cp.csv: data for children from zooniverse (first run); output from get-zooniverse-cps.R
* child-gender-zooniverse.csv: gender information for all children on Zooniverse; output from get-child-gender-zooniverse.R

raw data needed to reproduce the full pipeline start-to-finish (available in https://osf.io/tegsx/; request access by emailing Kasia Hitczenko):
* LAAC_Internship2020_Languages.xlsx - Usable as csv.csv: Gives syllable complexity of each language
* uniqchild.csv: gives information + canonical proportion for each child in babblecorpus
* demo_data.tsv: information about English-Seidl children age/gender
* chunks_maj_judgments_all_sources.csv: Contains Zooniverse classifications for calculating CP
* bsl_metadata_tsi_full.csv: Additional metadata for Tsimane' children in the sample
* CR_by_child-updated_21_01.csv: Canonical proportions from Semenzin work


/scripts (Run in this order to reproduce the full pipeline)

1. get-child-gender-zooniverse.R
2. get-babblecorpus-cps.R
3. get-solomon-data.R
4. get-semenzin-yeli-data.R
5. get-zooniverse-cps.R
6. analysis.R
