# Optimal Tradeoffs in Matched Designs Comparing US-Trained and Internationally Trained Surgeons

# Author Contributions Checklist Form

## Data

### Abstract

The data used in this project consists of hospital discharge claims for general surgery patients in the states of New York and Florida from 2008-2011, linked to a physician masterfile containing information about the attending surgeon for each patient claim.  The claims data were provided by the State of Florida Agency for Health Care Administration and the New York State Department of Health, and the physician masterfile was provided by the American Medical Association.  Variables contained in the linked dataset include unique IDs for hospitals where care was given, patient demographic information and health history information, ICD9 codes describing the surgical procedures performed, outcome variables include in-hospital mortality, and surgeon information including descriptors of medical education.  



### Availability 

The actual data used for the analysis in the manuscript will not be made publicly available. Our use of the data for the research described in the manuscript is strictly governed by data use agreements with the states of Florida and New York.  The data use agreement with Florida stipulates that the authors �shall not release or allow the release of the limited data set specified in this agreement to any persons or entities other than as permitted by the agreement,� limiting distribution of the data to those specifically named elsewhere in the document; the New York DUA contains similar restrictions.  In place of the actual data used for the analysis, we have created a simulated data file so that it is possible for readers of the manuscript to run the code for the data analysis. 


### Description 

The simulated data we include with this submission is generated by drawing pseudo-random vectors in R from distributions chosen to closely recover the summary statistics of the original data given in Table C1 of the online supplement.  We include the file simulate_data.R which can be used to replicate the creation of the simulated data (by running the command �source(simulate_data.R)� in R from the working directory optimal_tradeoffs_code/data) .  The data is included with this submission in the file my_slice.Rdata, which is an RData-format file containing a single data frame object named �my.slice� of 78,000 rows and 15 columns.  The data dictionary below provides information about the data field for each of the 15 columns.  All code, including the file used to generate the simulated data, is licensed under the MIT open source license.

hospid: factor variable with 498 levels, meant to correspond to unique hospital ID in the original data.
proc_type: factor variable with 32 levels, meant to correspond to surgical procedure performed in the original data.
comorb: numeric variable taking on whole number values from 0 to 14, meant to correspond to Elixhauser comorbidity index in the original data.
emergency: binary variable meant to correspond to an indicator of whether patient was admitted via the emergency room in the original data.
mf: binary variable meant to correspond to patient sex (equal to one if male) in the original data.
age: numeric variable taking on whole number values from 24 to 88, meant to correspond to patient age in the original data.
exp: numeric variable taking on whole number values from 0 to 48, meant to correspond to surgeon years of experience in the original data.  1,608 entries are missing values.
died: binary variable meant to correspond to an indicator of patient death within 30 days of surgery in the original data.
img: binary variable meant to correspond to an indicator for surgeons who are international medical graduates (if equal to one), as opposed to US medical graduates, in the original data.
med_income: numeric variable taking on whole number multiple of 1,000 from 24,000 to 177,000, meant to correspond to median income of patient ZIP code in the original data.  15,460 entries are missing values.
race_cat: factor variable of three levels, meant to correspond to race categories in the original data.
comorb_cat: factor variable of 5 levels labeled, 0, 1, 2, and 3+, a coarsened version of the comorb variable.
exp.quint: factor variable of 6 levels, containing labels for membership in quintiles of variable exp with a separate category for missing values.
age.quint: factor variable of 5 levels, containing labels for membership in quintiles of variable age.
exp.dec: factor variable of 10 levels, containing labels for membership in deciles of variable exp.  Missing values from exp are retained as missing values for exp.dec. 


## Code

### Abstract

The code consists of two parts, data assembly (conducted in SAS, Stata, and R) and data analysis (conducted in R).  

Data Assembly. The data assembly is done by six scripts.  The first 2 SAS scripts, PullGenSurgFL.sas and PullGenSurgNY.sas, extract the general surgery hospital discharge claims from raw data files provided by Florida and New York respectively.  The next 2 Stata scripts, CleanGSFL.do and CleanGSNY.do, clean this raw data into a more usable format.  The Stata script PullUSMGvsIMGData.do combines the cleaned claims data from both states and cleans and links in surgeon covariates from the AMA masterfile.  Finally, the R file selectOrlando.R converts data fields into R formats, adds geographic labels, creates several new variables, and selects the subset of operations conducted in the Orlando metropolitan area.   To add the geographic labels, two auxiliary files, NYfips.csv and FLfips.csv, are loaded; each contains two columns, the first giving FIPS codes and the second county names, for the appropriate states. The final output is a single RData file called my_slice.Rdata, which is used as the input to the data analysis.

Data Analysis. The data analysis is run using a single master R script, main_script_v2.R.  
This script is accompanied by five additional R files which define various types of helper functions used in the analysis (and are loaded as part of the main script): descr_stats.R, attributable_effects_v3.R, obj_to_match.R, my_dummy.R, and tradeoff_functions_v5.R.  For input, the master script relies only on the my_slice.Rdata file created in the data assembly stage. 


### Description

The code and simulated data will be hosted on the open science repository Zenodo in the event that the manuscript is accepted.



## Instructions for Use

### Reproducibility 

The main script reproduces Tables 3 and 4 and Figure 2 from the manuscript.  In addition, the key numerical results of Section 5.2 are reproduced, specifically the mortality rates in the treatment and control groups, the p-value for the equivalence test, and the sensitivity analysis p-values associated with Gamma-values just below and above the 0.05 threshold.  Note that since the dataset provided in the file my_slice.Rdata is simulated from summary statistics alone, the output produced by running the main analysis script with this file does not agree exactly with that in the main manuscript, although it is qualitatively very similar.  
