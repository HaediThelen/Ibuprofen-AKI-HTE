# Analysis Scripts

These scripts reproduce the analyses presented in the manuscript. Run the scripts in the order described below.

## 1. Download the Data

Download the study data from **[Zenodo link]** and place the downloaded files in the repository's `data/` folder.

## 2. Run the Analysis Scripts

Each analysis consists of two steps:

1. **R script:** Prepares the analysis dataset, evaluates covariate balance, estimates balancing weights, and saves the weighted analysis dataset to the `data/` folder.
2. **Stata do-file:** Reads the R-generated analysis dataset, fits the Poisson regression models, and saves the resulting estimates.

For each analysis, run the R script first, followed by the corresponding Stata do-file.

### Overall Ibuprofen–AKI Association

* `ibu-aki-overall.R`
* `ibu-aki-overall.do`

### Subgroup Analyses

#### Kidney Function

* `ibu-aki-gfr.R`
* `ibu-aki-gfr.do`

#### Age

* `ibu-aki-age.R`
* `ibu-aki-age.do`

#### Body Mass Index

* `ibu-aki-bmi.R`
* `ibu-aki-bmi.do`

#### Heart Failure

* `ibu-aki-hf.R`
* `ibu-aki-hf.do`

#### Diabetes

* `ibu-aki-dm.R`
* `ibu-aki-dm.do`

#### Current ICU Status

* `ibu-aki-icuCurrent.R`
* `ibu-aki-icuCurrent.do`

#### Perioperative Status

* `ibu-aki-periop.R`
* `ibu-aki-periop.do`

#### Nephrotoxic Medication Use

* `ibu-aki-anyNtx.R`
* `ibu-aki-anyNtx.do`

### Additional Analyses

#### Low-Risk Subset 

* `ibu-aki-low-risk-subset.R`
* `ibu-aki-low-risk-subset.do`

#### Dose–Response Analysis

* `ibu-aki-dose_response.R`
* `ibu-aki-dose-response.do`

## 3. Visualizing and Presenting Results

These scripts generate figures and tables from the analysis results.

### Continuous Covariate Effects

`composite.plots.R`

Generates plots of estimated effects across continuous covariates, including eGFR, age, and BMI.

### Forest Plots

`ibu-aki-forest-plot-figs.R`

Generates forest plots displaying the study results.

### Heterogeneity of Treatment Effects Summary

`HTE-Summary.R`

Generates the summary table of heterogeneity of treatment effects (HTE).


