library(foreign)
library(balancer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sandwich)
library(splines)
library(haven)
library(Hmisc)
library(tableone)
library(survey)
library(tibble)
library(janitor)
library(cobalt)

# Ibuprofen effect on AKI - Overall

# Step 0: Prep
  # Load in data
  data <- read_dta("./data/ibu-aki-data.dta") 
 
  # Clean data and and prep for balanceR 
  data <- data %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%
    mutate(across(where(~ all(. %in% c(0, 1))), as.integer)) %>%
    mutate(race.white = if_else(race == 0, 1, 0),  #make dummy variables
           race.black = if_else(race == 1, 1, 0),
           race.other = if_else(race == 2, 1, 0)) %>%
    mutate(center.hup = if_else(center == 1, 1, 0),
           center.presb = if_else(center == 2, 1, 0),
           center.pa = if_else(center == 3, 1, 0)) %>%
    mutate(presentation.ed = if_else(presentation == 1, 1, 0),
           presentation.icu = if_else(presentation == 2, 1, 0),
           presentation.or = if_else(presentation == 3, 1, 0),
           presentation.floor = if_else(presentation == 4,1,0),
           presentation.other = if_else(presentation == 5,1,0)) %>%
    mutate(periOp.no = if_else(periOp == 0, 1, 0),
           periOp.0 = if_else(periOp == 1, 1, 0),
           periOp.1 = if_else(periOp == 2, 1, 0),
           periOp.2 = if_else(periOp == 3,1,0),
           periOp.3 = if_else(periOp == 4,1,0)) %>%
    mutate(dm.no = if_else(dm == 0, 1, 0),
           dm.noncomp = if_else(dm == 1, 1, 0),
           dm.comp = if_else(dm == 2, 1, 0)) %>%
    mutate(cancer.no = if_else(cancer == 0, 1, 0),
           cancer.noncomp = if_else(cancer == 1, 1, 0),
           cancer.metastatic = if_else(cancer == 2, 1, 0)) %>%
    mutate(sup.no = if_else(sup == 0, 1, 0),
           sup.h2ra= if_else(sup == 1, 1, 0),
           sup.ppi = if_else(sup == 2, 1, 0))
  
  # Make a spline for age
  age.sp <- ns(data$age, df=3, intercept=FALSE) # fit natural cubic spline for age
  nos <- seq(1:ncol(age.sp)) # number of cols in age.sp
  
  colnames(age.sp) <- paste("age", nos, sep="") # rename the columns
  data <- cbind(data, age.sp) # add the spline columns to the data
  
  # Make a spline for indexGFR
  indexGFR.sp <- ns(data$indexGFR, df=4, intercept=FALSE) 
  nos <- seq(1:ncol(indexGFR.sp)) 
  
  colnames(indexGFR.sp) <- paste("indexGFR", nos, sep="") 
  data <- cbind(data, indexGFR.sp) # add the spline columns to the data
  
# 1. Choose Estimand
  # For primary analysis it will be ATT but ATE will be calculated

# 2. Check for positivity/overlap
  # Choose all covs to evaluate SMDs
  covs <- c("age", "sex",  "race.black", "race.white", "race.other",  "admType", 
            "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
            "presentation.or", "presentation.floor", "presentation.other", "priorLos", "icuCurrent",
            "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
            "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
            "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
            "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
            "hiv", "indexGFR", "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
            "labkBase", "rasBase", "metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
            "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
            "vancoBase", "bactrimBase",
            "abxNTX", "ntxOther", "pressBase", "bmi")
  
  # Table 1 for raw data
  source("./functions/table1.R")
  tab.1.raw <- table.1(data, wts = NULL, covs = covs) # one equal probability warning is expected
    
  # Median duration of Follow up time 
    data %>% group_by(ibu) %>%
      summarise(median_follow_up_time = median(pTime))
    
  # AKI By Stage
    # count of AKI by study drug
    data %>%
      filter(kEver == 1) %>%
      mutate(pain = factor(pain, levels = c(0, 1), labels = c("Oxycodone", "Ibuprofen"))) %>%
      tabyl(kStage, pain) %>%  # counts kStage x pain
      adorn_totals("row")      # adds total row
    
    # Percent of AKI by Study Drug
    data %>%
      filter(kEver == 1) %>%
      mutate(pain = factor(pain, levels = c(0, 1),
                           labels = c("Oxycodone", "Ibuprofen"))) %>%
      tabyl(kStage, pain) %>%
      adorn_totals("row") %>%                       
      adorn_percentages(denominator = "col") %>%
      adorn_pct_formatting(digits = 1)
    
  
  # Compare Density Curves
    source("./functions/overlap-eval.R")
   # Choose all covs to evaluate density functions
    continuous.covs <- c("age", "indexGFR", "bmi", "wbcBase", "hgbBase", "platBase",
                         "labclBase", "labkBase")
  
  # Ensure treatment variable is a factor 
    data.factor <- data %>% mutate(pain = as.factor(pain))
  
  # Plot density curves 
    density.plots <- dplots(data.factor, continuous.covs, "pain")
  
  # Compare Number Summaries of continuous covariates 
    tab <- sum.tabs(data.factor, continuous.covs, 'pain')
    tab
    
# Step 3: Estimate the balance weights
    # Make list of covariates for the model
    covs.bal <- c(colnames(age.sp), "sex", "race.white", "race.black", "race.other", "admType", 
                  "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
                  "presentation.or", "presentation.floor", "presentation.other", "priorLos", "icuCurrent",
                  "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
                  "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
                  "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
                  "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
                  "hiv", colnames(indexGFR.sp), "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
                  "labkBase", "rasBase", "metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
                  "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
                  "vancoBase", "bactrimBase",
                  "abxNTX", "ntxOther", "pressBase", "bmi", "-1")
    
 # More Prep for BalanceR
      basis <- reformulate(covs.bal) # prepare a formula object                       
      X <- scale(model.matrix(as.formula(basis), data)) # prepare a scaled matrix 
      # scaling is needed to calculate the weights, since they target a mean of 0
      trt <- data$pain
      n <- nrow(data)
      
      data.ctrl <- data %>% filter(pain==0)
      lambda.reg <- lm(reformulate(covs.bal, response = "kEver"), data=data.ctrl)
      var(lambda.reg$resid)
      
  # Balance Weights ATT
      # Calculate
        out.pain <- multilevel_qp(X, trt, rep(1,n), lambda = 0.05, verbose= TRUE, 
                              exact_global = TRUE, scale_sample_size = FALSE)
      # Process 
        data$ATTwts <- pmax(out.pain$weights, 0) 
        summary(data$ATTwts)
        data$ATTwts[data$pain == 1] <- 1
        summary(data$ATTwts)
        sd(data$ATTwts)

  # Balance Weights ATE
        # Calculate 
         out.pain.q <- multilevel_ate_qp(X, trt, rep(1,n), lambda = 0.05, lowlim = 0, uplim = 1,  
                                        verbose= TRUE, exact_global = TRUE, scale_sample_size = FALSE)
        # Process
         data$ATEwts <- out.pain.q$weights
         summary(data$ATEwts)
         sd(data$ATEwts)

# Step 4: Assess Balance, weights, and ESS 
  # Table 1 for ATT
    source("./functions/table1.R")
     # Table 1 for ATT
     tab.1.ATT <- table.1(data, "ATTwts", covs) # one equal probability warning is expected

  # SMD Plots and Total Bias Reduction 
     detach(package:Hmisc, unload=TRUE)
     source("./functions/balance-plots.R")
    # ATT (Balwts)
     bal.plots.ATT_clean <- bal.plot.clean(data = data, weights = "ATTwts",  
                                           treatment = "pain", covs = covs, subset = FALSE, main.title = "Balance of Baseline Covariates")
     if (!dir.exists("./results/overall/balplots")) {
       dir.create("./results/overall/balplots", recursive = TRUE)
     }
     ggsave(filename = "./results/overall/balplots/Overall-Balance.jpeg", device = "jpeg", 
            width = 7, height = 9, units = "in", dpi = 300)
     ggsave(filename = "./results/overall/balplots/Overall-Balance.pdf", device = "pdf", 
            width = 7, height = 9, units = "in", dpi = 300)     
   
  # Examine Balance in continuous covariate distributions
     # ATT
       source("./functions/density-plot.R")
       dplot("age", data, 'ATTwts')
       dplot("indexGFR", data,'ATTwts')
       dplot("bmi", data, 'ATTwts')
    
  # ESS
    # Effective Sample Size ATT
        source("./functions/ess-function.R")
        ess(data, "pain", "ATTwts")

     # Effective Sample Size ATE
        ess(data, "pain", "ATEwts")
        
# Step 5: Export to Stata for analysis 
        # write to data folder
        write.dta(data,"./data/ibu-aki-overall.dta") 

# Step 6: Supplemental Analysis: Serum Creatinine over follow-up
    # Make a table showing median and IQR range of serum creatinine measurements per day over followup, stratified by treatment group
  write_clip(data %>%  group_by(pain) %>%
    summarise(
      n = n(),
      creatinine = paste0(
        round(median(creMon, na.rm = TRUE), 1), " (",
        round(quantile(creMon, 0.25, na.rm = TRUE), 2), "-",
        round(quantile(creMon, 0.75, na.rm = TRUE), 2), ")")))

# Step 7: Evaluate duration of follow up by treatment group with extended 5 day follow up window
   data %>%
    group_by(pain) %>%
    summarise(n = n(), 
             med_iqr = paste0(
                round(median(p5Time, na.rm = TRUE), 1), " (",
                round(quantile(p5Time, 0.25, na.rm = TRUE), 2), "-",
                round(quantile(p5Time, 0.75, na.rm = TRUE), 2), ")"),
              k5Ever = sum(kEver5, na.rm=TRUE))
  # Crude any AKI rate
  data %>%
    group_by(pain) %>%
    summarise(
      kEver5 = sum(kEver5, na.rm = TRUE),
      person_time_1000 = sum(p5Time1000, na.rm = TRUE),
      rate_1000 = kEver5 / person_time_1000,
      n = n())
  
  # Crude Severe AKI rate
  data %>%
    group_by(pain) %>%
    mutate(kEver523 = ifelse(kStage5 ==2 | kStage5 ==3, 1, 0)) %>%
    summarise(
      kEver523 = sum(kEver523, na.rm = TRUE),
      person_time_1000 = sum(p5Time1000, na.rm = TRUE),
      rate_1000 = kEver523 / person_time_1000,
      n = n())
  
  # Crude KRT rate  
  data %>%
    group_by(pain) %>%
    summarise(
      rrt5 = sum(rrt5, na.rm = TRUE),
      person_time_1000 = sum(p5Time1000, na.rm = TRUE),
      rate_1000 = rrt5 / person_time_1000,
      n = n())
  # Remainder of analysis uses ibu-aki-overall.dta file in the ibu-aki-overall.do files in Stata
  
  # Step 8. Sensitivity Analysis -  Opioid exposure in baseline period
  # Include a measure of opioid exposure during the baseline period in the
  # balance weight model to adjust for potential confounding by indication.
  # 8a. Make summary table of IV opioid exposure in baseline period by pain status 
  cat.vars <- c("opBase", "h_Base", "m_Base", "f_Base", "opBasePO", "opBasePatch", "opBaseIV",
                 "opBasePCA", "opBaseGTT")
  cont.vars <- c("omeBaseTotal")
  
  source("./functions/baseOp-summary.R")
  data.baseline.opioid.tab.overall <- make_baseline_opioid_table(data, cat.vars, cont.vars, omeCat = "omeCat", ome.var = "omeBaseTotal", group.var = "pain")
  data.baseline.opioid.tab.overall

  # Step 8b: Estimate the balance weights
  # Estimate balancing weights in subgroups, including omeCat
  # omeCat is a categorical variable with 0 =  were omeBaseTotal was 0 indicating no opioid
  # 1 =  1-25th percentile of non-zero omeBaseTotal
  # 2 = 25-50th percentile of non-zero omeBaseTotal 
  # 3 = 50-75th percentile of non-zero omeBaseTotal
  # 4 = 75-100th percentile ofnon-zero omeBaseTotal
  
  # Make dummy variable of omeCat
  data <- data %>% 
    mutate(omeCat.0 = if_else(omeCat == 0, 1, 0),  #make dummy variables
           omeCat.1 = if_else(omeCat == 1, 1, 0),
           omeCat.2 = if_else(omeCat == 2, 1, 0),
           omeCat.3 = if_else(omeCat == 3, 1, 0),
           omeCat.4 = if_else(omeCat == 4, 1, 0))
  
  covs.bal.omeCat <- c(colnames(age.sp), "sex", "race.white", "race.black", "race.other", "admType", 
                "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
                "presentation.or", "presentation.floor", "presentation.other", "priorLos", "icuCurrent",
                "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
                "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
                "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
                "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
                "hiv", colnames(indexGFR.sp), "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
                "labkBase", "rasBase", "metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
                "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
                "vancoBase", "bactrimBase",
                "abxNTX", "ntxOther", "pressBase", "bmi","omeCat.0","omeCat.1",
                "omeCat.2", "omeCat.3", "omeCat.4", "-1")
  
  # More Prep for BalanceR
  basis <- reformulate(covs.bal.omeCat) # prepare a formula object                       
  X <- scale(model.matrix(as.formula(basis), data)) # prepare a scaled matrix 
  # scaling is needed to calculate the weights, since they target a mean of 0
  trt <- data$pain
  n <- nrow(data)
  
  # Balance Weights ATT
  out.pain <- multilevel_qp(X, trt, rep(1,n), lambda = 0.05, verbose= TRUE, 
                            exact_global = TRUE, scale_sample_size = FALSE)
  # Process 
  data$ATTwts.omeCat <- pmax(out.pain$weights, 0) 
  summary(data$ATTwts.omeCat)
  data$ATTwts.omeCat[data$pain == 1] <- 1
  summary(data$ATTwts.omeCat)
  sd(data$ATTwts.omeCat)

  # evaluate balance:
  source("./functions/table1.R")
  tab.1.ATT.omeCat <- table.1.omeCat(data, "ATTwts.omeCat", c("omeCat.0","omeCat.1",
                                              "omeCat.2", "omeCat.3", "omeCat.4")) # one equal probability warning is expected

  covs.omeCat <- c("age", "sex", "race.white", "race.black", "race.other", "admType", 
                  "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
                  "presentation.or", "presentation.floor", "presentation.other", "priorLos", "icuCurrent",
                  "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
                  "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
                  "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
                  "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
                  "hiv", "indexGFR", "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
                  "labkBase", "rasBase", "metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
                  "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
                  "vancoBase", "bactrimBase",
                  "abxNTX", "ntxOther", "pressBase", "bmi", "omeCat.0","omeCat.1",
                  "omeCat.2", "omeCat.3", "omeCat.4")
  
  long.names <- c("Age", "Sex", "Race - White", "Race -Black", "Race - Other", "Admisison Type", 
                  "Center - HUP", "Center - Presbyterian", "Center - Pennsylvania Hospital", "Presentation - ED", "Presentation ICU",
                  "Presentation - OR", "Presentation - Floor", "Presentation - Other", "Prior LOS", "ICU Status",
                  "Post Op", "POD 0", "POD 1", "POD 2", "POD3", "Ventilator Status", 
                  "Prior Ventilator", "Heart Failure", "Myocardial Infarction", "Arrhythmia", "Atrial Fibrillation", "Valvular Diseas", 
                  "Stroke", "Peripheral Vascular Disease", "Pulmonary Circulation Disorder", "Chronic Pulmonary Disease", "Liver Disease", 
                  "Diabetes Mellitus - None", "Diabetes Mellitus - Non-complicated", "Diabetes Mellitus - Complicated", "Chronic Kidney Disease", 
                  "Weight Loss", "Fluid and Electrolyte Disorder", "Cancer - None", "Cancer - Non-complicated", "Cancer - Metastatic",
                  "HIV", "eGFR", "Prior AKI", "WBC, x10^8 cells/dL", "Hemoglobin, g/dL", "Platelets, x10^11 cells/L", "Chloride, mEq/L",
                  "Potassium, mEq/L","RAS Inhibitor" ,"Metoprolol", "Combined Alpha and Beta Blocker", "Hydrochlorothiazide", "Hydralazine", "Loop Diuretics",
                  "Other Antihypertensives", "Acid Suppressants - None","Acid Suppressants - H2RA", "Acid Suppressants - PPI", "Broad Spectrum Antibiotics", "Narrow Spectrum Antibiotics", 
                  "Vancomycin", "Bactrim",
                  "Other Nephrotoxic Antibiotics", "Other Nephrotoxins", "Vasopressors", "BMI",
                  "Baseline Opioid - None","Baseline Opioid - Q1 OME", "Baseline Opioid - Q2 OME", "Baseline Opioid - Q3 OME", "Baseline Opioid - Q4 OME")
  
  detach(package:Hmisc, unload=TRUE)
  bal.plots.ATT_clean <- bal.plot.clean(data = data, weights = "ATTwts.omeCat",  
                                        treatment = "pain", covs = covs.omeCat, 
                                        subset = FALSE, main.title = "Balance of Baseline Covariates",
                                        long.names = long.names)

  ggsave(filename = "./results/Overall/balplots/Overall-BaseOp-Balance.jpeg", device = "jpeg", 
         width = 7, height = 9, units = "in", dpi = 300)
  ggsave(filename = "./results/Overall/balplots/Overall-BaseOp-Balance.pdf", device = "pdf", 
         width = 7, height = 9, units = "in", dpi = 300)     
  
  write.dta(data, "./data/ibu-aki-overall-opBase.dta")

# Step 9: Supplemental Analyses: Excluding patietns with Non-PO Opioid exposure in the baseline period
# Summary of patietns with opBaseIV, opBasePCA, and opBaseGTT
data <- data %>%
  mutate(icuCurrent.cat = as.factor(icuCurrent)) %>%
  mutate(periOp.bin = if_else(periOp ==0, 0, 1)) %>%
  mutate(periOp.bin = as.factor(periOp.bin))
  
  n_start <- nrow(data)
  n_excluded <- sum(!(data$opBaseIV == 0 & data$opBasePCA == 0 & data$opBaseGTT == 0 & data$opBasePatch == 0))
  n_remaining <- n_start - n_excluded
  data.opBasePOonly <- data %>% filter(opBaseIV == 0 & opBasePCA == 0 & opBaseGTT == 0 & opBasePatch == 0)
  
  nonPO.exclusions <- tibble(
    Step = c(
      "Total patients",
      "Excluded: any baseline non-PO opioid route (IV, PCA, Drip, Patch)",
      "Remaining",
      "  Oxycodone",
      "    Critically Ill",
      "    Postoperative ",
      "  Ibuprofen",
      "    Critically Ill",
      "    Postoperative"),
    N = c(n_start, n_excluded, n_remaining,
          sum(data.opBasePOonly$pain == 0),
          sum(data.opBasePOonly$pain == 1),
          sum(data.opBasePOonly$pain == 0 & data.opBasePOonly$icuCurrent.cat == 1),
          sum(data.opBasePOonly$pain == 0 & data.opBasePOonly$periOp.bin == 1),
          sum(data.opBasePOonly$pain == 1 & data.opBasePOonly$icuCurrent.cat == 1),
          sum(data.opBasePOonly$pain == 1 & data.opBasePOonly$periOp.bin == 1))) %>%
    mutate(Pct = round(100 * N / n_start, 1))
  
  nonPO.exclusions

  # Prep for BalanceR
  basis <- reformulate(covs.bal) # prepare a formula object                       
  X <- scale(model.matrix(as.formula(basis), data.opBasePOonly)) # prepare a scaled matrix 
  # scaling is needed to calculate the weights, since they target a mean of 0
  trt <- data.opBasePOonly$pain
  n <- nrow(data.opBasePOonly)
  
  # Balance Weights ATT
  # Calculate
  out.pain <- multilevel_qp(X, trt, rep(1,n), lambda = 0.05, 
                            lowlim = 0, uplim = 1,  verbose= TRUE, 
                            exact_global = TRUE, scale_sample_size = FALSE)
  
  # Process 
  data.opBasePOonly$ATTwts.opBasePOonly <- pmax(out.pain$weights, 0) 
  summary(data.opBasePOonly$ATTwts.opBasePOonly)
  data.opBasePOonly$ATTwts.opBasePOonly[data.opBasePOonly$pain == 1] <- 1
  summary(data.opBasePOonly$ATTwts.opBasePOonly)
  sd(data.opBasePOonly$ATTwts.opBasePOonly)
  
  # Assess balance, weights, and ESS
  # SMD Plots and Total Bias Reduction 
  detach(package:Hmisc, unload=TRUE)
  source("./functions/balance-plots.R")
  bal.plots.ATT.clean <- bal.plot.clean(data = data.opBasePOonly, weights = "ATTwts.opBasePOonly",
                                            treatment = "pain", covs = covs, subset = FALSE, main.title = "Balance Plot: Oral-Analgesics-Only Cohort")
  
  for (i in seq_along(bal.plots.ATT.clean)) {
    plot_i <- bal.plots.ATT.clean[[i]]
    file_name <- paste0("./results/Overall/balplots/balance_plot_overall_balplots.opBasenonPO_", i, ".jpeg")
    ggsave(filename = file_name, plot = plot_i, device = "jpeg", 
           width = 7, height = 9, units = "in", dpi = 300)
  }

  # ESS
  # Effective Sample Size ATT
  source("./functions/ess-function.R")
  ess(data.opBasePOonly, "pain", "ATTwts.opBasePOonly")
  
  # 7c. Export to Stata
  # write to data folder
  write.dta(data.opBasePOonly, "./data/ibu-aki-overall-opBasePOonly.dta")
  
  

