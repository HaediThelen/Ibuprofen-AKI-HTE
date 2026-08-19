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

# Ibuprofen effect on AKI - evaluation of HTE by ICU Status
# Step 0: Prep
# Load in data
data <- read_dta("./data/ibu-aki-data.dta") 

# Clean data and and prep for balanceR
  data <- data %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%
    mutate(across(where(~ all(. %in% c(0, 1))), as.integer)) %>%
    mutate(icuCurrent.cat = as.factor(icuCurrent)) %>%
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
  # For primary analysis it will be ATT 
  
  # 2. Check for positivity/overlap
  # Choose all covs to evaluate SMDs
  covs <- c("age", "sex", "race.black", "race.white", "race.other", "admType", 
            "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
            "presentation.or", "presentation.floor", "presentation.other", "priorLos",
            "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
            "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
            "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
            "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
            "hiv", "indexGFR", "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
            "labkBase", "rasBase","metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
            "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
            "vancoBase", "bactrimBase",
            "abxNTX", "ntxOther", "pressBase", "bmi")

  # Create SMD tables within each ICU level
  icuCurrent.values <- 0:1
    source("./functions/overlap-eval.R")
    smd.tab.list <- lapply(icuCurrent.values, function(icuCurrent.value) {
      subset <- data %>% filter(icuCurrent.cat == icuCurrent.value)
      smd.table(subset, covs)
    })
    View(smd.tab.list[[1]])
    View(smd.tab.list[[2]])
 
  # Compare Density Curves
    continuous.covs <- c("age", "indexGFR", "bmi", "wbcBase", "hgbBase", "platBase",
                       "labclBase", "labkBase")  
    data.factor <- data %>% mutate(pain = as.factor(pain))
  
  # Plot density curves overall
    density.plots <- dplots(data.factor, continuous.covs, "pain") 
    
  # Plot density curves in subsets
    density.plots.list <- lapply(icuCurrent.values, function(icuCurrent.value) {
      subset <- data.factor %>% filter(icuCurrent.cat == icuCurrent.value)
      dplots(subset, continuous.covs, "pain")
    })
    print(density.plots.list[[1]])
    print(density.plots.list[[2]])
  
  # Compare Number Summaries of continuous covariates  overall 
    tab <- sum.tabs(data.factor, continuous.covs, 'pain') 
    tab
    
  # Compare Number Summaries of continuous covariates  in subsets 
    num.summary.list <- lapply(icuCurrent.values, function(icuCurrent.value) {
      subset <- data.factor %>% filter(icuCurrent.cat == icuCurrent.value)
      sum.tabs(subset, continuous.covs, "pain")
    })
    print(num.summary.list[[1]])
    print(num.summary.list[[2]])
    
# Step 3: Estimate Weights
    # Remove icuCurrent from this list:
  covs.bal <- c(colnames(age.sp), "sex", "race.white", "race.black", "race.other", "admType", 
            "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
            "presentation.or", "presentation.floor", "presentation.other", "priorLos",
            "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
            "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
            "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
            "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
            "hiv", colnames(indexGFR.sp), "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
            "labkBase","rasBase", "metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
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
    
    # Identify Effect Modifier
    table(data$icuCurrent.cat)
    Z <- data$icuCurrent.cat # a factor
    
    # Balance Weights ATT
    # Calculate
      out.pain <- multilevel_qp(X, trt, Z, lambda = 0.05, 
                                lowlim = 0, uplim = 1,  verbose= TRUE, 
                                exact_global = TRUE, scale_sample_size = FALSE)
      
    # Process 
      data$ATTwts <- pmax(out.pain$weights, 0) 
      summary(data$ATTwts)
      data$ATTwts[data$pain == 1] <- 1
      summary(data$ATTwts)
      sd(data$ATTwts)
      
# Step 4: assess balance, weights, and ESS
  # SMD Plots and Total Bias Reduction 
    detach(package:Hmisc, unload=TRUE)
    source("./functions/balance-plots.R")
  # ATT (Balwts)
    bal.plots.ATT.clean <- bal.plot.clean.bin(data = data, weights = "ATTwts", strata = "icuCurrent.cat", 
                                              treatment = "pain", covs = covs, subset = TRUE, main.title = "Balance Plot ICU Status")
    # Save plots
    if (!dir.exists("./results/icuCurrent/balplots")) {
      dir.create("./results/icuCurrent/balplots", recursive = TRUE)
    }
    for (i in seq_along(bal.plots.ATT.clean)) {
      plot_i <- bal.plots.ATT.clean[[i]]
      file_name <- paste0("./results/icuCurrent/balplots/balance_plot_icuCurrent_", i, ".jpeg")
      ggsave(filename = file_name, plot = plot_i, device = "jpeg", 
             width = 7, height = 9, units = "in", dpi = 300)
    }
    composite.plot <- composite.bal.plot.bin(bal.plots.ATT.clean, main.title = "Balance Plots by ICU Status", strata = "icuCurrent.cat")
    composite.plot
    
    ggsave(filename = "./results/icuCurrent/balplots/Composite-icuCurrent.jpeg", plot = composite.plot, device = "jpeg", 
           width = 10, height = 10, units = "in", dpi = 300)
    ggsave(filename = "./results/icuCurrent/balplots/Composite-icuCurrent.pdf", plot = composite.plot, device = "pdf", 
           width = 10, height = 10, units = "in", dpi = 300)
  

    # Examine Balance in continuous covariate distributions
    source("./functions/density-plot.R")
    dplot("age", data, 'ATTwts')
    age.density.plots.list <- lapply(icuCurrent.values, function(icuCurrent.value) {
      subset <- data %>% filter(icuCurrent.cat == icuCurrent.value)
      dplot("age", subset, "ATTwts")
    })
    print(age.density.plots.list[[1]])
    print(age.density.plots.list[[2]])

    dplot("indexGFR", data, 'ATTwts')
    indexGFR.density.plots.list <- lapply(icuCurrent.values, function(icuCurrent.value) {
      subset <- data %>% filter(icuCurrent.cat == icuCurrent.value)
      dplot("indexGFR", subset, "ATTwts")
    })
    print(indexGFR.density.plots.list[[1]])
    print(indexGFR.density.plots.list[[2]])

    dplot("bmi", data, 'ATTwts')
    bmi.density.plots.list <- lapply(icuCurrent.values, function(icuCurrent.value) {
      subset <- data %>% filter(icuCurrent.cat == icuCurrent.value)
      dplot("bmi", subset, "ATTwts")
    })
    print(bmi.density.plots.list[[1]])
    print(bmi.density.plots.list[[2]])
 
  # ESS
    # Effective Sample Size ATT
      source("./functions/ess-function.R")
      ess(data, "pain", "ATTwts")
      
# Step 5: Export to Stata
      # write to data folder
      write.dta(data, "./data/ibu-aki-icuCurrent.dta") 
      
# 6. Sensitivity Analysis - IV Opioid exposure in baseline period
  # include IV opioid exposure during the baseline period in the balance weight model to adjust for potential confounding by indication.
  
  # 6a. Make summary table of IV opioid exposure in baseline period by pain status and icu status
  cat.vars <- c("opBase", "h_Base", "m_Base", "f_Base", "opBasePO", "opBasePatch", "opBaseIV",
                "opBasePCA", "opBaseGTT")
  cont.vars <- c("omeBaseTotal")
  
  source("./functions/baseOp-summary.R")
  data.baseline.opioid.tab.icu <- make_baseline_opioid_table(data %>% filter(icuCurrent.cat == 1), cat.vars, cont.vars, ome.var = "omeBaseTotal")
  data.baseline.opioid.tab.icu

  data.baseline.opioid.tab.nonicu <- make_baseline_opioid_table(data %>% filter(icuCurrent.cat == 0), cat.vars, cont.vars, ome.var = "omeBaseTotal")
  data.baseline.opioid.tab.nonicu

# 6b. Estimate balancing weights in subgroups, including omeCat
  #omeCat is a categorical variable with 0 =  were omeBaseTotal was 0 indicating no opioid
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
                "presentation.or", "presentation.floor", "presentation.other", "priorLos",
                "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
                "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
                "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
                "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
                "hiv", colnames(indexGFR.sp), "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
                "labkBase","rasBase", "metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
                "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
                "vancoBase", "bactrimBase",
                "abxNTX", "ntxOther", "pressBase", "bmi", "omeCat.0","omeCat.1",
                "omeCat.2", "omeCat.3", "omeCat.4", "-1")

  # More Prep for BalanceR
    basis <- reformulate(covs.bal.omeCat) # prepare a formula object                       
    X <- scale(model.matrix(as.formula(basis), data)) # prepare a scaled matrix 
    # scaling is needed to calculate the weights, since they target a mean of 0
    trt <- data$pain
    n <- nrow(data)
  
  # Identify Effect Modifier
    table(data$icuCurrent.cat)
    Z <- data$icuCurrent.cat # a factor
  
  # Balance Weights ATT
    out.pain <- multilevel_qp(X, trt, Z, lambda = 0.05, 
                              lowlim = 0, uplim = 1,  verbose= TRUE, 
                              exact_global = TRUE, scale_sample_size = FALSE)
    
  # Process 
    data$ATTwts.omeCat <- pmax(out.pain$weights, 0) 
    summary(data$ATTwts.omeCat)
    data$ATTwts.omeCat[data$pain == 1] <- 1
    summary(data$ATTwts.omeCat)
    sd(data$ATTwts.omeCat)
  
  # Evaluate Balance
  # Restrict to ICU and Non-ICU for table 1 style-summary of omeCat
    data.nonicu <- data %>% filter(icuCurrent==0)
    data.icu <- data %>% filter(icuCurrent==1)

  # Check Balance 
    source("./functions/table1.R")
    tab.1.nonicu.omeCat <- table.1.omeCat(data.nonicu, "ATTwts.omeCat", c("omeCat.0","omeCat.1",
                                                                          "omeCat.2", "omeCat.3", "omeCat.4"))
    tab.1.nonicu.omeCat
    
    tab.1.icu.omeCat <- table.1.omeCat(data.icu, "ATTwts.omeCat", c("omeCat.0","omeCat.1",
                                                                "omeCat.2", "omeCat.3", "omeCat.4"))
    tab.1.icu.omeCat
    
    covs_omeCat <- c("age", "sex", "race.white", "race.black", "race.other", "admType", 
              "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
              "presentation.or", "presentation.floor", "presentation.other", "priorLos",
              "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
            "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
            "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
            "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
            "hiv", "indexGFR", "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
            "labkBase", "rasBase","metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
            "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
            "vancoBase", "bactrimBase", "abxNTX", "ntxOther", "pressBase", "bmi",
            "omeCat.0","omeCat.1", "omeCat.2", "omeCat.3", "omeCat.4")
  
  long.names <- c("Age", "Sex (Female)", "Race - White", "Race - Black", "Race -Other", "Admisison Type", 
                  "Center - HUP", "Center - Presbyterian", "Center - Pennsylvania Hospital", "Presentation - ED", "Presentation ICU",
                  "Presentation - OR", "Presentation - Floor", "Presentation - Other", "Prior LOS",
                  "Post Op", "POD 0", "POD 1", "POD 2", "POD3", "Ventilator Status", 
                  "Prior Ventilator", "Heart Failure", "Myocardial Infarction", "Arrhythmia", "Atrial Fibrillation", "Valvular Diseas", 
                  "Stroke", "Peripheral Vascular Disease", "Pulmonary Circulation Disorder", "Chronic Pulmonary Disease", "Liver Disease", 
                  "Diabetes Mellitus - None", "Diabetes Mellitus - Non-complicated", "Diabetes Mellitus - Complicated", "Chronic Kidney Disease", 
                  "Weight Loss", "Fluid and Electrolyte Disorder", "Cancer - None", "Cancer - Non-complicated", "Cancer - Metastatic",
                  "HIV", "eGFR", "Prior AKI", "WBC, x10^8 cells/dL", "Hemoglobin, g/dL", "Platelets, x10^11 cells/L", "Chloride, mEq/L",
                  "Potassium, mEq/L","RAS Inhibitor", "Metoprolol", "Combined Alpha and Beta Blocker", "Hydrochlorothiazide", "Hydralazine", "Loop Diuretics",
                  "Other Antihypertensives", "Acid Suppressants - None","Acid Suppressants - H2RA", "Acid Suppressants - PPI", "Broad Spectrum Antibiotics", "Narrow Spectrum Antibiotics", 
                  "Vancomycin", "Bactrim", "Other Nephrotoxic Antibiotics", "Other Nephrotoxins", "Vasopressors", "BMI",
                  "Baseline Opioid - None","Baseline Opioid - Q1 OME", "Baseline Opioid - Q2 OME", "Baseline Opioid - Q3 OME", "Baseline Opioid - Q4 OME")
  
  detach(package:Hmisc, unload=TRUE)
  bal.plots.ATT.clean <- bal.plot.clean.bin(data = data, weights = "ATTwts.omeCat", strata = "icuCurrent.cat", 
                                            treatment = "pain", covs = covs_omeCat, subset = TRUE, main.title = "Balance Plot ICU Status", long.names = long.names)

  # Save plots
    for (i in seq_along(bal.plots.ATT.clean)) {
      plot_i <- bal.plots.ATT.clean[[i]]
      file_name <- paste0("./results/icuCurrent/balplots/balance-plot-icuCurrent-", i, "-baseOP.jpeg")
      ggsave(filename = file_name, plot = plot_i, device = "jpeg", 
             width = 7, height = 9, units = "in", dpi = 300)
    }
    composite.plot <- composite.bal.plot.bin(bal.plots.ATT.clean, main.title = "Balance Plots by ICU Status", strata = "icuCurrent.cat")
    composite.plot
  
    ggsave(filename = "./results/icuCurrent/balplots/Composite-icuCurrent-baseOP.jpeg", plot = composite.plot, device = "jpeg", 
           width = 10, height = 10, units = "in", dpi = 300)
    ggsave(filename = "./results/icuCurrent/balplots/Composite-icuCurrent-baseOP.pdf", plot = composite.plot, device = "pdf", 
           width = 10, height = 10, units = "in", dpi = 300)

#Export to stata
  write.dta(data, "./data/ibu-aki-icuCurrent-opBase.dta")

# 7.Sensitivity Analysis - ICU subgroup analysis restricted to patients who received only oral exposures 
  #7a. restrict to patients who received oral opioids and NSAIDs in the baseline period and estimate balance weights
  data.opBasePOonly <- data %>% filter(opBaseIV == 0 & opBasePCA == 0, opBaseGTT == 0, opBasePatch ==0)
  
  # Breakdown of treatment and ICU status in those who did not receive non-PO analgesics. 
  data.opBasePOonly %>% group_by(pain, icuCurrent) %>% summarise(n=n())
  
  # Estimate Weights
  
  # More Prep for BalanceR
   basis <- reformulate(covs.bal) # prepare a formula object                       
   X <- scale(model.matrix(as.formula(basis), data.opBasePOonly)) # prepare a scaled matrix 
   # scaling is needed to calculate the weights, since they target a mean of 0
   trt <- data.opBasePOonly$pain
   n <- nrow(data.opBasePOonly)
  
  # Identify Effect Modifier
    table(data.opBasePOonly$icuCurrent.cat)
    Z <- data.opBasePOonly$icuCurrent.cat # a factor
    
  # Balance Weights ATT
    out.pain <- multilevel_qp(X, trt, Z, lambda = 0.05, 
                            lowlim = 0, uplim = 1,  verbose= TRUE, 
                            exact_global = TRUE, scale_sample_size = FALSE)
  
  # Process 
    data.opBasePOonly$ATTwts.opBasePOonly <- pmax(out.pain$weights, 0) 
    summary(data.opBasePOonly$ATTwts.opBasePOonly)
    data.opBasePOonly$ATTwts.opBasePOonly[data.opBasePOonly$pain == 1] <- 1
    summary(data.opBasePOonly$ATTwts.opBasePOonly)
    sd(data.opBasePOonly$ATTwts.opBasePOonly)
  
  # 7b: assess balance, weights, and ESS
  # Table 1 for ATT overall
    detach(package:Hmisc, unload=TRUE)
    source("./functions/balance-plots.R")
    bal.plots.ATT.clean <- bal.plot.clean.bin(data = data.opBasePOonly, weights = "ATTwts.opBasePOonly", strata = "icuCurrent.cat", 
                                              treatment = "pain", covs = covs, subset = TRUE, main.title = "Balance Plot by ICU Status in \nOral-Analgesics-Only Cohort")
    
    for (i in seq_along(bal.plots.ATT.clean)) {
      plot_i <- bal.plots.ATT.clean[[i]]
      file_name <- paste0("./results/icuCurrent/balplots/balance_plot_icuCurrent_balplots.opBasePOonly_", i, ".jpeg")
      ggsave(filename = file_name, plot = plot_i, device = "jpeg", 
             width = 7, height = 9, units = "in", dpi = 300)
    }
    composite.plot <- composite.bal.plot.bin(bal.plots.ATT.clean, main.title = "Balance Plots by Critical Illness Status in \nOral-Analgesics-Only Cohort", strata = "icuCurrent.cat")
    composite.plot
    
    ggsave(filename = "./results//icuCurrent/balplots/Composite-icuCurrent-opBasePOonly.jpeg", plot = composite.plot, device = "jpeg", 
           width = 10, height = 10, units = "in", dpi = 300)
    ggsave(filename = "./results/icuCurrent/balplots/Composite-icuCurrent-opBasePOonly.pdf", plot = composite.plot, device = "pdf", 
           width = 10, height = 10, units = "in", dpi = 300)
    
    
  # ESS
  # Effective Sample Size ATT
    source("./functions/ess-function.R")
    ess(data.opBasePOonly, "pain", "ATTwts.opBasePOonly")

# 7c. Export to Stata
  # write to data folder
  write.dta(data.opBasePOonly, "./data/ibu-aki-icuCurrent-opBasePOonly.dta")

     
      