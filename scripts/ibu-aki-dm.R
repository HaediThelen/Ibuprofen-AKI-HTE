library(foreign)
library(balancer)
library(dplyr)
library(ggplot2)
library(sandwich)
library(splines)
library(haven)
library(Hmisc) 
library(tableone)
library(survey)
library(tibble)

# Ibuprofen effect on AKI - evaluation of HTE by DM
# Step 0: Prep
# Load in data
#data <- read_dta("./data/ibu-aki-data.dta") 
#TEMPORARY
data <- read_dta("/Users/haedi/Library/CloudStorage/Box-Box/Data/NSAID-AKI/data/ibu-aki-data.dta")

# Clean data and and prep for balanceR, make DM a binary variable
data <- data %>% 
  filter(pain !=2) %>% # drop patients exposed to both IBU and Opioids
  filter(bmi > 14 & bmi <70) %>%
  mutate(across(where(is.numeric), as.numeric)) %>%
  mutate(across(where(~ all(. %in% c(0, 1))), as.integer)) %>%
  mutate(dm.bin = if_else(dm ==0, 0, 1)) %>%
  mutate(dm.bin = as.factor(dm.bin)) %>%
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
  mutate(dm.no = if_else(dm == 0, 1, 0), # Might need to delete this
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
  # Prep and label variables for table  
  # Calculate SMDs for tx groups  
  #prep for SMD table
  data.smdtab <- data %>%
    mutate_if(is.integer, as.factor) %>% # need to label each level
    mutate(sex = factor(sex,labels=c("Male","Female"))) %>% # finish labels
    mutate(race.white = factor(race.white, labels=c("Not White","White"))) %>%
    mutate(race.black = factor(race.black, labels=c("Not Black","Black"))) %>%
    mutate(race.other = factor(race.other, labels=c("Not Other/Unknown","Other/Unknown"))) %>%
    mutate(admType = factor(admType, labels = c("Medicine", "Surgery"))) %>%
    mutate(center.hup = factor(center.hup, labels = c("Not HUP", "HUP"))) %>%
    mutate(center.presb = factor(center.presb, labels = c("Not Presbyterian", "Presbyterian"))) %>%
    mutate(center.pa = factor(center.pa, labels = c("Not Pennsylvania Hosptial", "Pennsylvania Hosptial"))) %>%
    mutate(presentation.ed= factor(presentation.ed, labels = c("Not ED", "ED"))) %>%
    mutate(presentation.icu= factor(presentation.icu, labels = c("Not ICU", "ICU"))) %>%
    mutate(presentation.or= factor(presentation.or, labels = c("Not OR", "OR"))) %>%
    mutate(presentation.floor= factor(presentation.floor, labels = c("Not Floor", "Floor"))) %>%
    mutate(presentation.other= factor(presentation.other, labels = c("Not Other", "Other"))) %>%
    mutate(icuCurrent= factor(icuCurrent, labels = c("No", "Yes"))) %>%
    mutate(periOp.no = factor(periOp.no, labels = c("Post Op", "Not post-op"))) %>%
    mutate(periOp.0 = factor(periOp.0, labels = c("Not POD 0", "POD 0"))) %>%
    mutate(periOp.1 = factor(periOp.1, labels = c("Not POD 1", "POD 1"))) %>%
    mutate(periOp.2 = factor(periOp.2, labels = c("Not POD 2", "POD 2"))) %>%
    mutate(periOp.3 = factor(periOp.3, labels = c("Not POD 3", "POD 3"))) %>%
    mutate(baseVentCurrent = factor(baseVentCurrent, labels = c("No", "Yes"))) %>%
    mutate(baseVentEver = factor(baseVentEver, labels = c("No", "Yes"))) %>%
    mutate(chf = factor(chf, labels = c("No", "Yes"))) %>%
    mutate(mif = factor(mif, labels = c("No", "Yes"))) %>%
    mutate(arry = factor(arry, labels = c("No", "Yes"))) %>%
    mutate(afib = factor(afib, labels = c("No", "Yes"))) %>%
    mutate(valve = factor(valve,  labels = c("No", "Yes"))) %>%
    mutate(cva = factor(cva, labels = c("No", "Yes"))) %>%
    mutate(pvd = factor(pvd, labels = c("No", "Yes"))) %>%
    mutate(pCirc = factor(pCirc, labels = c("No", "Yes"))) %>%
    mutate(cpd = factor(cpd, labels = c("No", "Yes"))) %>%
    mutate(liver = factor(liver, labels = c("No", "Yes"))) %>%
    mutate(dm.no = factor(dm.no, labels = c("No", "Yes"))) %>%
    mutate(dm.noncomp = factor(dm.noncomp, labels = c("No", "Yes"))) %>%
    mutate(dm.comp = factor(dm.comp, labels = c("No", "Yes"))) %>%
    mutate(ckd = factor(ckd, labels = c("No", "Yes"))) %>%
    mutate(wtLoss = factor(wtLoss, labels = c("No", "Yes"))) %>%
    mutate(fluid = factor(fluid, labels = c("No", "Yes"))) %>%
    mutate(cancer.no = factor(cancer.no, labels = c("No", "Yes"))) %>%
    mutate(cancer.noncomp = factor(cancer.noncomp, labels = c("No", "Yes"))) %>%
    mutate(cancer.metastatic = factor(cancer.metastatic, labels = c("No", "Yes"))) %>%
    mutate(hiv= factor(hiv, labels = c("No", "Yes"))) %>%
    mutate(preAkiStatus = factor(preAkiStatus, labels = c("No", "Yes"))) %>%
    mutate(metopBase = factor(metopBase, labels = c("No", "Yes"))) %>%
    mutate(abBlocker = factor(abBlocker, labels = c("No", "Yes"))) %>%
    mutate(hctzBase = factor(hctzBase, labels = c("No", "Yes"))) %>%
    mutate(loopBase = factor(loopBase, labels = c("No", "Yes"))) %>%
    mutate(htnOther = factor(htnOther, labels = c("No", "Yes"))) %>%
    mutate(sup.no = factor(sup.no, labels = c("No", "Yes"))) %>%
    mutate(sup.h2ra = factor(sup.h2ra, labels = c("No", "Yes"))) %>%
    mutate(sup.ppi = factor(sup.ppi, labels = c("No", "Yes"))) %>%
    mutate(gramNegBroad = factor(gramNegBroad, labels = c("No", "Yes"))) %>%
    mutate(gramNegNarrow = factor(gramNegNarrow, labels = c("No", "Yes"))) %>%
    mutate(vancoBase = factor(vancoBase, labels = c("No", "Yes"))) %>%
    mutate(bactrimBase = factor(bactrimBase, labels = c("No", "Yes"))) %>%
    mutate(abxNTX = factor(abxNTX, labels = c("No", "Yes"))) %>%
    mutate(ntxOther = factor(ntxOther, labels = c("No", "Yes"))) %>%
    mutate(pressBase = factor(pressBase, labels = c("No", "Yes"))) %>%
    mutate(pain = factor(pain, labels = c("Oxycodone", "Ibuprofen")))
  
  # Labels for the Table
  label(data.smdtab$sex)  <- "Sex" 
  label(data.smdtab$age) <- "Age"
  label(data.smdtab$race.white) <- "Race - White"
  label(data.smdtab$race.black) <- "Race - Black"
  label(data.smdtab$race.other) <- "Race - Other/Unknown"
  label(data.smdtab$admType) <- "Admission Type"
  label(data.smdtab$center.hup) <- "Hospital - HUP"
  label(data.smdtab$center.presb) <- "Hospital - Presbyterian"
  label(data.smdtab$center.pa) <- "Hospital - Pennsylvania"
  label(data.smdtab$presentation.ed) <- "Presentation - ED"
  label(data.smdtab$presentation.icu) <- "Presentation - ICU"
  label(data.smdtab$presentation.or) <- "Presentation - OR"
  label(data.smdtab$presentation.floor) <- "Presentation - Floor"
  label(data.smdtab$presentation.other) <- "Presentation - Other"
  label(data.smdtab$priorLos) <- "Prior Length of Stay"
  label(data.smdtab$icuCurrent) <- "ICU Status"
  label(data.smdtab$periOp.no) <- " Not Peri-Op "
  label(data.smdtab$periOp.0) <- "POD 0"
  label(data.smdtab$periOp.1) <- "POD 1"
  label(data.smdtab$periOp.2) <- "POD 2"
  label(data.smdtab$periOp.3) <- "POD 3"
  label(data.smdtab$baseVentCurrent) <- "Ventilator Status at Baseline"
  label(data.smdtab$baseVentEver) <- "Ventilator Status"
  label(data.smdtab$chf) <- "Heart Failure"
  label(data.smdtab$mif) <- "Myocardial Infarction"
  label(data.smdtab$arry) <- "Arrhythmia"
  label(data.smdtab$afib) <- "Atrial Fibrilation"
  label(data.smdtab$valve) <- "Valvular Disease"
  label(data.smdtab$cva) <- "Stroke"
  label(data.smdtab$pvd) <- "Peripheral Vascular Disease"
  label(data.smdtab$pCirc) <- "Pulmonary Circulation Disorder"
  label(data.smdtab$cpd) <- "Chronic Pulmonary Disease"
  label(data.smdtab$liver) <- "Liver Disease"
  label(data.smdtab$dm.no) <- "Diabetes Mellitus - None"
  label(data.smdtab$dm.noncomp) <- "Diabetes Mellitus - Non-complicated"
  label(data.smdtab$dm.comp) <- "Diabetes Mellitus - Complicated"
  label(data.smdtab$ckd) <- "Chronic Kidney Disease"
  label(data.smdtab$wtLoss) <- "Weight Loss"
  label(data.smdtab$fluid) <- "Fluid and Electrolyte Disorder"
  label(data.smdtab$cancer.no) <- "Cancer - None"
  label(data.smdtab$cancer.noncomp) <- "Cancer - Non-complicated"
  label(data.smdtab$cancer.metastatic) <- "Cancer - Metastatic"
  label(data.smdtab$hiv) <- "HIV"
  label(data.smdtab$indexGFR) <- "GFR ml/min per 1.73m^2 "
  label(data.smdtab$preAkiStatus) <- "Prior AKI"
  label(data.smdtab$wbcBase) <- "WBC, x10^8 cells/dL"
  label(data.smdtab$hgbBase) <- "Hemoglobin, g/dL"
  label(data.smdtab$platBase) <- "Platelets, x10^11 cells/L"
  label(data.smdtab$labclBase) <- "Chloride, mEq/L"
  label(data.smdtab$labkBase) <- "Potassium, mEq/L"
  label(data.smdtab$rasBase) <- "RAS Inhibitor"
  label(data.smdtab$metopBase) <- "Metoprolol"
  label(data.smdtab$abBlocker) <- "Combined Alpha and Beta Blocker"
  label(data.smdtab$hctzBase) <- "Hydrochlorothiazide"
  label(data.smdtab$loopBase) <- "Loop Diuretics"
  label(data.smdtab$htnOther) <- "Other Antihypertensives"
  label(data.smdtab$sup.no) <- "Acid Supressants - None"
  label(data.smdtab$sup.h2ra) <- "Acid Supressants - H2RA"
  label(data.smdtab$sup.ppi) <- "Acid Supressants - PPI"
  label(data.smdtab$gramNegBroad) <- "Broad Spectrum Antibiotics"
  label(data.smdtab$gramNegNarrow) <- "Narrow Spectrum Antibiotics"
  label(data.smdtab$vancoBase) <- "Vancomycin"
  label(data.smdtab$bactrimBase) <- "Bactrim"
  label(data.smdtab$abxNTX) <- "Other Nephrotoxic Antibiotics"
  label(data.smdtab$ntxOther) <- "Other Nephrotoxins"
  label(data.smdtab$pressBase) <- "Vasopressors"
  label(data.smdtab$bmi) <- "Body Mass Index"
  
  # Choose all covs to evaluate SMDs
  covs <- c("age", "sex", "race.white", "race.black", "race.other", "admType", 
            "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
            "presentation.or", "presentation.floor", "presentation.other", "priorLos", "icuCurrent",
            "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
            "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
            "cva", "pvd", "pCirc", "cpd", "liver", "ckd", 
            "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
            "hiv", "indexGFR", "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
            "labkBase", "rasBase","metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
            "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
            "vancoBase", "bactrimBase",
            "abxNTX", "ntxOther", "pressBase", "bmi")
  
  # Create SMD tables within each BMI level
  dm.values <- 0:1
  source("./functions/overlap-eval.R")
  smd.tab.list <- lapply(dm.values, function(dm.value) {
    subset <- data.smdtab %>% filter(dm.bin == dm.value)
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
    density.plots.list <- lapply(dm.values, function(dm.value) {
      subset <- data.factor %>% filter(dm.bin == dm.value)
      dplots(subset, continuous.covs, "pain")
    })
    print(density.plots.list[[1]])
    print(density.plots.list[[2]])
    
  # Compare Number Summaries of continuous covariates  in subsets 
    num.summary.list <- lapply(dm.values, function(dm.value) {
      subset <- data.factor %>% filter(dm.bin == dm.value)
      sum.tabs(subset, continuous.covs, "pain")
    })
    print(num.summary.list[[1]])
    print(num.summary.list[[2]])
    
# Step 3: Estimate Weights
  covs <- c(colnames(age.sp), "sex", "race.white", "race.black", "race.other", "admType", 
            "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
            "presentation.or", "presentation.floor", "presentation.other", "priorLos", "icuCurrent",
            "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
            "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
            "cva", "pvd", "pCirc", "cpd", "liver", "ckd", 
            "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
            "hiv", colnames(indexGFR.sp), "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
            "labkBase", "rasBase", "metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
            "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
            "vancoBase", "bactrimBase",
            "abxNTX", "ntxOther", "pressBase", "bmi", "-1")
  
  # More Prep for BalanceR
    basis <- reformulate(covs) # prepare a formula object                       
    X <- scale(model.matrix(as.formula(basis), data)) # prepare a scaled matrix 
    # scaling is needed to calculate the weights, since they target a mean of 0
    trt <- data$pain
    n <- nrow(data)
    
    data.ctrl <- data %>% filter(pain==0)
    lambda.reg <- lm(reformulate(covs, response = "kEver"), data=data.ctrl)
    var(lambda.reg$resid)
    
    # Identify Effect Modifier  
    table(data$dm.bin)
    Z <- data$dm.bin # a factor
    
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
    covs <- c("age", "sex", "race.white", "race.black", "race.other", "admType", 
              "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
              "presentation.or", "presentation.floor", "presentation.other", "priorLos", "icuCurrent",
              "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
              "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
              "cva", "pvd", "pCirc", "cpd", "liver", "ckd", 
              "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
              "hiv", "indexGFR", "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
              "labkBase", "rasBase", "metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
              "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
              "vancoBase", "bactrimBase",
              "abxNTX", "ntxOther", "pressBase", "bmi")
    
  # SMD Plots and Total Bias Reduction 
    detach(package:Hmisc, unload=TRUE)
    source("./functions/balance-plots.R")
  # ATT (Balwts)
    bal.plots.ATT <- bal.plots(data, "ATTwts", "dm.bin", 'pain', covs) 
    bal.plots.ATT_clean <- bal.plot.clean.bin(data = data, weights = "ATTwts", strata = "dm.bin", 
                                          treatment = "pain", covs = covs, subset = TRUE, main.title = "Balance Plot DM")
    # Save plots
    if (!dir.exists("./results/dm/balplots")) {
      dir.create("./results/dm/balplots", recursive = TRUE)
    }
    for (i in seq_along(bal.plots.ATT_clean)) {
      plot_i <- bal.plots.ATT_clean[[i]]
      file_name <- paste0("./results/dm/balplots/balance_plot_DM_", i, ".jpeg")
      ggsave(filename = file_name, plot = plot_i, device = "jpeg", 
             width = 7, height = 9, units = "in", dpi = 300)
    }
    composite.plot <- composite.bal.plot.bin(bal.plots.ATT_clean, main.title = "Balance Plots by DM Status", strata = "dm.bin")
    composite.plot
    ggsave(filename = "./results/dm/balplots/Composite-DM.jpeg", plot = composite.plot, device = "jpeg", 
           width = 10, height = 10, units = "in", dpi = 300)
    
    ggsave(filename = "./results/dm/balplots/Composite-DM.pdf", plot = composite.plot, device = "pdf", 
           width = 10, height = 10, units = "in", dpi = 300)
  
  #Examine Balance in continuous covariate distributions

    source("./functions/density-plot.R")
    dplot("age", data, 'ATTwts')
    age.density.plots.list <- lapply(dm.values, function(dm.value) {
      subset <- data %>% filter(dm.bin == dm.value)
      dplot("age", subset, "ATTwts")
    })
    print(age.density.plots.list[[1]])
    print(age.density.plots.list[[2]])

    source("./functions/density-plot.R")
    dplot("indexGFR", data, 'ATTwts')
    indexGFR.density.plots.list <- lapply(dm.values, function(dm.value) {
      subset <- data %>% filter(dm.bin == dm.value)
      dplot("indexGFR", subset, "ATTwts")
    })
    print(indexGFR.density.plots.list[[1]])
    print(indexGFR.density.plots.list[[2]])

    source("./functions/density-plot.R")
    dplot("bmi", data, 'ATTwts')
    bmi.density.plots.list <- lapply(dm.values, function(dm.value) {
      subset <- data %>% filter(dm.bin == dm.value)
      dplot("bmi", subset, "ATTwts")
    })
    print(bmi.density.plots.list[[1]])
    print(bmi.density.plots.list[[2]])
    
  # ESS
    # Effective Sample Size ATT
    source("./functions/ess-function.R")
    ess(data, "pain", "ATTwts")
    
# Step 5: Ouctome model
  # Export to Stata
    # write to data folder
    #write.dta("./Data/ibu-aki-dm.dta") 
    # TEMPORARY
    write.dta(data, "/Users/haedi/Library/CloudStorage/Box-Box/Data/NSAID-AKI/data/ibu-aki-dm.dta") 
    
    