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

# Ibuprofen effect on AKI - evaluation of HTE by HF
# Step 0: Prep
# Load in data
data <- read_dta("./data/ibu-aki-data.dta") 

# Clean data and and prep for balanceR, make BMI quintiles 
  data <- data %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%
    mutate(across(where(~ all(. %in% c(0, 1))), as.integer)) %>%
    mutate(chf.cat = as.factor(chf)) %>%
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
  covs <- c("age", "sex", "race.white", "race.black", "race.other", "admType", 
            "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
            "presentation.or", "presentation.floor", "presentation.other", "priorLos", "icuCurrent",
            "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
            "baseVentEver", "mif", "arry", "afib", "valve", 
            "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
            "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
            "hiv", "indexGFR", "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
            "labkBase", "rasBase","metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
            "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
            "vancoBase", "bactrimBase",
            "abxNTX", "ntxOther", "pressBase", "bmi")

  # Create SMD tables within each BMI level
    chf.values <- 0:1
    source("./functions/overlap-eval.R")
    smd.tab.list <- lapply(chf.values, function(chf.value) {
      subset <- data.smdtab %>% filter(chf.cat == chf.value)
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
    density.plots.list <- lapply(chf.values, function(chf.value) {
      subset <- data.factor %>% filter(chf.cat == chf.value)
      dplots(subset, continuous.covs, "pain")
    })
    print(density.plots.list[[1]])
    print(density.plots.list[[2]])
  
  # Compare Number Summaries of continuous covariates  overall 
    tab <- sum.tabs(data.factor, continuous.covs, 'pain') 
    tab
    
  # Compare Number Summaries of continuous covariates  in subsets 
    num.summary.list <- lapply(chf.values, function(chf.value) {
      subset <- data.factor %>% filter(chf.cat == chf.value)
      sum.tabs(subset, continuous.covs, "pain")
    })
    print(num.summary.list[[1]])
    print(num.summary.list[[2]])
    
# Step 3: Estimate Weights
    # Remove CHF from this list:
  covs.bal <- c(colnames(age.sp), "sex", "race.white", "race.black", "race.other", "admType", 
            "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
            "presentation.or", "presentation.floor", "presentation.other", "priorLos", "icuCurrent",
            "periOp.no", "periOp.0", "periOp.1", "periOp.2", "periOp.3", "baseVentCurrent", 
            "baseVentEver", "mif", "arry", "afib", "valve", 
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
    table(data$chf.cat)
    Z <- data$chf.cat # a factor
    
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
  # Table 1 for ATT overall
  # SMD Plots and Total Bias Reduction 
    detach(package:Hmisc, unload=TRUE)
    source("./functions/balance-plots.R")
  # ATT (Balwts)
    bal.plots.ATT.clean <- bal.plot.clean.bin(data = data, weights = "ATTwts", strata = "chf.cat", 
                                              treatment = "pain", covs = covs, subset = TRUE, main.title = "Balance Plot HF", long.names = NULL)
    # Save plots
    if (!dir.exists("./results/hf/balplots")) {
      dir.create("./results/hf/balplots", recursive = TRUE)
    }
    for (i in seq_along(bal.plots.ATT.clean)) {
      plot_i <- bal.plots.ATT.clean[[i]]
      file_name <- paste0("./results/hf/balplots/balance_plot_HF_", i, ".jpeg")
      ggsave(filename = file_name, plot = plot_i, device = "jpeg", 
             width = 7, height = 9, units = "in", dpi = 300)
    }
    composite.plot <- composite.bal.plot.bin(bal.plots.ATT.clean, main.title = "Balance Plots by HF Status", strata = "chf.cat")
    composite.plot
    
    ggsave(filename = "./results/hf/balplots/Composite-HF.jpeg", plot = composite.plot, device = "jpeg", 
           width = 10, height = 10, units = "in", dpi = 300)
    ggsave(filename = "./results/hf/balplots/Composite-HF.pdf", plot = composite.plot, device = "pdf", 
           width = 10, height = 10, units = "in", dpi = 300)
    ggsave(filename = "./results/hf/balplots/Composite-HF.svg", plot = composite.plot, device = svglite, 
           width = 10, height = 10, units = "in", dpi = 300)

    # Examine Balance in continuous covariate distributions
    source("./functions/density-plot.R")
    dplot("age", data, 'ATTwts')
    age.density.plots.list <- lapply(chf.values, function(chf.value) {
      subset <- data %>% filter(chf.cat == chf.value)
      dplot("age", subset, "ATTwts")
    })
    print(age.density.plots.list[[1]])
    print(age.density.plots.list[[2]])

    dplot("indexGFR", data, 'ATTwts')
    indexGFR.density.plots.list <- lapply(chf.values, function(chf.value) {
      subset <- data %>% filter(chf.cat == chf.value)
      dplot("indexGFR", subset, "ATTwts")
    })
    print(indexGFR.density.plots.list[[1]])
    print(indexGFR.density.plots.list[[2]])

    dplot("bmi", data, 'ATTwts')
    bmi.density.plots.list <- lapply(chf.values, function(chf.value) {
      subset <- data %>% filter(chf.cat == chf.value)
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
      write.dta(data, "./data/ibu-aki-hf.dta") 

      