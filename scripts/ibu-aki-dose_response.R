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
library(janitor)
library(cobalt)

# Ibuprofen effect on AKI - dose response

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
# For ATE:  
  # First calculate overall ATE ibu AKI rate
  # Then, we calculate weight of treatment with or without each level of doseCat,
  # for a total of 3 weights (likelihood of low-dose vs not low-dose  etc).
  # Estimate treatment effects using weights corresponding to the treatment actually received. 

# 2. Check for positivity/overlap
# summary table of doseCat 
  doseCat.summary <- data %>% tabyl(doseCat) %>% adorn_pct_formatting(digits = 2)
  doseCat.summary
  
  covs_bal <- c("age", "sex", "race.white", "race.black", "race.other", "admType", 
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
  
  data$doseCat <- factor(data$doseCat, 
                         levels = c(0, 1, 2, 3), 
                         labels = c("Oxycodone", "Low Dose", "Medium Dose", "High Dose"))

# Make Table 1 for this analysis (unweighted only)
  source("./functions/table1.R")
  tab.1.doseCat <- table.1.doseCat(data, covs = covs_bal) # one equal probability warning is expected
  tab.1.doseCat
# Percent of males by doseCat
  data %>%
    group_by(doseCat) %>%
    summarise(
      n_sex1 = sum(sex == 0, na.rm = TRUE),
      pct_sex1 = mean(sex == 0, na.rm = TRUE) * 100) 
  # many covariates have absolute SMD >0.1 when comparing low, med, and high dose ibu treatment groups to control group

# Median duration of Follow up time 
  data %>% group_by(doseCat) %>%
    summarise(median_follow_up_time = median(pTime))

# AKI By Stage
# count of AKI by study drug
  data %>%
    filter(kEver == 1) %>%
    tabyl(kStage, doseCat) %>%  
    adorn_totals("row")

# Compare Density Curves
# Choose all covs to evaluate density functions
  source("./functions/overlap-eval.R")
  continuous.covs <- c("age", "indexGFR", "bmi", "wbcBase", "hgbBase", "platBase",
                       "labclBase", "labkBase")

# Plot density curves 
  density.plots <- dplots(data, continuous.covs, "doseCat")

# Step 3: Estimate the balance weights
# Evaluate balance and effective sample size after calculating balancing weights 
# to target the ATE and to target the ATT in those treated with high dose ibuprofen (ATTH)

# Make list of covariates for the model
  covs <- c(colnames(age.sp), "sex", "race.black", "race.white", "race.other", "admType", 
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

# 3a. weights
# Make dummy variable for doseCat, then estimate binary weights 3 times, 
#each targeting the overall treated iwth ibuprofen population (n=6313):
# once for each doseCat level (so weights are estimating inverse probability of being low-dose vs not etc.)

# Make dummy doseCat variable
  data <- data %>% 
    mutate(doseCat.control = if_else(doseCat == "Oxycodone", 1, 0), 
           doseCat.low = if_else(doseCat == "Low Dose", 1, 0),
           doseCat.med = if_else(doseCat == "Medium Dose", 1, 0),
           doseCat.hi = if_else(doseCat == "High Dose", 1, 0))

# More Prep for BalanceR
  basis <- reformulate(covs) # prepare a formula object                       
  X <- scale(model.matrix(as.formula(basis), data)) # prepare a scaled matrix 
  # scaling is needed to calculate the weights, since they target a mean of 0

# 3a.1 Calculate ATE-Control wts
  trt.control <- data$doseCat.control
  n <- nrow(data)
  
  data.ctrl <- data %>% filter(pain==0)
  lambda.reg <- lm(reformulate(covs, response = "kEver"), data=data.ctrl)
  var(lambda.reg$resid) # use same labmda for all ATE weights
  
  out.doseCat.control <- multilevel_ate_qp(X, trt.control, rep(1,n), lambda = 0.05, lowlim = 0, uplim = 1, 
                                           verbose= TRUE, exact_global = TRUE, scale_sample_size = FALSE)

# Process doseCat.control.wts
  data$ATE.doseCat.control.wts <- out.doseCat.control$weights
  summary(data$ATE.doseCat.control.wts)
  sd(data$ATE.doseCat.control.wts)

# 3a.2 Calculate ATE-low dose wts
  trt.low <- data$doseCat.low
  out.doseCat.low <- multilevel_ate_qp(X, trt.low, rep(1,n), lambda = 0.05, lowlim = 0, uplim = 1, 
                                     verbose= TRUE, exact_global = TRUE, scale_sample_size = FALSE)
# Process doseCat.control.wts
  data$ATE.doseCat.low.wts <- out.doseCat.low$weights
  summary(data$ATE.doseCat.low.wts)
  sd(data$ATE.doseCat.low.wts)

# 3a.3 Calculate ATE-med dose wts
  trt.med <- data$doseCat.med
  out.doseCat.med <- multilevel_ate_qp(X, trt.med, rep(1,n), lambda = 0.05, lowlim = 0, uplim = 1, 
                                     verbose= TRUE, exact_global = TRUE, scale_sample_size = FALSE)
# Process doseCat.control.wts
  data$ATE.doseCat.med.wts <- out.doseCat.med$weights
  summary(data$ATE.doseCat.med.wts)
  sd(data$ATE.doseCat.med.wts)

# 3a.5 Calculate ATE-hi dose wts
trt.hi <- data$doseCat.hi
# Calculate balance weights
  out.doseCat.hi <- multilevel_ate_qp(X, trt.hi, rep(1,n), lambda = 0.05, lowlim = 0, uplim = 1, 
                                    verbose= TRUE, exact_global = TRUE, scale_sample_size = FALSE)
# Process doseCat.hi.wts
  data$ATE.doseCat.hi.wts <- out.doseCat.hi$weights
  summary(data$ATE.doseCat.hi.wts) # extreme weights here
  sd(data$ATE.doseCat.hi.wts)

# Make column for ATE weight corresponding to the actual treatment received
  data <- data %>%
    mutate(ATE.doseCat.wts = case_when(doseCat == "Oxycodone" ~ ATE.doseCat.control.wts,
                                       doseCat == "Low Dose" ~ ATE.doseCat.low.wts,
                                       doseCat == "Medium Dose" ~ ATE.doseCat.med.wts,
                                       doseCat == "High Dose" ~ ATE.doseCat.hi.wts))

# Step 4: Assess Balance, weights, and ESS  
  tab.1.doseCat.adj <-  table.1.doseCat(data, wts = "ATE.doseCat.wts", covs_bal)
  tab.1.doseCat.adj

# Effective Sample Size 
  source("./functions/ess-function.R")
  ess <- ess_mult(data, "doseCat", "ATE.doseCat.wts")
  ess


# Step 5: Export to Stata for analysis 
# write to data folder
  write.dta(data, "./Data/ibu-aki-dose-response.dta") 


