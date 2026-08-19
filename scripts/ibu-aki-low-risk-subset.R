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

# Ibuprofen effect on AKI - low-risk subset
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
         sup.ppi = if_else(sup == 2, 1, 0)) %>%
  # select low risk population: non-ICU, non-postoperative, age<60, eGFR>60
  mutate(periOp.bin = if_else(periOp ==0, 0, 1)) %>%
  mutate(periOp.bin = as.factor(periOp.bin)) %>%
  filter(icuCurrent == 0, periOp.bin == 0, age < 60, indexGFR > 60)

table(data$pain)
  
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


# Choose all covs to evaluate SMDs
covs <- c("age", "sex", "race.white", "race.black", "race.other", "admType", 
          "center.hup", "center.presb", "center.pa", "presentation.ed", "presentation.icu",
          "presentation.or", "presentation.floor", "presentation.other", "priorLos", "baseVentCurrent", 
          "baseVentEver", "chf", "mif", "arry", "afib", "valve", 
          "cva", "pvd", "pCirc", "cpd", "liver", "dm.no", "dm.noncomp", "dm.comp", "ckd", 
          "wtLoss", "fluid", "cancer.no", "cancer.noncomp", "cancer.metastatic",
          "hiv", "indexGFR", "preAkiStatus", "wbcBase", "hgbBase", "platBase", "labclBase",
          "labkBase", "rasBase", "metopBase", "abBlocker", "hctzBase", "hydralazineBase", "loopBase",
          "htnOther", "sup.no","sup.h2ra", "sup.ppi", "gramNegBroad", "gramNegNarrow", 
          "vancoBase", "bactrimBase",
          "abxNTX", "ntxOther", "pressBase", "bmi")

# Create SMD table
source("./functions/overlap-eval.R")
smd.tab <- smd.table(data, covs)

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
          "presentation.or", "presentation.floor", "presentation.other", "priorLos", "baseVentCurrent", 
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

# Step 4: Assess Balance, weights, and ESS 
# Table 1 for ATT
source("./functions/table1.R")
# Table 1 for ATT
tab.1.ATT <- table.1_low_risk(data, "ATTwts", covs) # one equal probability warning is expected

# SMD Plots and Total Bias Reduction 
detach(package:Hmisc, unload=TRUE)
source("./functions/balance-plots.R")
# ATT (Balwts)
bal.plots.ATT_clean <- bal.plot.clean.low.risk(data = data, weights = "ATTwts",  
                                      treatment = "pain", covs = covs, subset = FALSE, main.title = "Balance of Baseline Covariates")
if (!dir.exists("./results/low_risk_subset/balplots")) {
  dir.create("./results/low_risk_subset/balplots", recursive = TRUE)
}
ggsave(filename = "./results/low_risk_subset/balplots/Low-Risk-Balance.jpeg", device = "jpeg", 
       width = 7, height = 9, units = "in", dpi = 300)
ggsave(filename = "./results/low_risk_subset/balplots/Low-Risk-Balance.pdf", device = "pdf", 
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

# Step 5: Export to Stata for analysis 
# write to data folder
write.dta(data, "./data/low-risk-subset.dta") 

