# Make and Label Table 1
# Packages needed
library(tableone)
library(survey)
library(Hmisc)
library(tibble)

table.1 <-function(data, wts, covs) {
  ## Make Table 1

  # Label the factors
  data.tab1 <- data %>%
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
    mutate(rasBase = factor(rasBase, labels = c("No", "Yes"))) %>%
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
  label(data.tab1$sex)  <- "Sex" 
  label(data.tab1$age) <- "Age"
  label(data.tab1$race.white) <- "Race - White"
  label(data.tab1$race.black) <- "Race - Black"
  label(data.tab1$race.other) <- "Race - Other/Unknown"
  label(data.tab1$admType) <- "Admission Type"
  label(data.tab1$center.hup) <- "Hospital - HUP"
  label(data.tab1$center.presb) <- "Hospital - Presbyterian"
  label(data.tab1$center.pa) <- "Hospital - Pennsylvania"
  label(data.tab1$presentation.ed) <- "Presentation - ED"
  label(data.tab1$presentation.icu) <- "Presentation - ICU"
  label(data.tab1$presentation.or) <- "Presentation - OR"
  label(data.tab1$presentation.floor) <- "Presentation - Floor"
  label(data.tab1$presentation.other) <- "Presentation - Other"
  label(data.tab1$priorLos) <- "Prior Length of Stay"
  label(data.tab1$icuCurrent) <- "ICU Status"
  label(data.tab1$periOp.no) <- " Not Peri-Op "
  label(data.tab1$periOp.0) <- "POD 0"
  label(data.tab1$periOp.1) <- "POD 1"
  label(data.tab1$periOp.2) <- "POD 2"
  label(data.tab1$periOp.3) <- "POD 3"
  label(data.tab1$baseVentCurrent) <- "Ventilator Status at Baseline"
  label(data.tab1$baseVentEver) <- "Ventilator Status"
  label(data.tab1$chf) <- "Heart Failure"
  label(data.tab1$mif) <- "Myocardial Infarction"
  label(data.tab1$arry) <- "Arrhythmia"
  label(data.tab1$afib) <- "Atrial Fibrillation"
  label(data.tab1$valve) <- "Valvular Disease"
  label(data.tab1$cva) <- "Stroke"
  label(data.tab1$pvd) <- "Peripheral Vascular Disease"
  label(data.tab1$pCirc) <- "Pulmonary Circulation Disorder"
  label(data.tab1$cpd) <- "Chronic Pulmonary Disease"
  label(data.tab1$liver) <- "Liver Disease"
  label(data.tab1$dm.no) <- "Diabetes Mellitus - None"
  label(data.tab1$dm.noncomp) <- "Diabetes Mellitus - Non-complicated"
  label(data.tab1$dm.comp) <- "Diabetes Mellitus - Complicated"
  label(data.tab1$ckd) <- "Chronic Kidney Disease"
  label(data.tab1$wtLoss) <- "Weight Loss"
  label(data.tab1$fluid) <- "Fluid and Electrolyte Disorder"
  label(data.tab1$cancer.no) <- "Cancer - None"
  label(data.tab1$cancer.noncomp) <- "Cancer - Non-complicated"
  label(data.tab1$cancer.metastatic) <- "Cancer - Metastatic"
  label(data.tab1$hiv) <- "HIV"
  label(data.tab1$indexGFR) <- "GFR ml/min per 1.73m^2 "
  label(data.tab1$preAkiStatus) <- "Prior AKI"
  label(data.tab1$wbcBase) <- "WBC, x10^8 cells/dL"
  label(data.tab1$hgbBase) <- "Hemoglobin, g/dL"
  label(data.tab1$platBase) <- "Platelets, x10^11 cells/L"
  label(data.tab1$labclBase) <- "Chloride, mEq/L"
  label(data.tab1$labkBase) <- "Potassium, mEq/L"
  label(data.tab1$rasBase) <- "RAS Inhibitor"
  label(data.tab1$metopBase) <- "Metoprolol"
  label(data.tab1$abBlocker) <- "Combined Alpha and Beta Blocker"
  label(data.tab1$hctzBase) <- "Hydrochlorothiazide"
  label(data.tab1$loopBase) <- "Loop Diuretics"
  label(data.tab1$htnOther) <- "Other Antihypertensives"
  label(data.tab1$sup.no) <- "Acid Suppressants - None"
  label(data.tab1$sup.h2ra) <- "Acid Suppressants - H2RA"
  label(data.tab1$sup.ppi) <- "Acid Suppressants - PPI"
  label(data.tab1$gramNegBroad) <- "Broad Spectrum Antibiotics"
  label(data.tab1$gramNegNarrow) <- "Narrow Spectrum Antibiotics"
  label(data.tab1$vancoBase) <- "Vancomycin"
  label(data.tab1$bactrimBase) <- "Bactrim"
  label(data.tab1$abxNTX) <- "Other Nephrotoxic Antibiotics"
  label(data.tab1$ntxOther) <- "Other Nephrotoxins"
  label(data.tab1$pressBase) <- "Vasopressors"
  label(data.tab1$bmi) <- "Body Mass Index"
  
  surv.unadjusted <- svydesign(ids = ~ ddiID, strata = ~ pain, nest = T, data = data.tab1)
  table1.unadjusted <- svyCreateTableOne(vars = covs, strata = "pain", test = F, data = surv.unadjusted)
  tab.1.unadjusted <- print(table1.unadjusted, smd = TRUE, varLabels=T, test = F, showAllLevels = F, noSpaces =T,
                            contDigits = 1, formatOptions = list(drop0trailing = T))
 

 surv.adjusted <- svydesign(ids = ~ ddiID, strata = ~ pain, weights = data[[wts]], nest = T, data = data.tab1)
  table1.adjusted <- svyCreateTableOne(vars = covs, strata = "pain", test = F, data = surv.adjusted)
  tab.1.adjusted <- print(table1.adjusted, smd = TRUE, varLabels=T, test = F, showAllLevels = F, noSpaces =T,
                          contDigits = 1, formatOptions = list(drop0trailing = T))

  tab.1.unadjusted <- as.data.frame(tab.1.unadjusted) %>% rownames_to_column(var = "row.names")
  tab.1.adjusted <- as.data.frame(tab.1.adjusted) %>% rownames_to_column(var = "row.names")
  tab.1 <- tab.1.unadjusted %>%
    left_join(tab.1.adjusted, by = "row.names") %>%
    rename("Oxycodone (Unweighted)" = 2,
           "Ibuprofen (Unweighted)" = 3,
           "SMD (Unweighted)" = 4,
           "Oxycodone (Weighted)" = 5,
           "Ibuprofen (Weighted)" = 6, 
           "SMD (Weighted)" = 7)
  
  return(tab.1)
}
