
# Make Table for Supplement and calculate descriptive statistics for Manuscript

# summary of descriptive statistics.
median(data$age)
IQR(data$age)
quantile(data$age, 0.25)
quantile(data$age, 0.75)
prop.table(table(data$sex))

test.data <- data %>%
  filter(pain==0)
prop.table(table(test.data$sex))

test.data <- data 
prop.table(table(test.data$race))

test.data <- data %>%
filter(pain==0)
prop.table(table(test.data$race))

median(data$pTime)
quantile(data$pTime, 0.25)
quantile(data$pTime, 0.75)


#  HTE by age Table 
egfr.data <- read_excel('./results/ibu-aki-GFR-ATT-no-spline.xlsx')
egfr.data <- egfr.data %>%
  mutate(
    formatted = paste0(round(Margin, 2), " (", 
                       round(LB, 2), "–", 
                       round(UB, 2), ")"),
    Drug = ifelse(Pain == 1, "Ibuprofen", "Oxycodone")
  ) %>%
  filter(!eGFR %in% c(20, 150))

# reshape estimated IR
egfr.data_wide <- egfr.data %>%
  select(eGFR, Drug, formatted) %>%
  pivot_wider(names_from = Drug,
              values_from = formatted,
              names_glue = "{Drug} est IR (95%CI)")
# load differences in IR
egfr.ird.data <- read_excel('./results/ibu-aki-GFR-ATT-IRD.xlsx')
egfr.ird.data <- egfr.ird.data %>%
  mutate(
    IRD = paste0(round(IRD, 2), " (", 
                       round(LB, 2), "–", 
                       round(UB, 2), ")")) %>%
  rename(eGFR = indexGFR) %>%
  select(c(eGFR, IRD))

# load ratios of IR
egfr.irr.data <- read_excel('./results/ibu-aki-GFR-ATT-IRR.xlsx')
egfr.irr.data <- egfr.irr.data %>%
  mutate(
    IRR = paste0(round(IRR, 2), " (", 
                 round(LB, 2), "–", 
                 round(UB, 2), ")")) %>%
  rename(eGFR = indexGFR) %>%
  select(c(eGFR, IRR))

# merge
egfr.data_wide <- egfr.data_wide %>%
  left_join(egfr.ird.data, by = "eGFR") %>%
    left_join(egfr.irr.data, by = "eGFR")
egfr.data_wide

#  HTE by age Table 
age.data <- read_excel('./results/ibu-aki-age-ATT-no-spline.xlsx')
age.data <- age.data %>%
  mutate(
    formatted = paste0(round(Margin, 2), " (", 
                       round(LB, 2), "–", 
                       round(UB, 2), ")"),
    Drug = ifelse(Pain == 1, "Ibuprofen", "Oxycodone")
  ) %>%
  filter(!Age %in% c(20, 100, 110))

# reshape estimated IR
age.data_wide <- age.data %>%
  select(Age, Drug, formatted) %>%
  pivot_wider(names_from = Drug,
              values_from = formatted,
              names_glue = "{Drug} est IR (95%CI)")
# load differences in IR
age.ird.data <- read_excel('./results/ibu-aki-age-ATT-IRD.xlsx')
age.ird.data <- age.ird.data %>%
  mutate(
    IRD = paste0(round(IRD, 2), " (", 
                 round(LB, 2), "–", 
                 round(UB, 2), ")")) %>%
  rename(Age = age) %>%
  select(c(Age, IRD))

# load ratios of IR
age.irr.data <- read_excel('./results/ibu-aki-age-ATT-IRR.xlsx')
age.irr.data <- age.irr.data %>%
  mutate(
    IRR = paste0(round(IRR, 2), " (", 
                 round(LB, 2), "–", 
                 round(UB, 2), ")")) %>%
  rename(Age = age) %>%
  select(c(Age, IRR))

# merge
age.data_wide <- age.data_wide %>%
  left_join(age.ird.data, by = "Age") %>%
  left_join(age.irr.data, by = "Age")
age.data_wide

#  HTE by BMI Table 
bmi.data <- read_excel('./results/ibu-aki-bmi-ATT-no-spline.xlsx')
bmi.data <- bmi.data %>%
  mutate(
    formatted = paste0(round(Margin, 2), " (", 
                       round(LB, 2), "–", 
                       round(UB, 2), ")"),
    Drug = ifelse(Pain == 1, "Ibuprofen", "Oxycodone")
  ) %>%
  filter(BMI != 50)

# reshape estimated IR
bmi.data_wide <- bmi.data %>%
  select(BMI, Drug, formatted) %>%
  pivot_wider(names_from = Drug,
              values_from = formatted,
              names_glue = "{Drug} est IR (95%CI)")
# load differences in IR
bmi.ird.data <- read_excel('./results/ibu-aki-bmi-ATT-IRD.xlsx')
bmi.ird.data <- bmi.ird.data %>%
  mutate(
    IRD = paste0(round(IRD, 2), " (", 
                 round(LB, 2), "–", 
                 round(UB, 2), ")")) %>%
  rename(BMI = bmi) %>%
  select(c(BMI, IRD))

# load ratios of IR
bmi.irr.data <- read_excel('./results/ibu-aki-bmi-ATT-IRR.xlsx')
bmi.irr.data <- bmi.irr.data %>%
  mutate(
    IRR = paste0(round(IRR, 2), " (", 
                 round(LB, 2), "–", 
                 round(UB, 2), ")")) %>%
  rename(BMI = bmi) %>%
  select(c(BMI, IRR))

# merge
bmi.data_wide <- bmi.data_wide %>%
  left_join(bmi.ird.data, by = "BMI") %>%
  left_join(bmi.irr.data, by = "BMI")
bmi.data_wide
    