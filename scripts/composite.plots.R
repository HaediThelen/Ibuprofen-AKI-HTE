# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(magick)
library(ggpubr)
library(tidyr)

# Set WD 
#TEMPORARY
setwd('/Users/haedi/Library/CloudStorage/Box-Box/Data/NSAID-AKI/')

# Plot Function: 
c.results.plot <- function(data, x_axis, y_axis, x_lab, y_lab) {
  ggplot(data, aes(x = {{x_axis}}, y = {{y_axis}}, color = Pain, group = Pain, shape = Pain)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = LB, ymax = UB), width = 2) +
    scale_color_manual(values = c("Oxycodone" = "blue3", "Ibuprofen" = "deeppink3")) +
    scale_x_continuous(breaks = unique(data %>% pull({{x_axis}}))) +
    ylim(0,50) +
    labs(
      x = x_lab,
      y = y_lab,
      color = "Treatment Group",
      shape = "Treatment Group"
    ) +
    theme_minimal()
}

# Composite Result Plots (No Splines)
egfr.data <- read_excel('./results/ibu-aki-GFR-ATT-no-spline.xlsx')
egfr.data <- egfr.data %>%
  mutate(
    eGFR = as.numeric(eGFR),
    Pain = recode(as.factor(Pain), `0` = "Oxycodone", `1` = "Ibuprofen"),  
    Margin = as.numeric(Margin),
    std. = as.numeric(`std.err.`),
    LB = as.numeric(LB),
    UB = as.numeric(UB)
  )  %>%
  filter(!eGFR %in% c(20, 150))

egfr.plot <- c.results.plot(egfr.data, x_axis=eGFR, y_axis = Margin, x_lab = expression("eGFR (ml/min/1.73 m"^2*")"), y_lab = " AKI Incidence Rate")
egfr.plot 

# Age
age.data <- read_excel('./results/ibu-aki-age-ATT-no-spline.xlsx')
age.data <- age.data %>%
  mutate(
    Age = as.numeric(Age),
    Pain = recode(as.factor(Pain), `0` = "Oxycodone", `1` = "Ibuprofen"),
    Margin = as.numeric(Margin),
    std. = as.numeric(`std.err.`),
    LB = as.numeric(LB),
    UB = as.numeric(UB)
  ) %>%
  filter(!Age %in% c(20, 100,110))

# Plot
age.plot <- c.results.plot(age.data, x_axis=Age, y_axis = Margin, x_lab = "Age (years)", y_lab = "AKI Incidence Rate")
age.plot 

# BMI
BMI.data <- read_excel('./results/ibu-aki-bmi-ATT-no-spline.xlsx')
BMI.data <- BMI.data %>%
  mutate(
    BMI = as.numeric(BMI),
    Pain = recode(as.factor(Pain), `0` = "Oxycodone", `1` = "Ibuprofen"),
    Margin = as.numeric(Margin),
    std. = as.numeric(`std.err.`),
    LB = as.numeric(LB),
    UB = as.numeric(UB)
  ) %>%
  filter(BMI != 50)
bmi.plot <- c.results.plot(BMI.data, x_axis=BMI, y_axis = Margin, x_lab = expression("BMI (kg/m"^2*")"), y_lab = "AKI Incidence Rate")
bmi.plot

# Composite Result (no splines)
composite <- ggarrange(
  plotlist = list(egfr.plot, age.plot, bmi.plot),
  ncol = 1,  
  common.legend = TRUE,
  legend = "bottom",
  align = "v",
  labels = "AUTO"
)
composite <- annotate_figure(composite, top = text_grob("Estimated Incidence Rate with 95% CI by Treatment Group", face = "bold", size = 12))
composite
#TEMPORARY
setwd('/Users/haedi/Library/CloudStorage/Box-Box/Repos/Ibuprofen-AKI-HTE')
if (!dir.exists("./results/summary")) {
  dir.create("./results/summary", recursive = TRUE)
}
ggsave(filename = './results/summary/composite-plot.jpeg',
       plot = composite, device = "jpeg",
       width = 5, height = 9, units = "in", dpi = 300)
ggsave(filename = './results/summary/composite-plot.pdf',
       plot = composite, device = "pdf",
       width = 5, height = 9, units = "in", dpi = 300)


# Composite Result Plots with Splines

# File path
# Read Excel file
#TEMPORARY
setwd('/Users/haedi/Library/CloudStorage/Box-Box/Data/NSAID-AKI')
egfr.data.spline <- read_excel("./results/ibu-aki-GFR-ATT-spline.xlsx")

# Prepare data
egfr.data.spline <- egfr.data.spline %>%
  mutate(
    eGFR = as.numeric(eGFR),
    Pain = recode(as.factor(Pain), `0` = "Oxycodone", `1` = "Ibuprofen"),  
    Margin = as.numeric(Margin),
    std.err. = as.numeric(`std.err.`),
    LB = as.numeric(LB),
    UB = as.numeric(UB)
  )  %>%
  filter(!eGFR %in% c(20, 150))

# Plot

egfr.plot.spline <-c.results.plot(egfr.data.spline, x_axis=eGFR, y_axis = Margin, x_lab = expression("eGFR (ml/min/1.73 m"^2*")"), y_lab = "AKI Incidence Rate")
egfr.plot.spline

# Age

# Read Excel file
age.data.spline <- read_excel("./results/ibu-aki-age-ATT-spline.xlsx")

# Prepare data
age.data.spline <- age.data.spline %>%
  mutate(
    Age = as.numeric(Age),
    Pain = recode(as.factor(Pain), `0` = "Oxycodone", `1` = "Ibuprofen"),
    Margin = as.numeric(Margin),
    std.err. = as.numeric(`std.err.`),
    LB = as.numeric(LB),
    UB = as.numeric(UB)
  ) %>%
  filter(!Age %in% c(20, 100,110))

# Plot
age.plot.spline <-c.results.plot(age.data.spline, x_axis=Age, y_axis = Margin, x_lab = "Age (years)", y_lab = "AKI Incidence Rate")
age.plot.spline

# BMI

# Read Excel file
BMI.data.spline <- read_excel("./results/ibu-aki-bmi-ATT-spline.xlsx")

# Prepare data
BMI.data.spline <- BMI.data.spline %>%
  mutate(
    BMI = as.numeric(BMI),
    Pain = recode(as.factor(Pain), `0` = "Oxycodone", `1` = "Ibuprofen"),
    Margin = as.numeric(Margin),
    std.err. = as.numeric(`std.err.`),
    LB = as.numeric(LB),
    UB = as.numeric(UB)
  ) %>%
  filter(BMI != 50)

# Plot
bmi.plot.spline <-c.results.plot(BMI.data.spline, x_axis=BMI, y_axis = Margin, x_lab = expression("BMI (kg/m"^2*")"), y_lab = "AKI Incidence Rate")
bmi.plot.spline

composite.spline <- ggarrange(
  plotlist = list(egfr.plot.spline, age.plot.spline, bmi.plot.spline),
  ncol = 1,  
  common.legend = TRUE,
  legend = "bottom",
  align = "v",
  labels = "AUTO"
)
composite.spline <- annotate_figure(composite.spline, top = text_grob("Estimated Incidence Rate with 95% CI by Treatment Group", face = "bold", size = 12))
composite.spline


#TEMPORARY
setwd('/Users/haedi/Library/CloudStorage/Box-Box/Repos/Ibuprofen-AKI-HTE')

if (!dir.exists("./results/summary")) {
  dir.create("./results/summary", recursive = TRUE)
}
ggsave(filename = './results/summary/composite-plot-spline.jpeg',
       plot = composite.spline, device = "jpeg",
       width = 5, height = 9, units = "in", dpi = 300)
ggsave(filename = './results/summary/composite-plot-spline.pdf',
       plot = composite.spline, device = "pdf",
       width = 5, height = 9, units = "in", dpi = 300)



