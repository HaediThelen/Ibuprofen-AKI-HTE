# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(magick)
library(ggpubr)
library(tidyr)
library(cowplot)

# Set WD 
#setwd()

# Plot Function: ----
c.results.plot <- function(data, x_axis, y_axis, x_lab, y_lab, ixn_p) {
  ggplot(data, aes(x = {{x_axis}}, y = {{y_axis}}, color = Pain, group = Pain, shape = Pain)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = LB, ymax = UB), width = 2) +
    scale_color_manual(values = c("Oxycodone" = "blue3", "Ibuprofen" = "deeppink3")) +
    scale_x_continuous(breaks = unique(data %>% pull({{x_axis}}))) +
    ylim(0,155) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = paste0("P-value for Interaction: ", signif(ixn_p, 2)),
      hjust = 1.1,
      vjust = 2.5,
      size = 3.5
    ) +
    labs(
      x = x_lab,
      y = y_lab,
      color = "Treatment Group",
      shape = "Treatment Group"
    ) +
    theme_minimal(base_size = 11) + 
    theme(
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 11)
    )
}  

getwd()

# Composite Result Plots with Splines----

# File path
# Read Excel file
egfr.data.spline <- read_excel("./results/ibu-aki-GFR-ATT-spline.xlsx")

# Prepare data
egfr.ixn.p.spline <- egfr.data.spline$`Ixn p value`[1]
egfr.data.spline <- egfr.data.spline %>%
  select(-`Ixn p value`)
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

egfr.plot.spline <-c.results.plot(egfr.data.spline, x_axis=eGFR, y_axis = Margin, x_lab = expression("eGFR (ml/min/1.73 m"^2*")"), y_lab = "AKI Incidence Rate", egfr.ixn.p.spline)
egfr.plot.spline

# Age

# Read Excel file
age.data.spline <- read_excel("./results/ibu-aki-age-ATT-spline.xlsx")

# Prepare data
age.ixn.p.spline <- age.data.spline$`Ixn p value`[1]
age.data.spline <- age.data.spline %>%
  select(-`Ixn p value`)
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
age.plot.spline <-c.results.plot(age.data.spline, x_axis=Age, y_axis = Margin, x_lab = "Age (years)", y_lab = "AKI Incidence Rate", age.ixn.p.spline)
age.plot.spline

# BMI

# Read Excel file
BMI.data.spline <- read_excel("./results/ibu-aki-bmi-ATT-spline.xlsx")

# Prepare data
BMI.ixn.p.spline <- BMI.data.spline$`Ixn p value`[1]
BMI.data.spline <- BMI.data.spline %>%
  select(-`Ixn p value`)
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
bmi.plot.spline <-c.results.plot(BMI.data.spline, x_axis=BMI, y_axis = Margin, x_lab = expression("BMI (kg/m"^2*")"), y_lab = "AKI Incidence Rate", BMI.ixn.p.spline)
bmi.plot.spline

# Stack the 3 plots vertically with a shared legend
combined <- plot_grid(
  egfr.plot.spline + theme(legend.position = "none"),
  age.plot.spline + theme(legend.position = "none"),
  bmi.plot.spline + theme(legend.position = "none"),
  ncol = 1,
  align = "v",
  labels = "AUTO"
)

# Extract legend from one of the plots
g <- ggplotGrob(
  egfr.plot.spline +
    theme(legend.position = "bottom")
)

legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]

# add legend below
composite.spline <- plot_grid(
  combined,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.1)   # adjust 0.1 to give legend more/less space
)

# aAd title
composite.spline <- ggdraw() +
  draw_label(
    " ",
    fontface = "bold",
    size = 12,
    y = 1, vjust = 1
  ) +
  draw_plot(composite.spline, y = 0, height = 0.95)  # adjust height to make room for title

composite.spline


if (!dir.exists("./results/summary")) {
  dir.create("./results/summary", recursive = TRUE)
}
ggsave(filename = './results/summary/composite-plot-spline-pval.jpeg',
       plot = composite.spline, device = "jpeg",
       width = 5, height = 9, units = "in", dpi = 300)
ggsave(filename = './results/summary/composite-plot-spline-pval.pdf',
       plot = composite.spline, device = "pdf",
       width = 5, height = 9, units = "in", dpi = 300)



