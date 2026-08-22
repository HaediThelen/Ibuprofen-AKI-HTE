# make summary Forest for presenting IRs 

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

make_forest_plot <- function(forest.data){
  
  forest.data <- forest.data %>%
    mutate(row = cumsum(c(1, ifelse(group[-1] == group[-n()], 1, 0.5)))) %>%
    mutate(row = rev(row),
           irr_ci = sprintf("%.2f (%.2f–%.2f)", irr, irr_lb, irr_ub),
           pval_label = case_when(is.na(pval) ~ "",
                                  pval < 0.01 ~ sprintf("%.3f", pval),
                                  TRUE ~ sprintf("%.2f", pval)))
  print(forest.data)
  ymin <- min(forest.data$row) - 0.5
  ymax <- max(forest.data$row) + 1
  row_lines <- (forest.data$row[seq(1, nrow(forest.data)-1, by=2)] + 
                  forest.data$row[seq(2, nrow(forest.data), by=2)]) / 2
  make_forest_table <- function(forest.data) {
    
    p <- ggplot(forest.data, aes(y = row)) +
      geom_hline(yintercept = row_lines, color = "grey90", linewidth = 0.3) +
      geom_text(aes(x = 0, label = level), hjust = 0, size = 3.5) +
      geom_text(aes(x = 1.4, label = scales::comma(n)), hjust = 1, size = 3.2) +
      geom_text(aes(x = 2.4, label = irr_ci), hjust = 1, size = 3.2) +
      geom_text(aes(x = 2.9, label = pval_label), hjust = 1, size = 3.2) +
      annotate("text", x = 0, y = ymax, label = " ", hjust = 0, fontface = "bold", size = 3.2) +
      annotate("text", x = 1.4, y = ymax, label = "No. of\nPatients", hjust = 1, fontface = "bold", size = 3.2) +
      annotate("text", x = 2.0, y = ymax, label = "RR\n(95% CI)", hjust = 0.5, fontface = "bold", size = 3.2) +
      annotate("text", x = 3.1, y = ymax, label = "P-value for \nInteraction", hjust = 1, fontface = "bold", size = 3.2) +
      geom_segment(aes(x = 0, xend = 3.1, y = ymax - 0.25, yend = ymax - 0.25), linewidth = 0.5) +
      scale_x_continuous(limits = c(0, 3.15), expand = c(0, 0)) +
      scale_y_continuous(breaks = forest.data$row, labels = NULL, limits = c(ymin, ymax + 0.5), expand = c(0, 0)) +
      coord_cartesian(clip = "off") +
      theme_classic() +
      theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
            axis.title = element_blank(), plot.margin = margin(t = 1, r = 0, b = 5, l = 0))
    
    return(p)
  }
  fp_table <- make_forest_table(forest.data)
  
  make_forest_plot_panel <- function(forest.data) {
    
    p <- ggplot(forest.data, aes(y = row, x = irr)) +
      geom_hline(yintercept = row_lines, color = "grey90", linewidth = 0.3) +
      geom_segment(aes(x = irr_lb, xend = irr_ub, y = row, yend = row), linewidth = 0.7) +
      geom_point(shape = 15, size = 3) +
      geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.6) +
      annotate("text", x = 0.65, y = ymax, label = "Decreased AKI Risk\nwith Ibuprofen", fontface = "bold", size = 3.2) +
      annotate("text", x = 1.8, y = ymax, label = "Increased AKI Risk\nwith Ibuprofen", fontface = "bold", size = 3.2) +
      scale_x_log10(
        # breaks = c(0.5, 1, 1.5, 2, 2.5, 3),
        #             labels = c("0.5", "1", "1.5", "2", "2.5", "3")
      ) +
      coord_cartesian(xlim = c(0.5, 3), clip = "off") +
      annotation_logticks(sides = "b") +
      scale_y_continuous(breaks = forest.data$row, labels = NULL,
                         limits = c(ymin, ymax + 0.5), expand = c(0, 0)) +
      labs(x = "IRR (95% CI)", y = NULL) +
      theme_classic() +
      theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.y = element_blank(), axis.title.y = element_blank(),
            plot.margin = margin(t = 1, r = 0, b = 5, l = 0))
    
    return(p)
  }
  fp_plot <- make_forest_plot_panel(forest.data)
  fp <- (fp_table | fp_plot) + plot_layout(ncol = 2, widths = c(1.3, 1.2))
  fp
}

# Figure 2: main analysis results 
# Supplementary analysis adjusting for IV opioids in baseline period
# read in data
overall <- read_excel("./results/ibu-aki-overall.xlsx",.name_repair = "minimal") 
names(overall) <- c( "level",
                            "oxy", "oxy_lb", "oxy_ub", 
                            "ibu", "ibu_lb", "ibu_ub",
                            "rd", "rd_lb", "rd_ub",
                            "irr", "irr_lb", "irr_ub")
overall.irr <- overall %>%
  #make dummy cols for level and pval
  mutate(pval = NA) %>%
  select(level, irr, irr_lb, irr_ub,pval)
overall.irr

# Column names for effect modification tables
col.names.irr <- c("level", "oxy", "oxy_lb", "oxy_ub", "ibu", "ibu_lb", "ibu_ub",
                   "rd", "rd_lb", "rd_ub", "did", "did_lb", "did_ub",
                   "irr", "irr_lb", "irr_ub", "ratio_irr", "ratio_irr_lb",
                   "ratio_irr_ub", "pval")

periOp <- read_excel("./results/ibu-aki-periOp-bin-ATT.xlsx",.name_repair = "minimal")
names(periOp) <- col.names.irr
periOp.irr <- periOp %>%
  select(level, irr, irr_lb, irr_ub, pval)

icu <- read_excel("./results/ibu-aki-icuCurrent-ATT.xlsx",.name_repair = "minimal")
names(icu) <- col.names.irr
icu.irr <- icu %>%
  select(level, irr, irr_lb, irr_ub, pval)

hf <- read_excel("./results/ibu-aki-hf-ATT.xlsx",.name_repair = "minimal")
names(hf) <- col.names.irr
hf.irr <- hf %>%
  select(level, irr, irr_lb, irr_ub, pval)


dm <- read_excel("./results/ibu-aki-dm-ATT.xlsx",.name_repair = "minimal")
names(dm) <- col.names.irr
dm.irr <- dm %>%
  select(level, irr, irr_lb, irr_ub, pval)

data <- read_dta("./data/ibu-aki-data.dta") 

data <- data %>%
  mutate(across(where(is.numeric), as.numeric)) %>%
  mutate(across(where(~ all(. %in% c(0, 1))), as.integer)) %>%
  mutate(periOp.no = if_else(periOp == 0, 1, 0),
         periOp.0 = if_else(periOp == 1, 1, 0),
         periOp.1 = if_else(periOp == 2, 1, 0),
         periOp.2 = if_else(periOp == 3,1,0),
         periOp.3 = if_else(periOp == 4,1,0)) %>%
  mutate(dm.no = if_else(dm == 0, 1, 0),
         dm.noncomp = if_else(dm == 1, 1, 0),
         dm.comp = if_else(dm == 2, 1, 0)) %>%
  mutate(periOp.bin = if_else(periOp ==0, 0, 1)) %>%
  mutate(periOp.bin = as.factor(periOp.bin)) %>%
  mutate(icuCurrent.cat = as.factor(icuCurrent)) %>%
  mutate(dm.bin = if_else(dm ==0, 0, 1)) %>%
  mutate(dm.bin = as.factor(dm.bin)) %>%
  mutate(chf.cat = as.factor(chf))

forest.n <- data %>%
  reframe(
    n = c(
      n(),
      sum(icuCurrent.cat == 0, na.rm = TRUE),
      sum(icuCurrent.cat == 1, na.rm = TRUE),
      sum(periOp.bin == 0, na.rm = TRUE),
      sum(periOp.bin == 1, na.rm = TRUE),
      sum(chf.cat == 0, na.rm = TRUE),
      sum(chf.cat == 1, na.rm = TRUE),
      sum(dm.bin == 0, na.rm = TRUE),
      sum(dm.bin == 1, na.rm = TRUE)))

forest.data <- bind_rows(overall.irr, icu.irr, periOp.irr, hf.irr, dm.irr)
forest.data <- forest.data %>% 
  mutate(n = forest.n$n,
         group = c("Overall", "ICU Current", "ICU Current", "Perioperative", "Perioperative", "HF", "HF", "DM", "DM"),
         level = case_when(
           level == "Not in ICU" ~ "Not Critically Ill",
           level == "In ICU" ~ "Critically Ill",
           level == "Not PostOp" ~ "Not Postoperative",
           level == "PostOp" ~ "Postoperative",
           level == "No HF" ~ "No Heart Failure",
           level == "HF" ~ "Heart Failure",
           level == "No DM" ~ "No Diabetes",
           level == "DM" ~ "Diabetes",
           TRUE ~ level))
forest.data


fp_main <- make_forest_plot(forest.data)
fp_main

# Save
if (!dir.exists("./results/Summary")) {
  dir.create("./results/Summary", recursive = TRUE)
}
ggsave(filename = "./results/Summary/Forest-Plot-bin-covs.jpeg", device = "jpeg", 
       width = 7, height = 6, units = "in", dpi = 300)
ggsave(filename = "./results/Summary/Forest-Plot-bin-covs.pdf", device = "pdf", 
       width = 7, height = 6, units = "in", dpi = 300)     


# Supplementary analysis adjusting for IV opioids in baseline period
# read in data
overall.opBase <- read_excel("./results/ibu-aki-overall-opBase.xlsx",.name_repair = "minimal") 
names(overall.opBase) <- c( "level",
                            "oxy", "oxy_lb", "oxy_ub", 
                            "ibu", "ibu_lb", "ibu_ub",
                            "rd", "rd_lb", "rd_ub",
                            "irr", "irr_lb", "irr_ub")
overall.opBase.irr <- overall.opBase %>%
  #make dummy cols for level and pval
  mutate(pval = NA) %>%
  select(level, irr, irr_lb, irr_ub,pval)
overall.opBase.irr

icu.opBase <- read_excel("./results/ibu-aki-icuCurrent-ATT-opBase.xlsx",.name_repair = "minimal")
periOp.opBase <- read_excel("./results/ibu-aki-periOp-bin-ATT-opBase.xlsx",.name_repair = "minimal")

col.names.irr <- c("level", "oxy", "oxy_lb", "oxy_ub", "ibu", "ibu_lb", "ibu_ub",
                   "rd", "rd_lb", "rd_ub", "did", "did_lb", "did_ub",
                   "irr", "irr_lb", "irr_ub", "ratio_irr", "ratio_irr_lb",
                   "ratio_irr_ub", "pval")
names(periOp.opBase) <- col.names.irr

periOp.opBase.irr <- periOp.opBase %>%
  select(level, irr, irr_lb, irr_ub, pval)
names(icu.opBase) <- col.names.irr
icu.opBase.irr <- icu.opBase %>%
  select(level, irr, irr_lb, irr_ub, pval)

data <- read_dta("./data/ibu-aki-data.dta") 

data <- data %>%
  mutate(across(where(is.numeric), as.numeric)) %>%
  mutate(across(where(~ all(. %in% c(0, 1))), as.integer)) %>%
  mutate(periOp.no = if_else(periOp == 0, 1, 0),
         periOp.0 = if_else(periOp == 1, 1, 0),
         periOp.1 = if_else(periOp == 2, 1, 0),
         periOp.2 = if_else(periOp == 3,1,0),
         periOp.3 = if_else(periOp == 4,1,0)) %>%
  mutate(dm.no = if_else(dm == 0, 1, 0),
         dm.noncomp = if_else(dm == 1, 1, 0),
         dm.comp = if_else(dm == 2, 1, 0)) %>%
  mutate(periOp.bin = if_else(periOp ==0, 0, 1)) %>%
  mutate(periOp.bin = as.factor(periOp.bin)) %>%
  mutate(icuCurrent.cat = as.factor(icuCurrent)) 
 
forest.n.opBase <- data %>%
  reframe(
    n = c(
      n(),
      sum(icuCurrent.cat == 0, na.rm = TRUE),
      sum(icuCurrent.cat == 1, na.rm = TRUE),
      sum(periOp.bin == 0, na.rm = TRUE),
      sum(periOp.bin == 1, na.rm = TRUE)))

forest.data.opBase <- bind_rows(overall.opBase.irr, icu.opBase.irr, periOp.opBase.irr)
forest.data.opBase <- forest.data.opBase %>% 
  mutate(n = forest.n.opBase$n,
         group = c("Overall", "ICU Current", "ICU Current", "Perioperative", "Perioperative"),
         level = case_when(
           level == "Not in ICU" ~ "Not Critically Ill",
           level == "In ICU" ~ "Critically Ill",
           level == "Not PostOp" ~ "Not Postoperative",
           level == "PostOp" ~ "Postoperative",
           TRUE ~ level))
forest.data.opBase


make_forest_plot(forest.data.opBase)

# Save
if (!dir.exists("./results/SuppAnalyses")) {
  dir.create("./results/SuppAnalyses", recursive = TRUE)
}
ggsave(filename = "./results/SuppAnalyses/Forest-Plot-BaseOp.jpeg", device = "jpeg", 
       width = 7, height = 3.9, units = "in", dpi = 300)
ggsave(filename = "./results/SuppAnalyses/Forest-Plot-BaseOp.pdf", device = "pdf", 
       width = 7, height = 3.9, units = "in", dpi = 300)  
ggsave(filename = "./results/SuppAnalyses/Forest-Plot-BaseOp.svg", device = svglite, 
       width = 7, height = 3.9, units = "in", dpi = 300)  

# Supplementary analysis excluding patietns who received IV opioids in the baseline period
# Supplementary analysis adjusting for IV opioids in baseline period
# read in data
overall.opBasePOonly <- read_excel("./results/ibu-aki-overall-opBasePOonly.xlsx",.name_repair = "minimal") 
names(overall.opBasePOonly) <- c( "level",
                            "oxy", "oxy_lb", "oxy_ub", 
                            "ibu", "ibu_lb", "ibu_ub",
                            "rd", "rd_lb", "rd_ub",
                            "irr", "irr_lb", "irr_ub")
overall.opBasePOonly.irr <- overall.opBasePOonly %>%
  #make dummy cols for level and pval
  mutate(pval = NA) %>%
  select(level, irr, irr_lb, irr_ub,pval)
overall.opBasePOonly.irr

icu.opBasePOonly <- read_excel("./results/ibu-aki-icuCurrent-ATT-opBasePOonly.xlsx",.name_repair = "minimal")
periOp.opBasePOonly <- read_excel("./results/ibu-aki-periOp-bin-ATT-opBasePOonly.xlsx",.name_repair = "minimal")

names(periOp.opBasePOonly) <- col.names.irr

periOp.opBasePOonly.irr <- periOp.opBasePOonly %>%
  select(level, irr, irr_lb, irr_ub, pval)
names(icu.opBasePOonly) <- col.names.irr
icu.opBasePOonly.irr <- icu.opBasePOonly %>%
  select(level, irr, irr_lb, irr_ub, pval)

data <- read_dta("./data/ibu-aki-data.dta") 

data <- data %>%
  mutate(across(where(is.numeric), as.numeric)) %>%
  mutate(across(where(~ all(. %in% c(0, 1))), as.integer)) %>%
  mutate(periOp.no = if_else(periOp == 0, 1, 0),
         periOp.0 = if_else(periOp == 1, 1, 0),
         periOp.1 = if_else(periOp == 2, 1, 0),
         periOp.2 = if_else(periOp == 3,1,0),
         periOp.3 = if_else(periOp == 4,1,0)) %>%
  mutate(dm.no = if_else(dm == 0, 1, 0),
         dm.noncomp = if_else(dm == 1, 1, 0),
         dm.comp = if_else(dm == 2, 1, 0)) %>%
  mutate(periOp.bin = if_else(periOp ==0, 0, 1)) %>%
  mutate(periOp.bin = as.factor(periOp.bin)) %>%
  mutate(icuCurrent.cat = as.factor(icuCurrent))%>%
  filter(opBaseIV == 0 & opBasePCA == 0, opBaseGTT == 0, opBasePatch ==0)

forest.opBasePOonly.n <- data %>%
  reframe(
    n = c(
      n(),
      sum(icuCurrent.cat == 0, na.rm = TRUE),
      sum(icuCurrent.cat == 1, na.rm = TRUE),
      sum(periOp.bin == 0, na.rm = TRUE),
      sum(periOp.bin == 1, na.rm = TRUE)))
forest.opBasePOonly.n

forest.opBasePOonly.data <- bind_rows(overall.opBasePOonly.irr, icu.opBasePOonly.irr, periOp.opBasePOonly.irr)
forest.opBasePOonly.data <- forest.opBasePOonly.data %>% 
  mutate(n = forest.opBasePOonly.n$n,
         group = c("Overall", "ICU Current", "ICU Current", "Perioperative", "Perioperative"),
         level = case_when(
           level == "Not in ICU" ~ "Not Critically Ill",
           level == "In ICU" ~ "Critically Ill",
           level == "Not PostOp" ~ "Not Postoperative",
           level == "PostOp" ~ "Postoperative",
           TRUE ~ level))
forest.opBasePOonly.data
forest.opBasePOonly <- make_forest_plot(forest.opBasePOonly.data)
forest.opBasePOonly

ggsave(filename = "./results/SuppAnalyses/Forest-Plot-BaseOpPOonly.jpeg", device = "jpeg", 
       width = 7, height = 3.9, units = "in", dpi = 300)
ggsave(filename = "./results/SuppAnalyses/Forest-Plot-BaseOpPOonly.pdf", device = "pdf", 
       width = 7, height = 3.9, units = "in", dpi = 300) 
ggsave(filename = "./results/SuppAnalyses/Forest-Plot-BaseOpPOonly.svg", device = svglite, 
       width = 7, height = 3.9, units = "in", dpi = 300) 


# Supplementary analysis evaluating effect modification by any concomitant neprhotoxin
# read in data
overall <- read_excel("./results/ibu-aki-overall.xlsx",.name_repair = "minimal") 
names(overall) <- c( "level",
                            "oxy", "oxy_lb", "oxy_ub", 
                            "ibu", "ibu_lb", "ibu_ub",
                            "rd", "rd_lb", "rd_ub",
                            "irr", "irr_lb", "irr_ub")
overall.irr <- overall%>%
  #make dummy cols for level and pval
  mutate(pval = NA) %>%
  select(level, irr, irr_lb, irr_ub,pval)
overall.irr

anyNtx <- read_excel("./results/ibu-aki-anyNtx-ATT.xlsx",.name_repair = "minimal")
names(anyNtx) <- col.names.irr
anyNtx.irr <- anyNtx %>%
  select(level, irr, irr_lb, irr_ub, pval)

data <- read_dta("./data/ibu-aki-data.dta") 

data <- data %>%
  mutate(across(where(is.numeric), as.numeric)) %>%
  mutate(across(where(~ all(. %in% c(0, 1))), as.integer)) %>%
  mutate(periOp.no = if_else(periOp == 0, 1, 0),
         periOp.0 = if_else(periOp == 1, 1, 0),
         periOp.1 = if_else(periOp == 2, 1, 0),
         periOp.2 = if_else(periOp == 3,1,0),
         periOp.3 = if_else(periOp == 4,1,0)) %>%
  mutate(dm.no = if_else(dm == 0, 1, 0),
         dm.noncomp = if_else(dm == 1, 1, 0),
         dm.comp = if_else(dm == 2, 1, 0)) %>%
  mutate(periOp.bin = if_else(periOp ==0, 0, 1)) %>%
  mutate(periOp.bin = as.factor(periOp.bin)) %>%
  mutate(icuCurrent.cat = as.factor(icuCurrent))%>%
  mutate(anyNtx = if_else(abxNTX == 1 | ntxOther ==1 |vancoBase ==1, 1, 0)) %>%
  mutate(anyNtx.cat = as.factor(anyNtx))

forest.anyNtx.data <- bind_rows(overall.irr, anyNtx.irr)

forest.anyNtx.n <- data %>%
  reframe(
    n = c(
      n(),
      sum(anyNtx.cat == 0, na.rm = TRUE),
      sum(anyNtx.cat == 1, na.rm = TRUE)))
forest.anyNtx.n

forest.anyNtx.data <- forest.anyNtx.data %>% 
  mutate(n = forest.anyNtx.n$n,
         group = c("Overall",  "Ntx", "Ntx"),
         level = case_when(
           level == "No Ntx" ~ "No Nephrotoxin",
           level == "Ntx" ~ "With Nephrotoxin",
           TRUE ~ level))
forest.anyNtx.data
forest.anyNtx<- make_forest_plot(forest.anyNtx.data)
forest.anyNtx

ggsave(filename = "./results/SuppAnalyses/Forest-Plot-anyNtx.jpeg", device = "jpeg", 
       width = 7, height = 2.9, units = "in", dpi = 300)
ggsave(filename = "./results/SuppAnalyses/Forest-Plot-anyNtx.pdf", device = "pdf", 
       width = 7, height = 3.9, units = "in", dpi = 300) 

ggsave(filename = "./results/SuppAnalyses/Forest-Plot-anyNtx.svg", device = svglite, 
       width = 7, height = 2.9, units = "in", dpi = 300) 
