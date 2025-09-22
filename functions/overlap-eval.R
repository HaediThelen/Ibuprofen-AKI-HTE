# Covariate Evaluation Functions
library(haven)
library(tableone)
library(survey)

# Examine the SMDs of all covariates
# Requires all data be formatted already (factored, etc)
smd.table <- function(data, covs){
  surv <- svydesign(ids = ~ ddiID, strata = ~ pain, nest = T, data = data)
  table <- svyCreateTableOne(vars = covs, strata = "pain", test = F, data = surv)
  smd.table <- print(table, smd = TRUE, varLabels=T, test = F, showAllLevels = F)
 
  return(smd.table)
  
}

# Examine Density functions of continuous covs
  # function makes a density plot for each cov in covs
  # the tx var indicates the column of treatment needs to be a factor
dplots <-function(data, covs, tx){
  plot.list <- list()
  
  for (cov in covs) {
    #plot.data <- data[[cov]]  # Extract data for the current covariate
    density.plot <- ggplot(data, aes(x = .data[[cov]], group = .data[[tx]],
                                     fill = .data[[tx]])) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Density Plot of", cov),
           x = cov,
           y = "Density") +
      theme_minimal()
    
    max.values <- data %>%
      group_by(.data[[tx]]) %>%
      summarise(max.value = max(.data[[cov]]))
    
    min.values <- data %>%
      group_by(.data[[tx]]) %>%
      summarise(min.value = min(.data[[cov]]))
    
    max.lines <- geom_vline(data = max.values, aes(xintercept = max.value, color = .data[[tx]]), linetype = "dashed")
    min.lines <- geom_vline(data = min.values, aes(xintercept = min.value, color = .data[[tx]]), linetype = "dashed")
    
    density.plot <- density.plot + max.lines + min.lines
    
    print(density.plot)
     plot.list[[cov]] <- density.plot
    
  }
  return(plot.list)
}

# Function to calculate 5 number summaries of individual continuous covs 
sum.tab.indiv <- function(data, cov, tx) {
 
  # Break data into tx an ctrl
  data.tx <- data %>% filter(.data[[tx]] == 1)
  data.ctl <- data %>% filter(.data[[tx]] == 0)
  
  # make summary table
  tx.sum <- summary(data.tx[[cov]])
  ctl.sum <- summary(data.ctl[[cov]])
  ov.sum <- summary(data[[cov]])
  
  sum.table <- data.frame(
    Group = c("Treatment", "Control", "Overall"),
    Min = c(tx.sum[1], ctl.sum[1], ov.sum[1]),
    Q1 = c(tx.sum[2], ctl.sum[2], ov.sum[2]),
    Median = c(tx.sum[3], ctl.sum[3], ov.sum[3]),
    Mean = c(tx.sum[4], ctl.sum[4], ov.sum[4]),
    Q3 = c(tx.sum[5], ctl.sum[5], ov.sum[5]),
    Max = c(tx.sum[6], ctl.sum[6], ov.sum[6]),
    SD = c(sd(data.tx[[cov]]), sd(data.ctl[[cov]]), sd(data[[cov]]))
  )
  as.data.frame(sum.table)
  return(sum.table)
}

# Function to calculate 5 number summaries on all continuous covs
sum.tabs <- function(data, covs, tx) {
  sum.list <- list()
  
  for(cov in covs){
    cov.sum <- sum.tab.indiv(data, cov, tx)
    sum.list[[cov]] <- cov.sum
    }

  return(sum.list)
}

