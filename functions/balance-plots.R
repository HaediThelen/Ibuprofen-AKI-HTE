# Balance Plot Generation
# make a funciton that creates balance plots
# arguments include: 
  # data = data set
  # weights = column of weights 
  # strata = column with indicator variable for the strata
  # treatment = treatment variable
  # list of covs
library(ggpubr)

bal.plots <- function(data, weights, strata, treatment, covs){
    # subset the data
    subsets <- data %>% group_split(.data[[strata]]) 
    
    # pull out the subsets from list of subsets
    for (i in 1:length(subsets)) {
      df <- subsets[[i]]
      name <- paste0(strata,".subset." , i)
      assign(name, df)
    }
    
    # make list including each dataframe and the initial data
    subset.list <- list()
    for (i in 1:length(subsets)){
      subset.name <- paste0(strata, ".subset.", i)
      subset.list[[i]] <- get(subset.name)
    }
    data.list <- c(list(data), subset.list)
    data.name <- deparse(substitute(data))
    data.frame.names <- c(data.name, paste0(strata, ".subset.", 1:length(subsets)))
    
    plot.list<-list()
    # for loop to make each plot and print TBR
    for (i in seq_along(data.list)){
      # for naming
      df <- data.list[[i]]
      df.name <- data.frame.names[i]
      print(df.name)
      
      # Variance 
      data.var <- df %>% group_by(.data[[treatment]]) %>% 
        summarize(across(covs, ~var(.x))) %>% as.data.frame() # if trouble with across, restart R session

     # Calculate pooled var in the overall unweighted data
     c.var <- as.numeric(data.var[1,])
     t.var <- as.numeric(data.var[2,])
     c.var <- c.var[-1]
     t.var <- t.var[-1]
     pooled.var <- sqrt((t.var + c.var)/2)
     
     # Calculate the mean in the unweighted data
     um.wt <- df %>% group_by(.data[[treatment]]) %>% 
       summarize(across(covs, ~mean(.x))) %>% as.data.frame()
     
     # Calculate the mean in the weighted data
     bal.st <- df %>% group_by(.data[[treatment]]) %>% 
       summarize(across(covs, ~ weighted.mean(.x, .data[[weights]]))) %>% as.data.frame()
     
     # Make table of unweighted means in treated, untreated, and SMD
     um.wt.tab <- matrix(NA, length(covs), 3)
     um.wt.tab[,1] <- unlist(um.wt[1,-1]) 
     um.wt.tab[,2] <- unlist(um.wt[2,-1])                        
     um.wt.tab[,3] <- (unlist(um.wt[2,-1]) - unlist(um.wt[1,-1]))/pooled.var
     
     # Make table of weighted means in treated, untreated, and SMD
     bal.wt.tab <- matrix(NA, length(covs), 3)
     bal.wt.tab[,1] <- unlist(bal.st[1,-1]) 
     bal.wt.tab[,2] <- unlist(bal.st[2,-1])                        
     bal.wt.tab[,3] <- (unlist(bal.st[2,-1]) - unlist(bal.st[1,-1]))/pooled.var 
     
     ## Rename columns using covs
     rownames(um.wt.tab) <- covs
     rownames(bal.wt.tab) <- covs
     
     # Store tables
     um.wt.tab.name <- paste0(df.name, ".um.wt.tab")
     assign(um.wt.tab.name, um.wt.tab)
     
     bal.wt.tab.name <- paste0(df.name, ".wt.tab")
     assign(bal.wt.tab.name, bal.wt.tab)
     
     # Largest Imbalances
     lg.un <-  um.wt.tab#[which(abs(um.wt.tab[,3]) > .2),]
     lg.wt <- bal.wt.tab#[which(abs(um.wt.tab[,3]) > .2),]
     
     n.covs <- nrow(lg.un)
     
     ## Total Imbalance Reduction
     um.wt.bias <- um.wt.tab[,3]
     bal.bias <- bal.wt.tab[,3] 
     pbr <- (1 - (mean(abs(bal.bias))/mean(abs(um.wt.bias))))*100
     message("Total Imbalance Reduction is: " , pbr)
     
     # Plots              			  
     data.plot <- c(lg.wt[,3], lg.un[,3])
     data.plot <- as.data.frame(data.plot)
     names(data.plot) <- "std.dif"
     data.plot$contrast <- c(rep(1, n.covs), rep(2, n.covs))
     data.plot$contrast <- factor(data.plot$contrast, levels = c(1,2), 
                                  labels = c("Weighted", "Unweighted"))
     data.plot$covariate <- as.factor(rownames(lg.un))
     
     ### BW plot
     #plot <- ggplot(data=data.plot, aes(x=std.dif, y=covariate, shape=factor(contrast)))  + 
       #geom_point(size=1.75) + 
       #scale_shape_manual(name= "Contrast", values=c(1,15)) +
       #xlab("Standardized Difference") + ylab("Covariates") +
       #ggtitle(df.name) +
       #scale_y_discrete(limits = rev(levels(data.plot$covariate))) +
       #geom_vline(xintercept= 0) +
       #geom_vline(xintercept= 0.1) +
       #geom_vline(xintercept= -0.1) +
       #theme_bw()
       
     plot <- ggplot(data = data.plot, aes(x = std.dif, y = covariate, 
                                          shape = factor(contrast), color = factor(contrast))) +
       geom_point(size = 2, shape = 16) +
       scale_shape_manual(name = "Contrast", values = c(1, 15)) +
       scale_color_manual(name = "Contrast", values = c("darkred", "darkblue")) +
       xlab("Standardized Difference") + ylab("Covariates") +
       ggtitle(df.name) +
       scale_y_discrete(limits = rev(levels(data.plot$covariate))) +
       geom_vline(xintercept = 0) +
       geom_vline(xintercept = 0.1) +
       geom_vline(xintercept = -0.1) +
       theme_bw() +
       guides(shape = FALSE)  # Hide the shape legend
     
       
       print(plot)
       
       
     print(plot)
     
     plot.name <- paste0(df.name, ".plot")
     assign(plot.name, plot)
     plot.list[[i]] <- plot
     
     }
    return(plot.list)
}

bal.plot.clean <- function(data, weights, strata, treatment, covs, subset = TRUE, main.title = NULL) {
  
  if (subset) {
    # subset the data
    subsets <- data %>% group_split(.data[[strata]])
    
    # pull out the subsets from list of subsets
    for (i in 1:length(subsets)) {
      df <- subsets[[i]]
      name <- paste0(strata,".subset." , i)
      assign(name, df)
    }
    
    # make list including each dataframe and the initial data
    subset.list <- list()
    for (i in 1:length(subsets)){
      subset.name <- paste0(strata, ".subset.", i)
      subset.list[[i]] <- get(subset.name)
    }
    data.list <- c(list(data), subset.list)
    data.name <- deparse(substitute(data))
    data.frame.names <- c(data.name, paste0(strata, ".subset.", 1:length(subsets)))
  } else {
    # Only use the overall data
    data.list <- list(data)
    data.name <- deparse(substitute(data))
    data.frame.names <- data.name
  }
  
  plot.list <- list()
  
  # get xlim 
  all.std.dif <- c()
  
  for (df in data.list) {
    data.var <- df %>% group_by(.data[[treatment]]) %>%
      summarize(across(covs, ~var(.x))) %>% as.data.frame()
    
    c.var <- as.numeric(data.var[1, -1])
    t.var <- as.numeric(data.var[2, -1])
    pooled.var <- sqrt((t.var + c.var) / 2)
    
    um.wt <- df %>% group_by(.data[[treatment]]) %>%
      summarize(across(covs, ~mean(.x))) %>% as.data.frame()
    
    bal.st <- df %>% group_by(.data[[treatment]]) %>%
      summarize(across(covs, ~ weighted.mean(.x, .data[[weights]]))) %>% as.data.frame()
    
    unweighted_smd <- (unlist(um.wt[2, -1]) - unlist(um.wt[1, -1])) / pooled.var
    weighted_smd <- (unlist(bal.st[2, -1]) - unlist(bal.st[1, -1])) / pooled.var
    
    all.std.dif <- c(all.std.dif, unweighted_smd, weighted_smd)
  }
  
  xlim <- range(all.std.dif, na.rm = TRUE)
  print(xlim)
  # for loop to make each plot and print TBR
  for (i in seq_along(data.list)) {
    df <- data.list[[i]]
    df.name <- data.frame.names[i]
    print(df.name)
    
    # Variance 
    data.var <- df %>% group_by(.data[[treatment]]) %>% 
      summarize(across(covs, ~var(.x))) %>% as.data.frame() # if trouble with across, restart R session
    
    # Calculate pooled var in the overall unweighted data
    c.var <- as.numeric(data.var[1,])
    t.var <- as.numeric(data.var[2,])
    c.var <- c.var[-1]
    t.var <- t.var[-1]
    pooled.var <- sqrt((t.var + c.var)/2)
    
    # Calculate the mean in the unweighted data
    um.wt <- df %>% group_by(.data[[treatment]]) %>% 
      summarize(across(covs, ~mean(.x))) %>% as.data.frame()
    
    # Calculate the mean in the weighted data
    bal.st <- df %>% group_by(.data[[treatment]]) %>% 
      summarize(across(covs, ~ weighted.mean(.x, .data[[weights]]))) %>% as.data.frame()
    
    # Make table of unweighted means in treated, untreated, and SMD
    um.wt.tab <- matrix(NA, length(covs), 3)
    um.wt.tab[,1] <- unlist(um.wt[1,-1]) 
    um.wt.tab[,2] <- unlist(um.wt[2,-1])                        
    um.wt.tab[,3] <- (unlist(um.wt[2,-1]) - unlist(um.wt[1,-1]))/pooled.var
    
    # Make table of weighted means in treated, untreated, and SMD
    bal.wt.tab <- matrix(NA, length(covs), 3)
    bal.wt.tab[,1] <- unlist(bal.st[1,-1]) 
    bal.wt.tab[,2] <- unlist(bal.st[2,-1])                        
    bal.wt.tab[,3] <- (unlist(bal.st[2,-1]) - unlist(bal.st[1,-1]))/pooled.var 
    
    ## Rename columns using covs
    long.names <- c("Age", "Sex", "Race - White", "Race -Black", "Race - Other", "Admisison Type", 
                    "Center - HUP", "Center - Presbyterian", "Center - Pennsylvania Hospital", "Presentation - ED", "Presentation ICU",
                    "Presentation - OR", "Presentation - Floor", "Presentation - Other", "Prior LOS", "ICU Status",
                    "Post Op", "POD 0", "POD 1", "POD 2", "POD3", "Ventilator Status", 
                    "Prior Ventilator", "Heart Failure", "Myocardial Infarction", "Arrhythmia", "Atrial Fibrillation", "Valvular Diseas", 
                    "Stroke", "Peripheral Vascular Disease", "Pulmonary Circulation Disorder", "Chronic Pulmonary Disease", "Liver Disease", 
                    "Diabetes Mellitus - None", "Diabetes Mellitus - Non-complicated", "Diabetes Mellitus - Complicated", "Chronic Kidney Disease", 
                    "Weight Loss", "Fluid and Electrolyte Disorder", "Cancer - None", "Cancer - Non-complicated", "Cancer - Metastatic",
                    "HIV", "eGFR", "Prior AKI", "WBC, x10^8 cells/dL", "Hemoglobin, g/dL", "Platelets, x10^11 cells/L", "Chloride, mEq/L",
                    "Potassium, mEq/L","RAS Inhibitor" ,"Metoprolol", "Combined Alpha and Beta Blocker", "Hydrochlorothiazide", "Hydralazine", "Loop Diuretics",
                    "Other Antihypertensives", "Acid Suppressants - None","Acid Suppressants - H2RA", "Acid Suppressants - PPI", "Broad Spectrum Antibiotics", "Narrow Spectrum Antibiotics", 
                    "Vancomycin", "Bactrim",
                    "Other Nephrotoxic Antibiotics", "Other Nephrotoxins", "Vasopressors", "BMI")
    
    rownames(um.wt.tab) <- long.names
    rownames(bal.wt.tab) <- long.names
    
    # Store tables
    um.wt.tab.name <- paste0(df.name, ".um.wt.tab")
    assign(um.wt.tab.name, um.wt.tab)
    
    bal.wt.tab.name <- paste0(df.name, ".wt.tab")
    assign(bal.wt.tab.name, bal.wt.tab)
    
    n.covs <- nrow(um.wt.tab)
    
    ## Total Imbalance Reduction
    um.wt.bias <- um.wt.tab[,3]
    bal.bias <- bal.wt.tab[,3] 
    pbr <- (1 - (mean(abs(bal.bias))/mean(abs(um.wt.bias))))*100
    message("Total Imbalance Reduction is: " , pbr)
    
    # Plots              			  
    data.plot <- c(bal.wt.tab[,3], um.wt.tab[,3])
    data.plot <- as.data.frame(data.plot)
    names(data.plot) <- "std.dif"
    data.plot$contrast <- c(rep(1, n.covs), rep(2, n.covs))
    data.plot$contrast <- factor(data.plot$contrast, levels = c(1,2), 
                                 labels = c("Weighted", "Unweighted"))
    data.plot$covariate <- as.factor(rownames(um.wt.tab))
    
    
    plot <- ggplot(data = data.plot, aes(x = std.dif, y = covariate, 
                                         shape = factor(contrast), color = factor(contrast))) +
      geom_point(size = 2, shape = 16) +
      scale_shape_manual(name = "Contrast", values = c(1, 15)) +
      scale_color_manual(name = "Contrast", values = c("darkred", "darkblue")) +
      xlab("Standardized Difference") + ylab("Covariates") +
      ggtitle(if (subset) paste0(main.title, " - Quintile ", i - 1) else main.title) +
      scale_y_discrete(limits = rev(levels(data.plot$covariate))) +
      geom_vline(xintercept = 0) +
      geom_vline(xintercept = 0.1) +
      geom_vline(xintercept = -0.1) +
      xlim(xlim) + 
      theme_bw() +
      guides(shape = FALSE) +
      theme(legend.title = element_blank())
    
    print(plot)
    
    plot.name <- paste0(df.name, ".plot")
    assign(plot.name, plot)
    plot.list[[i]] <- plot
  }
  
  return(plot.list)
}

# Function to combine quintiles into a composite plot
composite.bal.plot <- function(plot.list, main.title) {
  # Drop overall plot
  plot.list <- plot.list[-1]
  
  # Add subplot titles and remove y-axis text for all but the first plot
  plot.list.titled <- lapply(seq_along(plot.list), function(i) {
    p <- plot.list[[i]] + ggtitle(paste0("Quintile ", i))
    if (i != 1) {
      p <- p +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank())
    }
    return(p)
  })
  
  composite <- ggarrange(
    plotlist = plot.list.titled,
    ncol = length(plot.list),  
    common.legend = TRUE,
    legend = "bottom",
    align = "h",
    widths = c(1.4, rep(0.8, length(plot.list) - 1))  # give first plot more width
  )
    composite <- annotate_figure(composite, top = text_grob(main.title, face = "bold", size = 16))
  
  return(composite)
}


# for binary variables DM and CHF
bal.plot.clean.bin <- function(data, weights, strata, treatment, covs, subset = TRUE, main.title = NULL) {
  
  if (subset) {
    # subset the data
    subsets <- data %>% group_split(.data[[strata]])
    
    # pull out the subsets from list of subsets
    for (i in 1:length(subsets)) {
      df <- subsets[[i]]
      name <- paste0(strata,".subset." , i)
      assign(name, df)
    }
    
    # make list including each dataframe and the initial data
    subset.list <- list()
    for (i in 1:length(subsets)){
      subset.name <- paste0(strata, ".subset.", i)
      subset.list[[i]] <- get(subset.name)
    }
    data.list <- c(list(data), subset.list)
    data.name <- deparse(substitute(data))
    data.frame.names <- c(data.name, paste0(strata, ".subset.", 1:length(subsets)))
  } else {
    # Only use the overall data
    data.list <- list(data)
    data.name <- deparse(substitute(data))
    data.frame.names <- data.name
  }
  
  plot.list <- list()
  
  # get xlim 
  all.std.dif <- c()
  
  for (df in data.list) {
    data.var <- df %>% group_by(.data[[treatment]]) %>%
      summarize(across(covs, ~var(.x))) %>% as.data.frame()
    
    c.var <- as.numeric(data.var[1, -1])
    t.var <- as.numeric(data.var[2, -1])
    pooled.var <- sqrt((t.var + c.var) / 2)
    
    um.wt <- df %>% group_by(.data[[treatment]]) %>%
      summarize(across(covs, ~mean(.x))) %>% as.data.frame()
    
    bal.st <- df %>% group_by(.data[[treatment]]) %>%
      summarize(across(covs, ~ weighted.mean(.x, .data[[weights]]))) %>% as.data.frame()
    
    unweighted_smd <- (unlist(um.wt[2, -1]) - unlist(um.wt[1, -1])) / pooled.var
    weighted_smd <- (unlist(bal.st[2, -1]) - unlist(bal.st[1, -1])) / pooled.var
    
    all.std.dif <- c(all.std.dif, unweighted_smd, weighted_smd)
  }
  
  xlim <- range(all.std.dif, na.rm = TRUE)
  print(xlim)
  
  # for loop to make each plot and print TBR
  for (i in seq_along(data.list)) {
    df <- data.list[[i]]
    df.name <- data.frame.names[i]
    print(df.name)
    
    # Variance 
    data.var <- df %>% group_by(.data[[treatment]]) %>% 
      summarize(across(covs, ~var(.x))) %>% as.data.frame() # if trouble with across, restart R session
    
    # Calculate pooled var in the overall unweighted data
    c.var <- as.numeric(data.var[1,])
    t.var <- as.numeric(data.var[2,])
    c.var <- c.var[-1]
    t.var <- t.var[-1]
    pooled.var <- sqrt((t.var + c.var)/2)
    
    # Calculate the mean in the unweighted data
    um.wt <- df %>% group_by(.data[[treatment]]) %>% 
      summarize(across(covs, ~mean(.x))) %>% as.data.frame()
    
    # Calculate the mean in the weighted data
    bal.st <- df %>% group_by(.data[[treatment]]) %>% 
      summarize(across(covs, ~ weighted.mean(.x, .data[[weights]]))) %>% as.data.frame()
    
    # Make table of unweighted means in treated, untreated, and SMD
    um.wt.tab <- matrix(NA, length(covs), 3)
    um.wt.tab[,1] <- unlist(um.wt[1,-1]) 
    um.wt.tab[,2] <- unlist(um.wt[2,-1])                        
    um.wt.tab[,3] <- (unlist(um.wt[2,-1]) - unlist(um.wt[1,-1]))/pooled.var
    
    # Make table of weighted means in treated, untreated, and SMD
    bal.wt.tab <- matrix(NA, length(covs), 3)
    bal.wt.tab[,1] <- unlist(bal.st[1,-1]) 
    bal.wt.tab[,2] <- unlist(bal.st[2,-1])                        
    bal.wt.tab[,3] <- (unlist(bal.st[2,-1]) - unlist(bal.st[1,-1]))/pooled.var 
    
    ## Rename columns using covs
    long.names <- c("Age", "Sex", "Race - White", "Race -Black", "Race - Other", "Admisison Type", 
                    "Center - HUP", "Center - Presbyterian", "Center - Pennsylvania Hospital", "Presentation - ED", "Presentation ICU",
                    "Presentation - OR", "Presentation - Floor", "Presentation - Other", "Prior LOS", "ICU Status",
                    "Post Op", "POD 0", "POD 1", "POD 2", "POD3", "Ventilator Status", 
                    "Prior Ventilator", "Heart Failure", "Myocardial Infarction", "Arrhythmia", "Atrial Fibrillation", "Valvular Diseas", 
                    "Stroke", "Peripheral Vascular Disease", "Pulmonary Circulation Disorder", "Chronic Pulmonary Disease", "Liver Disease", 
                    "Diabetes Mellitus - None", "Diabetes Mellitus - Non-complicated", "Diabetes Mellitus - Complicated", "Chronic Kidney Disease", 
                    "Weight Loss", "Fluid and Electrolyte Disorder", "Cancer - None", "Cancer - Non-complicated", "Cancer - Metastatic",
                    "HIV", "eGFR", "Prior AKI", "WBC, x10^8 cells/dL", "Hemoglobin, g/dL", "Platelets, x10^11 cells/L", "Chloride, mEq/L",
                    "Potassium, mEq/L","RAS Inhibitor", "Metoprolol", "Combined Alpha and Beta Blocker", "Hydrochlorothiazide", "Hydralazine", "Loop Diuretics",
                    "Other Antihypertensives", "Acid Suppressants - None","Acid Suppressants - H2RA", "Acid Suppressants - PPI", "Broad Spectrum Antibiotics", "Narrow Spectrum Antibiotics", 
                    "Vancomycin", "Bactrim",
                    "Other Nephrotoxic Antibiotics", "Other Nephrotoxins", "Vasopressors", "BMI")
    
    if (strata == "dm.bin") {
      # Names to drop
      drop_names <- c(
        "Diabetes Mellitus - None",
        "Diabetes Mellitus - Non-complicated",
        "Diabetes Mellitus - Complicated"
      )
      
      long.names <- long.names[!long.names %in% drop_names]
    }
    
    if (strata == "chf.cat") {
      # Names to drop
      drop_names <- c(
        "Heart Failure"
      )
      
      long.names <- long.names[!long.names %in% drop_names]
    }
    
    rownames(um.wt.tab) <- long.names
    rownames(bal.wt.tab) <- long.names
    
    # Store tables
    um.wt.tab.name <- paste0(df.name, ".um.wt.tab")
    assign(um.wt.tab.name, um.wt.tab)
    
    bal.wt.tab.name <- paste0(df.name, ".wt.tab")
    assign(bal.wt.tab.name, bal.wt.tab)
    
    n.covs <- nrow(um.wt.tab)
    
    ## Total Imbalance Reduction
    um.wt.bias <- um.wt.tab[,3]
    bal.bias <- bal.wt.tab[,3] 
    pbr <- (1 - (mean(abs(bal.bias))/mean(abs(um.wt.bias))))*100
    message("Total Imbalance Reduction is: " , pbr)
    
    # Create plot title based on strata and subset
    if (subset) {
      if (strata == "dm.bin") {
        var_name <- "DM"
        if (i == 1) {
          plot_title <- main.title
        } else if (i == 2) {
          plot_title <- paste0(main.title, " (no ", var_name, ")")
        } else if (i == 3) {
          plot_title <- paste0(main.title, " (with ", var_name, ")")
        }
      } else if (strata == "chf.cat") {
        var_name <- "HF"
        if (i == 1) {
          plot_title <- main.title
        } else if (i == 2) {
          plot_title <- paste0(main.title, " (no ", var_name, ")")
        } else if (i == 3) {
          plot_title <- paste0(main.title, " (with ", var_name, ")")
        }
      } else {
        # Fallback for other strata
        plot_title <- paste0(main.title, " - With/without ", i - 1)
      }
    } else {
      plot_title <- main.title
    }
    
    
    # Plots              			  
    data.plot <- c(bal.wt.tab[,3], um.wt.tab[,3])
    data.plot <- as.data.frame(data.plot)
    names(data.plot) <- "std.dif"
    data.plot$contrast <- c(rep(1, n.covs), rep(2, n.covs))
    data.plot$contrast <- factor(data.plot$contrast, levels = c(1,2), 
                                 labels = c("Weighted", "Unweighted"))
    data.plot$covariate <- as.factor(rownames(um.wt.tab))
    
    
    plot <- ggplot(data = data.plot, aes(x = std.dif, y = covariate, 
                                         shape = factor(contrast), color = factor(contrast))) +
      geom_point(size = 2, shape = 16) +
      scale_shape_manual(name = "Contrast", values = c(1, 15)) +
      scale_color_manual(name = "Contrast", values = c("darkred", "darkblue")) +
      xlab("Standardized Difference") + ylab("Covariates") +
      ggtitle(plot_title) +
      scale_y_discrete(limits = rev(levels(data.plot$covariate))) +
      geom_vline(xintercept = 0) +
      geom_vline(xintercept = 0.1) +
      geom_vline(xintercept = -0.1) +
      xlim(xlim)+
      theme_bw() +
      guides(shape = FALSE) +
      theme(legend.title = element_blank())
    
    print(plot)
    
    plot.name <- paste0(df.name, ".plot")
    assign(plot.name, plot)
    plot.list[[i]] <- plot
  }
  
  return(plot.list)
}

composite.bal.plot.bin <- function(plot.list, main.title, strata) {
  # Drop overall plot
  plot.list <- plot.list[-1]
  
  # Define subplot titles based on strata
  if (strata == "dm.bin") {
    subplot_titles <- c("Without DM", "With DM")
  } else if (strata == "chf.cat") {
    subplot_titles <- c("Without HF", "With HF")
  } else {
    # Fallback for other strata
    subplot_titles <- paste0("Group ", 1:length(plot.list))
  }
  
  # Add subplot titles and remove y-axis text for all but the first plot
  plot.list.titled <- lapply(seq_along(plot.list), function(i) {
    p <- plot.list[[i]] + ggtitle(subplot_titles[i])
    if (i != 1) {
      p <- p +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank())
    }
    return(p)
  })
  
  composite <- ggarrange(
    plotlist = plot.list.titled,
    ncol = length(plot.list),  
    common.legend = TRUE,
    legend = "bottom",
    align = "h",
    widths = c(1.2, rep(0.8, length(plot.list) - 1))  # give first plot more width
  )
  composite <- annotate_figure(composite, top = text_grob(main.title, face = "bold", size = 16))
  
  return(composite)
}
