# Make a  density plot of covariates in the unweighted and weighted data

library(ggpubr)
dplot<- function(var, data, weights) {
  data.dplot <- data %>%
    mutate(pain = factor(pain, labels = c("Oxycodone", "Ibuprofen")))
  
  max_density <- max(
    density(data.dplot[[var]][data.dplot$pain == "Oxycodone"])$y,
    density(data.dplot[[var]][data.dplot$pain == "Ibuprofen"])$y
  )
  
  p1 <- ggplot(data.dplot, aes(x = .data[[var]], group = pain, fill = pain)) +
    geom_density(alpha = 0.5) +
    labs(x = var, y = "Density") +
    ggtitle("Unweighted") +
    ggplot2::theme_minimal() +
    ylim(0, max_density*1.15)
  
  p2 <- ggplot(data.dplot, aes(x = .data[[var]], weight = .data[[weights]], group = pain, fill = pain)) +
    geom_density(alpha = 0.5) +
    labs(x = var, y = "Density") +
    ggtitle("Weighted") +
    ggplot2::theme_minimal() +
    ylim(0, max_density*1.15)
  
  
  combined <- ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom" )
  
  return(combined)
}

