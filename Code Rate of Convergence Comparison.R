library(ggplot2)
library(fdrtool)
library(patchwork)
rm(list = ls())


#plot the two rate of convergence
plot_data2 <- function(dfg, dfw, lambda) {
  df_both <- rbind(dfg, dfw)
  
  # Fit linear models
  model_g <- lm(y ~ x, data = subset(df_both, Type == "Grenander"))
  model_w <- lm(y ~ x, data = subset(df_both, Type == "Wasserstein"))
  
  # Extract slopes (coefficients)
  slope_g <- round(coef(model_g)[2], 3)
  slope_w <- round(coef(model_w)[2], 3)
  
  # Create annotated plot
  p <- ggplot(df_both, aes(x = x, y = y, color = Type)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = paste("Log-Log Plot of Rate of Convergence of Wasserstein and MLE Estimators for Exp(", lambda, ")", sep = ""),
      x = "Log10 (Number of Samples)",
      y = "Log10 (Wasserstein Distance)"
    ) +
    annotate("label", x = min(df_both$x), y = max(df_both$y), 
             label = paste0("Slope (Grenander): ", slope_g), 
             hjust = 0, vjust = 13, 
             fill = "white", color = "black", size = 5, 
             label.size = 0.5, label.r = unit(0.15, "lines")) +
    annotate("label", x = min(df_both$x), y = max(df_both$y) - 0.3, 
             label = paste0("Slope (Wasserstein): ", slope_w), 
             hjust = 0, vjust = 1, 
             fill = "white", color = "black", size = 5, 
             label.size = 0.5, label.r = unit(0.15, "lines")) +
    theme_minimal(base_size = 11)+
    theme(legend.position = "none",
          axis.title = element_text(size = 16),   # axis titles bigger
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 20, face = "bold")# axis tick labels bigger
    )
  return(p)
}


#data for grenander
data_g <- read.csv("Data_Grenander.csv")
dfg <- data.frame(x = data_g[["x"]], y = data_g[["dists"]], Type = "Grenander")
dfg_log <- data.frame(x = log10(dfg$x), y = log10(dfg$y), Type = dfg$Type)

#data for Wasserstein
data_w <- read.csv("Data_Wasserstein.csv")
dfw <- data.frame(x = data_w[["x"]], y = data_w[["dists"]], Type = "Wasserstein")
dfw_log <- data.frame(x = log10(dfw$x), y = log10(dfw$y), Type = dfw$Type)

plot_data2(dfg_log,dfw_log,1)

