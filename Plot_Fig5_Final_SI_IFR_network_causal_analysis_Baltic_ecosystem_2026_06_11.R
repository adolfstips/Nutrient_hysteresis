# ------------------------------------------------------------------------------
# Baltic Ecosystem Deep and Shallow Variables
# Global System Impact (GI) and Multidimensional Information Flow Rate (IFR) Analysis
# Figure 5 for hysteresis article
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggpubr)
library(scales)
library(igraph)
library(ggnetwork)
library(ggrepel)

# ------------------------------------------------------------------------------
# 1. ANALYTICAL FUNCTIONS
# ------------------------------------------------------------------------------

global_impact = function(xxx, np = 1) {
  dt = 1 
  nm = dim(xxx)[1]   
  M  = dim(xxx)[2]   
  N  = nm - np         
  
  dx1 = xxx[1:N,] * 0.0
  x   = dx1
  
  for (i in 1:M) {  
    dx1[1:N,i] = diff(xxx[,i], lag=np) / (np*dt) 
  }
  
  x[1:N,1:M] = xxx[1:N, 1:M, drop=FALSE]
  
  C = cov(x)
  C1 = C  
  C1[1:M,1] = 0
  C1[1,1:M] = 0
  C1[1,1] = 1
  
  dC <- matrix(0, nrow = M, ncol = M)
  for (i in 1:M ) {
    for (k in 1:M) {
      dC[k,i] = sum((x[,k] - mean(x[,k])) * (dx1[,i] - mean(dx1[,i])))
    }
  }
  
  dC = dC / (N-1)
  invC = solve(C)
  invC1 = solve(C1)
  
  A <- matrix(0, nrow = M, ncol = M)
  for (i in 1:M) {
    A[,i] = invC %*% dC[,i, drop=FALSE]
  }
  A = t(A) 
  
  AC = A %*% C 
  
  T1 = sum(diag(invC1[2:M, 2:M, drop=FALSE] %*% AC[2:M, 2:M, drop=FALSE])) - 
    sum(diag(A[2:M, 2:M, drop = FALSE]))
  
  return(T1)
}

causal_error_multidim_95 = function(xxx, np = 1){
  dt = 1 
  nm = dim(xxx)[1]   
  M  = dim(xxx)[2]   
  N  = nm - np         
  
  dx1 = xxx[1:N,] * 0.0
  x = dx1
  
  dx1[1:N,1] = diff(xxx[,1], lag=np) 
  x[,] = xxx[1:N,]
  
  C = cov(x) 
  dC = C * 0  
  
  for (k in 1:M) {
    dC[k,1] = sum((x[,k] - mean(x[,k])) * (dx1 - mean(dx1[,1])))
  }
  
  dC = dC / (N-1)
  a1n = solve(C) %*% dC
  a12 = a1n[2,1]
  
  T21 = C[1,2]/C[1,1] * a12
  
  f1 = mean(dx1[,1])
  for (k in 1:M) { f1 = f1 - a1n[k,1] * mean(x[,k]) }
  R1 = dx1[,1] - f1
  for (k in 1:M) { R1 = R1 - a1n[k,1] * x[,k] }
  
  Q1 = sum(R1 * R1)
  b1 = sqrt(Q1 * dt / N)
  
  NI = matrix(0, ncol=(M+2), nrow=(M+2))
  NI[1,1] = N * dt / (b1 *b1)
  NI[M+2,M+2] = 3*dt /b1^4 * sum(R1 * R1) - N/(b1*b1)
  
  for (k in 1:M) { NI[1,k+1] = dt / (b1*b1) * sum(x[,k]) }
  NI[1,M+2] = 2 * dt /b1^3 * sum(R1)
  
  for (k in 1:M) {
    for (j in 1:M) { NI[j+1,k+1] = dt /(b1*b1) * sum(x[,j] * x[,k]) }
  }
  
  for (k in 1:M) { NI[k+1,M+2] = 2 * dt /b1^3 * sum(R1 * x[,k]) }
  
  for (j in 1:(M+2)) {
    for (k in 1:j) { NI[j,k] = NI[k,j] }
  }
  
  invNI = solve(NI) 
  var_a12 = invNI[3,3]
  var_T21 = (C[1,2]/C[1,1])^2 * var_a12
  err95 = 1.96 * sqrt(var_T21)
  
  return(c(T21, err95))
}

# ------------------------------------------------------------------------------
# 2. DATA PREPARATION
# ------------------------------------------------------------------------------

grand_df <- read.csv(file = "Baltic_variables_Mt_4_Causal_Input.csv")

s_date <- grand_df$Year[1]
e_date <- grand_df$Year[nrow(grand_df)]

deep_vars <- c("TN_load","TP_load", "DIP_DEEP", "DIN_DEEP", "CHLA_DEEP", "DO2_Mt_DEEP", "Secchi_DEEP","TTI_DEEP")
shallow_vars <- c("TN_load","TP_load", "DIP_SHALLOW", "DIN_SHALLOW", "CHLA_SHALLOW", "DO2_Mt_SHALLOW", "Secchi_SHALLOW","TTI_SHALLOW")

deep_df <- scale(grand_df[, deep_vars])
shallow_df <- scale(grand_df[, shallow_vars])

# ------------------------------------------------------------------------------
# 3. MASTER PROCESSING FUNCTION
# ------------------------------------------------------------------------------

generate_region_plots <- function(plot_data, region_name, s_date, e_date) {
  
  x = as.matrix(plot_data)
  M = dim(x)[2]
  
  # Strip suffixes to clean up labels in plots
  var_names = gsub("_DEEP|_SHALLOW", "", colnames(plot_data))
  
  # --- A. Calculate Global Impact (GI) ---
  GG1 = vector(mode='numeric', length=M)
  for (k in 1:M) {
    indi = c(k, (1:M)[!(1:M %in% k)])
    xxx <- x[,indi]
    dummy = global_impact(xxx, 1)
    GG1[k] = dummy[1]
  }
  names(GG1) = var_names
  gg_ad = round(GG1, 2)
  
  ymin = floor(min(gg_ad)*10 - 0.4)/10 
  ymax = ceiling(max(gg_ad)*10 + 0.35)/10
  
  GI_df <- data.frame(GI = factor(names(gg_ad), levels = names(gg_ad)), y = gg_ad)
  
  # --- B. Plot GI Bar Chart ---
  p_gi <- ggplot(GI_df, aes(x = GI, y = y, fill = y)) +
    geom_col(width = 0.95, color = "black", linewidth = 0.2) +
    theme_pubr() +
    geom_text(aes(label = y, y = y + 0.03 * sign(y)), fontface = 'bold', size = 4) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    ylab('System Impact [Nat/year]') +
    xlab('') +
    scale_fill_gradientn(
      colours = rev(c(muted("red"), "white", muted("blue"))),
      limits = c(ymin, ymax), 
      values = rescale(c(ymin, 0, ymax)),
      oob = squish) +
    labs(title = paste("System Impact -", region_name),
         subtitle = paste("Period:", s_date, '-', e_date)) +
    geom_hline(yintercept = 0) +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 12, face = 'bold'),
      axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold', size = 10),
      legend.position = 'none',
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")
    )
  
  # --- C. Calculate Multidimensional IFR ---
  TT = matrix(0, ncol=M, nrow=M)
  for (i in 1:M) {
    for (j in 1:M) {
      if (i != j) {
        xxx = cbind(x[,i], x[,j])
        for (k in 1:M) { if (k != i & k != j) { xxx = cbind(xxx, x[,k]) } }
        dummy = causal_error_multidim_95(xxx, 1)
        TT[j,i] = dummy[1]
      }
    }
  }
  
  infoflow = round(TT, 2)
  colnames(infoflow) = var_names
  rownames(infoflow) = var_names
  
  # --- D. Build and Plot Hybrid Network ---
  adj_matrix <- as.matrix(infoflow)
  adj_matrix[is.na(adj_matrix)] <- 0
  adj_matrix[abs(adj_matrix) < 0.1] <- 0
  
  gr09 <- graph_from_adjacency_matrix(adj_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  E(gr09)$flow_sign <- ifelse(E(gr09)$weight > 0, "Positive", "Negative")
  E(gr09)$flow_strength <- abs(E(gr09)$weight)
  
  node_names <- V(gr09)$name
  matching_values <- GG1[node_names]
  V(gr09)$gi_abs <- as.numeric(abs(matching_values))
  V(gr09)$gi_sign <- ifelse(as.numeric(matching_values) < 0, "Negative", "Positive")
  
  set.seed(1234)
  gg_net <- ggnetwork(gr09, arrow.gap = 0.07, layout = layout_nicely(gr09))
  
  p_net <- ggplot(gg_net, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
      aes(color = name, linewidth = flow_strength, linetype = flow_sign, alpha = flow_strength),
      curvature = 0.15,
      arrow = arrow(length = unit(1, "lines"), type = "closed") 
    ) +
    geom_nodes(aes(color = name, size = gi_abs, shape = gi_sign), alpha = 0.8) +  
    scale_size_continuous(range = c(6, 28)) + 
    scale_shape_manual(values = c("Positive" = 16, "Negative" = 15), name = "SI Polarity") +
    geom_nodetext_repel(aes(label = name), color = "black", size = 5, fontface = "bold") +
    scale_linewidth_continuous(range = c(0.8, 5.0)) + 
    scale_alpha_continuous(range = c(0.35, 0.95)) + 
    scale_linetype_manual(values = c("Positive" = "solid", "Negative" = "41"), name = "Causal Direction") +
    coord_cartesian(clip = "off") + 
    theme_blank() +
    
    scale_x_continuous(expand = expansion(mult = 0.07)) +
    scale_y_continuous(expand = expansion(mult = 0.04)) + 
    
    labs(title = paste('IFR Causality Network -', region_name)) +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
      legend.position = "bottom",     
      legend.direction = "horizontal",    
      legend.box = "horizontal",          
      legend.background = element_blank(), 
      legend.key = element_blank(),
      legend.key.width = unit(1.0, "cm"), 
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.margin = margin(t = -50, r = 0, b = 0, l = 0),
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")
    ) +
    guides(
      color = "none", size = "none", linewidth = "none", alpha = "none",
      linetype = guide_legend(override.aes = list(linewidth = 1, color = "grey40", alpha = 1, arrow = NULL), order = 1),
      shape = guide_legend(override.aes = list(size = 5, color = "grey30"), order = 2)
    )
  
  return(list(gi = p_gi, net = p_net))
}

# ------------------------------------------------------------------------------
# 4. EXECUTE ANALYSIS AND COMBINE PLOTS
# ------------------------------------------------------------------------------

plots_deep <- generate_region_plots(deep_df, "Deep Baltic", s_date, e_date)
plots_shallow <- generate_region_plots(shallow_df, "Shallow Baltic", s_date, e_date)

final_combined_plot <- ggarrange(
  plots_deep$gi, plots_deep$net,
  plots_shallow$gi, plots_shallow$net,
  labels = c("A", "B", "C", "D"),
  font.label=list(color="black",size=24),
  ncol = 2, nrow = 2,
  align = "hv"
)

ggsave(
  filename = "Figure_5_Baltic_Causality_Combined.png", 
  plot = final_combined_plot, 
  width = 18,      
  height = 14,      
  units = "in", 
  dpi = 300,       
  bg = "white"     
)

print("Analysis complete. Figure 5 successfully saved.")

