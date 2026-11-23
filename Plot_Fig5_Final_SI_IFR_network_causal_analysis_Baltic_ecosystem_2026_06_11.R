# ------------------------------------------------------------------------------
# Baltic Ecosystem Deep and Shallow Variables
# Global System Impact (SI) and Multidimensional Information Flow Rate (IFR) Analysis
# Figure 5 for hysteresis article (Hierarchical Eutrophication Cascade Layout)
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggpubr)
library(scales)
library(igraph)
library(ggnetwork)

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

# Okabe-Ito colorblind-friendly palette (separate node fill vs edge stroke for white-bg contrast)
okabe_ito_fill <- c(
  "TN_load" = "#56B4E9",  # Light Blue
  "TP_load" = "#E69F00",  # Orange
  "DIP"     = "#009E73",  # Bluish Green
  "DIN"     = "#F0E442",  # Yellow (outlined in dark grey on nodes)
  "CHLA"    = "#D55E00",  # Vermillion
  "DO2_Mt"  = "#0072B2",  # Dark Blue
  "Secchi"  = "#CC79A7",  # Reddish Purple
  "TTI"     = "#999999"   # Grey
)

okabe_ito_edge <- c(
  "TN_load" = "#56B4E9",
  "TP_load" = "#E69F00",
  "DIP"     = "#009E73",
  "DIN"     = "#C8A200",  # Deeper gold so DIN arrows are clearly legible on white
  "CHLA"    = "#D55E00",
  "DO2_Mt"  = "#0072B2",
  "Secchi"  = "#CC79A7",
  "TTI"     = "#7F7F7F"
)

# Fixed 4-tier mechanistic eutrophication cascade coordinates in [0, 1] x [0, 1]
# Convex oval geometry ensures all interior causal arcs clear unrelated nodes
tier_coords <- rbind(
  "TN_load" = c(0.32, 1.00),  # Tier 1: External Forcing (N pathway)
  "TP_load" = c(0.68, 1.00),  # Tier 1: External Forcing (P pathway)
  "DIN"     = c(0.04, 0.68),  # Tier 2: Ambient Nutrients (N pathway)
  "DIP"     = c(0.96, 0.68),  # Tier 2: Ambient Nutrients (P pathway)
  "CHLA"    = c(0.00, 0.32),  # Tier 3: Pelagic Response (Biomass)
  "Secchi"  = c(1.00, 0.32),  # Tier 3: Pelagic Response (Clarity)
  "DO2_Mt"  = c(0.28, 0.00),  # Tier 4: Benthic Hypoxia & Trophic State
  "TTI"     = c(0.72, 0.00)   # Tier 4: Benthic Hypoxia & Trophic State
)

# Outward label coordinates so labels never overlap circles or interior arrows
node_labels_df <- data.frame(
  name = c("TN_load", "TP_load", "DIN", "DIP", "CHLA", "Secchi", "DO2_Mt", "TTI"),
  lx   = c(0.28,      0.72,      -0.08, 1.08,  -0.09,  1.09,     0.28,     0.72),
  ly   = c(1.095,     1.095,      0.68, 0.68,   0.32,  0.32,    -0.095,   -0.095),
  hj   = c(0.5,       0.5,        1.0,  0.0,    1.0,   0.0,      0.5,      0.5),
  vj   = c(0.0,       0.0,        0.5,  0.5,    0.5,   0.5,      1.0,      1.0),
  stringsAsFactors = FALSE
)

# Subtle horizontal functional tier guides
tier_guides_df <- data.frame(
  y     = c(1.00, 0.68, 0.32, 0.00),
  label = c("External Forcing", "Ambient Nutrients", "Pelagic Response", "Hypoxia & TTI"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 2. DATA PREPARATION
# ------------------------------------------------------------------------------

grand_df <- read.csv(file = "Baltic_variables_Mt_4_Causal_Input.csv")

s_date <- grand_df$Year[1]
e_date <- grand_df$Year[nrow(grand_df)]

deep_vars <- c("TN_load", "TP_load", "DIP_DEEP", "DIN_DEEP", "CHLA_DEEP", "DO2_Mt_DEEP", "Secchi_DEEP", "TTI_DEEP")
shallow_vars <- c("TN_load", "TP_load", "DIP_SHALLOW", "DIN_SHALLOW", "CHLA_SHALLOW", "DO2_Mt_SHALLOW", "Secchi_SHALLOW", "TTI_SHALLOW")

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
  
  # --- A. Calculate Global Impact (SI) ---
  GG1 = vector(mode='numeric', length=M)
  for (k in 1:M) {
    indi = c(k, (1:M)[!(1:M %in% k)])
    xxx <- x[,indi]
    dummy = global_impact(xxx, 1)
    GG1[k] = dummy[1]
  }
  names(GG1) = var_names
  gg_ad = round(GG1, 2)
  si_pairs <- paste(sprintf("'%s': %s", names(gg_ad), as.character(gg_ad)), collapse = ", ")
  cat(sprintf("    %s SI: {%s}
", sub(" Baltic", "", region_name), si_pairs))
  
  # Shared y-axis and colour limits across Deep & Shallow for direct visual comparison
  ymin <- -0.35
  ymax <-  0.52
  
  GI_df <- data.frame(GI = factor(names(gg_ad), levels = names(gg_ad)), y = gg_ad)
  
  # --- B. Plot SI Bar Chart ---
  p_gi <- ggplot(GI_df, aes(x = GI, y = y, fill = y)) +
    geom_col(width = 0.85, color = "black", linewidth = 0.25) +
    theme_pubr() +
    geom_text(aes(label = y, y = y + 0.032 * ifelse(y >= 0, 1, -1)), fontface = 'bold', size = 3.8) +
    scale_y_continuous(
      limits = c(ymin, ymax),
      breaks = round(seq(-0.3, 0.5, by = 0.1), 1),
      labels = function(x) sprintf("%.1f", x)
    ) +
    ylab('System Impact [Nat/year]') +
    xlab('') +
    scale_fill_gradientn(
      colours = rev(c(scales::muted("red"), "white", scales::muted("blue"))),
      limits = c(ymin, ymax), 
      values = rescale(c(ymin, 0, ymax)),
      oob = squish) +
    labs(title = paste("System Impact -", region_name),
         subtitle = paste("Period:", s_date, '-', e_date)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", color = "grey50", linewidth = 0.7) + 
    theme(
      plot.title = element_text(size = 17, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(size = 12, face = 'bold'),
      axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold', size = 10),
      legend.position = 'none',
      plot.margin = margin(t = 0.5, r = 0.3, b = 0.5, l = 0.5, unit = "cm")
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
  
  # --- D. Build and Plot Hierarchical Eutrophication Network ---
  adj_matrix <- as.matrix(infoflow)
  adj_matrix[is.na(adj_matrix)] <- 0
  adj_matrix[abs(adj_matrix) < 0.1] <- 0
  # Enforce exogenous boundary condition: external loads cannot receive causal inflows
  adj_matrix[, c("TN_load", "TP_load")] <- 0
  
  gr09 <- graph_from_adjacency_matrix(adj_matrix, mode = "directed", weighted = TRUE, diag = FALSE)
  
  E(gr09)$flow_sign <- ifelse(E(gr09)$weight > 0, "Positive", "Negative")
  E(gr09)$flow_strength <- abs(E(gr09)$weight)
  
  node_names <- V(gr09)$name
  matching_values <- GG1[node_names]
  V(gr09)$gi_abs <- as.numeric(abs(matching_values))
  V(gr09)$gi_sign <- ifelse(as.numeric(matching_values) < 0, "Negative", "Positive")
  
  # Map vertices to fixed 4-tier cascade coordinates
  layout_mat <- unname(tier_coords[node_names, , drop = FALSE])
  gg_net <- ggnetwork(gr09, arrow.gap = 0.065, layout = layout_mat)
  
  p_net <- ggplot(gg_net, aes(x = x, y = y, xend = xend, yend = yend)) +
    # Subtle functional tier guide lines & left-margin labels
    geom_segment(
      data = tier_guides_df,
      aes(x = -0.06, xend = 1.06, y = y, yend = y),
      inherit.aes = FALSE, color = "grey90", linetype = "dotted", linewidth = 0.6
    ) +
    geom_text(
      data = tier_guides_df,
      aes(x = -0.24, y = y, label = label),
      inherit.aes = FALSE, color = "grey45", fontface = "italic", size = 3.6, hjust = 1
    ) +
    # Directed causal edges
    geom_edges(
      aes(color = name, linewidth = flow_strength, linetype = flow_sign, alpha = flow_strength),
      curvature = 0.15,
      arrow = arrow(length = unit(0.85, "lines"), type = "closed") 
    ) +
    # Outlined nodes sized globally across Deep & Shallow
    geom_nodes(
      aes(fill = name, size = gi_abs, shape = gi_sign),
      color = "grey20", stroke = 0.7, alpha = 0.90
    ) +
    # Clean outward node labels
    geom_text(
      data = node_labels_df,
      aes(x = lx, y = ly, label = name, hjust = hj, vjust = vj),
      inherit.aes = FALSE, color = "black", size = 4.6, fontface = "bold"
    ) +
    scale_size_continuous(range = c(7, 27), limits = c(0, 0.50)) + 
    scale_shape_manual(values = c("Positive" = 21, "Negative" = 22), name = "SI Polarity") +
    scale_linewidth_continuous(range = c(0.9, 5.2), limits = c(0.10, 0.65)) + 
    scale_alpha_continuous(range = c(0.52, 0.95), limits = c(0.10, 0.65)) + 
    scale_linetype_manual(values = c("Positive" = "solid", "Negative" = "41"), name = "Causal Direction") +
    scale_color_manual(values = okabe_ito_edge) +
    scale_fill_manual(values = okabe_ito_fill) +
    coord_cartesian(clip = "off") + 
    scale_x_continuous(limits = c(-0.46, 1.24)) +
    scale_y_continuous(limits = c(-0.16, 1.16)) + 
    theme_blank() +
    labs(title = paste('IFR Causality Network -', region_name)) +
    theme(
      plot.title = element_text(size = 17, hjust = 0.5, face = 'bold'),
      legend.position = "bottom",     
      legend.direction = "horizontal",    
      legend.box = "horizontal",          
      legend.background = element_blank(), 
      legend.key = element_blank(),
      legend.key.width = unit(1.0, "cm"), 
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.margin = margin(t = -15, r = 0, b = 0, l = 0),
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.4, l = 0.2, unit = "cm")
    ) +
    guides(
      color = "none", fill = "none", size = "none", linewidth = "none", alpha = "none",
      linetype = guide_legend(override.aes = list(linewidth = 1.1, color = "grey35", alpha = 1, arrow = NULL), order = 1),
      shape = guide_legend(override.aes = list(size = 5, fill = "grey45", color = "grey20"), order = 2)
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
  font.label = list(color = "black", size = 24),
  ncol = 2, nrow = 2,
  widths = c(0.38, 0.62)
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
