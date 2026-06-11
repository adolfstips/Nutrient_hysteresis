# ------------------------------------------------------------------------------
# Baltic Ecosystem Nutrients Narrative
# SUPPLEMENTARY FIGURE 3: TP, Breakpoints, CCF and snail trail
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(ggpubr)
library(strucchange)
library(patchwork)
library(ggrepel)

# --- 1. SETTINGS & THRESHOLDS ---
ges_threshold <- 0.71
breaks_final <- c(1974, 1988, 2012)
highlight_year <- 1988

# =====================================================================
# 2. DATA LOADING & PREPARATION
# =====================================================================

# Read the unified dataset
grand_df <- read.csv("Baltic_variables_Mt_4_Causal_Input.csv") %>%
  dplyr::filter(Year >= 1954 & Year <= 2022) %>%
  # Rename columns locally to match the legacy script's internal logic
  dplyr::rename(
    TTI = TTI_ALL,
    GES = GES_ALL,
    LAG1 = LAG1_ALL
  )

# =====================================================================
# 3. MULTIVARIATE STRUCTURAL CHANGE
# =====================================================================

# Subset variables for structural change analysis
bp_vars <- c("TTI", "GES", "LAG1", "TN_load", "TP_load")
bp_data <- grand_df[, bp_vars]

# Standardize (CRITICAL STEP: so loads don't overpower indices)
grand_matrix <- scale(bp_data)
grand_ts <- ts(grand_matrix, start = 1954, end = 2022, frequency = 1)

# Run breakpoints on all 5 variables 
# (Included for diagnostic printing, though breaks_final is used for the plot)
grand_bp <- breakpoints(grand_ts ~ 1)
grand_ci <- confint(grand_bp, level = 0.95)
bp_idx <- grand_ci$confint[, "breakpoints"]
final_bps <- grand_df$Year[bp_idx]

print("--- Unified System Breakpoints (5 Vars) ---")
print(final_bps)

# =====================================================================
# 4. PLOT A: TIME SERIES (The History)
# =====================================================================

p_tti <- ggplot(grand_df, aes(x = Year, y = TTI)) +
  annotate("rect", xmin = 1954, xmax = 1974, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.05) +
  annotate("rect", xmin = 1974, xmax = 1988, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.05) +
  annotate("rect", xmin = 1988, xmax = 2012, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.05) +
  annotate("rect", xmin = 2012, xmax = 2022, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.05) +
  
  scale_x_continuous(breaks = seq(1950, 2030, by = 10)) +
  
  geom_hline(yintercept = ges_threshold, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  annotate("text", x = 1960, y = ges_threshold + 0.005, vjust= 0,
           label = paste0("GES (", ges_threshold, ")"), color = "darkgreen", size = 5) +
  
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 1.5) +
  
  geom_vline(xintercept = breaks_final, linetype = "dotted", linewidth = 0.6) +
  
  theme_pubr(base_size=14) +
  labs(title = "Baltic Sea Ecosystem State History (1954-2022)",
       subtitle = "Shaded regions indicate regimes",
       y = "Trophic Transfer Index (TTI)", x = "")

# =====================================================================
# 5. PLOT B: CCF
# =====================================================================

lag.max <- 24
# Fixed to use TP_load to match titles (Original code used TN_load)
ccf_res <- ccf(grand_df$TP_load, grand_df$TTI, plot = FALSE, lag.max = lag.max)

ccf_df <- data.frame(Lag = ccf_res$lag, Correlation = ccf_res$acf)
max_lag <- ccf_df$Lag[which.max(ccf_df$Correlation)]
sig_level <- 2 / sqrt(nrow(grand_df))

p_ccf <- ggplot(ccf_df, aes(x = Lag, y = Correlation)) +
  scale_x_continuous(breaks = seq(-8, 24, 2)) + 
  coord_cartesian(xlim = c(-6, 24)) +
  geom_col(aes(fill = Lag == max_lag), show.legend = FALSE) +
  scale_fill_manual(values = c("grey70", "#D55E00")) + 
  
  geom_hline(yintercept = c(sig_level, -sig_level), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  
  geom_point(data = ccf_df[which.max(ccf_df$Correlation),], 
             aes(x = Lag, y = Correlation), color = "orange", size = 3) +
  
  theme_pubr(base_size=14) +
  labs(title = "Cross-Correlation: TP Load vs. TTI Score",
       subtitle = paste("Max Correlation at Lag:", max_lag, "years"),
       x = "Lag (Years) - Positive means TP lead TTI",
       y = "Correlation Coefficient") 

# =====================================================================
# 6. PLOT C: SNAIL TRAIL (HYSTERESIS)
# =====================================================================

load_lim <- c(min(grand_df$TP_load) - 5, max(grand_df$TP_load) + 1)

p_TTI_hyst <- ggplot(grand_df, aes(x = TP_load, y = TTI)) +
  geom_path(arrow = arrow(length = unit(0.5, "cm"), type = "closed"), 
            color = "grey60", alpha = 0.8) +
  geom_point(aes(fill = Year), shape = 21, size = 4, stroke = 0.3) +
  
  geom_point(data = filter(grand_df, Year == highlight_year),
             shape = 1, size = 7, color = "red", stroke = 2) +
  
  coord_cartesian(xlim = load_lim) +
  
  geom_text_repel(data = subset(grand_df, Year %in% breaks_final),
                  aes(label = Year),
                  nudge_y = 0.02, 
                  fontface = "bold", 
                  color = "black",
                  size = 4.5) +
  
  geom_hline(yintercept = ges_threshold, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  annotate("text", x = load_lim[1] * 1.07 + 15, y = ges_threshold, vjust= -0.5,
           label = paste0("GES (", ges_threshold, ")"), color = "darkgreen", size = 5) +
  
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1,
    name = "Year",
    guide = guide_colorbar(
      barwidth = unit(1, "cm"),  
      barheight = unit(8, "cm"),   
      ticks.linewidth = 1.5        
    )
  ) +
  
  theme_pubr(base_size = 14) +
  labs(title = 'Phase-Space Trajectories Baltic Sea: TTI', 
       subtitle = "Clockwise trajectory: Degradation vs Recovery",
       x = "Total TP Load (kt)",
       y = "TTI [-]") +
  theme(legend.position = "right",
        legend.margin = margin(t = 0), 
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm") 
  ) 

# =====================================================================
# 7. COMBINE PLOTS & EXPORT
# =====================================================================

final_plot <- p_tti / (p_ccf + p_TTI_hyst) +
  plot_annotation(
    title = 'Dynamics of Baltic Sea Ecosystem Degradation',
    theme = theme(plot.title = element_text(size = 20, face = "bold"))
  )

ggsave("Figure_SM3_Baltic_TTI_LAG_TP_CCF_Analysis_Composite.png",
       final_plot, 
       width = 14, 
       height = 10, 
       dpi = 300,
       bg = "white")

print("Baltic narrative Analysis complete")
