# ------------------------------------------------------------------------------
# Baltic Ecosystem Time Lag Analysis
# CCF of External TN Load vs Deep MSFD Indicators
# Figure SM 1 for hysteresis article
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(ggpubr) # Using ggpubr to harmonize with previous figures

# =====================================================================
# 1. DATA LOADING
# =====================================================================

# Load unified dataset and filter to study period
grand_df <- read.csv("Baltic_variables_Mt_4_Causal_Input.csv") %>%
  dplyr::filter(Year >= 1954 & Year <= 2022)

# =====================================================================
# 2. TIME LAG ANALYSIS (CCF)
# =====================================================================

target_vars <- c("DIP_DEEP", "DIN_DEEP", "CHLA_DEEP", "DO2_Mt_DEEP", "Secchi_DEEP")

ccf_results <- data.frame()

for (var in target_vars) {
  
  # Calculate CCF (lag.max = 25 years)
  ccf_out <- ccf(grand_df$TN_load, grand_df[[var]], plot = FALSE, lag.max = 25)
  
  temp_df <- data.frame(
    Variable = var,
    Lag = ccf_out$lag,
    Correlation = ccf_out$acf
  )
  
  # Causality Constraint:
  # 1. Isolate plausible lags (Lag >= 0)
  causal_df <- temp_df %>% dplyr::filter(Lag >= 0)
  
  # 2. Find dominant signal (max absolute correlation)
  max_causal_idx <- which.max(abs(causal_df$Correlation))
  dominant_lag <- causal_df$Lag[max_causal_idx]
  
  # 3. Mark in dataframe
  temp_df$Is_Max <- (temp_df$Lag == dominant_lag)
  temp_df$Max_Lag_Value <- dominant_lag
  
  ccf_results <- rbind(ccf_results, temp_df)
}

# Clean variable names for plot
ccf_results$Variable <- gsub("_DEEP", "", ccf_results$Variable)

# Order factors if desired (optional, to match network flow order)
# ccf_results$Variable <- factor(ccf_results$Variable, levels = c("CHLA", "DIN", "DIP", "DO2_Mt", "Secchi"))

# =====================================================================
# 3. PLOT GENERATION
# =====================================================================

p_ccf <- ggplot(ccf_results, aes(x = Lag, y = Correlation)) +
  # Use discrete colors for causal regimes
  geom_col(aes(fill = ifelse(Lag < 0, "Non-Causal", ifelse(Is_Max, "Dominant", "Causal"))), 
           show.legend = FALSE, width = 0.8) +
  scale_fill_manual(values = c("Non-Causal" = "grey85", "Causal" = "grey60", "Dominant" = "#D55E00")) + 
  
  # Baselines and significance bounds
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(0.25, -0.25), linetype = "dashed", color = "blue", alpha = 0.5) +
  
  facet_wrap(~ Variable, ncol = 1, scales = "free_y") +
  
  # Text placement
  geom_text(data = subset(ccf_results, Is_Max == TRUE),
            aes(label = paste("Lag:", Max_Lag_Value, "yrs"), 
                y = -sign(Correlation) * 0.15),
            size = 4.5, fontface = "bold", color = "#D55E00") +
  
  coord_cartesian(xlim = c(-10, 25)) +
  scale_x_continuous(breaks = seq(-10, 25, 5)) +
  
  # Harmonized Theme
  theme_pubr(base_size = 14) +
  labs(
    title = "Causally Constrained CCF: TN Load vs Deep Indicators",
    subtitle = "Analysis restricted to Lag \u2265 0. Highlights strongest absolute correlation.",
    x = "Lag (Years)",
    y = "Correlation Coefficient"
  ) +
  theme(
    plot.margin = margin(t = 0.5, r = 1, b = 0.5, l = 0.5, unit = "cm"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0, color = "grey30"),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey95", color = NA), # Soft background for facet labels
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size=12),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5) # Add faint horizontal gridlines for readability
  )

print(p_ccf)

# =====================================================================
# 4. EXPORT
# =====================================================================

ggsave(
  filename = "Figure_SM1_Baltic_CCF_TN_HELCOM_deep.png", 
  plot = p_ccf, 
  width = 10,      
  height = 8,      # Slightly taller to accommodate the 5 stacked facets cleanly
  units = "in", 
  dpi = 300,       
  bg = "white"     
)

print("Analysis complete. SM Figure 1 successfully saved.")

