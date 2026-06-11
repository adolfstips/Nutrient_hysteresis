# ------------------------------------------------------------------------------
# Baltic Ecosystem Time Series Summary
# Figure 1: Asynchronous Structural Regime Shifts
# Panel A: Nutrients (5 breaks)
# Panel B: MSFD Biogeochemistry Standardized ALL REGION (2 breaks)
# Panel C: Coupled System Drivers + Response (3 breaks)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Baltic Ecosystem Time Series Summary
# Figure 1: Asynchronous Structural Regime Shifts
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Baltic Ecosystem Time Series Summary
# Figure 1: Asynchronous Structural Regime Shifts
# ------------------------------------------------------------------------------

library(xts)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(strucchange)
library(patchwork)

# --- 1. GLOBAL SETTINGS ---
s_date <- 1954
e_date <- 2022
years_vec <- s_date:e_date

# Helper Function: Calculate breakpoints naturally and map INDICES to YEARS
get_bp_ci_mapped <- function(ts_data, years_vector) {
  bp <- breakpoints(ts_data ~ 1)
  ci <- confint(bp, level = 0.95)
  
  idx_matrix <- ci$confint
  df <- data.frame(
    Lower = years_vector[idx_matrix[, 1]],
    Break = years_vector[idx_matrix[, 2]],
    Upper = years_vector[idx_matrix[, 3]]
  )
  return(df)
}

# =====================================================================
# 2. DATA LOADING & PROCESSING
# =====================================================================

# Load the unified dataset
df_all <- read.csv("Baltic_variables_Mt_4_Causal_Input.csv") %>%
  filter(Year >= s_date & Year <= e_date)

# --- A. External Nutrient Loads ---
nutrients_wide <- df_all %>% select(Year, TN_load, TP_load)

nutrients_long <- nutrients_wide %>%
  pivot_longer(cols = c(TN_load, TP_load), names_to = "Variable", values_to = "Value")
nutrients_long$Variable <- factor(nutrients_long$Variable, levels = c("TN_load", "TP_load"))

# Breakpoints for Panel A
ts_nutrients <- ts(scale(nutrients_wide[, c("TN_load", "TP_load")]), start = s_date)
ci_nutrients <- get_bp_ci_mapped(ts_nutrients, years_vec)


# --- B. HELCOM Biogeochemistry ALL REGION ---
msfd_vars <- c("DIP_ALL", "DIN_ALL", "CHLA_ALL", "DO2_Mt_ALL", "Secchi_ALL")

msfd_scaled <- as.data.frame(scale(df_all[, msfd_vars]))
msfd_scaled$Year <- df_all$Year

msfd_long <- pivot_longer(msfd_scaled, cols = all_of(msfd_vars), names_to = "Variable", values_to = "Z_Score")
msfd_long$Variable <- gsub("_ALL", "", msfd_long$Variable)

# Breakpoints for Panel B
ts_msfd <- ts(as.matrix(msfd_scaled[, msfd_vars]), start = s_date)
ci_msfd <- get_bp_ci_mapped(ts_msfd, years_vec)


# --- C. Coupled System (TTI, TN, TP) ---
df_eco <- data.frame(
  Year = df_all$Year,
  TTI = df_all$TTI_ALL,
  GES = df_all$GES_ALL,  # Awaiting addition to CSV
  LAG1 = df_all$LAG1_ALL      # Awaiting addition to CSV
)

grand_df <- merge(df_eco, nutrients_wide, by = "Year")

grand_matrix <- scale(grand_df[, -1])
grand_ts <- ts(grand_matrix, start = s_date, frequency = 1)

# Breakpoints for Panel C
ci_grand <- get_bp_ci_mapped(grand_ts, years_vec)

grand_df_scaled <- as.data.frame(grand_matrix)
grand_df_scaled$Year <- grand_df$Year
grand_long <- pivot_longer(grand_df_scaled, cols = -Year, names_to = "Metric", values_to = "Z_Score")
grand_long$Type <- ifelse(grand_long$Metric %in% c("TN_load", "TP_load"), "Driver (Nutrients)", "Response (Eco)")


# =====================================================================
# 3. PLOT GENERATION
# =====================================================================

# --- PLOT A: Nutrient Loads ---
p_loads <- ggplot(nutrients_long, aes(x = Year, y = Value)) +
  geom_rect(data = ci_nutrients, aes(xmin = Lower, xmax = Upper, ymin = -Inf, ymax = Inf), 
            fill = "grey70", alpha = 0.4, inherit.aes = FALSE) +
  geom_vline(data = ci_nutrients, aes(xintercept = Break), color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_line(aes(color = Variable), linewidth = 0.8) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(1950, 2030, by = 10)) +
  theme_pubr(base_size = 14) +
  labs(title = "Baltic Sea Nutrient Loadings (TN & TP)", 
       subtitle = paste("Detected Breakpoints:", paste(ci_nutrients$Break, collapse = ", ")),
       x = "", y = "Load (kt)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "none")

# --- PLOT B: Standardized MSFD Biogeochemistry (ALL REGION) ---
p_msfd <- ggplot(msfd_long, aes(x = Year, y = Z_Score, color = Variable)) +
  geom_rect(data = ci_msfd, aes(xmin = Lower, xmax = Upper, ymin = -Inf, ymax = Inf), 
            fill = "grey70", alpha = 0.4, inherit.aes = FALSE) +
  geom_vline(data = ci_msfd, aes(xintercept = Break), color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", alpha = 0.6) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  scale_color_brewer(palette = "Set1", name = "Indicator") +
  scale_x_continuous(breaks = seq(1950, 2030, by = 10)) +
  theme_pubr(base_size = 14) +
  labs(title = "Temporal Evolution of Standardized MSFD Indicators (1954-2022)", 
       subtitle = paste("Multivariate Regime Shifts detected at", paste(ci_msfd$Break, collapse = " and ")),
       x = "", y = "Standardized Anomaly (Z-Score)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "right")

# --- PLOT C: Coupled System ---
p_coupled <- ggplot() +
  geom_rect(data = ci_grand, aes(xmin = Lower, xmax = Upper, ymin = -Inf, ymax = Inf), 
            fill = "grey70", alpha = 0.4) +
  geom_vline(data = ci_grand, aes(xintercept = Break), color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_line(data = grand_long, aes(x = Year, y = Z_Score, color = Metric, linetype = Type), linewidth = 0.8) +
  scale_x_continuous(breaks = seq(1950, 2030, by = 10)) +
  scale_color_brewer(palette = "Dark2") +
  theme_pubr(base_size = 14) +
  labs(title = "Baltic Sea Coupled System (Drivers + Response)",
       subtitle = paste("Unified Breakpoints:", paste(ci_grand$Break, collapse = ", ")),
       x = "Year", y = "Standardized Value (Z-Score)") +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "bottom")

# =====================================================================
# 4. COMBINE & EXPORT WITH PATCHWORK
# =====================================================================

final_fig_1 <- (p_loads / p_msfd / p_coupled) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 18, face = "bold"))

ggsave(
  filename = "Figure_1_Baltic_Asynchronous_Shifts.png", 
  plot = final_fig_1, 
  width = 12,      
  height = 14,      
  units = "in", 
  dpi = 300,       
  bg = "white"     
)

print("Analysis complete. Figure 1 successfully saved.")
