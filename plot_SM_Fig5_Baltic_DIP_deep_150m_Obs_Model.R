# ------------------------------------------------------------------------------
# Baltic Ecosystem Model Validation
# SUPPLEMENTARY FIGURE 5: Deep Water (150m) DIP - Observations vs. Model (EGB)
# Initial draft 2026.09.02
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

# =====================================================================
# 1. LOAD AND PREP MODEL DATA (150m)
# =====================================================================
# Read the CDO text file, filling empty columns and reading as text
model_raw <- read.table("EGB_183_586_model_mon_DIP.txt", header = TRUE, fill = TRUE, stringsAsFactors = FALSE)

# Filter out the trailing CDO log lines and convert to numeric
model_clean <- model_raw %>%
  filter(grepl("^[0-9]+$", year)) %>% 
  mutate(
    Year = as.numeric(year),
    Model_DIP = as.numeric(value)
  )

# Calculate Annual Mean
model_annual <- model_clean %>%
  group_by(Year) %>%
  summarise(Model_DIP = mean(Model_DIP, na.rm = TRUE))

# =====================================================================
# 2. LOAD AND PREP ICES OBSERVATIONAL DATA (150m)
# =====================================================================
# Read CSV, forcing R to keep exact column names (including brackets/spaces)
obs_raw <- read.csv("Phosphate_EGB_ICES_18.24_58.58_150m_1960_2022.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Extract year from the timestamp string and isolate the Phosphate column
obs_clean <- obs_raw %>%
  mutate(
    Year = as.numeric(substr(`yyyy-mm-ddThh:mm:ss.sss`, 1, 4)),
    Obs_DIP = as.numeric(`Phosphate (PHOSZZXX_UPOX) [umol/l]`)
  )

# Function for Standard Error
se <- function(x) { 
  n <- sum(!is.na(x))
  if (n > 1) sd(x, na.rm = TRUE) / sqrt(n) else NA 
}

# Calculate Annual Mean and 95% CI
obs_annual <- obs_clean %>%
  group_by(Year) %>%
  summarise(
    Obs_DIP_mean = mean(Obs_DIP, na.rm = TRUE),
    Obs_DIP_se = se(Obs_DIP)
  ) %>%
  mutate(
    CI_lower = Obs_DIP_mean - 1.96 * Obs_DIP_se,
    CI_upper = Obs_DIP_mean + 1.96 * Obs_DIP_se
  )

# =====================================================================
# 3. STATISTICAL EVALUATION
# =====================================================================
df_eval <- merge(obs_annual, model_annual, by = "Year") %>%
  drop_na(Obs_DIP_mean, Model_DIP)

eval_metrics <- data.frame(
  Station = "EGB 26DA (150m)",
  Variable = "Phosphate (DIP)",
  Pearson_r = cor(df_eval$Obs_DIP_mean, df_eval$Model_DIP, method = "pearson"),
  RMSE = sqrt(mean((df_eval$Obs_DIP_mean - df_eval$Model_DIP)^2)),
  Mean_Bias = mean(df_eval$Model_DIP - df_eval$Obs_DIP_mean)
) %>% mutate(across(where(is.numeric), ~round(., 3)))

print("--- Deep Station (150m) Statistical Skill ---")
print(eval_metrics)

# =====================================================================
# 4. PLOTTING
# =====================================================================
# Match the SM5 Okabe-Ito Bluish-Green for Phosphate
color_dip <- "#009E73"

plot_150m <- ggplot() +
  
  # Observational Data (Ribbon + Points + Lines)
  geom_ribbon(data = obs_annual, aes(x = Year, ymin = CI_lower, ymax = CI_upper), 
              fill = color_dip, alpha = 0.4) +
  geom_line(data = obs_annual, aes(x = Year, y = Obs_DIP_mean), 
            color = color_dip, linewidth = 1) +
  geom_point(data = obs_annual, aes(x = Year, y = Obs_DIP_mean), 
             fill = color_dip, size = 2.5, shape = 21, color = "black", stroke = 0.5) +
  
  # Model Data (Thin dashed line + Triangles)
  geom_line(data = model_annual, aes(x = Year, y = Model_DIP), 
            color = "grey30", linewidth = 1.0, linetype = "dashed") +
  geom_point(data = model_annual, aes(x = Year, y = Model_DIP), 
             color = "grey30", shape = 17, size = 1.8) +
  
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  
  theme_pubr(base_size = 14) +
  labs(
    title = "Eastern Gotland Basin (150m): Observations vs. Blue2 Model",
    subtitle = "Annual Mean Deep-Water DIP (Green = ICES Obs with 95% CI | Grey Triangles = Blue2 Model)",
    x = "Year",
    y = expression(paste("Phosphate (", mu, "mol / L)"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")
  )

print(plot_150m)

ggsave("Figure_SM5_Baltic_Deep_150m_DIP.png", 
       plot_150m, width = 11, height = 6, dpi = 300, bg = "white")


