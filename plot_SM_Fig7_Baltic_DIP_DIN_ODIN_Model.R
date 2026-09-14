# ------------------------------------------------------------------------------
# Baltic Ecosystem Model Validation
# SUPPLEMENTARY FIGURE 7: Arkona (Shallow) and EGB (Deep) Obs vs. Model
# Initial draft 2026.09.01
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(patchwork)

# =====================================================================
# 1. LOAD PRE-CALCULATED DATA
# =====================================================================

# Load Observational Annual Means (Generated from ODIN raw data)
obs_ark <- read.csv("Arkona_annual_means_summary.csv") %>% filter(Year >= 1969 & Year <= 2022)
obs_egb <- read.csv("EGB_annual_means_summary.csv") %>% filter(Year >= 1969 & Year <= 2022)

# Load Blue2 Model Data
df_model_full <- read.csv("Baltic_variables_Mt_4_Causal_Input.csv") %>% filter(Year >= 1969 & Year <= 2022)
# =====================================================================
# 1. LOAD PRE-CALCULATED DATA (CDO ASCII & ODIN CSV)
# =====================================================================

# Helper function to read CDO outputtab ASCII files
read_cdo <- function(filepath) {
  # CDO outputs a commented header (e.g., "# year  value")
  df <- read.table(filepath, header = TRUE, comment.char = "")
  # Rename columns cleanly
  colnames(df) <- c("Year", "Value")
  return(df)
}

# Read the newly extracted localized model data
ark_mod_din <- read_cdo("ARK_model_DIN.txt")
ark_mod_dip <- read_cdo("ARK_model_DIP.txt")
egb_mod_din <- read_cdo("EGB_model_DIN.txt")
egb_mod_dip <- read_cdo("EGB_model_DIP.txt")

# Reconstruct the model dataframes to plug perfectly into the rest of the script
# Reconstruct the model dataframes and filter to the 1969-2022 window
model_shallow <- data.frame(
  Year = ark_mod_din$Year,
  Model_Nitrate = ark_mod_din$Value,
  Model_Phosphate = ark_mod_dip$Value
) %>% filter(Year >= 1969 & Year <= 2022)

model_deep <- data.frame(
  Year = egb_mod_din$Year,
  Model_Nitrate = egb_mod_din$Value,
  Model_Phosphate = egb_mod_dip$Value
) %>% filter(Year >= 1969 & Year <= 2022)

# =====================================================================
# 2. STATISTICAL EVALUATION (Obs vs. Model)
# =====================================================================

eval_ark <- merge(obs_ark, model_shallow, by = "Year") %>% drop_na()
eval_egb <- merge(obs_egb, model_deep, by = "Year") %>% drop_na()

# Function to calculate correlation, RMSE, and Bias
calc_metrics <- function(df, region_name) {
  data.frame(
    Region = region_name,
    Nutrient = c("Nitrate", "Phosphate"),
    Pearson_r = c(
      cor(df$Nitrate_mean, df$Model_Nitrate, method = "pearson"),
      cor(df$Phosphate_mean, df$Model_Phosphate, method = "pearson")
    ),
    RMSE = c(
      sqrt(mean((df$Nitrate_mean - df$Model_Nitrate)^2)),
      sqrt(mean((df$Phosphate_mean - df$Model_Phosphate)^2))
    ),
    Mean_Bias = c(
      mean(df$Model_Nitrate - df$Nitrate_mean),
      mean(df$Model_Phosphate - df$Phosphate_mean)
    )
  )
}

metrics_combined <- rbind(calc_metrics(eval_ark, "Arkona (Shallow)"), calc_metrics(eval_egb, "EGB (Deep)"))
metrics_combined <- metrics_combined %>% mutate(across(where(is.numeric), ~round(., 3)))

print("--- Model vs. Observation Statistical Skill ---")
print(metrics_combined)

# =====================================================================
# 3. PLOT PREPARATION & FORMATTING
# =====================================================================

okabe_ito_obs <- c("Nitrate" = "#E69F00", "Phosphate" = "#009E73") 

# Function to pivot observations and calculate 95% CI
prep_plot_data <- function(obs_df) {
  obs_df %>%
    pivot_longer(cols = -Year, names_to = c("Nutrient", ".value"), names_sep = "_") %>%
    mutate(
      CI_lower = mean - 1.96 * rms,
      CI_upper = mean + 1.96 * rms
    )
}

obs_ark_long <- prep_plot_data(obs_ark)
obs_egb_long <- prep_plot_data(obs_egb)

model_ark_long <- model_shallow %>% 
  pivot_longer(cols = -Year, names_to = "Nutrient", values_to = "Model_Value", names_prefix = "Model_")
model_egb_long <- model_deep %>% 
  pivot_longer(cols = -Year, names_to = "Nutrient", values_to = "Model_Value", names_prefix = "Model_")

# =====================================================================
# 4. PLOTTING FUNCTION
# =====================================================================

create_basin_plot <- function(obs_long, mod_long, title_text, hide_x = FALSE) {
  p <- ggplot(obs_long, aes(x = Year, y = mean, color = Nutrient, fill = Nutrient)) +
    
    # Observational Data (Ribbon + Points + Lines)
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, color = "black", stroke = 0.5) +
    
    # Model Data (Thin dashed line + Triangles)
    geom_line(data = mod_long, aes(x = Year, y = Model_Value), 
              color = "grey30", linewidth = 1.0, linetype = "dashed", inherit.aes = FALSE) +
    geom_point(data = mod_long, aes(x = Year, y = Model_Value), 
               color = "grey30", shape = 17, size = 1.8, inherit.aes = FALSE) +
    
    facet_wrap(~ Nutrient, scales = "free_y", ncol = 1) +
    scale_color_manual(values = okabe_ito_obs) +
    scale_fill_manual(values = okabe_ito_obs) +
    scale_x_continuous(breaks = seq(1970, 2020, by = 10)) +
    
    theme_pubr(base_size = 14) +
    labs(
      title = title_text,
      x = ifelse(hide_x, "", "Year"),
      y = expression(paste("Conc. (mmol / ", m^3, ")"))
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 15),
      strip.text = element_text(face = "bold", size = 13),
      strip.background = element_rect(fill = "grey95", color = NA),
      plot.margin = margin(t = 0.2, r = 0.5, b = 0.2, l = 0.5, unit = "cm")
    )
  
  if(hide_x) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  return(p)
}

# =====================================================================
# 5. COMBINE AND EXPORT
# =====================================================================

plot_arkona <- create_basin_plot(obs_ark_long, model_ark_long, "A. Arkona Basin (Shallow): Observations vs. Blue2 Model", hide_x = TRUE)
plot_egb <- create_basin_plot(obs_egb_long, model_egb_long, "B. Eastern Gotland Basin (Deep): Observations vs. Blue2 Model", hide_x = FALSE)

final_composite <- plot_arkona / plot_egb +
  plot_annotation(
    subtitle = "Annual Mean 0-10m Concentration (Colored = ODIN Obs with 95% CI | Grey Triangles = Blue2 Model)"
  )

print(final_composite)

ggsave(
  filename = "Figure_SM7_Baltic_ODIN_vs_Model_Combined.png", 
  plot = final_composite, 
  width = 11,      
  height = 12,      
  units = "in", 
  dpi = 300,       
  bg = "white"     
)

print("Analysis complete. Composite figure SM7 successfully saved.")
