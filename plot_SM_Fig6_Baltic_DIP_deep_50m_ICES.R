# ------------------------------------------------------------------------------
# Baltic Ecosystem Model Validation
# SUPPLEMENTARY FIGURE 6: Regional Deep Water (>50m) DIP 
# Initial draft 2026.09.03
# ------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggpubr)

# =====================================================================
# 1. LOAD AND PREP OBSERVATIONAL DATA
# =====================================================================
# Read CSVs, forcing R to keep exact column names from the ODV export
df_central_raw <- read.csv("./Gotland_dip_gt50m.csv", stringsAsFactors = FALSE, check.names = FALSE)
df_bornholm_raw <- read.csv("./Bornholm_dip_gt50m.csv", stringsAsFactors = FALSE, check.names = FALSE)

# Helper function to clean and filter data
clean_regional_data <- function(df, region_name) {
  df %>%
    mutate(
      Year = as.numeric(substr(`yyyy-mm-ddThh:mm:ss.sss`, 1, 4)),
      DIP = as.numeric(`Phosphate (PHOSZZXX_UPOX) [umol/l]`),
      Depth = as.numeric(`Depth (ADEPZZ01_ULAA) [m]`)
    ) %>%
    # Ensure we only use deep water data (> 50m) and drop NA values
    filter(Depth > 50 & !is.na(DIP)) %>%
    mutate(Region = region_name)
}

df_central_clean <- clean_regional_data(df_central_raw, "Central Baltic")
df_bornholm_clean <- clean_regional_data(df_bornholm_raw, "Bornholm Basin")

# Combine datasets
df_combined <- bind_rows(df_central_clean, df_bornholm_clean)

# =====================================================================
# 2. CALCULATE ANNUAL MEANS AND 95% CI
# =====================================================================
se <- function(x) { 
  n <- sum(!is.na(x))
  if (n > 1) sd(x, na.rm = TRUE) / sqrt(n) else NA 
}

df_annual <- df_combined %>%
  group_by(Year, Region) %>%
  summarise(
    DIP_mean = mean(DIP, na.rm = TRUE),
    DIP_se = se(DIP),
    .groups = "drop"
  ) %>%
  mutate(
    CI_lower = DIP_mean - 1.96 * DIP_se,
    CI_upper = DIP_mean + 1.96 * DIP_se
  )


#Save teh data

write.csv(df_annual, file="ICES_Bornholm_Gotland_DIP_annual_gt50m.csv",
          row.names = FALSE)

# =====================================================================
# 3. PLOTTING
# =====================================================================
okabe_ito_regions <- c("Central Baltic" = "#0072B2", "Bornholm Basin" = "#D55E00")

plot_regional_dip <- ggplot(df_annual, aes(x = Year, y = DIP_mean, color = Region, fill = Region)) +
  
  # Add 95% CI Ribbons
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.3, color = NA) +
  
  # Add Lines and Points
  geom_line(linewidth = 1) +
  geom_point(size = 2.5, shape = 21, color = "black", stroke = 0.5) +
  
  scale_color_manual(values = okabe_ito_regions) +
  scale_fill_manual(values = okabe_ito_regions) +
  scale_x_continuous(breaks = seq(1950, 2030, by = 10)) +
  ylim(0,4.2) +
  theme_pubr(base_size = 14) +
  labs(
    title = "Regional Deep-Water (>50m) ICES Observational Data",
    subtitle = "Annual Mean DIP (Solid lines with 95% Confidence Intervals)",
    x = "Year",
    y = expression(paste("Phosphate (", mu, "mol / L)"))
  ) +
  theme(
    #legend.position = "bottom",              # Moves the legend below the x-axis
    legend.direction = "horizontal",         # Arranges the legend items side-by-side
    legend.title = element_blank(),          # Removes the legend title
    legend.position = c(0.2, 0.95),         # Places legend inside, top-left corner
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5), # Adds a clean box around it
    legend.text = element_text(size = 12),   # Makes legend text readable
    plot.title = element_text(face = "bold", size = 15),
    plot.margin = margin(t = 0.5, r = 1, b = 0.5, l = 0.5, unit = "cm")
  )

print(plot_regional_dip)

ggsave("Figure_SM6_Baltic_Regional_Deep_DIP.png", 
       plot_regional_dip, width = 10, height = 6, dpi = 300, bg = "white")

print("Analysis complete. Figure SM6 successfully saved.")
