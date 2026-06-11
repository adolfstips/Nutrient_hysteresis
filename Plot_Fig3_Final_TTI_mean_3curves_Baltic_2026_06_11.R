# ------------------------------------------------------------------------------
# Baltic Ecosystem TTI Analysis
# Plot annual mean TTI (All, Deep, Shallow) 
# Figure 3 for hysteresis article
# ------------------------------------------------------------------------------

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)

# --- 1. SETTINGS & THRESHOLDS ---
GES_THRESHOLD <- 0.71
CORR_LENGTH <- 36
TTI_method <- "max_cor"
s_date <- 1954
e_date <- 2022

# --- 2. DATA LOADING & RESHAPING ---
df_all <- read.csv("Baltic_variables_Mt_4_Causal_Input.csv") %>%
  filter(Year >= s_date & Year <= e_date)

# Select only the needed TTI columns and pivot
tti_df_long <- df_all %>%
  select(Year, TTI_ALL, TTI_DEEP, TTI_SHALLOW) %>%
  pivot_longer(
    cols = starts_with("TTI_"),
    names_to = "Source",
    values_to = "Value"
  )

# Clean up source names to match original labels
tti_df_long$Source <- recode(tti_df_long$Source, 
                             "TTI_ALL" = "TTI All", 
                             "TTI_DEEP" = "TTI Deep", 
                             "TTI_SHALLOW" = "TTI Shallow")

tti_df_long$Source <- factor(tti_df_long$Source, 
                             levels = c("TTI All", "TTI Deep", "TTI Shallow"))

# --- 3. AESTHETICS & FACTORS ---
all_colors_tti <- c("TTI All" = "firebrick", "TTI Deep" = "steelblue", "TTI Shallow" = "darkgreen")
plot_shapes <- c("TTI All" = 16, "TTI Deep" = 17, "TTI Shallow" = 15)
plot_linewidths <- c("TTI All" = 1.8, "TTI Deep" = 1.2, "TTI Shallow" = 1.2)

# --- 4. LABEL POSITIONING FUNCTION ---
get_tti_label_pos <- function(data_long, year_offset = 1.5) {
  # Use the last 3 years of raw data to estimate end-point for labels
  filtered_data <- subset(data_long, Year > (max(Year) - 3))
  label_pos <- stats::aggregate(Value ~ Source, data = filtered_data, FUN = mean, na.rm = TRUE)
  names(label_pos)[2] <- "Label_Y"
  label_pos$Label_X <- max(data_long$Year) + year_offset
  label_pos$Label_Name <- gsub("TTI ", "", label_pos$Source) 
  return(label_pos)
}

tti_label_positions <- get_tti_label_pos(tti_df_long)

# --- 5. PLOT GENERATION ---
baltic_tti_plot_final <- ggplot(tti_df_long, aes(x = Year, y = Value, color = Source)) +
  
  # A. Background Regime Shading
  annotate("rect", xmin = 1953, xmax = 1974, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.07) +
  annotate("rect", xmin = 1974, xmax = 1988, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.07) +
  annotate("rect", xmin = 1988, xmax = 2012, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.07) +
  annotate("rect", xmin = 2012, xmax = 2022, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.07) +
  
  # B. Reference Lines
  geom_vline(xintercept = c(1974, 1988, 2012), color = "red", linetype = "longdash", linewidth = 0.8) +
  annotate("segment", x = 1953, xend = 2022, y = GES_THRESHOLD, yend = GES_THRESHOLD, 
           linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  annotate("text", x = 1960, y = GES_THRESHOLD, vjust = -0.5, 
           label = paste0("GES (", GES_THRESHOLD, ")"), color = "darkgreen", size = 5) +
  
  annotate("text", x = 1960, y = GES_THRESHOLD, vjust = -0.5, 
           label = paste0("GES (", GES_THRESHOLD, ")"), color = "darkgreen", size = 5) +
  
  # C. Lines and Points
  # Plot the raw points with slight transparency
  geom_point(aes(shape = Source), size = 2, alpha = 0.6, na.rm = TRUE) +
  # Apply loess smoothing to emulate the 3-year rollmean (span=0.15 creates a tight, localized fit)
  geom_smooth(aes(linewidth = Source), method = "loess", span = 0.15, se = FALSE, na.rm = TRUE) +
  
  # D. Direct Labels (Deep, Shallow, All)
  geom_text(data = tti_label_positions, 
            aes(x = Label_X, y = Label_Y, label = Label_Name, color = Source), 
            size = 5.5, fontface = "bold", inherit.aes = FALSE,
            hjust = 0, vjust = 0.5) +
  
  # E. Scales
  scale_color_manual(values = all_colors_tti) +
  scale_shape_manual(values = plot_shapes) +
  scale_linewidth_manual(values = plot_linewidths) +
  scale_y_continuous(limits = c(0.45, 0.82), breaks = seq(0.4, 0.8, 0.1)) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10), 
                     expand = expansion(mult = c(0.02, 0.12))) + 
  
  # F. Theme & Labels
  labs(
    title = "Baltic Sea spatial Mean TTI (All, Shallow, Deep)", 
    x = "Year", 
    y = "Mean TTI [-]"
  ) +
  theme_pubr(base_size = 14) +
  theme(
    plot.margin = margin(t = 0.5, r = 1, b = 0.5, l = 0.5, unit = "cm"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0, color = "grey30"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none" 
  )

print(baltic_tti_plot_final)

# --- 6. EXPORT ---
ggsave(
  filename = "Figure_3_Baltic_TTI_deep_shallow_smoothed.png", 
  plot = baltic_tti_plot_final, 
  width = 10,      
  height = 6,      
  units = "in", 
  dpi = 300,       
  bg = "white"     
)

print("Analysis complete. Figure 3 successfully saved.")

