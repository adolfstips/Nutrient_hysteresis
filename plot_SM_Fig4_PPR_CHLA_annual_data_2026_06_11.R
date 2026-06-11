# ------------------------------------------------------------------------------
# Baltic Ecosystem Productivity
# SUPPLEMENTARY FIGURE 4: PPR vs Chlorophyll-a (ALL REGION)
# ------------------------------------------------------------------------------

library(ggplot2)
library(zoo)
library(ggpubr)
library(dplyr)

# --- 1. DATA LOADING ---
# Read directly from the unified dataset (assuming PPR_ALL is now included)
df_all <- read.csv("Baltic_variables_Mt_4_Causal_Input.csv") %>%
  dplyr::filter(Year >= 1954 & Year <= 2022) %>%
  dplyr::select(Year, PPR = PPR_ALL, CHLA = CHLA_ALL)

# --- 2. FILTERING & SCALING ---
# 3-year rolling mean for smoothed lines
df_all$PPR_fil  <- zoo::rollmean(df_all$PPR, k = 3, fill = NA, align = "center")
df_all$CHLA_fil <- zoo::rollmean(df_all$CHLA, k = 3, fill = NA, align = "center")

# Scaling factor to plot CHLA on the same axis as PPR
scale_factor <- max(df_all$PPR, na.rm = TRUE) / max(df_all$CHLA, na.rm = TRUE)

# --- 3. STATISTICAL CALCULATION ---
cor_res <- cor.test(df_all$PPR, df_all$CHLA)

# Format the correlation string for the subtitle
cor_label <- paste0("r = ", round(cor_res$estimate, 2), 
                    " [CI: ", round(cor_res$conf.int[1], 2), "-", round(cor_res$conf.int[2], 2), "], ",
                    "p < 0.001")

# --- 4. PLOTTING ---
plot_PPR_CHLA <- ggplot(df_all, aes(x = Year)) +
  
  # Background Shading
  annotate("rect", xmin = 1953, xmax = 1974, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.07) +
  annotate("rect", xmin = 1974, xmax = 1988, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.07) +
  annotate("rect", xmin = 1988, xmax = 2012, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.07) +
  annotate("rect", xmin = 2012, xmax = 2022, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.07) +
  
  # Raw Points (Transparent)
  geom_point(aes(y = PPR), color = "darkgreen", alpha = 0.3, size = 1.5) +
  geom_point(aes(y = CHLA * scale_factor), color = "steelblue", alpha = 0.3, size = 1.5) +
  
  # Filtered Lines
  geom_line(aes(y = PPR_fil), color = "darkgreen", linewidth = 1.2, na.rm = TRUE) +
  geom_line(aes(y = CHLA_fil * scale_factor), color = "steelblue", linewidth = 1.2, na.rm = TRUE) +
  
  # Breakpoints
  geom_vline(xintercept = c(1974, 1988, 2012), color = "red", linetype = "longdash", linewidth = 0.8) +
  
  # Dual Y-Axis with Fixed Unit Labels
  scale_y_continuous(
    name = expression(paste("PPR (", mg~C~m^{-2}~d^{-1}, ")")),
    sec.axis = sec_axis(~ . / scale_factor, 
                        name = expression(paste("Chl-a (", mg~m^{-3}, ")")))
  ) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), expand = expansion(mult = c(0.02, 0.12))) +
  
  # Direct Line Labels
  annotate("text", x = 2023, y = tail(na.omit(df_all$PPR_fil), 1), 
           label = "PPR", color = "darkgreen", fontface = "bold", hjust = 0) +
  annotate("text", x = 2023, y = tail(na.omit(df_all$CHLA_fil), 1) * scale_factor, 
           label = "Chl-a", color = "steelblue", fontface = "bold", hjust = 0) +
  
  # Titles
  labs(title = "Baltic Sea Productivity: PPR vs Chlorophyll-a",
       subtitle = paste("Area-weighted PPR vs. Chl-a | Statistics:", cor_label),
       x = "Year") +
  
  theme_pubr(base_size = 14) +
  theme(
    axis.title.y.left  = element_text(color = "darkgreen", face = "bold"),
    axis.title.y.right = element_text(color = "steelblue", face = "bold"),
    plot.title = element_text(face = "bold")
  )

print(plot_PPR_CHLA)

# --- 5. EXPORT ---
ggsave(
  filename = "Figure_SM4_Baltic_PPR_CHLA.png", 
  plot = plot_PPR_CHLA, 
  width = 10,      
  height = 6,      
  units = "in", 
  dpi = 300,       
  bg = "white"     
)

print("Analysis complete. Figure PPR-CHLa successfully saved.")

