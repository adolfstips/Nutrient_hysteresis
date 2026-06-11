# ------------------------------------------------------------------------------
# Baltic Ecosystem Phase-Space Trajectories (Hysteresis)
# SUPPLEMENTARY FIGURE: Shallow Regions
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggrepel)

# --- 1. SETTINGS & THRESHOLDS ---
ges_threshold <- 0.71
bp_years <- c(1974, 1988, 2012) 

# Focus ONLY on the critical 1988 regime shift
highlight_year <- 1988

# =====================================================================
# 2. DATA LOADING
# =====================================================================

# Load unified dataset and filter to study period
grand_df <- read.csv("Baltic_variables_Mt_4_Causal_Input.csv") %>%
  dplyr::filter(Year >= 1954 & Year <= 2022) %>%
  dplyr::arrange(Year)

# Subset for label highlighting
labels_df <- grand_df %>% filter(Year %in% bp_years)

# =====================================================================
# 3. PLOT A: TTI HYSTERESIS (PHASE-SPACE)
# =====================================================================

p_TTI_hyst_shallow <- ggplot(grand_df, aes(x = TP_load, y = TTI_SHALLOW)) +
  
  # Trajectory path and arrows
  geom_path(arrow = arrow(length = unit(0.2, "cm"), type = "closed"), 
            color = "grey60", alpha = 0.8) +
  
  # Threshold line and label
  geom_hline(yintercept = ges_threshold, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  annotate("text", x = min(grand_df$TP_load) + 3, y = ges_threshold, vjust = -0.8,
           label = paste0("GES (", ges_threshold, ")"), color = "darkgreen", size = 5, fontface = "bold") +
  
  # Data Points (Plotted after path so circles sit neatly on top of the arrow tips)
  geom_point(aes(fill = Year), shape = 21, size = 4.5, stroke = 0.5, color = "black") +
  
  # Annotate break point years
  geom_text_repel(data = labels_df, aes(label = Year),
                  fontface = "bold", color = "black", size = 4.5,
                  box.padding = 0.5, point.padding = 0.3) +
  
  # Emphasize 1988 Regime Shift (Red Halo)
  geom_point(data = filter(grand_df, Year == highlight_year),
             shape = 1, size = 7, color = "red", stroke = 2) +
  
  # Label 1988 prominently
  geom_text_repel(data = filter(grand_df, Year == highlight_year),
                  aes(label = "Regime Shift"),
                  nudge_x = -0.4, nudge_y = -0.05, 
                  fontface = "bold", color = "red", size = 5) +
  
  # Custom, tall colorbar on the right side
  scale_fill_viridis_c(
    option = "magma", direction = -1, name = "Year",
    guide = guide_colorbar(
      barwidth = unit(1.2, "cm"),  
      barheight = unit(7, "cm"),   
      ticks.linewidth = 1.5        
    )
  ) +
  
  theme_pubr(base_size = 14) +
  labs(
    title = "Phase-Space Trajectories Shallow Baltic Sea: TTI", 
    subtitle = "Reversibility and Hysteresis in Coastal Margins",
    x = "Total Phosphorus Load (kt)",
    y = "TTI [-]"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "grey30"),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "right", 
    legend.title = element_text(size = 14, face = "bold"),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")
  )

# =====================================================================
# 4. PLOT B: BIOGEOCHEMISTRY FACETS
# =====================================================================

shallow_biogeo <- grand_df %>%
  select(Year, TP_load, CHLA_SHALLOW, DIP_SHALLOW, DIN_SHALLOW) %>%
  pivot_longer(cols = c(CHLA_SHALLOW, DIP_SHALLOW, DIN_SHALLOW), names_to = "Variable", values_to = "Value")

shallow_biogeo$Variable <- factor(
  shallow_biogeo$Variable,
  levels = c("CHLA_SHALLOW", "DIP_SHALLOW", "DIN_SHALLOW"),
  labels = c("Chlorophyll-a (Shallow)", "DIP (Shallow)", "DIN (Shallow)")
)

labels_biogeo_df <- shallow_biogeo %>% filter(Year %in% bp_years)

p_shallow_facet <- ggplot(shallow_biogeo, aes(x = TP_load, y = Value)) +
  geom_path(color = "grey60", linewidth = 0.6, alpha = 0.8) +
  geom_point(aes(fill = Year), shape = 21, size = 3, stroke = 0.5, color = "black") +
  
  geom_text_repel(data = labels_biogeo_df, aes(label = Year), 
                  size = 4, fontface = "bold",
                  box.padding = 0.4, point.padding = 0.2) +
  
  scale_fill_viridis_c(option = "magma", direction = -1) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) + 
  
  theme_pubr(base_size = 14) +
  labs(
    title = "Memory Effects in Shallow Baltic Biogeochemistry",
    subtitle = "Tighter linear reversibility compared to deep basins",
    x = "Total Phosphorus Load (kt)",
    y = bquote("Concentration ("*mg/m^3*")")
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "grey30"),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey95", color = NA),
    axis.title = element_text(size = 13, face = "bold"),
    panel.spacing.x = unit(1.5, "lines"),
    legend.position = "none", 
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")
  )

# =====================================================================
# 5. COMBINE AND EXPORT
# =====================================================================

final_hysteresis_shallow_plot <- ggarrange(
  p_TTI_hyst_shallow, p_shallow_facet,
  ncol = 1, nrow = 2,
  labels = c("A", "B")
)

plot(final_hysteresis_shallow_plot)

ggsave(
  filename = "Figure_SM2_Baltic_Hysteresis_Shallow.png", 
  plot = final_hysteresis_shallow_plot, 
  width = 12,      
  height = 11,      
  units = "in", 
  dpi = 300,       
  bg = "white"     
)

print("Analysis complete. Shallow Hysteresis figure successfully saved.")
