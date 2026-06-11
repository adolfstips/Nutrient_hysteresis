# Baltic Sea Nutrient Hysteresis

This repository contains the data, R scripts, and generated figures for the hysteresis and regime shift analysis of the Baltic Sea ecosystem. 

By utilizing a unified dataset containing annual means for Deep, Shallow, and All-region MSFD biogeochemical indicators alongside external nutrient loads, these scripts reproduce the phase-space trajectories, causality networks, and cross-correlation functions discussed in the article.

## 📂 Repository Structure

If exploring this repository, the files are categorized as follows:

### 📊 Data
* **`Baltic_variables_Mt_4_Causal_Input.csv`**: The master unified dataset (1954–2022). It contains annual values for Total Nitrogen (TN) and Total Phosphorus (TP) loads, as well as TTI (Trophic Transfer Index), GES (Good Environmental Status), MSFD biogeochemical variables (DIP, DIN, CHLA, Secchi), and Oxygen Deficit (DO2_Mt). Variables are aggregated for Deep basins, Shallow margins, and the entire Baltic region.

### 💻 Scripts & 📈 Figures

#### Main Article
* **Figure 1: Asynchronous Structural Regime Shifts**
  * Script: `Plot_Fig1_Final_TNTP_MSFD_TTI_vars_breakpoints_2026_06_11.R`
  * Output: `Figure_1_Baltic_Asynchronous_Shifts.png`
* **Figure 2: Time Lag Analysis (TP Load)**
  * Script: `Plot_Fig2_Final_CCF_MSFD_vars_TP_load_2026_06_11.R`
  * Output: `Figure_2_Baltic_CCF_TP_HELCOM_deep.png`
* **Figure 3: Trophic Transfer Index (TTI) Over Time**
  * Script: `Plot_Fig3_Final_TTI_mean_3curves_Baltic_2026_06_11.R`
  * Output: `Figure_3_Baltic_TTI_deep_shallow_smoothed.png`
* **Figure 4: Phase-Space Trajectories (Deep Basin Hysteresis)**
  * Script: `Plot_Fig4_Final_Hysteresis_deep_vars_TP_load_2026_06_11.R`
  * Output: `Figure_4_Baltic_Hysteresis_Combined.png`
* **Figure 5: System Impact & IFR Causality Networks**
  * Script: `Plot_Fig5_Final_SI_IFR_network_causal_analysis_Baltic_ecosystem_2026_06_11.R`
  * Output: `Figure_5_Baltic_Causality_Combined.png`

#### Supplementary Material (SM)
* **SM Figure 1: Time Lag Analysis (TN Load)**
  * Script: `Plot_SM_Fig1_CCF_MSFD_vars_TN_load_2026_06_11.R`
  * Output: `Figure_SM1_Baltic_CCF_TN_HELCOM_deep.png`
* **SM Figure 2: Phase-Space Trajectories (Shallow Basin Hysteresis)**
  * Script: `Plot_SM_Fig2_Final_Hysteresis_shallow_vars_TP_load_2026_06_11.R`
  * Output: `Figure_SM2_Baltic_Hysteresis_Shallow.png`
* **SM Figure 3: Nutrients Narrative & CCF Composite**
  * Script: `plot_SM_Fig3_BP_TTI_LAG_nutrients_narrative_2026_06_11.R`
  * Output: `Figure_SM3_Baltic_TTI_LAG_TP_CCF_Analysis_Composite.png`
* **SM Figure 4: Baltic Sea Productivity (PPR vs Chl-a)**
  * Script: `plot_SM_Fig4_PPR_CHLA_annual_data_2026_06_11.R`
  * Output: `Figure_SM4_Baltic_PPR_CHLA.png`

## 🚀 How to Run
1. Clone this repository to your local machine.
2. Ensure you have R installed along with the required libraries: `ggplot2`, `dplyr`, `tidyr`, `ggpubr`, `strucchange`, `patchwork`, `igraph`, and `ggnetwork`.
3. Run any of the `.R` scripts. They are fully self-contained and will automatically read from the unified `.csv` file and export high-resolution `.png` figures.