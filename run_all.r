# ------------------------------------------------------------------------------
# Master Execution Script: Baltic Sea Nutrient Hysteresis (R Repository)
# Runs all main (Figures 1-5) and supplementary (SM Figures 1-7) scripts
# and logs all statistical and console outputs to 'collected_output.txt'
# ------------------------------------------------------------------------------

scripts <- c(
  "Plot_Fig1_Final_TNTP_MSFD_TTI_vars_breakpoints_2026_06_11.R",
  "Plot_Fig2_Final_CCF_MSFD_vars_TP_load_2026_06_11.R",
  "Plot_Fig3_Final_TTI_mean_3curves_Baltic_2026_06_11.R",
  "Plot_Fig4_Final_Hysteresis_deep_vars_TP_load_2026_06_11.R",
  "Plot_Fig5_Final_SI_IFR_network_causal_analysis_Baltic_ecosystem_2026_06_11.R",
  "Plot_SM_Fig1_CCF_MSFD_vars_TN_load_2026_06_11.R",
  "Plot_SM_Fig2_Final_Hysteresis_shallow_vars_TP_load_2026_06_11.R",
  "Plot_SM_Fig3_BP_TTI_LAG_nutrients_narrative_2026_06_11.R",
  "Plot_SM_Fig4_PPR_CHLA_annual_data_2026_06_11.R",
  "Plot_SM_Fig5_Baltic_DIP_deep_150m_Obs_Model.R",
  "Plot_SM_Fig6_Baltic_DIP_deep_50m_ICES.R",
  "Plot_SM_Fig7_Baltic_DIP_DIN_ODIN_Model.R"
)

output_log <- "collected_output.txt"
log_con <- file(output_log, open = "wt")
sink(log_con, split = TRUE)

cat("========================================================================\n")
cat("BALTIC SEA NUTRIENT HYSTERESIS - R BATCH EXECUTION LOG\n")
cat(sprintf("Timestamp: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("========================================================================\n\n")

start_total <- Sys.time()
success_count <- 0

for (i in seq_along(scripts)) {
  scr <- scripts[i]
  if (!file.exists(scr)) {
    cat(sprintf("[%d/%d] [SKIP] File not found: %s\n\n", i, length(scripts), scr))
    next
  }
  
  cat(sprintf("[%d/%d] Running: %s ...\n", i, length(scripts), scr))
  t0 <- Sys.time()
  
  tryCatch({
    source(scr, local = new.env(), print.eval = FALSE)
    dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    cat(sprintf("    --> [OK] Completed in %s s\n\n", dt))
    success_count <- success_count + 1
  }, error = function(e) {
    dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    cat(sprintf("    --> [ERROR] Failed after %s s: %s\n\n", dt, e$message))
  })
}

total_dt <- round(as.numeric(difftime(Sys.time(), start_total, units = "secs")), 2)
cat("========================================================================\n")
cat(sprintf("Batch run finished: %d/%d scripts succeeded in %s s.\n", 
            success_count, length(scripts), total_dt))
cat(sprintf("Full console output saved to: %s\n", output_log))
cat("========================================================================\n")

sink()
close(log_con)
