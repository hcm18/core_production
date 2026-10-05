# =============================================================================*
# export_complete_datasets.R ----
# -----------------------------------------------------------------------------*
# PURPOSE
#   Defines export_complete_datasets(): writes the COMPLETE series (every row,
#   every demographic group) from the suppressed rates file.
#
#   Three exports, each written as .csv and .rds:
#     1. complete_<year>_<prod date>            every row
#     2. complete_death_<year>_<prod date>      death rows only
#     3. complete_morbidity_<year>_<prod date>  morbidity rows only
#   The death / morbidity splits also respect outcome_group from
#   classify_conditions.csv, so a condition flagged "death" never appears in a
#   morbidity file and vice versa.
#
# INPUTS  : none (sourced by 6_final_files_prodn.R, which passes the data in)
# OUTPUTS : the files above, written to the out_dir given by the caller
#
# NOTE    : this script only DEFINES the function. Nothing runs until the caller
#           calls export_complete_datasets().
# =============================================================================*

# ===========================================================================*
# Step 0 — Configuration ----
# ===========================================================================*
# This script has no paths of its own: the caller passes out_dir. Packages are
# normally already loaded by 6_final_files_prodn.R; the fallback covers the case
# where this file is sourced on its own.
if (!exists("export_complete_packages_loaded")) {
  if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
  pacman::p_load(dplyr)
  export_complete_packages_loaded <- TRUE
}

# ===========================================================================*
# Step 1 — Write the complete series ----
# ===========================================================================*
# data    : the classified, suppressed rates data (one row per geography,
#           outcome, condition, and demographic group)
# out_dir : folder the files are written to
#
# Returns (invisibly) a data frame of everything written: file, rows, and which
# export it belongs to. 6_final_files_prodn.R uses it for its QC checks.
export_complete_datasets <- function(data, out_dir = ".") {
  
  # --- Year and production date used in every file name ---------------------
  year_val  <- unique(data$Year)
  prod_date <- format(Sys.Date(), "%m%d%Y")
  
  written <- list()
  
  # --- Helper: write one export as .csv and .rds ----  SAVES A FILE ->
  write_pair <- function(df, file_base, label) {
    csv_path <- file.path(out_dir, paste0(file_base, ".csv"))
    rds_path <- file.path(out_dir, paste0(file_base, ".rds"))
    
    write.csv(df, csv_path, row.names = FALSE, na = "")   # SAVES A FILE ->
    saveRDS(df, rds_path)                                 # SAVES A FILE ->
    
    message("Created: ", csv_path)
    message("Created: ", rds_path)
    
    data.frame(
      export = label,
      file   = c(csv_path, rds_path),
      rows   = nrow(df),
      stringsAsFactors = FALSE
    )
  }
  
  # --- Helper: the death / morbidity splits ---------------------------------
  # outcome_group comes from classify_conditions.csv:
  #   "death"     -> the condition is only reported for deaths
  #   "morbidity" -> only for ED, hospitalization, and in-patient
  #   "both"      -> reported for both, so it appears in both files
  filtered_export <- function(df, outcome_group_values, death_rows, label) {
    
    df_filtered <- df %>%
      filter(outcome_group %in% outcome_group_values,
             if (death_rows) OUTCOME == "Death" else OUTCOME != "Death")
    
    if (nrow(df_filtered) == 0) {
      warning("No rows found for: ", label)
      return(NULL)
    }
    
    write_pair(df_filtered,
               paste0("complete_", label, "_", year_val, "_", prod_date),
               paste0("complete_", label))
  }
  
  # --- Export 1: complete (unfiltered) ----  SAVES A FILE ->
  written[["complete"]] <- write_pair(
    data, paste0("complete_", year_val, "_", prod_date), "complete"
  )
  
  # --- Export 2: death only ----  SAVES A FILE ->
  written[["death"]] <- filtered_export(
    data,
    outcome_group_values = c("both", "death"),
    death_rows           = TRUE,
    label                = "death"
  )
  
  # --- Export 3: morbidity only ----  SAVES A FILE ->
  written[["morbidity"]] <- filtered_export(
    data,
    outcome_group_values = c("both", "morbidity"),
    death_rows           = FALSE,
    label                = "morbidity"
  )
  
  message("All complete-series exports finished.")
  invisible(do.call(rbind, written))
}