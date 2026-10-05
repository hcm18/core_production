# =============================================================================*
# export_profiles_dataset.R ----
# -----------------------------------------------------------------------------*
# PURPOSE
#   Defines the PROFILES side of the final files: the published community
#   profiles extract. Four functions, in the order the caller uses them:
#
#     process_shared()            tags each demographic key with its Index_Group
#                                 (Total, Sex, Age, Race/Ethnicity)
#     process_profiles()          keeps the profiles rows, renames the columns to
#                                 their published names, rounds rates to 2
#                                 decimals, and rounds cases down
#     validate_profiles_columns() stops if a required column is missing or a
#                                 numeric field is not numeric
#     export_profiles_parallel()  writes the six profiles files (three geography
#                                 groupings x death / morbidity), in parallel
#
#   Files written, each as .csv and .rds:
#     profiles_all_geos_death       profiles_all_geos_morbidity
#     profiles_municipal_death      profiles_municipal_morbidity
#     profiles_regional_death       profiles_regional_morbidity
#     profiles_all_geos_all_outcomes   (death and morbidity in one file, used by
#                                       7_core_rates_review.R)
#
# INPUTS  : none (sourced by 6_final_files_prodn.R, which passes the data in)
# OUTPUTS : the files above, written to the out_dir given by the caller
#
# NOTE    : this script only DEFINES functions. Nothing runs until the caller
#           calls them.
# =============================================================================*

# ===========================================================================*
# Step 0 — Configuration ----
# ===========================================================================*
# No paths of its own: the caller passes out_dir. Packages are normally already
# loaded by 6_final_files_prodn.R; the fallback covers sourcing this on its own.
if (!exists("export_profiles_packages_loaded")) {
  if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
  pacman::p_load(dplyr, glue, future, future.apply)
  export_profiles_packages_loaded <- TRUE
}

# Columns the published profiles files must have, and which of them must hold
# numbers. Edit here if the published layout changes.
profiles_required_cols <- c(
  "Geography", "Year", "Outcome", "Condition_Group", "Condition",
  "Demographic_Group", "Demographic", "Cases",
  "Crude Rate", "Crude CI Lower", "Crude CI Upper",
  "Age-Adjusted (AA) Rate", "AA CI Lower", "AA CI Upper",
  "GeoType", "GeoName", "GeoID", "Region", "SES", "GeoName2"
)

profiles_numeric_cols <- c(
  "Cases",
  "Crude Rate", "Crude CI Lower", "Crude CI Upper",
  "Age-Adjusted (AA) Rate", "AA CI Lower", "AA CI Upper"
)

# Demographic keys published in the profiles, and the group each one belongs to.
# A demo_key that is not listed here is dropped from the profiles.
profiles_index_map <- c(
  "total"        = "Total",
  "female_total" = "Sex",
  "male_total"   = "Sex",
  "api_total"    = "Race/Ethnicity",
  "black_total"  = "Race/Ethnicity",
  "hisp_total"   = "Race/Ethnicity",
  "other_total"  = "Race/Ethnicity",
  "white_total"  = "Race/Ethnicity",
  "age0_9"       = "Age",
  "age10_19"     = "Age",
  "age20_29"     = "Age",
  "age30_39"     = "Age",
  "age40_49"     = "Age",
  "age50_59"     = "Age",
  "age60_69"     = "Age",
  "age70_79"     = "Age",
  "age80plus"    = "Age"
)

# How each demographic key is labelled in the published files.
profiles_demo_labels <- c(
  "total"        = "Total",
  "age0_9"       = "Age 0-9",
  "age10_19"     = "Age 10-19",
  "age20_29"     = "Age 20-29",
  "age30_39"     = "Age 30-39",
  "age40_49"     = "Age 40-49",
  "age50_59"     = "Age 50-59",
  "age60_69"     = "Age 60-69",
  "age70_79"     = "Age 70-79",
  "age80plus"    = "Age 80+",
  "api_total"    = "NH Asian/Pacific Islander",
  "black_total"  = "NH Black",
  "female_total" = "Female",
  "hisp_total"   = "Hispanic",
  "male_total"   = "Male",
  "other_total"  = "NH Other",
  "white_total"  = "NH White"
)

# Which GeoTypes go into each profiles geography grouping.
profiles_geotype_groups <- list(
  all_geos  = NULL,                             # NULL = every geography
  municipal = c("County", "Municipal"),
  regional  = c("County", "Region", "SRA")
)

# ===========================================================================*
# Step 1 — Validate the profiles structure ----
# ===========================================================================*
# Stops the run if the published layout is wrong. Called both before and after
# the export split, so a problem is caught before anything is written.
validate_profiles_columns <- function(df) {
  
  missing <- setdiff(profiles_required_cols, names(df))
  if (length(missing) > 0) {
    stop("PROFILES dataset is missing required columns:\n",
         paste(missing, collapse = ", "))
  }
  
  non_numeric <- profiles_numeric_cols[!vapply(df[profiles_numeric_cols], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop("These PROFILES fields must be numeric but are not:\n",
         paste(non_numeric, collapse = ", "))
  }
  
  message("PROFILES dataset passed validation.")
  df
}

# ===========================================================================*
# Step 2 — Shared processing ----
# ===========================================================================*
# Adds Index_Group: the demographic grouping each demo_key belongs to. Keys that
# are not published in the profiles get NA and are dropped in Step 3.
process_shared <- function(data) {
  data %>%
    mutate(
      Index_Group = ifelse(demo_key %in% names(profiles_index_map),
                           profiles_index_map[demo_key],
                           NA_character_)
    )
}

# ===========================================================================*
# Step 3 — Profiles processing ----
# ===========================================================================*
# Keeps the profiles rows, renames columns to their published names, rounds the
# rates, and rounds cases down. outcome_group is kept here for the death /
# morbidity split and removed again just before each file is written.
process_profiles <- function(shared) {
  
  shared %>%
    filter(use_group == "profiles",
           !is.na(Index_Group)) %>%
    mutate(
      Demographic       = profiles_demo_labels[demo_key],
      Demographic_Group = Index_Group
    ) %>%
    rename(
      Outcome                  = OUTCOME,
      Condition                = CONDITION,
      Cases                    = final_cases,
      `Crude Rate`             = final_crude_rate,
      `Crude CI Lower`         = final_crude_ci_lower,
      `Crude CI Upper`         = final_crude_ci_upper,
      `Age-Adjusted (AA) Rate` = final_aa_rate,
      `AA CI Lower`            = final_aa_ci_lower,
      `AA CI Upper`            = final_aa_ci_upper
    ) %>%
    mutate(
      Cases = floor(Cases),
      across(
        c(`Crude Rate`, `Crude CI Lower`, `Crude CI Upper`,
          `Age-Adjusted (AA) Rate`, `AA CI Lower`, `AA CI Upper`),
        ~ round(.x, 2)
      )
    ) %>%
    select(
      Geography, Year, Outcome, Condition_Group, Condition,
      Demographic_Group, Demographic,
      Cases,
      `Crude Rate`, `Crude CI Lower`, `Crude CI Upper`,
      `Age-Adjusted (AA) Rate`, `AA CI Lower`, `AA CI Upper`,
      GeoType, GeoName, GeoID, Region, SES, GeoName2,
      outcome_group   # kept for the death / morbidity filter, dropped on export
    )
}

# ===========================================================================*
# Step 4 — Write one profiles file ----  SAVES A FILE ->
# ===========================================================================*
export_profiles_dataset <- function(df, file_prefix, year_val, prod_date, out_dir) {
  
  df <- validate_profiles_columns(df)
  df <- df %>% select(-outcome_group)   # never published
  
  file_base <- paste0(file_prefix, "_", year_val, "_", prod_date)
  csv_path  <- file.path(out_dir, paste0(file_base, ".csv"))
  rds_path  <- file.path(out_dir, paste0(file_base, ".rds"))
  
  write.csv(df, csv_path, row.names = FALSE, na = "")   # SAVES A FILE ->
  saveRDS(df, rds_path)                                 # SAVES A FILE ->
  
  message(glue("Exported profiles: {file_prefix}"))
  
  data.frame(
    export = file_prefix,
    file   = c(csv_path, rds_path),
    rows   = nrow(df),
    stringsAsFactors = FALSE
  )
}

# ===========================================================================*
# Step 5 — Write all seven profiles files (in parallel) ----  SAVES A FILE ->
# ===========================================================================*
# Three geography groupings x death / morbidity, plus one all-geographies file
# holding both outcome groups together. The files are written by background
# workers, so their messages may not appear in the console; the returned table
# is what 6_final_files_prodn.R checks.
export_profiles_parallel <- function(profiles_clean, out_dir = ".") {
  
  profiles_clean <- validate_profiles_columns(profiles_clean)
  
  year_val  <- unique(profiles_clean$Year)
  prod_date <- format(Sys.Date(), "%m%d%Y")
  
  geotype_groups <- lapply(profiles_geotype_groups, function(types) {
    if (is.null(types)) profiles_clean else profiles_clean %>% filter(GeoType %in% types)
  })
  
  task_list <- rbind(
    expand.grid(geo_label = names(geotype_groups),
                out_label = c("death", "morbidity"),
                stringsAsFactors = FALSE),
    # One combined file: every geography, both outcome groups.
    data.frame(geo_label = "all_geos", out_label = "all_outcomes",
               stringsAsFactors = FALSE)
  )
  
  workers <- min(future::availableCores(), nrow(task_list))
  plan(multisession, workers = workers)
  on.exit(plan(sequential), add = TRUE)   # release the workers when done
  
  written <- future_lapply(seq_len(nrow(task_list)), function(i) {
    
    geo_label <- task_list$geo_label[i]
    out_label <- task_list$out_label[i]
    df        <- geotype_groups[[geo_label]]
    
    df <- if (out_label == "death") {
      df %>% filter(outcome_group %in% c("death", "both"), Outcome == "Death")
    } else if (out_label == "morbidity") {
      df %>% filter(outcome_group %in% c("morbidity", "both"), Outcome != "Death")
    } else {
      # all_outcomes: keep each row only where its condition belongs to that
      # outcome side, so the combined file matches the two split files exactly.
      df %>% filter((Outcome == "Death" & outcome_group %in% c("death", "both")) |
                      (Outcome != "Death" & outcome_group %in% c("morbidity", "both")))
    }
    
    if (nrow(df) == 0) {
      warning("No rows found for: profiles_", geo_label, "_", out_label)
      return(NULL)
    }
    
    export_profiles_dataset(df, paste0("profiles_", geo_label, "_", out_label),
                            year_val, prod_date, out_dir)
  })
  
  message("All profiles exports finished.")
  invisible(do.call(rbind, written))
}