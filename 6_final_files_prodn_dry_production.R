# =============================================================================*
# 6_final_files_prodn_dry_production.R ----
# -----------------------------------------------------------------------------*
# PURPOSE
#   Dry-production variant of 6_final_files_prodn.R with the QC checkpoint
#   removed. Same production path, and it still writes the data check workbook
#   with all its tabs, but it does NOT source qc_functions.R and writes no
#   qc_*.txt report. Use 6_final_files_prodn.R for a run that needs QC.
#
#   This is the last step of the rates step. Takes the suppressed rates file 
#   from step 5, attaches the condition classification, and writes the two 
#   published series:
#
#     COMPLETE : every row and every demographic group (internal use)
#     PROFILES : the community profiles extract, published column names, rates
#                rounded to 2 decimals, cases rounded down
#
#   The writing is done by two sourced scripts:
#     export_complete_datasets.R   the complete series
#     export_profiles_datasets.R    the profiles series
#
# INPUTS
#   - project_root/rates/data/output/ddi/df_with_suppression_all.rds   (from step 5)
#   - project_root/update_lists/classify_conditions.csv
#
# OUTPUTS (written to disk)
#   - project_root/rates/data/output/final/complete_*.csv and .rds       3 exports
#   - project_root/rates/data/output/final/profiles_*.csv and .rds       7 exports
#   - project_root/rates/data check/export_check_final_files_<year>.xlsx  data check workbook
# =============================================================================*

# ===========================================================================*
# Step 0 — Configuration ----
# ===========================================================================*
# --- Single editable root. Everything else is derived from it. ---
project_root <- "D:/core_r_HM"

rates_dir        <- file.path(project_root, "rates")
scripts_dir      <- file.path(rates_dir, "scripts")
ddi_dir          <- file.path(rates_dir, "data", "output", "ddi")
final_dir        <- file.path(rates_dir, "data", "output", "final")
update_lists_dir <- file.path(project_root, "update_lists")

# --- Input files and the two export scripts ---
rates_input            <- file.path(ddi_dir, "df_with_suppression_all.rds")
classify_file          <- file.path(update_lists_dir, "classify_conditions.csv")
export_complete_script <- file.path(scripts_dir, "export_complete_datasets.R")
export_profiles_script <- file.path(scripts_dir, "export_profiles_datasets.R")


# --- Folder for the data check workbook ---
data_check_dir <- file.path(rates_dir, "data check")

# --- Geographies used internally that are NOT published in the profiles ---
# Add or remove names here. Use character(0) to publish every geography.
internal_geographies_to_drop <- c("Harbison Crest-El Cajon")

# --- Create this project's output directories if they don't exist ---
for (d in c(final_dir, data_check_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ---- Load Packages ---------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyr,
  readr,
  glue,
  future,
  future.apply,
  openxlsx     # writes the data check workbook in Step 7
)

# ===========================================================================*
# Step 1 — Load the export scripts ----
# ===========================================================================*
source(export_complete_script)
source(export_profiles_script)

# ===========================================================================*
# Step 2 — Load the suppressed rates file ----
# ===========================================================================*
raw_data <- readRDS(rates_input)

# Drop rows with a missing Year.
raw_data <- raw_data %>% 
  filter(!is.na(Year))

# ===========================================================================*
# Step 3 — Attach the condition classification ----
# ===========================================================================*
# classify_conditions.csv decides which conditions are published in the profiles
# (use_group), whether a condition belongs to death, morbidity, or both
# (outcome_group), and how it is grouped on the profile (Condition_Group).
classify <- read_csv(classify_file, show_col_types = FALSE)

raw_data <- raw_data %>%
  left_join(classify, by = "CONDITION")

# ===========================================================================*
# Step 4 — Shared processing ----
# ===========================================================================*
# Tags every demographic key with its published grouping (Total, Sex, Age,
# Race/Ethnicity). Keys that are not published get NA and drop out in Step 6.
shared <- process_shared(raw_data)

# ===========================================================================*
# Step 5 — COMPLETE series ----  SAVES A FILE ->
# ===========================================================================*
# Every row and every demographic group. Not filtered by use_group.
complete_clean <- raw_data

complete_files <- export_complete_datasets(
  complete_clean,
  out_dir = final_dir
)

# ===========================================================================*
# Step 6 — PROFILES series ----  SAVES A FILE ->
# ===========================================================================*
profiles_clean <- process_profiles(shared)

# Remove geographies that are used internally and are not published.
profiles_clean <- profiles_clean %>%
  filter(!Geography %in% internal_geographies_to_drop)

profiles_clean <- validate_profiles_columns(profiles_clean)

profiles_files <- export_profiles_parallel(
  profiles_clean,
  out_dir = final_dir
)

message("Pipeline finished.")

# ===========================================================================*
# Step 7 — Export check workbook ----
# ===========================================================================*
# A short workbook written to project_root/rates/data check, confirming the exports
# hold what they should: the right conditions, outcomes, condition groups,
# demographic (index) groups, geographies, and GeoTypes. The full data check on
# the rates themselves is produced by step 5 from df_with_suppression_all.

export_check_file <- file.path(data_check_dir,
                               paste0("export_check_final_files_",
                                      paste(unique(complete_clean$Year), collapse = "_"), ".xlsx"))

# --- 1. Contents of each export -----------------------------------------------
# Every export is rebuilt here exactly as the export scripts filtered it, so the
# contents can be checked without re-reading the files from disk.
shared_for_exports <- shared   # carries Index_Group for the COMPLETE series

export_frames <- list(
  "complete" = shared_for_exports,
  "complete_death" = shared_for_exports %>%
    filter(outcome_group %in% c("both", "death"), OUTCOME == "Death"),
  "complete_morbidity" = shared_for_exports %>%
    filter(outcome_group %in% c("both", "morbidity"), OUTCOME != "Death"),
  "profiles_all_geos_death" = profiles_clean %>%
    filter(outcome_group %in% c("both", "death"), Outcome == "Death"),
  "profiles_all_geos_morbidity" = profiles_clean %>%
    filter(outcome_group %in% c("both", "morbidity"), Outcome != "Death"),
  "profiles_all_geos_all_outcomes" = profiles_clean %>%
    filter((Outcome == "Death" & outcome_group %in% c("both", "death")) |
             (Outcome != "Death" & outcome_group %in% c("both", "morbidity"))),
  "profiles_municipal_death" = profiles_clean %>%
    filter(GeoType %in% c("County", "Municipal"),
           outcome_group %in% c("both", "death"), Outcome == "Death"),
  "profiles_municipal_morbidity" = profiles_clean %>%
    filter(GeoType %in% c("County", "Municipal"),
           outcome_group %in% c("both", "morbidity"), Outcome != "Death"),
  "profiles_regional_death" = profiles_clean %>%
    filter(GeoType %in% c("County", "Region", "SRA"),
           outcome_group %in% c("both", "death"), Outcome == "Death"),
  "profiles_regional_morbidity" = profiles_clean %>%
    filter(GeoType %in% c("County", "Region", "SRA"),
           outcome_group %in% c("both", "morbidity"), Outcome != "Death")
)

# Each row is one export, with what it actually contains. Read it across: an
# export that picked up an outcome or GeoType it should not have is obvious here.
summarise_export <- function(df, export_name) {
  outcome_col <- if ("Outcome" %in% names(df)) "Outcome" else "OUTCOME"
  cond_col    <- if ("Condition" %in% names(df)) "Condition" else "CONDITION"
  demo_col    <- if ("Demographic_Group" %in% names(df)) "Demographic_Group" else "Index_Group"
  tibble(
    export             = export_name,
    rows               = nrow(df),
    outcomes           = paste(sort(unique(as.character(df[[outcome_col]]))), collapse = ", "),
    n_conditions       = n_distinct(df[[cond_col]]),
    condition_groups   = if ("Condition_Group" %in% names(df))
      paste(sort(unique(as.character(df$Condition_Group))), collapse = ", ")
    else NA_character_,
    demographic_groups = if (demo_col %in% names(df))
      paste(sort(unique(as.character(df[[demo_col]]))), collapse = ", ")
    else NA_character_,
    n_demographics     = if ("Demographic" %in% names(df)) n_distinct(df$Demographic)
    else n_distinct(df$demo_key),
    n_geographies      = n_distinct(df$Geography),
    geotypes           = paste(sort(unique(as.character(df$GeoType))), collapse = ", ")
  )
}

final_contents_tab <- bind_rows(
  lapply(names(export_frames), function(nm) summarise_export(export_frames[[nm]], nm))
)

# --- 2. Which condition and outcome is in which export -------------------------
# One row per condition and outcome, with a TRUE/FALSE column per export file.
condition_outcome_base <- shared_for_exports %>%
  distinct(Condition = CONDITION, Outcome = OUTCOME, use_group, outcome_group)

in_export <- function(df, export_name) {
  outcome_col <- if ("Outcome" %in% names(df)) "Outcome" else "OUTCOME"
  cond_col    <- if ("Condition" %in% names(df)) "Condition" else "CONDITION"
  df %>%
    distinct(Condition = .data[[cond_col]], Outcome = .data[[outcome_col]]) %>%
    mutate("in_{export_name}" := TRUE)
}

conditions_tab <- Reduce(
  function(acc, nm) left_join(acc, in_export(export_frames[[nm]], nm),
                              by = c("Condition", "Outcome")),
  names(export_frames),
  init = condition_outcome_base
) %>%
  mutate(across(starts_with("in_"), ~ !is.na(.x) & .x)) %>%
  arrange(Condition, Outcome)

# --- 3. Geographies and GeoTypes in the published profiles ---------------------
geographies_tab <- complete_clean %>%
  distinct(Geography, GeoType, GeoName, GeoID, Region) %>%
  mutate(in_profiles = Geography %in% profiles_clean$Geography) %>%
  arrange(GeoType, Geography)

geography_counts <- geographies_tab %>% count(GeoType, in_profiles, name = "n_geographies")

# --- 4. Demographic (index) groups --------------------------------------------
index_groups_tab <- profiles_clean %>%
  count(Demographic_Group, Demographic, name = "rows") %>%
  arrange(Demographic_Group, Demographic)

# --- 5. Geography completeness per profiles export ----------------------------
# For every condition/outcome/demographic combination present in a profiles export,
# there should be one row per required geography. Required geographies = the
# published geographies (from the COMPLETE series, minus the internal-only ones)
# whose GeoType that export is meant to carry. Any missing (combination x geography)
# cell is a gap. The geography universe itself (count of 74) is checked in step 5;
# this confirms the exports are rectangular across it.
published_geos <- complete_clean %>%
  distinct(Geography, GeoType) %>%
  filter(!Geography %in% internal_geographies_to_drop)

# GeoType scope per profiles export (NULL = all published GeoTypes).
profiles_geotype_scope <- list(
  profiles_all_geos_death        = NULL,
  profiles_all_geos_morbidity    = NULL,
  profiles_all_geos_all_outcomes = NULL,
  profiles_municipal_death       = c("County", "Municipal"),
  profiles_municipal_morbidity   = c("County", "Municipal"),
  profiles_regional_death        = c("County", "Region", "SRA"),
  profiles_regional_morbidity    = c("County", "Region", "SRA")
)
combo_keys <- c("Condition", "Outcome", "Demographic_Group", "Demographic")

required_geos_for <- function(export_name) {
  scope <- profiles_geotype_scope[[export_name]]
  if (is.null(scope)) published_geos else published_geos %>% filter(GeoType %in% scope)
}

# One export: the (combination x required geography) cells with no row in the export.
geo_gaps_for <- function(export_name) {
  df       <- export_frames[[export_name]]
  req_geos <- required_geos_for(export_name)
  combos   <- df %>% distinct(across(all_of(combo_keys)))
  expected <- tidyr::expand_grid(combos, req_geos)
  actual   <- df %>% distinct(across(all_of(c(combo_keys, "Geography"))))
  expected %>%
    anti_join(actual, by = c(combo_keys, "Geography")) %>%
    transmute(export = export_name, Condition, Outcome, Demographic_Group,
              Demographic, Geography, GeoType)
}

profiles_export_names <- names(profiles_geotype_scope)
geo_gaps <- bind_rows(lapply(profiles_export_names, geo_gaps_for))

geo_completeness_summary <- bind_rows(lapply(profiles_export_names, function(nm) {
  df       <- export_frames[[nm]]
  scope    <- profiles_geotype_scope[[nm]]
  req_geos <- required_geos_for(nm)
  n_combos <- df %>% distinct(across(all_of(combo_keys))) %>% nrow()
  tibble(
    export            = nm,
    geotypes_required = if (is.null(scope)) "all published" else paste(scope, collapse = ", "),
    n_required_geos   = nrow(req_geos),
    n_combinations    = n_combos,
    expected_rows     = n_combos * nrow(req_geos),
    present_rows      = df %>% distinct(across(all_of(c(combo_keys, "Geography")))) %>% nrow(),
    missing_rows      = sum(geo_gaps$export == nm)
  )
}))

# Missing-cells tab, guarded for size.
geo_gaps_tab <- if (nrow(geo_gaps) == 0) {
  data.frame(note = "None. Every condition/outcome/demographic has a row for every required geography in every profiles export.")
} else if (nrow(geo_gaps) > 1000000) {
  data.frame(note = paste0("Too many missing rows to list in a worksheet (",
                           format(nrow(geo_gaps), big.mark = ","),
                           "). See the summary tab; a whole geography or GeoType is likely absent."))
} else {
  geo_gaps
}

# --- Write the workbook ----  SAVES A FILE ->
export_check_intro <- c(
  paste0("EXPORT CHECK: final files, data year ",
         paste(unique(complete_clean$Year), collapse = ", ")),
  paste("Written:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Prepared by:", Sys.info()[["user"]]),
  "",
  "This workbook confirms the exports hold what they should. The data check on the",
  "rates themselves is the step 5 workbook, data_check_rates_<year>.xlsx.",
  "",
  "WHAT TO CHECK:",
  "1. Export contents: each export should list the right outcomes, condition groups,",
  "      demographic groups, number of conditions, geographies, and GeoTypes.",
  "      Confirm the following: ",
  "      -Death exports hold only Death.",
  "      -Municipal exports hold County and Municipal.",
  "      -Regional exports hold County, Region, and SRA.",
  "2. Conditions by export: each condition and outcome should show TRUE in the exports it",
  "      belongs to and FALSE in the ones it does not (a death-only condition is FALSE in",
  "      the morbidity files, and so on). Use the columns 'use_group' and 'outcome_group' to confirm",
  "       NOTE: complete datasets (complete, complete_death, and complete_morbidity) include internal only conditions.",
  "        Profiles datasets should NOT include internal only conditions",
  "3. Geographies: confirm only the intended geographies are marked in_profiles = FALSE: HCEC",
  "      3b. Confirm n_geographies for county =1, HCEC=1, Municipal=19, North County=1, Region = 6",
  "         SRA =41 and SupervisorDistrict=5",
  "4. Demographics: confirm the published demographic groups and labels are as expected.",
  "5. Geo completeness: for each profiles export, every condition/outcome/demographic",
  "      combination should have a row for each required geography. Confirm missing_rows = 0 for every export.",
  "5b. Missing geo rows: should say 'None'. Any rows are (combination x geography) cells",
  "      missing from that export.",
  "",
  "Notes:"
)

export_check_sheets <- list(
  "Cover page"          = data.frame(instructions = export_check_intro),
  "1 Export contents"   = final_contents_tab,
  "2 Conditions by export" = conditions_tab,
  "3 Geographies"       = geographies_tab,
  "3b Geography counts" = geography_counts,
  "4 Demographics"      = index_groups_tab,
  "5 Geo completeness"  = geo_completeness_summary,
  "5b Missing geo rows" = geo_gaps_tab
)

write.xlsx(export_check_sheets, file = export_check_file, overwrite = TRUE,
           headerStyle = createStyle(textDecoration = "bold"),
           firstRow = TRUE, colWidths = "auto")
message("Export check workbook written: ", export_check_file)
