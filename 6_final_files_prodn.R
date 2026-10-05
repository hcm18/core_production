# =============================================================================*
# 6_final_files_prodn.R ----
# -----------------------------------------------------------------------------*
# PURPOSE
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
#   - project_root/rates/qc/qc_6_final_files_prodn.txt                   QC checkpoint
#   - project_root/rates/data check/data_check_rates_<year>.xlsx   workbook for data check
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


# --- Shared QC helper functions and the QC report folder ---
qc_functions_script <- file.path(project_root, "shared_scripts", "qc_functions.R")
qc_dir              <- file.path(rates_dir, "qc")

# --- Folder for the export check workbook ---
# The full human-eyes data check on the rates is produced by step 5. This step
# only confirms the exports hold what they should.
data_check_dir <- file.path(rates_dir, "data check")

# --- How many geographies the pipeline should produce ---
# Used to confirm every condition, outcome, and demographic combination has a
# row for every geography.
expected_geography_count <- 74

# --- Geographies used internally that are NOT published in the profiles ---
# Add or remove names here. Use character(0) to publish every geography.
internal_geographies_to_drop <- c("Harbison Crest-El Cajon")

# --- Create this project's output directories if they don't exist ---
for (d in c(final_dir, qc_dir, data_check_dir)) {
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

# ---- Start QC log ------------------------------------------------------------
# Checks and printouts below are written to project_root/rates/qc/qc_6_final_files_prodn.txt
# by the QC CHECK section at the bottom. Any FAIL (or R error) stops the script
# AND writes the report.
if (!file.exists(qc_functions_script))
  stop("QC helper script not found: ", qc_functions_script)
source(qc_functions_script)
qc_start("6_final_files_prodn.R", qc_dir, reset = TRUE)

# ===========================================================================*
# Step 1 — Load the export scripts ----
# ===========================================================================*
qc_input_exists(export_complete_script, "export_complete_datasets.R")
qc_input_exists(export_profiles_script, "export_profiles_datasets.R")

source(export_complete_script)
source(export_profiles_script)

# QC CHECK: every function the steps below call is now defined. If one is
# missing, the message names it and the file that should have defined it.
required_export_funs <- c(
  "export_complete_datasets",                                  # export_complete_datasets.R
  "process_shared", "process_profiles", "validate_profiles_columns",
  "export_profiles_dataset", "export_profiles_parallel"        # export_profiles_dataset.R
)
missing_export_funs <- required_export_funs[
  !vapply(required_export_funs, function(f) exists(f, mode = "function"), logical(1))
]
qc_log("Export scripts sourced",
       data.frame(script = c(export_complete_script, export_profiles_script),
                  modified = format(file.mtime(c(export_complete_script, export_profiles_script)),
                                    "%Y-%m-%d %H:%M:%S")))
qc_check("Export functions loaded", length(missing_export_funs) == 0,
         details = c(
           paste("Not defined after sourcing:", paste(missing_export_funs, collapse = ", ")),
           "Check that the two scripts listed in the printout above are the updated versions",
           "(each opens with a header block naming the script and its purpose), and that there",
           "is no second, older copy of either one in the scripts folder."
         ))

# ===========================================================================*
# Step 2 — Load the suppressed rates file ----
# ===========================================================================*
qc_input_exists(rates_input, "df_with_suppression_all.rds")
raw_data <- readRDS(rates_input)

qc_required_columns(
  raw_data,
  c("Geography", "Year", "OUTCOME", "CONDITION", "demo_key", "GeoType", "GeoName",
    "GeoID", "Region", "SES", "GeoName2", "final_cases", "final_crude_rate",
    "final_crude_ci_lower", "final_crude_ci_upper", "final_aa_rate",
    "final_aa_ci_lower", "final_aa_ci_upper"),
  "df_with_suppression_all.rds"
)
qc_set_year(unique(na.omit(raw_data$Year)))
n_rows_input <- nrow(raw_data)

# Drop rows with a missing Year (temporary step).
qc_check("No rows with a missing Year", !anyNA(raw_data$Year), on_fail = "warn",
         details = c("These rows are dropped and never reach the published files:",
                     .qc_capture(raw_data %>% filter(is.na(Year)) %>%
                                   count(OUTCOME, CONDITION, name = "rows_dropped"))))

raw_data <- raw_data %>% 
  filter(!is.na(Year))

# ===========================================================================*
# Step 3 — Attach the condition classification ----
# ===========================================================================*
# classify_conditions.csv decides which conditions are published in the profiles
# (use_group), whether a condition belongs to death, morbidity, or both
# (outcome_group), and how it is grouped on the profile (Condition_Group).
qc_input_exists(classify_file, "classify_conditions.csv")
classify <- read_csv(classify_file, show_col_types = FALSE)

qc_required_columns(classify, c("CONDITION", "use_group", "outcome_group", "Condition_Group"),
                    "classify_conditions.csv")
qc_check("classify_conditions.csv has one row per CONDITION",
         !any(duplicated(classify$CONDITION)),
         details = classify %>% filter(CONDITION %in% CONDITION[duplicated(CONDITION)]))

n_rows_before_classify <- nrow(raw_data)
raw_data <- raw_data %>%
  left_join(classify, by = "CONDITION")

qc_check("Classification join did not change the row count",
         nrow(raw_data) == n_rows_before_classify,
         details = sprintf("before = %s rows, after = %s rows",
                           n_rows_before_classify, nrow(raw_data)))
qc_check("Every CONDITION has a classification", !anyNA(raw_data$use_group),
         on_fail = "warn",
         details = c("Missing from classify_conditions.csv (a new condition, or the label changed):",
                     .qc_capture(raw_data %>% filter(is.na(use_group)) %>%
                                   distinct(OUTCOME, CONDITION))))

qc_log("Conditions by use_group and outcome_group",
       raw_data %>%
         distinct(CONDITION, use_group, outcome_group) %>%
         count(use_group, outcome_group, name = "n_conditions"))

# ===========================================================================*
# Step 4 — Shared processing ----
# ===========================================================================*
# Tags every demographic key with its published grouping (Total, Sex, Age,
# Race/Ethnicity). Keys that are not published get NA and drop out in Step 6.
shared <- process_shared(raw_data)

qc_log("Demographic keys by published grouping (Index_Group)",
       shared %>% count(Index_Group, name = "n_rows"),
       note = "NA rows are demographic keys that are not published in the profiles, for example the age-adjustment age bands. They stay in the COMPLETE series.")

# ===========================================================================*
# Step 5 — COMPLETE series ----  SAVES A FILE ->
# ===========================================================================*
# Every row and every demographic group. Not filtered by use_group.
complete_clean <- raw_data

complete_files <- export_complete_datasets(
  complete_clean,
  out_dir = final_dir
)

qc_log("COMPLETE series files written", complete_files)

# ===========================================================================*
# Step 6 — PROFILES series ----  SAVES A FILE ->
# ===========================================================================*
profiles_clean <- process_profiles(shared)

qc_check("PROFILES series has rows", nrow(profiles_clean) > 0,
         details = "No rows have use_group == 'profiles' with a published demographic key. Check classify_conditions.csv.")

# Remove geographies that are used internally and are not published.
n_rows_before_geo_drop <- nrow(profiles_clean)
profiles_clean <- profiles_clean %>%
  filter(!Geography %in% internal_geographies_to_drop)

qc_check("Internal geographies were found and removed",
         length(internal_geographies_to_drop) == 0 ||
           nrow(profiles_clean) < n_rows_before_geo_drop,
         on_fail = "warn",
         details = c("None of these geography names matched, so nothing was removed. Check the spelling against the data:",
                     paste(internal_geographies_to_drop, collapse = ", ")))
qc_log("Internal geographies removed from the profiles", data.frame(
  measure = c("rows before removal", "rows removed", "rows published",
              "geographies removed"),
  value   = c(n_rows_before_geo_drop,
              n_rows_before_geo_drop - nrow(profiles_clean),
              nrow(profiles_clean),
              paste(internal_geographies_to_drop, collapse = ", "))
))

profiles_clean <- validate_profiles_columns(profiles_clean)

profiles_files <- export_profiles_parallel(
  profiles_clean,
  out_dir = final_dir
)

qc_log("PROFILES series files written", profiles_files)

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

qc_log("Contents of each export", final_contents_tab, max_rows = 50)

# QC CHECK: the outcome split and the geography split did what they should.
death_files_with_morbidity <- final_contents_tab %>%
  filter(grepl("_death$", export), grepl("Discharge|Hospitalization|In-Patient", outcomes))
morbidity_files_with_death <- final_contents_tab %>%
  filter(grepl("_morbidity$", export), grepl("Death", outcomes))
qc_check("Death exports hold only Death rows, morbidity exports hold no Death rows",
         nrow(death_files_with_morbidity) + nrow(morbidity_files_with_death) == 0,
         details = bind_rows(death_files_with_morbidity, morbidity_files_with_death))

municipal_wrong_geotype <- final_contents_tab %>%
  filter(grepl("^profiles_municipal", export), grepl("Region|SRA", geotypes))
regional_wrong_geotype <- final_contents_tab %>%
  filter(grepl("^profiles_regional", export), grepl("Municipal", geotypes))
qc_check("Municipal and regional exports hold only their own GeoTypes",
         nrow(municipal_wrong_geotype) + nrow(regional_wrong_geotype) == 0,
         details = bind_rows(municipal_wrong_geotype, regional_wrong_geotype))

qc_check("Every export has rows", all(final_contents_tab$rows > 0), on_fail = "warn",
         details = final_contents_tab %>% filter(rows == 0))

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

# The condition-and-outcome coverage matrix goes to the workbook (tab
# "2 Conditions by export"), not the QC text file.

# QC CHECK: a condition published in the profiles must reach the profiles files.
profiles_gaps <- conditions_tab %>%
  filter(use_group == "profiles",
         !in_profiles_all_geos_all_outcomes)
qc_check("Every profiles condition and outcome reached the profiles exports",
         nrow(profiles_gaps) == 0, on_fail = "warn",
         details = profiles_gaps %>%
           select(Condition, Outcome, use_group, outcome_group))

complete_gaps <- conditions_tab %>% filter(!in_complete)
qc_check("Every condition and outcome reached the COMPLETE export",
         nrow(complete_gaps) == 0, on_fail = "warn",
         details = complete_gaps %>% select(Condition, Outcome, use_group, outcome_group))

# --- 3. Geographies and GeoTypes in the published profiles ---------------------
geographies_tab <- complete_clean %>%
  distinct(Geography, GeoType, GeoName, GeoID, Region) %>%
  mutate(in_profiles = Geography %in% profiles_clean$Geography) %>%
  arrange(GeoType, Geography)

geography_counts <- geographies_tab %>% count(GeoType, in_profiles, name = "n_geographies")

qc_log("Geographies by GeoType (published vs internal only)", geography_counts)

# --- 4. Demographic (index) groups --------------------------------------------
index_groups_tab <- profiles_clean %>%
  count(Demographic_Group, Demographic, name = "rows") %>%
  arrange(Demographic_Group, Demographic)

qc_log("Demographics published in the profiles", index_groups_tab, max_rows = 100)

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

# QC CHECK (summary only in the text file; the detail is in the workbook tabs).
qc_check("Every condition/outcome/demographic has a row for each required geography in every profiles export",
         nrow(geo_gaps) == 0, on_fail = "warn",
         details = c("Gaps found. See workbook tabs '5 Geo completeness' and '5b Missing geo rows':",
                     .qc_capture(geo_completeness_summary %>% filter(missing_rows > 0))))

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
  "1 Export contents: each export should list the right outcomes, condition groups,",
  "      demographic groups, number of conditions, geographies, and GeoTypes.",
  "      Confirm the following: ",
  "      Death exports hold only Death.",
  "      Municipal exports hold County and Municipal.",
  "      Regional exports hold County, Region, and SRA.",
  "2 Conditions by export: each condition and outcome should show TRUE in the exports it",
  "      belongs to and FALSE in the ones it does not (a death-only condition is FALSE in",
  "      the morbidity files, and so on). Use the columns 'use_group' and 'outcome_group' to confirm",
  "3 Geographies: the published list is right, and only the intended geographies",
  "      are marked in_profiles = FALSE.",
  "      3b. Confirm n_geographies for county =1, HCEC=1, Municipal=19, North County=1, Region = 6",
  "         SRA =41 and SupervisorDistrict=5",
  "[ ] 4 Demographics: the published demographic groups and labels are as expected.",
  "5 Geo completeness: for each profiles export, every condition/outcome/demographic",
  "      combination should have a row for each required geography. missing_rows = 0 for every export.",
  "5b Missing geo rows: should say 'None'. Any rows are (combination x geography) cells",
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


# ===========================================================================*
# QC CHECK — End-of-script checks and QC report ----
# ===========================================================================*
# Writes rates/qc/qc_6_final_files_prodn.txt: a cover page (what to confirm),
# any FAIL/WARN entries, a check summary, and every printout from this script.

# QC CHECK: all nine exports were written, each as .csv and .rds.
expected_exports <- c("complete", "complete_death", "complete_morbidity",
                      "profiles_all_geos_death", "profiles_all_geos_morbidity",
                      "profiles_all_geos_all_outcomes",
                      "profiles_municipal_death", "profiles_municipal_morbidity",
                      "profiles_regional_death", "profiles_regional_morbidity")

files_written <- rbind(complete_files, profiles_files)

missing_exports <- setdiff(expected_exports, unique(files_written$export))
qc_check("All ten exports were produced", length(missing_exports) == 0,
         on_fail = "warn",
         details = c("These exports wrote no file, which means their filter matched no rows:",
                     paste(missing_exports, collapse = ", ")))
qc_files_written(c(files_written$file, export_check_file),
                 title = "Final files and export check workbook written this run")

# PRINTOUT: older files in the final folder. File names carry a production date,
# so earlier releases stay until they are cleared out on purpose.
all_final <- list.files(final_dir, full.names = TRUE)
older     <- setdiff(all_final, files_written$file)
qc_log("Older files in data/output/final (not written by this run)",
       if (length(older)) basename(older) else "None.")

# PRINTOUT: what is actually published. Blank cells are suppressed.
qc_log("PROFILES series: rows and published values by Outcome",
       profiles_clean %>%
         group_by(Outcome) %>%
         summarise(rows = n(),
                   conditions = n_distinct(Condition),
                   geographies = n_distinct(Geography),
                   cases_published = sum(!is.na(Cases)),
                   crude_rates_published = sum(!is.na(`Crude Rate`)),
                   aa_rates_published = sum(!is.na(`Age-Adjusted (AA) Rate`)),
                   .groups = "drop"))

qc_log("COMPLETE series: rows by OUTCOME",
       complete_clean %>%
         group_by(OUTCOME) %>%
         summarise(rows = n(), conditions = n_distinct(CONDITION),
                   geographies = n_distinct(Geography),
                   cases_published = sum(!is.na(final_cases)), .groups = "drop"))

qc_finish(
  confirm = c(
    "RUN STATUS is PASSED or PASSED WITH WARNINGS.",
    "Data year in the header is the year you intended to publish.",
    "Missing Year check: 0 rows dropped, or the dropped rows are understood.",
    "Every CONDITION has a classification, or classify_conditions.csv has been updated.",
    "Conditions by use_group and outcome_group: the profiles condition list matches what was requested this year.",
    "Index_Group printout: only the published demographic keys carry a group; the NA rows are the ones you do not publish.",
    "Internal geographies removed: the rows removed count is above 0 and matches the geographies listed.",
    "All ten exports were produced (3 complete, 7 profiles), each as .csv and .rds.",
    "PROFILES rows by Outcome: condition and geography counts look right, and the number of published cases and rates is in line with last year.",
    "Export contents: each export holds the right outcomes, condition groups, demographic groups, geographies, and GeoTypes.",
    "Geographies by GeoType: the published geography list is right, and only the intended geographies are internal-only.",
    "Geography completeness: missing_rows is 0 for every profiles export (see the export check workbook, tab 5)."
  ),
  review_files = c(
    "project_root/rates/data/output/final/profiles_all_geos_death_<year>_<date>.csv: confirm the column names are the published ones and that suppressed cells are blank, not zero.",
    "project_root/rates/data/output/final/profiles_municipal_* and profiles_regional_*: confirm each holds only its geography types (municipal: County and Municipal; regional: County, Region, and SRA).",
    "project_root/rates/data/output/final/complete_death_* and complete_morbidity_*: confirm death-only conditions appear only in the death file and morbidity-only conditions only in the morbidity file.",
    "project_root/rates/data check/export_check_final_files_<year>.xlsx: read every tab; it confirms the exports did not pick up rows they should not have.",
    "Run 7_core_rates_review.R next to compare this year's published rates with last year's."
  )
)
