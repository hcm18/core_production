# ===========================================================================*
# 1_Death_Ori_to_Core.R: Script Description ---------------------------------
# ===========================================================================*
# PURPOSE
#   Take the raw VRBIS death file (exported from SPSS as a .sav) and build the
#   standardized "core" analytic file used by the rest of the pipeline.
#
#   This script:
#     1. Reads the raw SPSS death file and saves a raw checkpoint copy.
#     2. Adds the reporting Year (read from a shared CSV, not typed by hand).
#     3. Creates standard analytic variables:
#          - OUTCOME       (always "Death" in this script)
#          - Sex           (renamed from raw 'sex')
#          - RACE_ETH      (recoded + labeled race/ethnicity)
#          - Zipcode       (cleaned residence ZIP)
#          - ENCLOSINGZIP  (from the enclosing-ZIP crosswalk)
#          - CountyCases   (1 = a final county resident death)
#     4. Sources the demographic sub-scripts (add the *_case counter columns).
#     5. Sources the condition sub-scripts (add one column per condition).
#     6. Exports the finished core file (.rds / .csv / .sav) with dynamic names.
#     7. Runs the aggregation script (2a) to build condition-level output.
#
# INPUTS (read from disk)
#   - project_root/3_prep_cases/data/input/core_ready_death.sav      raw VRBIS death extract
#   - project_root/3_prep_cases/data/input/ezc.csv                   enclosing-ZIP crosswalk
#   - project_root/update_lists/datayear.csv reporting year (single source of truth)
#   - project_root/3_prep_cases/scripts/vrbis_demographics/*.R             scripts that add *_case columns
#   - project_root/3_prep_cases/scripts/code_death/*.R                     scripts that add condition columns
#
# OUTPUTS (written to disk)  -- see "SAVES A FILE" markers below
#   - project_root/3_prep_cases/data/output/ori_death.rds                    raw checkpoint
#   - project_root/3_prep_cases/data/output/{outcome}_core_{year}.rds/.csv/.sav   finished core file
#   - project_root/3_prep_cases/qc/qc_1_Death_Ori_to_Core.txt               QC report (see QC CHECK section)
#   - (aggregation script writes its own files - see below)
#         STEP 12: 2a_aggregate_conditions_death.R
#         -Aggregates DEATH conditions by enclosing ZIP and writes condition/QC files.
#           -Reads : project_root/3_prep_cases/data/output/death_core_<year>.rds
#           -Writes: project_root/3_prep_cases/data/output/{aggfiles,condfiles}/death/... and ...output/death/qc/...
#
# PATHS: Defined in the CONFIGURATION step.
# --------------------------------------------------------------------------*

# ---- Step 0: Configuration: EDIT HERE BEFORE RUNNING SCRIPT --------------------------
# Note: Before running this script, you must update the csv files in the update_lists 
# folder. This includes the following: 
# - condition_vars_death (defines conditions for death)
# - datayear (defines data year)

# Main project root: UPDATE PROJECT ROOT FOLDER
project_root <- "D:/core_production"

# Input data directory
# NOTE: This data should already be loaded into the folder below, including the 
# core_ready_death SPSS file and the ezc.csv (enclosing zip crosswalk).
input_dir <- file.path(project_root, "3_prep_cases", "data", "input")

# Data output directory
output_dir <- file.path(project_root, "3_prep_cases", "data", "output")

# Update lists folder
update_lists_dir <- file.path(project_root, "update_lists")

# Folder containing demographic scripts
demo_script <- file.path(project_root, "3_prep_cases", "scripts", "vrbis_demographics.R")

# Folder containing condition files
cond_scripts_dir <- file.path(project_root, "3_prep_cases", "scripts", "code_death")

#Step 12 aggregate script source
aggregate_script <- file.path(project_root, "3_prep_cases", "scripts", "aggregate_conditions_fn.R")

# Shared QC helper functions (one copy used by 3_prep_cases and rates)
qc_functions_script <- file.path(project_root, "shared_scripts", "qc_functions.R")

# QC report folder (one qc_<script>.txt report per numbered script)
qc_dir <- file.path(project_root, "3_prep_cases", "qc")

#Create directories if they don't exist
dirs_to_create <- c(
  input_dir,
  output_dir,
  update_lists_dir,
  cond_scripts_dir,
  qc_dir
)
for (d in dirs_to_create) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}


# The reporting outcome for this script. Used to name output files.
outcome_label <- "Death"

# ---- Load Packages ---------------------------------------------------------------
# Loaded once here. Several of these are needed by the sourced sub-scripts
# (demographics / conditions), not by this script directly, so trim with care.
pacman::p_load(
  rio,        # import/export in many formats
  here,       # project-relative file paths
  haven,      # read/write SPSS .sav
  labelled,   # attach value labels to variables
  glue,       # build dynamic file names
  janitor,    # tidy/clean helpers
  skimr,      # quick data review (interactive)
  epikit,     # age-group categories (used by demographic sub-scripts)
  expss,      # SPSS-style value labels (used by sub-scripts)
  tidyverse   # dplyr, stringr, readr, purrr, etc.
)

# ---- Start QC log ------------------------------------------------------------
# Checks and printouts below are collected and written to
# qc/qc_1_Death_Ori_to_Core.txt by the QC CHECK section at the bottom.
# Any FAIL (or any R error) stops the script AND writes the report.
if (!file.exists(qc_functions_script))
  stop("QC helper script not found: ", qc_functions_script)
source(qc_functions_script)
qc_start("1_Death_Ori_to_Core.R", qc_dir, reset = TRUE)

# ===========================================================================*
# # Step 1: Read the raw SPSS file and save a checkpoint copy ---------------
# ===========================================================================*
# Read the SPSS export. read_sav() returns a tibble that keeps SPSS metadata
# (value labels, etc.).
qc_input_exists(file.path(input_dir, "core_ready_death.sav"))
ori <- read_sav(file.path(input_dir, "core_ready_death.sav"))

head(ori)  # QC CHECK: eyeball the first rows

# QC CHECK: raw file has rows and every column this script and the sourced
# demographics script rely on.
qc_check("Raw death file has rows", nrow(ori) > 0, details = "core_ready_death.sav has 0 rows.")
qc_log("Raw death file dimensions", sprintf("%s rows x %s columns", nrow(ori), ncol(ori)))
qc_required_columns(
  ori,
  c("sex", "CORErace5", "RESZIP", "SDresident_NCHS_FINAL",
    "Age", "racecat8", "RCODE1", "RCODE2", "RCODE3"),
  "Raw death file"
)

# SAVES A FILE -> project_root/3_prep_cases/data/output/ori_death.rds
# A raw, unmodified checkpoint so you can always return to the starting point.
rio::export(ori, file.path(output_dir, "ori_death.rds"))

# Work on a copy called ori_core from here on (leaves 'ori' untouched).
ori_core <- ori


# ============================================================================*
# Step 2 - Add the reporting Year (from the shared CSV, not typed  --------
# ============================================================================*
# Reading Year from one CSV means every script uses the same value and there is
# no risk of someone hard-coding the wrong year.
year_value <- read_csv(file.path(update_lists_dir, "datayear.csv"),
                       show_col_types = FALSE) |>
  pull(Year) |>
  as.character()
qc_data_year(year_value)   # QC CHECK: exactly one Year in datayear.csv

ori_core <- ori_core %>% 
  mutate(Year = year_value)

# QC CHECK: one row here, showing the Year and the total record count.
qc_log("Year and total record count", ori_core %>% count(Year, name = "n_rows"))


# ============================================================================*
# # Step 3 - OUTCOME (all rows are deaths in this script) -------------------
# ============================================================================*
ori_core <- ori_core %>% 
  mutate(OUTCOME = outcome_label)

# QC CHECK: should show every row under a single OUTCOME = "Death".
qc_log("OUTCOME", ori_core %>% count(OUTCOME, name = "n_rows"))


# ============================================================================*
# STEP 4 — Sex (standardize the variable name)----
# ============================================================================*
ori_core <- ori_core %>% 
  rename(Sex = sex)

# QC CHECK: raw sex vs the renamed Sex, and the values that are expected.
# 1 = male, 2 = female, 4 = unknown (kept as-is, not recoded). The demographic
# counters count only 1 and 2, so 4 and missing fall out of the sex breakdowns.
qc_log("Sex", ori_core %>% count(Sex = as.numeric(Sex), name = "n_rows"),
       note = "1 = male; 2 = female; 4 = unknown (not recoded). Only 1 and 2 are counted in male_*/female_* counters.")
bad_sex <- ori_core %>% filter(!is.na(Sex), !as.numeric(Sex) %in% c(1, 2, 4)) %>%
  count(Sex = as.numeric(Sex), name = "n_rows")
qc_check("Sex values are only 1, 2, 4 (unknown), or missing", nrow(bad_sex) == 0,
         details = bad_sex, on_fail = "warn")


# ============================================================================*
# STEP 5 — Race/Ethnicity (recode raw codes to standard RACE_ETH + labels) ----
# ============================================================================*
# Maps the raw CORErace5 codes to the project's standard RACE_ETH codes, then
# attaches human-readable labels. Codes 0 and 9 (and anything blank) -> 99 = Unknown.
ori_core <- ori_core %>%
  mutate(
    RACE_ETH = case_when(
      CORErace5 == 2         ~ 1L,   # White
      CORErace5 == 3         ~ 2L,   # Black
      CORErace5 == 1         ~ 3L,   # Hispanic
      CORErace5 == 4         ~ 4L,   # Asian/PI
      CORErace5 == 5         ~ 5L,   # Other
      CORErace5 == 6         ~ 6L,   # (kept as-is)
      CORErace5 %in% c(0, 9) ~ 99L,  # Unknown
      TRUE                   ~ as.integer(CORErace5)  # keep any other codes
    )
  )

ori_core$RACE_ETH <- labelled(
  ori_core$RACE_ETH,
  labels = c("White" = 1L, 
             "Black" = 2L, 
             "Hispanic" = 3L,
             "Asian/PI" = 4L, 
             "Other" = 5L, 
             "Unknown" = 99L)
)

# QC CHECK: confirm the recode itself. One row per raw code x recoded code with
# both sets of labels, so each mapping and its record count can be verified.
qc_recode_crosstab(
  ori_core, from = "CORErace5", to = "RACE_ETH",
  title = "Recode check: CORErace5 (raw) -> RACE_ETH (recoded)",
  note = paste("Intended mapping: 2 -> 1 White; 3 -> 2 Black; 1 -> 3 Hispanic;",
               "4 -> 4 Asian/PI; 5 -> 5 Other; 6 -> 6 (kept as-is); 0 and 9 -> 99 Unknown.",
               "Every row of this table should match one of those pairs, and the",
               "n_rows column should match the raw CORErace5 counts.")
)

# racecat8 is what the demographics script uses for the race/ethnicity counters.
# Printout only (racecat8 is built upstream, not recoded here).
qc_recode_crosstab(
  ori_core, from = "racecat8", to = "RACE_ETH",
  title = "Consistency check: racecat8 (drives the race/ethnicity counters) -> RACE_ETH",
  check_unmapped = FALSE,
  note = paste("racecat8 arrives already built in the raw file. Confirms the two",
               "race/ethnicity variables agree: Hispanic (1) should sit in RACE_ETH 3,",
               "NH White (2) in 1, NH Black (3) in 2, NH Asian (5) and NH PI (6) in 4,",
               "and Unknown (9) in 99.")
)

# ============================================================================*
# STEP 6 — Residence ZIP (clean, and fill missing county-resident ZIPs)----
# ============================================================================*
# All records here are considered county residents. Some have missing or
# out-of-county ZIPs. Below, we (1) clean RESZIP to an integer ZIP, then 
# (2) for FINAL county residents with a missing ZIP, we set 99999 ("unknown ZIP").

# (1) Helper: standardize a ZIP code 
# Takes the first 1-5 digits of any value and returns an integer ZIP (NA if
# none found). Defined once and reused for residence ZIP, the crosswalk, and
# ENCLOSINGZIP so the cleaning rule is identical everywhere.

standardize_zip <- function(x) {
  suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d{1,5}")))
}

# (2) set unknown zip to 99999
ori_core <- ori_core %>%
  mutate(
    Zipcode = standardize_zip(RESZIP),
    Zipcode = if_else(
      SDresident_NCHS_FINAL == "Yes a FINAL SD Resident NCHS" & is.na(Zipcode),
      99999L,
      Zipcode
    )
  )

# QC CHECK: distribution of cleaned residence ZIPs (largest first).
qc_log("Cleaned residence ZIP (Zipcode), largest first",
       ori_core %>% count(Zipcode, name = "n_rows", sort = TRUE))


# ============================================================================*
# STEP 7 — Join the enclosing-ZIP crosswalk -> ENCLOSINGZIP----
# ============================================================================*
# The crosswalk maps each residence ZIP to its "enclosing" ZIP used for
# geographic roll-ups. Both sides are cleaned with the same standardize_zip()
# rule so the join keys always match.

# Read crosswalk.
xwalk <- read_csv(file.path(input_dir, "ezc.csv"), show_col_types = FALSE)

# The crosswalk must have a column named exactly "Zipcode". Accept common
# alternate names (ZIP / ZIPCODE); otherwise stop with a clear message.
if (!"Zipcode" %in% names(xwalk)) {
  if ("ZIP" %in% names(xwalk)) {
    xwalk <- rename(xwalk, Zipcode = ZIP)
  } else if ("ZIPCODE" %in% names(xwalk)) {
    xwalk <- rename(xwalk, Zipcode = ZIPCODE)
  } else {
    stop("Crosswalk (ezc.csv) has no 'Zipcode' column. Rename its ZIP column to 'Zipcode'.")
  }
}

# QC CHECK: a ZIP mapped to two different ENCLOSINGZIPs would be resolved
# silently by distinct() below, so stop here instead.
qc_zip_crosswalk_conflicts(xwalk, standardize_zip)

# Clean the crosswalk key and keep one row per ZIP.
xwalk <- xwalk %>%
  mutate(Zipcode = standardize_zip(Zipcode)) %>%
  distinct(Zipcode, .keep_all = TRUE)

# Join, then normalize ENCLOSINGZIP and set any missing to 99999.
n_rows_before_xwalk <- nrow(ori_core)
ori_core <- ori_core %>%
  left_join(xwalk, by = "Zipcode") %>%
  mutate(ENCLOSINGZIP = coalesce(standardize_zip(ENCLOSINGZIP), 99999L))

# NOTE: the ZIP match QC runs in Step 8, once CountyCases exists, so the summary
# can be reported for county cases.


# ============================================================================*
# STEP 8 — CountyCases (flag every final county-resident death as 1)----
# ============================================================================*
ori_core <- ori_core %>%
  mutate(
    SDresident_NCHS_FINAL = as.character(SDresident_NCHS_FINAL),
    CountyCases = if_else(
      SDresident_NCHS_FINAL == "Yes a FINAL SD Resident NCHS", 1L, NA_integer_
    )
  )

# QC CHECK: resident flag vs. the CountyCases counter.
qc_log("SDresident_NCHS_FINAL (raw) vs CountyCases (derived)",
       ori_core %>%
         count(SDresident_NCHS_FINAL, CountyCases, name = "n_rows") %>%
         mutate(counted_in_this_pipeline = if_else(!is.na(CountyCases) & CountyCases == 1,
                                                   "yes", "no")),
       note = paste("CountyCases is created in this step. It is 1 when",
                    "SDresident_NCHS_FINAL is 'Yes a FINAL SD Resident NCHS' (a final San Diego",
                    "resident death) and NA for every other value. It is not a count of",
                    "counties: it is the include/exclude flag the demographic counters use.",
                    "Rows with NA stay in the core file but are never counted, so total_case",
                    "and every *_case counter equal the number of CountyCases == 1 rows.",
                    "The n_rows column is the number of death records in each combination."))
qc_check("At least one county case (CountyCases == 1)",
         sum(ori_core$CountyCases == 1, na.rm = TRUE) > 0,
         details = "No rows matched 'Yes a FINAL SD Resident NCHS'. Check the raw value spelling.")

# QC CHECK (from Step 7): row count unchanged by the crosswalk join, and the
# county-case ZIPs that did NOT match the crosswalk (they roll up to 99999).
qc_zip_join_results(ori_core, xwalk, n_rows_before_xwalk)


# ============================================================================*
# STEP 9 — Add demographic case counters (source vrbis_demographics/*.R)----
# ============================================================================*
# This script ADDS "*_case" counter columns
# (e.g. by sex, race/ethnicity, age group). 

source(demo_script)

# QC CHECK: counters exist and add up (total = county cases, sex, age, race).
qc_demographic_counters(ori_core, race_counters_gated = FALSE)

# ============================================================================*
# STEP 10 — Code conditions (source code_death/*.R)----
# ============================================================================*
# Each .R file in this folder ADDS one condition column (e.g. asthma_ucod,
# asthma_mcod). The column name must match an entry in condition_vars_death.csv,
# which the aggregation script (Step 7 / 2a) reads.
# ignore.case = TRUE: a script saved as ".r" must not be skipped silently.
cond_scripts <- list.files(cond_scripts_dir, pattern = "\\.R$",
                           ignore.case = TRUE, full.names = TRUE)
invisible(lapply(cond_scripts, source))

# QC CHECK: every condition listed in the CSV now exists as a column.
qc_input_exists(file.path(update_lists_dir, "condition_vars_death.csv"))
cond_vars <- read_csv(file.path(update_lists_dir, "condition_vars_death.csv"),
                      show_col_types = FALSE) |>
  pull(var) |>
  unique()
qc_condition_columns(ori_core, cond_vars, "condition_vars_death.csv", cond_scripts)


# ============================================================================*
# STEP 11 — Export the finished core file (dynamic file names)----
# ============================================================================*
# File names are built from the data itself (outcome + year) so they are always
# correct and never hard-coded, e.g. "death_core_2024.rds".

# ---- Helper: turn a label into a safe file-name slug ------------------------*
# e.g. "Death" -> "death". Keeps output file names predictable. (Lightweight,
# no network/package install needed.)
make_slug <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)   # non-alphanumerics -> underscore
  gsub("^_+|_+$", "", x)            # trim leading/trailing underscores
}

# Guard: these two columns must exist and hold a single value each.
stopifnot(all(c("Year", "OUTCOME") %in% names(ori_core)))
yr  <- unique(ori_core$Year);    stopifnot(length(yr)  == 1)
out <- unique(ori_core$OUTCOME); stopifnot(length(out) == 1)
out <- make_slug(out)  # "Death" -> "death"

# SAVES FILES -> project_root/3_prep_cases/data/output/{out}_core_{yr}.(rds|csv)
export(ori_core, file.path(output_dir, glue("{out}_core_{yr}.rds")))
export(ori_core, file.path(output_dir, glue("{out}_core_{yr}.csv")))

# QC CHECK: core files exist and were written by this run.
qc_files_written(file.path(output_dir, c(glue("{out}_core_{yr}.rds"), glue("{out}_core_{yr}.csv"))),
                 title = "Core files written this run")


# ============================================================================*
# STEP 12 — Run the aggregation script (builds condition-level output)----
# ============================================================================*
# (originally 2a_aggregate_conditions_death.R)
# Aggregates DEATH conditions by enclosing ZIP and writes condition/QC files.
# All logic lives in aggregate_conditions_fn.R; this section calls it.
# 
# Reads : project_root/3_prep_cases/data/output/death_core_<year>.rds
# Writes: project_root/3_prep_cases/data/output/{aggfiles,condfiles}/death/... and ...output/death/qc/...

source(aggregate_script)

# The merged condition table is kept (agg_death) for the QC CHECK section.
agg_death <- aggregate_conditions(
  outcome_label = "Death",                    # OUTCOME value to keep
  outcome_tag   = "death",                     # folder/filename tag
  input_stem    = "death",                      # reads death_core_<year>.rds
  cond_csv      = "condition_vars_death.csv"     # death condition list
)


# ============================================================================*
# QC CHECK — End-of-script checks and QC report ----
# ============================================================================*
# Writes qc/qc_1_Death_Ori_to_Core.txt: a cover page (what to confirm), any
# FAIL/WARN entries, a check summary, and every printout from this script.

death_cond_dir <- file.path(output_dir, "condfiles", "death")
death_qc_dir   <- file.path(death_cond_dir, "qc")

# QC CHECK: aggregated condition counts reconcile to the core file.
qc_aggregation_reconcile(ori_core, agg_death, outcome_label = "Death",
                         outcome_tag = "death", cond_vars = cond_vars, year = yr)

# QC CHECK: aggregation outputs exist and were written by this run.
qc_files_written(c(
  file.path(death_cond_dir, glue("merged_condfiles_death_{yr}.rds")),
  file.path(death_cond_dir, glue("merged_condfiles_death_{yr}.csv")),
  file.path(death_qc_dir, "condition_variable_check.csv"),
  file.path(death_qc_dir, glue("merged_quality_by_condition_death_{yr}.csv"))
), title = "Aggregation files written this run")

# PRINTOUT: rows and ZIPs per condition in the merged file.
qc_log("Merged condition file: rows, ENCLOSINGZIPs, and total_case_sum by CONDITION",
       agg_death %>%
         group_by(CONDITION) %>%
         summarise(n_rows = n(), n_zip = n_distinct(ENCLOSINGZIP),
                   total_case_sum = sum(total_case_sum, na.rm = TRUE), .groups = "drop") %>%
         arrange(desc(total_case_sum)),
       max_rows = 500)

qc_finish(
  confirm = c(
    "RUN STATUS is PASSED or PASSED WITH WARNINGS.",
    "Data year in the header is the year you intended to process.",
    "Raw death file row count is in line with last year's report.",
    "Sex printout: only 1, 2, U, or a small number of missing values.",
    "Recode check CORErace5 -> RACE_ETH: every row of the table matches the intended mapping listed in its note, and no raw code was left unrecoded.",
    "Consistency check racecat8 -> RACE_ETH: the two race/ethnicity variables line up as described in the note, and racecat8 unknown (9) is a small share.",
    "SDresident_NCHS_FINAL vs CountyCases: only 'Yes a FINAL SD Resident NCHS' rows get CountyCases = 1, and that count is in line with last year.",
    "ZIP match summary: percent ENCLOSINGZIP unknown is small and similar to last year; unmatched ZIPs are out-of-county or invalid, not San Diego ZIPs missing from ezc.csv.",
    "Demographic counter totals: total_case equals county cases; unknown sex is small; race sum vs total_case note reviewed (known VRBIS gating item).",
    "Condition scripts sourced: every expected condition script is listed.",
    "Reconciliation table: difference is 0 for every condition; top conditions by count look plausible vs last year."
  ),
  review_files = c(
    glue("project_root/3_prep_cases/data/output/death_core_{yr}.csv: spot-check a few records for Year, OUTCOME, Sex, Zipcode, ENCLOSINGZIP, CountyCases, and condition columns."),
    "project_root/3_prep_cases/data/output/condfiles/death/qc/condition_variable_check.csv: every condition shows status 'found'.",
    glue("project_root/3_prep_cases/data/output/condfiles/death/qc/merged_quality_by_condition_death_{yr}.csv: n_zip and *_case_sum totals are plausible; no unexpected conditions."),
    glue("project_root/3_prep_cases/data/output/condfiles/death/merged_condfiles_death_{yr}.csv: no blank CONDITION values; ENCLOSINGZIP values are 5-digit ZIPs (99999 = unknown).")
  )
)