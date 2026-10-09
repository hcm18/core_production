# ===========================================================================*
# 1_prep_cases_add_pops.R ------------------------------
# ===========================================================================*
# PURPOSE
#   Entry point for the RATES pipeline. Loads the wide all-outcomes case file
#   produced by 3_prep_cases, then runs steps 2-4 in order:
#     2  pivot cases long, allocate to final geographies, aggregate
#     3  match adjusted cases to populations
#     4  sex-specific denominator processing
#   (Step 5 (crude + AA rates + suppression) is run separately.)
#
# INPUTS (read from disk)
#   - project_root/3_prep_cases/data/output/condfiles/all_outcomes_merged.rds
#     (the combined case file produced by 3_prep_cases step 3)
#
# OUTPUTS (written to disk)
#   - project_root/rates/qc/qc_1_prep_cases_add_pops.txt   QC report for this script, including the
#                                       status of the step 2-4 QC reports
#   - the sourced steps 2-4 write their own files and their own QC reports.
#
# PATHS: Defined in the CONFIGURATION step. This script runs steps 2-4 by
#   sourcing them, so they inherit the Step 0 configuration below.
# --------------------------------------------------------------------------*

# ---- Step 0: Configuration: EDIT HERE BEFORE RUNNING SCRIPT -----------------
# Before running, confirm the sibling projects (3_prep_cases, 2_prep_populations,
# 1_1_prep_allocation_xwalk) have produced their outputs and that update_lists is
# current.

# Main project root: UPDATE PROJECT ROOT FOLDER
project_root <- "D:/core_production"

# --- Rates project folders (this project) ---
rates_dir   <- file.path(project_root, "rates")
scripts_dir <- file.path(rates_dir, "scripts")
output_dir  <- file.path(rates_dir, "data", "output")
clean_dir   <- file.path(output_dir, "cleaned cases")

# Output subfolders built from clean_dir.
final_adjusted_dir <- file.path(clean_dir, "final_adjusted")
qc_dir             <- file.path(project_root, "rates", "qc")

#Create directories if they don't exist
dirs_to_create <- c(
  clean_dir,
  output_dir,
  final_adjusted_dir,
  qc_dir 
)
for (d in dirs_to_create) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

# --- Shared / sibling-project inputs ---
#update lists directory 
update_lists_dir <- file.path(project_root, "update_lists")
# populations directory
pop_dir   <- file.path(project_root, "2_prep_populations", "data", "output")

#All outcomes merged file location
cases_input <- file.path(project_root, "3_prep_cases", "data", "output",
                         "condfiles", "all_outcomes_merged.rds")
# Allocation crosswalk location
alloc_xwalk_file <- file.path(project_root, "1_prep_allocation_xwalk",
                              "data", "clean", "allocxwalk.rds")


# --- Pipeline step scripts (sourced below) ---
step2_script <- file.path(scripts_dir, "1a_pivot_cases_long_allocate_aggregate_final_geog.R")
step3_script <- file.path(scripts_dir, "1b_match_up_adjusted_cases_pops.R")
step4_script <- file.path(scripts_dir, "1c_sex_specific_processing.R")

# --- Shared QC helper functions ---
# Steps 2-4 inherit this path and write their own reports to qc_dir.
qc_functions_script <- file.path(project_root, "shared_scripts", "qc_functions.R")


# ---- Load Packages ---------------------------------------------------------
pacman::p_load(
  rio,        # import/export in many formats
  tidyverse,  # dplyr, tidyr, stringr, readr, purrr
  fs          # filesystem helpers
)

# ---- Start QC log ------------------------------------------------------------
# Checks and printouts below are written to qc/qc_1_prep_cases_add_pops.txt by the
# QC CHECK section at the bottom. Steps 2-4 each write their own report too.
# Any FAIL (or R error) in this script OR in a sourced step stops the run and
# writes every open report.
if (!file.exists(qc_functions_script))
  stop("QC helper script not found: ", qc_functions_script)
source(qc_functions_script)
qc_start("1_prep_cases_add_pops.R", qc_dir, reset = TRUE)
orchestrator_started <- Sys.time()

# QC CHECK: inputs used by steps 2-4 exist before any work starts.
year_value <- read_csv(file.path(update_lists_dir, "datayear.csv"),
                       show_col_types = FALSE) |>
  pull(Year) |>
  as.character()
qc_data_year(year_value)
qc_input_exists(cases_input, "all_outcomes_merged.rds (prep_cases)")
qc_input_exists(alloc_xwalk_file, "allocxwalk.rds (1_prep_allocation_xwalk)")
qc_check("Population folder exists", dir.exists(pop_dir), details = paste("Not found:", pop_dir))
qc_input_exists(file.path(update_lists_dir, "demo_key_female_condition.csv"))
qc_input_exists(file.path(update_lists_dir, "demo_key_male_condition.csv"))
for (sc in c(step2_script, step3_script, step4_script)) qc_input_exists(sc)

# ===========================================================================*
# Step 1: Load the wide-format case dataset --------------------------------
# ===========================================================================*
# Produced by prep_cases (3_merge_all_outcomes). This is the input the whole
# rates pipeline is built from.
cases_wide <- readRDS(cases_input)

# Identify ID columns and the case-count columns
# case_group_cols (everything ending in "_sum") is used by the pivot in the
# next script. id_cols documents the record keys.
id_cols         <- c("ENCLOSINGZIP", "Year", "OUTCOME", "CONDITION", "source_file")
case_group_cols <- names(cases_wide)[grepl("_sum$", names(cases_wide))]

# QC CHECK: the case file has rows, its key columns, counters, and the right Year.
qc_check("Case file has rows", nrow(cases_wide) > 0, details = "all_outcomes_merged.rds has 0 rows.")
qc_required_columns(cases_wide, id_cols, "all_outcomes_merged.rds")
qc_check("Case file has *_sum counter columns", length(case_group_cols) > 0,
         details = "No columns ending in '_sum'.",
         pass_note = paste(length(case_group_cols), "counter columns"))
qc_check("Case file Year matches datayear.csv",
         identical(unique(as.character(cases_wide$Year)), year_value),
         details = paste("Years in case file:", paste(unique(cases_wide$Year), collapse = ", ")))
qc_log("Case file rows by OUTCOME",
       cases_wide %>% count(OUTCOME, name = "n_rows"))

# ===========================================================================*
# STEP 2: Pivot_cases_long_allocate_aggregate_final_geog.R: Script Description ----
# ===========================================================================*
# PURPOSE
#   1. Pivot the wide case file to long (one row per case_group), setting aside
#      the small-population race groups into a separate file.
#   2. Allocate ZIP-level cases to final geographies using an allocation
#      crosswalk (cases * pct), then aggregate to each final geography.
#
# INPUTS (read from disk)
#   - data/clean/cases_long.rds                       (re-read checkpoint, below)
#   - 1_prep_allocation_xwalk/data/clean/allocxwalk.rds allocation crosswalk
#   Also uses cases_wide / case_group_cols from step 1 (this script is sourced).
#
# OUTPUTS (written to disk)  -- see "SAVES A FILE" markers
#   - data/clean/cases_long.rds/.csv                  long case file
#   - data/clean/small_cases_groups.rds/.csv          set-aside small groups
#   - data/clean/final_adjusted/final_adjusted_cases.rds/.csv
#   - data/clean/final_adjusted/by_condition/<cond>/final_adjusted_<cond>.rds/.csv
#   - data/clean/final_adjusted/by_outcome/<outcome>/final_adjusted_<outcome>.rds/.csv
#
# PATHS: Normally sourced by 1_prep_cases_add_pops.R, so it inherits that
#   script's Step 0 configuration. A minimal Step 0 below lets it run on its own.
# --------------------------------------------------------------------------*

source(step2_script)

# ===========================================================================*
# STEP 3: Match_up_adjusted_cases_pops.R: Script Description -----------------
# ===========================================================================*
# PURPOSE
#   Attach population denominators to the geography-allocated case file, matching
#   on Geography + Year + a demographic key derived from the case/pop group names.
#
# INPUTS (read from disk)
#   - data/clean/final_adjusted/final_adjusted_cases.rds        (from step 2)
#   - 2_prep_populations/data/clean/final_populations_xgeography_vert_<YYYY>.rds
#
# OUTPUTS (written to disk)  -- see "SAVES A FILE" markers
#   - data/clean/final_adjusted/final_adjusted_cases_with_pop.rds/.csv
#
# PATHS: Normally sourced by 1_prep_cases_add_pops.R (inherits Step 0 config).
#   A minimal Step 0 below lets it run on its own.
# --------------------------------------------------------------------------*

source(step3_script)

# ===========================================================================*
# STEP 4_sex_specific_processing.R: Script Description -----------------------
# ===========================================================================*
# PURPOSE
#   For sex-specific conditions (e.g. prostate cancer = male only), the correct
#   denominator is the sex-specific population, not the combined one. This step
#   swaps in "donor" case and population values for those conditions so the rate
#   later uses, e.g., the male population for a male-only condition.
#
# INPUTS (read from disk)
#   - data/clean/final_adjusted/final_adjusted_cases_with_pop.rds   (from step 3)
#   - update_lists/demo_key_female_condition.csv                    donor lookups
#   - update_lists/demo_key_male_condition.csv
#
# OUTPUTS (written to disk)  -- see "SAVES A FILE" markers
#   - data/clean/final_adjusted/final_adjusted_ss_4rates.rds/.csv   input for step 5
#   - qc/qc_sex_specific.csv and several qc/*.csv summaries
#   - qc/by_outcome/qc_<outcome>.csv
#
# PATHS: Normally sourced by 1_prep_cases_add_pops.R (inherits Step 0 config).
#   A minimal Step 0 below lets it run on its own.
# --------------------------------------------------------------------------*


source(step4_script)


# ===========================================================================*
# QC CHECK — Step 2-4 report status and QC report ----
# ===========================================================================*
# Writes qc/qc_1_prep_cases_add_pops.txt. If any step had failed, the run would
# already have stopped; this section confirms each step finished during THIS run
# and flags any step that finished with warnings.

step_names <- c(
  "2_pivot_cases_long_allocate_aggregate_final_geog.R",
  "3_match_up_adjusted_cases_pops.R",
  "4_sex_specific_processing.R"
)
step_status <- tibble(
  step     = step_names,
  status   = map_chr(step_names, ~ .qc_env$completed[[.x]]$status %||% "NOT RUN"),
  n_warn   = map_int(step_names, ~ as.integer(.qc_env$completed[[.x]]$n_warn %||% NA_integer_)),
  finished = map_chr(step_names, ~ {
    f <- .qc_env$completed[[.x]]$finished
    if (is.null(f)) NA_character_ else format(f, "%Y-%m-%d %H:%M:%S")
  }),
  this_run = map_lgl(step_names, ~ {
    f <- .qc_env$completed[[.x]]$finished
    !is.null(f) && f >= orchestrator_started
  }),
  report   = map_chr(step_names, ~ .qc_env$completed[[.x]]$report %||% NA_character_)
)
qc_log("Step 2-4 QC report status", step_status)

qc_check("Steps 2, 3, and 4 each finished during this run", all(step_status$this_run),
         details = step_status %>% filter(!this_run))
qc_check("Steps 2, 3, and 4 finished without warnings", all(step_status$status == "PASSED"),
         details = c("Open these reports and review each WARN:",
                     step_status$report[step_status$status != "PASSED"]),
         on_fail = "warn")

qc_finish(
  confirm = c(
    "RUN STATUS is PASSED or PASSED WITH WARNINGS, and every WARN has a reviewer note.",
    "Data year in the header is the year you intended to process.",
    "Case file rows by OUTCOME: all four outcomes present with counts in line with last year.",
    "Step 2-4 QC report status: all three steps finished this run."
  ),
  review_files = c(
    "qc/qc_2_pivot_cases_long_allocate_aggregate_final_geog.txt: work through its cover page.",
    "qc/qc_3_match_up_adjusted_cases_pops.txt: work through its cover page.",
    "qc/qc_4_sex_specific_processing.txt: work through its cover page."
  )
)
