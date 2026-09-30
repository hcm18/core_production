# =============================================================================*
# aggregate_conditions_fn.R----
# -----------------------------------------------------------------------------*
# PURPOSE
#   Defines ONE function, aggregate_conditions(), that turns a finished "core"
#   file into condition-level aggregate tables, merges them, and writes QC
#   reports. 
#
#   Sourcing this file does NOT run anything — it only defines the function.
#   The prep cases scripts source it and then call aggregate_conditions().
#   The aggregate_conditions() call is what runs this script/function.
#
# WHAT THE FUNCTION DOES (for one outcome)
#   1. Read the core file and keep only the target OUTCOME.
#   2. Check which condition columns from the CSV are present; log found/missing.
#   3. Find the demographic "*_case" counter columns to sum.
#   4. For each condition column: sum the counters by ENCLOSINGZIP -> write an
#      "aggfile", then standardize to a CONDITION column, drop non-cases -> write
#      a "condfile".
#   5. Merge all condfiles into one long file.
#   6. Write a QC report with one summary row per condition.
#
# ARGUMENTS
#   outcome_label     OUTCOME value to keep, e.g. "Hospitalization"
#   outcome_tag       short tag used for output folders + filenames, e.g. "hosp"
#   input_stem        stem of the core file to read; reads <input_stem>_core_<year>.rds
#                     (death -> "death", edd -> "edd", BOTH hosp & ipt -> "pdd")
#   cond_csv          condition-list filename in update_lists, e.g.
#                     "condition_vars_death.csv" or "condition_vars_morbidity.csv"
#   year              reporting year; default reads update_lists/datayear.csv
#   output_dir         folder holding the core file + output subfolders
#   update_lists_dir  folder holding datayear.csv + the condition-list CSV
#
# NOTE ON FILE NAMING
#   All output names use the clean lowercase <outcome_tag> (death/edd/hosp/ipt).
#   This standardizes the previously-mixed casing (_Death_/_edd_/_Hosp_/_ipt_)
#   and removes spaces that used to appear in QC filenames. The merge step finds
#   files by the "merged_condfiles*" prefix, so this change is safe for it.
# =============================================================================*

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(tidyr)
})

# ---- Small shared helpers ---------------------------------------------------
# A CONDITION value is "invalid" (not an actual case) if it is NA, blank, or 
# "condition" (as written by the condition-coding scripts).
.is_condition_invalid <- function(x) {
  y <- tolower(trimws(as.character(x)))
  is.na(y) | y == "" | y == "condition"
}
# TRUE when a value is missing/blank (used to drop rows with no ENCLOSINGZIP).
.is_blank_chr <- function(x) {
  y <- trimws(as.character(x))
  is.na(y) | y == ""
}
# Save one table as both .rds (for R) and .csv (human-readable).
.write_outputs <- function(df, rds_path, csv_path) {
  saveRDS(df, rds_path)      # SAVES A FILE (.rds)
  write_csv(df, csv_path)    # SAVES A FILE (.csv)
}
.timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")


# ---- The main function ------------------------------------------------------
aggregate_conditions <- function(outcome_label,
                                 outcome_tag,
                                 input_stem,
                                 cond_csv,
                                 year             = NULL,
                                 output_dir        = file.path(project_root, "3_prep_cases", "data", "output"),
                                 update_lists_dir = file.path(project_root, "update_lists")) {

  # ---- Resolve year + paths -------------------------------------------------
  if (is.null(year)) {
    year <- read_csv(file.path(update_lists_dir, "datayear.csv"),
                     show_col_types = FALSE) |>
      pull(Year) |>
      as.integer()
  }

  input_file <- file.path(output_dir, paste0(input_stem, "_core_", year, ".rds"))
  agg_dir    <- file.path(output_dir, "aggfiles",  outcome_tag)
  cond_dir   <- file.path(output_dir, "condfiles", outcome_tag)
  qc_dir     <- file.path(cond_dir, "qc")
  stem       <- paste0(outcome_tag, "_", year)   # e.g. "hosp_2024"

  dir.create(agg_dir,  showWarnings = FALSE, recursive = TRUE)
  dir.create(cond_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(qc_dir,   showWarnings = FALSE, recursive = TRUE)

  # ---- STEP 1: read the core file, keep only this outcome -------------------
  message("[", outcome_tag, "] Reading core file: ", input_file)
  stopifnot(file.exists(input_file))
  dat <- readRDS(input_file)
  if (!inherits(dat, "data.frame")) stop("Core file must contain a data.frame/tibble.")

  base <- dat %>% filter(as.character(OUTCOME) == outcome_label)
  message("[", outcome_tag, "] Rows with OUTCOME == '", outcome_label, "': ", nrow(base))
  if (nrow(base) == 0) stop("No rows matched OUTCOME == '", outcome_label, "'. Check the label/input file.")

  # ---- STEP 2: which condition columns exist? (write a QC record) -----------
  group_vars <- read_csv(file.path(update_lists_dir, cond_csv),
                         show_col_types = FALSE) |>
    pull(var) |>
    unique()

  existing_vars <- intersect(group_vars, names(base))
  missing_vars  <- setdiff(group_vars, names(base))

  message("[", outcome_tag, "] Condition columns found: ", length(existing_vars),
          " | missing: ", length(missing_vars))

  # SAVES A FILE -> qc/condition_variable_check.csv
  write_csv(
    tibble(varname = c(existing_vars, missing_vars),
           status  = c(rep("found", length(existing_vars)),
                       rep("missing", length(missing_vars)))),
    file.path(qc_dir, "condition_variable_check.csv")
  )
  # SAVES A FILE -> qc/condition_variable_check_log.txt
  log_conn <- file(file.path(qc_dir, "condition_variable_check_log.txt"), open = "wt")
  writeLines(c(paste0("Condition variable QC log (", outcome_label, ", ", .timestamp(), ")"),
               "", "FOUND:", paste("  -", existing_vars),
               "", "MISSING:", paste("  -", missing_vars)), log_conn)
  close(log_conn)

  group_vars <- existing_vars
  if (length(group_vars) == 0) stop("No valid condition variables to process.")

  # ---- STEP 3: find the "*_case" counters and check the break variables -----
  sum_vars <- grep("_case$", names(base), value = TRUE, ignore.case = TRUE)
  if (length(sum_vars) == 0) stop("No columns ending in '_case' were found.")
  message("[", outcome_tag, "] Summing ", length(sum_vars), " '_case' counters.")

  missing_breaks <- setdiff(c("ENCLOSINGZIP", "OUTCOME", "Year"), names(base))
  if (length(missing_breaks) > 0)
    stop("Missing required break variables: ", paste(missing_breaks, collapse = ", "))

  # ---- STEP 4: aggregate each condition; write aggfile + condfile -----------
  for (gvar in group_vars) {
    message("[", outcome_tag, "] Processing condition: ", gvar)

    # (a) Sum every *_case counter by condition value x ENCLOSINGZIP x OUTCOME x Year.
    agg <- base %>%
      group_by(.data[[gvar]], ENCLOSINGZIP, OUTCOME, Year) %>%
      summarise(across(all_of(sum_vars),
                       ~ sum(as.numeric(.x), na.rm = TRUE),
                       .names = "{.col}_sum"),
                .groups = "drop")

    # SAVES FILES -> aggfiles/<tag>/<cond>_<tag>_<year>.(rds|csv)
    .write_outputs(agg,
                   file.path(agg_dir, paste0(gvar, "_", stem, ".rds")),
                   file.path(agg_dir, paste0(gvar, "_", stem, ".csv")))

    # (b) Standardize to a CONDITION column; keep only real cases w/ valid ZIP.
    out <- agg %>%
      mutate(CONDITION = as.character(.data[[gvar]])) %>%
      filter(!.is_condition_invalid(CONDITION), !.is_blank_chr(ENCLOSINGZIP)) %>%
      select(-all_of(gvar))

    sum_cols <- grep("_sum$", names(out), value = TRUE)
    out <- out %>% select(any_of(c("ENCLOSINGZIP", "Year", "OUTCOME", "CONDITION")),
                          any_of(sum_cols))

    # SAVES FILES -> condfiles/<tag>/<cond>_<tag>_<year>.(rds|csv)
    .write_outputs(out,
                   file.path(cond_dir, paste0(gvar, "_", stem, ".rds")),
                   file.path(cond_dir, paste0(gvar, "_", stem, ".csv")))
  }
  message("[", outcome_tag, "] Per-condition exports complete.")

  # ---- STEP 5: merge all condfiles into one long file -----------------------
  rds_files <- list.files(cond_dir, pattern = "\\.rds$", full.names = TRUE)
  # Exclude any previously-merged file so re-runs don't fold the merge into itself.
  rds_files <- rds_files[!grepl("^merged_condfiles", basename(rds_files))]
  if (length(rds_files) == 0) stop("No per-condition RDS files found to merge.")

  read_one <- function(fp) {
    df <- tryCatch(readRDS(fp), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    df %>% mutate(
      source_file  = basename(fp),
      ENCLOSINGZIP = as.character(ENCLOSINGZIP),
      OUTCOME      = as.character(OUTCOME),
      CONDITION    = as.character(CONDITION),
      Year         = suppressWarnings(as.integer(Year))
    )
  }

  dfs <- lapply(rds_files, read_one)
  dfs <- Filter(Negate(is.null), dfs)   # drop any unreadable files (base R; no purrr)
  if (length(dfs) == 0) stop("No readable per-condition files.")
  merged <- bind_rows(dfs)
  if (nrow(merged) == 0) stop("No readable per-condition files.")

  id_cols  <- c("ENCLOSINGZIP", "Year", "OUTCOME", "CONDITION")
  sum_cols <- grep("_sum$", names(merged), value = TRUE)
  merged <- merged %>%
    filter(!.is_condition_invalid(CONDITION), !.is_blank_chr(ENCLOSINGZIP)) %>%
    distinct(across(all_of(c(id_cols, sum_cols))), .keep_all = TRUE) %>%
    select(any_of(c(id_cols, "source_file")), everything())

  # SAVES FILES -> condfiles/<tag>/merged_condfiles_<tag>_<year>.(rds|csv)
  saveRDS(merged, file.path(cond_dir, paste0("merged_condfiles_", stem, ".rds")))
  write_csv(merged, file.path(cond_dir, paste0("merged_condfiles_", stem, ".csv")))

  # ---- STEP 6: QC report — one summary row per condition --------------------
  if (length(sum_cols) > 0)
    merged <- merged %>% mutate(across(all_of(sum_cols), ~ suppressWarnings(as.numeric(.x))))

  quality_by_condition <- merged %>%
    group_by(CONDITION) %>%
    summarise(
      n_rows    = n(),
      n_zip     = n_distinct(ENCLOSINGZIP),
      years_min = min(Year, na.rm = TRUE),
      years_max = max(Year, na.rm = TRUE),
      across(all_of(sum_cols), ~ sum(replace_na(.x, 0), na.rm = TRUE)),
      .groups = "drop"
    )

  # Sort by the grand-total counter if present (it is `total_case` -> `total_case_sum`),
  # otherwise by row count.
  quality_by_condition <- if ("total_case_sum" %in% names(quality_by_condition)) {
    quality_by_condition %>% arrange(desc(total_case_sum), desc(n_rows))
  } else {
    quality_by_condition %>% arrange(desc(n_rows))
  }

  # SAVES FILES -> project_root/prep_cases/qc/merged_quality_by_condition_<tag>_<year>.(rds|csv)
  saveRDS(quality_by_condition,
          file.path(qc_dir, paste0("merged_quality_by_condition_", stem, ".rds")))
  write_csv(quality_by_condition,
            file.path(qc_dir, paste0("merged_quality_by_condition_", stem, ".csv")))

  message("[", outcome_tag, "] Done. Rows merged: ", nrow(merged),
          " | conditions: ", nrow(quality_by_condition))

  # Return the merged table invisibly in case the caller wants it.
  invisible(merged)
}
