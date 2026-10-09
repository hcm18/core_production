# ===========================================================================*
# 1b_match_up_adjusted_cases_pops.R: Script Description ----------------------
# ===========================================================================*
# PURPOSE
#   Attach population denominators to the geography-allocated case file AND build
#   the complete reporting grid: one row per Geography x demo_key x CONDITION x
#   OUTCOME, including true zeros, so downstream files are rectangular.
#
#   WHY THE GRID STEP EXISTS: the case file only contains combinations that had
#   at least one event, so a condition/geography/demographic with zero cases was
#   simply MISSING (no row). Rates need a row for every valid combination -
#   including 0-count ones. This script starts from the POPULATION file (which
#   enumerates every geography x demographic that has a denominator), crosses it
#   with the valid condition x outcome pairs, then attaches cases; combinations
#   with no events become 0, not missing rows.
#
#   DEMOGRAPHIC UNIVERSE: taken directly from the population file (pop_df). Every
#   geography x demo_key present in pop_df defines the grid, so the grid should be
#   n_geo x n_demo per condition x outcome (target: 74 geographies x 163 groups).
#
#   SEX-SPECIFIC NOTE: this step intentionally CREATES rows for every demographic
#   x condition, including impossible ones (e.g. female prostate cancer). Those
#   are removed in step 4 (sex-specific processing) BEFORE rates are computed in
#   step 5, so no rate is ever produced for them.
#
# INPUTS (read from disk)
#   - data/clean/final_adjusted/final_adjusted_cases.rds        (from step 2)
#   - prep_populations/data/clean/final_populations_xgeography_vert_<YYYY>.rds
#
# OUTPUTS (written to disk)  -- see "SAVES A FILE" markers
#   - project_root/data/output/cleaned cases/final_adjusted/final_adjusted_cases_with_pop.rds/.csv
#   - project_root/rates/qc/grid_condition_outcome_lookup.csv          valid condition x outcome pairs
#   - project_root/rates/qc/grid_rows_per_condition_outcome.csv        rows per combo vs expected
#   - project_root/rates/qc/grid_coverage_checks.csv                   coverage/QC summary
#   - project_root/rates/qc/grid_dropped_numerator_cases.csv           (only if any cases were dropped)
#   - project_root/rates/qc/qc_1b_match_up_adjusted_cases_pops.txt      QC report (see QC CHECK section)
#
# PATHS: Normally sourced by 1_prep_cases_add_pops.R (inherits Step 0 config).
#   A minimal Step 0 below lets it run on its own.
# --------------------------------------------------------------------------*

# ---- Step 0 — Configuration SET IN 1_prep cases add pops.R -----
# The following are defined in step 1 and inherited here when sourced:
#   final_adjusted_dir, pop_dir, qc_dir
# Uncomment to run this script on its own:
# project_root       <- "D:/core_production"
# clean_dir          <- file.path(project_root, "rates", "data", "output", "cleaned cases")
# final_adjusted_dir <- file.path(clean_dir, "final_adjusted")
# pop_dir            <- file.path(project_root, "prep_populations", "data", "output")
# qc_dir             <- file.path(project_root, "rates", "qc")
# qc_functions_script <- file.path(project_root, "shared_scripts", "qc_functions.R")
# pacman::p_load(tidyverse, fs)

# ---- Start QC log ------------------------------------------------------------
# Checks and printouts below are written to qc/qc_1b_match_up_adjusted_cases_pops.txt
# by the QC CHECK section at the bottom. Any FAIL (or R error) stops the run AND
# writes the report.
if (!exists("qc_start")) source(qc_functions_script)
qc_start("1b_match_up_adjusted_cases_pops.R", qc_dir)

# ===========================================================================*
# Step 1 — Locate the population file (one per year) ------------------------
# ===========================================================================*
qc_check("Cases folder exists", dir.exists(final_adjusted_dir),
         details = paste("Cases path missing:", final_adjusted_dir))
qc_check("Population folder exists", dir.exists(pop_dir),
         details = paste("Population path missing:", pop_dir))

pop_file <- list.files(
  pop_dir,
  pattern    = "^final_populations_xgeography_vert_\\d{4}\\.rds$",
  full.names = TRUE
)
qc_check("A final_populations_xgeography_vert_YYYY.rds file exists", length(pop_file) > 0,
         details = paste("No 'final_populations_xgeography_vert_YYYY.rds' in", pop_dir))
qc_check("Only one population file in the folder", length(pop_file) == 1,
         details = c("Multiple population files found; using the first:", pop_file),
         on_fail = "warn")
pop_file <- pop_file[1]

# ===========================================================================*
# Step 2 — Load both datasets -----------------------------------------------
# ===========================================================================*
#load population file
pop_df   <- readRDS(pop_file)
#load cases file
qc_input_exists(file.path(final_adjusted_dir, "final_adjusted_cases.rds"))
cases_df <- readRDS(file.path(final_adjusted_dir, "final_adjusted_cases.rds"))

# QC CHECK: population file structure, and cases and population are the same year.
qc_required_columns(
  pop_df,
  c("Geography", "Year", "GeoType", "GeoName", "GeoID", "Region", "SES", "sourcevar",
    "GeoName2", "pop_demog", "pop_counts"),
  "Population file"
)
pop_file_year <- sub("^.*_(\\d{4})\\.rds$", "\\1", basename(pop_file))
case_years    <- unique(as.character(cases_df$Year))
qc_log("Population file used", c(pop_file, paste("File year:", pop_file_year)))
qc_check("Population Year values match the case file Year (join key)",
         setequal(unique(as.character(pop_df$Year)), case_years),
         details = sprintf("case Year(s) = %s; population Year values = %s. No population rows would match.",
                           paste(case_years, collapse = ", "),
                           paste(unique(pop_df$Year), collapse = ", ")))
qc_check("Population file name year matches the case file Year",
         identical(case_years, pop_file_year), on_fail = "warn",
         details = sprintf("case Year(s) = %s; population file name year = %s. With several files in the folder, the first one listed is used.",
                           paste(case_years, collapse = ", "), pop_file_year))
qc_check("Year has the same type in cases and population (numeric vs character)",
         is.numeric(cases_df$Year) == is.numeric(pop_df$Year),
         details = sprintf("cases Year class = %s, population Year class = %s. Convert one before joining.",
                           class(cases_df$Year)[1], class(pop_df$Year)[1]))

# ===========================================================================*
# Step 3 — Build the demographic matching key on each side ------------------
# ===========================================================================*
# Strip the group-type suffix so "hisp_total_pop_adj" and "hisp_total_case_adj"
# both reduce to the shared key "hisp_total".
pop_df   <- pop_df   %>% 
  mutate(demo_key = str_replace(pop_demog,  "_pop_adj$",  ""))
cases_df <- cases_df %>% 
  mutate(demo_key = str_replace(case_group, "_case_adj$", ""))

# ===========================================================================*
# ***QC CHECK*** (does every case have a denominator?) ------------
# ===========================================================================*
# The demographic universe IS whatever pop_df contains. The important risk is the
# reverse: a case whose demo_key or geography is NOT in pop_df will be dropped by
# the population-spine grid, silently losing a real numerator. Surface that here.


case_no_pop      <- setdiff(unique(cases_df$demo_key),  unique(pop_df$demo_key))   # DROPPED numerator (demo)
geo_cases_no_pop <- setdiff(unique(cases_df$Geography), unique(pop_df$Geography))  # DROPPED numerator (geo)

cat("\n=== POPULATION UNIVERSE / COVERAGE ===\n")
cat("Geographies in pop:  ", n_distinct(pop_df$Geography), "\n")
cat("demo_keys in pop:    ", n_distinct(pop_df$demo_key), "\n")
cat("======================================\n\n")

qc_log("Population universe", data.frame(
  measure = c("Geographies in population file", "demo_keys in population file",
              "demo_keys in case file"),
  value   = c(n_distinct(pop_df$Geography), n_distinct(pop_df$demo_key),
              n_distinct(cases_df$demo_key))
))
qc_check("Every case demo_key has a population (otherwise its numerator is dropped)",
         length(case_no_pop) == 0, on_fail = "warn",
         details = cases_df %>%
           filter(demo_key %in% case_no_pop) %>%
           group_by(demo_key) %>%
           summarise(total_cases_dropped = sum(total_adjusted_cases, na.rm = TRUE), .groups = "drop") %>%
           arrange(desc(total_cases_dropped)))
qc_check("Every case Geography has a population (otherwise its cases are dropped)",
         length(geo_cases_no_pop) == 0, on_fail = "warn",
         details = cases_df %>%
           filter(Geography %in% geo_cases_no_pop) %>%
           group_by(Geography) %>%
           summarise(total_cases_dropped = sum(total_adjusted_cases, na.rm = TRUE), .groups = "drop"))

# END QC CHECK

# ===========================================================================*
# Step 4 — Build the complete reporting grid --------------------------------
# ===========================================================================*
# (a) Valid condition x outcome pairs, taken from the cases themselves. Because
#     an mcod-only (death) condition only ever appears with OUTCOME == "Death"
#     in the case file, it never gets an "ED Discharge" (or other morbidity) row.
cond_outcome <- cases_df %>%
  distinct(CONDITION, OUTCOME)

# (b) The spine: one row per geography x demo_key x Year straight from pop_df,
#     carrying the geography attributes and the population denominator.
pop_spine <- pop_df %>%
  distinct(Geography, Year, GeoType, GeoName, GeoID, Region, SES, sourcevar,
           GeoName2, demo_key, pop_demog, pop_counts)

# QC CHECK: one spine row per Geography x Year x demo_key. A second row (e.g. two
# different pop_counts) would duplicate every grid row for that group.
dup_spine <- pop_spine %>% count(Geography, Year, demo_key, name = "n") %>% filter(n > 1)
qc_check("Population spine has one row per Geography x Year x demo_key", nrow(dup_spine) == 0,
         details = pop_spine %>% semi_join(dup_spine, by = c("Geography", "Year", "demo_key")))

# (c) Full grid = every spine row crossed with every valid condition x outcome.
full_grid <- pop_spine %>%
  cross_join(cond_outcome)

# (d) Attach the allocated cases; genuine zeros become 0 (not missing rows).
#     Join to a SLIM cases table (keys + count only) so no geography/pop columns
#     collide - the geography attributes come from the population spine.
cases_slim <- cases_df %>%
  distinct(Geography, Year, demo_key, CONDITION, OUTCOME, total_adjusted_cases)

# QC CHECK: cases_slim must be unique on the join keys, otherwise the join below
# multiplies grid rows.
dup_cases <- cases_slim %>%
  count(Geography, Year, demo_key, CONDITION, OUTCOME, name = "n") %>%
  filter(n > 1)
qc_check("Case file has one row per Geography x Year x demo_key x CONDITION x OUTCOME",
         nrow(dup_cases) == 0,
         details = cases_slim %>%
           semi_join(dup_cases, by = c("Geography", "Year", "demo_key", "CONDITION", "OUTCOME")) %>%
           arrange(Geography, OUTCOME, CONDITION, demo_key))

# Zero-fill ONLY grid rows that had no case row at all (genuine zeros).
# A matched case row whose total is NA keeps its NA so step 2's fail-loud NA is
# not converted to a false zero; it is caught by the QC check right below.
merged_df <- full_grid %>%
  left_join(cases_slim %>% mutate(.case_row_matched = TRUE),
            by = c("Geography", "Year", "demo_key", "CONDITION", "OUTCOME")) %>%
  mutate(total_adjusted_cases = if_else(is.na(.case_row_matched), 0, total_adjusted_cases)) %>%
  select(-.case_row_matched)

# QC CHECK (fail-loud): no NA cases survive the join.
qc_check("No NA total_adjusted_cases on matched case rows", !anyNA(merged_df$total_adjusted_cases),
         details = merged_df %>%
           filter(is.na(total_adjusted_cases)) %>%
           count(OUTCOME, CONDITION, demo_key, name = "n_rows_na"))

# Guard: the left_join must not multiply rows. If it does, cases_slim had
# duplicate keys (investigate step 2's aggregation).
qc_check("Grid row count unchanged by the case join", nrow(merged_df) == nrow(full_grid),
         details = sprintf("grid = %s rows, after join = %s rows", nrow(full_grid), nrow(merged_df)))

# QC CHECK: every case that has a population and geography lands in the grid.
cases_kept_total <- cases_slim %>%
  semi_join(pop_spine, by = c("Geography", "Year", "demo_key")) %>%
  pull(total_adjusted_cases) %>%
  sum(na.rm = TRUE)
grid_total <- sum(merged_df$total_adjusted_cases, na.rm = TRUE)
qc_check("Grid total cases equal case-file total for groups with a population",
         abs(grid_total - cases_kept_total) < 1e-6,
         details = sprintf("grid total = %s, case-file total (with population) = %s",
                           grid_total, cases_kept_total),
         pass_note = sprintf("total_adjusted_cases in grid = %s", round(grid_total, 2)))

# ===========================================================================*
# ***QC CHECK*** (confirm every combination is present) ----------------
# ===========================================================================*
# Every condition x outcome should have exactly one row per geography x demo_key
# in the population file. spine_rows is that expected count; expected_full is the
# fully-rectangular target (n_geo x n_demo, e.g. 74 x 163 = 12,062). If pop is
# perfectly rectangular the two match; if not, spine_rows is the real expectation.
n_geo         <- n_distinct(pop_spine$Geography)
n_demo        <- n_distinct(pop_spine$demo_key)
spine_rows    <- nrow(pop_spine)          # actual geo x demo combinations in pop
expected_full <- n_geo * n_demo           # rectangular target

# Rows per condition x outcome (the check you asked for).
rows_per_combo <- merged_df %>%
  count(CONDITION, OUTCOME, name = "n_rows") %>%
  mutate(
    spine_rows    = spine_rows,
    expected_full = expected_full,
    complete      = (n_rows == spine_rows)
  ) %>%
  arrange(complete, CONDITION, OUTCOME)   # any incomplete combos sort to the top

n_incomplete  <- sum(!rows_per_combo$complete)
n_zero_filled <- sum(merged_df$total_adjusted_cases == 0, na.rm = TRUE)
n_no_pop_rows <- sum(is.na(merged_df$pop_counts))

cat("=== GRID COMPLETENESS ===\n")
cat("Geographies:                ", n_geo, "\n")
cat("demo_keys:                  ", n_demo, "\n")
cat("Rows per combo (spine_rows):", spine_rows, "\n")
cat("Rectangular target (geoxdemo):", expected_full,
    if (spine_rows == expected_full) " (pop is rectangular)" else " (pop NOT rectangular)", "\n")
cat("Condition x outcome pairs:  ", nrow(cond_outcome), "\n")
cat("Rows in grid:               ", nrow(merged_df), "\n")
cat("Rows zero-filled:           ", n_zero_filled, "\n")
cat("Combos NOT complete:        ", n_incomplete, "of", nrow(rows_per_combo), "\n")
cat("=========================\n\n")
qc_check("Every condition x outcome combo has the expected row count", n_incomplete == 0,
         on_fail = "warn",
         details = rows_per_combo %>% filter(!complete))
qc_check("Population file is rectangular (every geography has every demo_key)",
         spine_rows == expected_full, on_fail = "warn",
         details = c(sprintf("%s geo x demo rows vs %s expected (%s x %s).",
                             spine_rows, expected_full, n_geo, n_demo),
                     .qc_capture(pop_spine %>% count(Geography, name = "n_demo_keys") %>%
                                   filter(n_demo_keys != n_demo))))

# ---- Save QC files ---------------------------------------------------------*
dir_create(qc_dir)

# SAVES A FILE -> qc/grid_condition_outcome_lookup.csv  (valid pairs used)
write_csv(cond_outcome, file.path(qc_dir, "grid_condition_outcome_lookup.csv"))

# SAVES A FILE -> qc/grid_rows_per_condition_outcome.csv  (row count per combo)
write_csv(rows_per_combo, file.path(qc_dir, "grid_rows_per_condition_outcome.csv"))

# SAVES A FILE -> qc/grid_coverage_checks.csv  (one-row summary)
coverage_summary <- tibble(
  n_geographies            = n_geo,
  n_demo_keys              = n_demo,
  rows_per_combo           = spine_rows,
  rectangular_target       = expected_full,
  pop_is_rectangular       = (spine_rows == expected_full),
  n_condition_outcome      = nrow(cond_outcome),
  grid_rows                = nrow(merged_df),
  rows_zero_filled         = n_zero_filled,
  rows_missing_denominator = n_no_pop_rows,
  combos_incomplete        = n_incomplete,
  case_keys_no_pop         = length(case_no_pop),
  geographies_cases_no_pop = length(geo_cases_no_pop)
)
write_csv(coverage_summary, file.path(qc_dir, "grid_coverage_checks.csv"))

# SAVES A FILE (only if needed) -> qc/grid_dropped_numerator_cases.csv
# The actual case rows dropped because their demo_key has no population, so you
# can investigate a real numerator loss rather than just see a count.
if (length(case_no_pop) > 0) {
  cases_df %>%
    filter(demo_key %in% case_no_pop) %>%
    write_csv(file.path(qc_dir, "grid_dropped_numerator_cases.csv"))
}

# END QC CHECK

# ===========================================================================*
# Step 5 — Save -------------------------------------------------------------
# ===========================================================================*
# SAVES FILES -> data/clean/final_adjusted/final_adjusted_cases_with_pop.(rds|csv)
saveRDS(merged_df, file.path(final_adjusted_dir, "final_adjusted_cases_with_pop.rds"))
write_csv(merged_df, file.path(final_adjusted_dir, "final_adjusted_cases_with_pop.csv"))


cat("Saved merged cases+pop dataset (complete grid) to:", final_adjusted_dir, "\n")


# ===========================================================================*
# QC CHECK — End-of-script checks and QC report ----
# ===========================================================================*
# Writes qc/qc_1b_match_up_adjusted_cases_pops.txt: a cover page (what to
# confirm), any FAIL/WARN entries, a check summary, and every printout.

# QC CHECK: denominators. NA or zero populations give NA/Inf rates in step 5.
qc_check("Every grid row has a population (pop_counts not NA)",
         !anyNA(merged_df$pop_counts), on_fail = "warn",
         details = merged_df %>% filter(is.na(pop_counts)) %>%
           distinct(Geography, demo_key) %>% count(demo_key, name = "n_geographies"))
qc_check("No rows with cases > 0 and population 0",
         !any(merged_df$total_adjusted_cases > 0 & merged_df$pop_counts == 0, na.rm = TRUE),
         on_fail = "warn",
         details = merged_df %>%
           filter(total_adjusted_cases > 0, pop_counts == 0) %>%
           select(Geography, OUTCOME, CONDITION, demo_key, total_adjusted_cases, pop_counts))

# PRINTOUTS: grid summary.
qc_log("Grid coverage summary (also saved as qc/grid_coverage_checks.csv)",
       coverage_summary %>% pivot_longer(everything(), names_to = "measure",
                                         values_to = "value", values_transform = as.character))
qc_log("Grid rows and total cases by OUTCOME",
       merged_df %>%
         group_by(OUTCOME) %>%
         summarise(n_rows = n(), n_conditions = n_distinct(CONDITION),
                   rows_zero_filled = sum(total_adjusted_cases == 0),
                   total_adjusted_cases = round(sum(total_adjusted_cases), 2), .groups = "drop"))

# QC CHECK: outputs written this run.
qc_files_written(c(
  file.path(final_adjusted_dir, c("final_adjusted_cases_with_pop.rds",
                                  "final_adjusted_cases_with_pop.csv")),
  file.path(qc_dir, c("grid_condition_outcome_lookup.csv",
                      "grid_rows_per_condition_outcome.csv",
                      "grid_coverage_checks.csv"))
))

qc_finish(
  confirm = c(
    "RUN STATUS is PASSED or PASSED WITH WARNINGS, and every WARN has a reviewer note.",
    "Population file used: the file year matches the data year.",
    "Population universe: geographies (target 74) and demo_keys (target 163) match expectations.",
    "Case demo_keys with no population (WARN, if present): the listed demo_keys are groups intentionally not published (no denominator), not naming mismatches such as a typo in a demo_key.",
    "Geographies with no population (WARN, if present): should normally be none.",
    "Grid completeness: no incomplete condition x outcome combos, and the population file is rectangular (or the gaps are explained).",
    "Grid total cases check passed (no cases lost in the join).",
    "Denominators: no NA pop_counts and no cases with population 0, or each listed row is explained.",
    "Grid rows by OUTCOME: condition counts per outcome match step 2; total cases are in line with last year."
  ),
  review_files = c(
    "qc/grid_rows_per_condition_outcome.csv: 'complete' is TRUE for every row.",
    "qc/grid_coverage_checks.csv: case_keys_no_pop and geographies_cases_no_pop match what this report explains.",
    "qc/grid_dropped_numerator_cases.csv (only written when cases were dropped): confirm every dropped group is intentional.",
    "data/clean/final_adjusted/final_adjusted_cases_with_pop.csv: spot-check one Geography x CONDITION; total_adjusted_cases matches final_adjusted_cases.csv and pop_counts matches the population file."
  )
)