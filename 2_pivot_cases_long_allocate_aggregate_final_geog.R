# ===========================================================================*
# 2_pivot_cases_long_allocate_aggregate_final_geog.R: Script Description ----
# ===========================================================================*
# PURPOSE
#   1. Pivot the wide case file to long (one row per case_group), setting aside
#      the small-population race groups into a separate file.
#   2. Allocate ZIP-level cases to final geographies using an allocation
#      crosswalk (cases * pct), then aggregate to each final geography.
#
# INPUTS (read from disk)
#   - data/clean/cases_long.rds                       (re-read checkpoint, below)
#   - prep_allocation_xwalk/data/clean/allocxwalk.rds allocation crosswalk
#   Also uses cases_wide / case_group_cols from step 1 (this script is sourced).
#
# OUTPUTS (written to disk)  -- see "SAVES A FILE" markers
#   - data/clean/cases_long.rds/.csv                  long case file
#   - data/clean/small_cases_groups.rds/.csv          set-aside small groups
#   - data/clean/final_adjusted/final_adjusted_cases.rds/.csv
#   - data/clean/final_adjusted/by_condition/<cond>/final_adjusted_<cond>.rds/.csv
#   - data/clean/final_adjusted/by_outcome/<outcome>/final_adjusted_<outcome>.rds/.csv
#   - qc/qc_2_pivot_cases_long_allocate_aggregate_final_geog.txt   QC report
#
# PATHS: Normally sourced by 1_prep_cases_add_pops.R, so it inherits that
#   script's Step 0 configuration. A minimal Step 0 below lets it run on its own.
# --------------------------------------------------------------------------*

# ---- Step 0 — Configuration SET IN 1_prep cases add pops.R -----
# Allocation crosswalk (sibling project).
#alloc_xwalk_file <- file.path(project_root, "prep_allocation_xwalk",
#"data", "clean", "allocxwalk.rds")

# Output subfolders built from clean_dir.
#final_adjusted_dir <- file.path(clean_dir, "final_adjusted")
#dir_create(final_adjusted_dir)

# QC helper script and QC report folder.
#qc_functions_script <- file.path(project_root, "shared_scripts", "qc_functions.R")
#qc_dir              <- file.path(project_root, "rates", "qc")


# ---- Load Packages ---------------------------------------------------------
#pacman::p_load(tidyverse, fs)

# ---- Start QC log ------------------------------------------------------------
# Checks and printouts below are written to qc/qc_2_pivot_cases_long_allocate_aggregate_final_geog.txt
# by the QC CHECK section at the bottom. Any FAIL (or R error) stops the run
# AND writes the report.
if (!exists("qc_start")) source(qc_functions_script)
qc_start("2_pivot_cases_long_allocate_aggregate_final_geog.R", qc_dir)

# QC CHECK: objects inherited from step 1 are present.
qc_check("Inherited cases_wide and case_group_cols exist (from step 1)",
         exists("cases_wide") && exists("case_group_cols"),
         details = "Run this script through 1_prep_cases_add_pops.R, or load them first.")

# ---- Small-population race groups pulled OUT of the main long file ----------
# Handled separately from the main groups. (Behavior unchanged — same list.)
removed_case_groups <- c(
  "nhpi_total_case_sum",
  "nhpi_male_total_case_sum",
  "nhpi_female_total_case_sum",
  "multiplerace_total_case_sum",
  "multiplerace_male_total_case_sum",
  "multiplerace_female_total_case_sum",
  "asian_total_case_sum",
  "asian_male_total_case_sum",
  "asian_female_total_case_sum",
  "all_aian_total_case_sum",
  "all_aian_male_total_case_sum",
  "all_aian_female_total_case_sum",
  "multiplerace_some_other_total_case_sum",
  "multiplerace_some_other_male_total_case_sum",
  "multiplerace_some_other_female_total_case_sum"
)

# QC CHECK: every name in the set-aside list exists (a typo would leave that
# group in the main file).
not_found_removed <- setdiff(removed_case_groups, case_group_cols)
qc_check("Every set-aside small-population group exists in the case file",
         length(not_found_removed) == 0, on_fail = "warn",
         details = paste("Not found:", paste(not_found_removed, collapse = ", ")))

# ===========================================================================*
# Step 1 — Pivot wide -> long, keeping the main case groups -----------------
# ===========================================================================*
cases_long <- cases_wide %>%
  pivot_longer(
    cols      = all_of(case_group_cols),
    names_to  = "case_group",
    values_to = "cases"
  ) %>%
  filter(!case_group %in% removed_case_groups)

# ===========================================================================*
# Step 2 — Pivot again, keeping ONLY the set-aside small groups -------------
# ===========================================================================*
cases_removed <- cases_wide %>%
  pivot_longer(
    cols      = all_of(case_group_cols),
    names_to  = "case_group",
    values_to = "cases"
  ) %>%
  filter(case_group %in% removed_case_groups)

# QC CHECK: the two long files together hold every wide cell exactly once.
qc_check("Pivot row counts: main + set-aside = wide rows x counter columns",
         nrow(cases_long) + nrow(cases_removed) == nrow(cases_wide) * length(case_group_cols),
         details = sprintf("main = %s, set-aside = %s, expected total = %s",
                           nrow(cases_long), nrow(cases_removed),
                           nrow(cases_wide) * length(case_group_cols)))
qc_log("Pivot row counts", data.frame(
  file = c("cases_long (main)", "small_cases_groups (set aside)"),
  rows = c(nrow(cases_long), nrow(cases_removed)),
  case_groups = c(n_distinct(cases_long$case_group), n_distinct(cases_removed$case_group))
))

# ===========================================================================*
# Step 3 — Save the long files ----------------------------------------------
# ===========================================================================*
# SAVES FILES -> data/clean/cases_long.(rds|csv)
saveRDS(cases_long, file.path(clean_dir, "cases_long.rds"))
write_csv(cases_long, file.path(clean_dir, "cases_long.csv"))

# SAVES FILES -> data/clean/small_cases_groups.(rds|csv)
saveRDS(cases_removed, file.path(clean_dir, "small_cases_groups.rds"))
write_csv(cases_removed, file.path(clean_dir, "small_cases_groups.csv"))

cat("Saved cases_long and small_cases_groups to:", clean_dir, "\n")

# ===========================================================================*
# Step 4 — Reload long file (checkpoint) + load allocation crosswalk --------
# ===========================================================================*
#load long case dataset
cases_long <- readRDS(file.path(clean_dir, "cases_long.rds")) %>%
  select(-source_file) %>%
  mutate(ENCLOSINGZIP = as.character(ENCLOSINGZIP))

#load crosswalk
qc_input_exists(alloc_xwalk_file, "allocxwalk.rds")
crosswalk <- readRDS(alloc_xwalk_file) %>%
  mutate(ENCLOSINGZIP = as.character(ENCLOSINGZIP))

# QC CHECK: crosswalk structure and allocation percentages.
qc_required_columns(
  crosswalk,
  c("ENCLOSINGZIP", "pct", "Geography", "GeoType", "GeoName", "GeoID", "Region",
    "Year", "SES", "sourcevar", "GeoName2"),
  "Allocation crosswalk"
)
bad_pct <- crosswalk %>% filter(is.na(pct) | pct < 0 | pct > 1)
qc_check("Crosswalk pct is present and between 0 and 1", nrow(bad_pct) == 0, details = bad_pct)

pct_sums <- crosswalk %>%
  group_by(GeoType, ENCLOSINGZIP) %>%
  summarise(pct_sum = sum(pct), .groups = "drop")
over_allocated <- pct_sums %>% filter(pct_sum > 1 + 1e-6)
qc_check("Crosswalk pct sums to no more than 1 per ENCLOSINGZIP within each GeoType",
         nrow(over_allocated) == 0, details = over_allocated)
qc_log("Crosswalk coverage by GeoType",
       pct_sums %>%
         group_by(GeoType) %>%
         summarise(n_zip = n(),
                   zips_pct_sum_1 = sum(abs(pct_sum - 1) < 1e-6),
                   zips_pct_sum_below_1 = sum(pct_sum < 1 - 1e-6),
                   min_pct_sum = min(pct_sum), .groups = "drop") %>%
         left_join(crosswalk %>% group_by(GeoType) %>%
                     summarise(n_geographies = n_distinct(Geography), .groups = "drop"),
                   by = "GeoType"),
       note = "A ZIP with pct_sum below 1 in a GeoType is only partly covered by that GeoType (e.g. unincorporated areas). Confirm this matches the crosswalk design.")

# ===========================================================================*
# Step 5 — Allocate ZIP cases to final geographies --------------------------
# ===========================================================================*
# many-to-many: each ZIP can map to several final geographies, each with a pct.
# adjusted_cases = cases * pct. Rename the case columns from _sum to _adj.

# ***QC CHECK*** -------------------------------------------------------------
# CHECK NUMBER OF ENCLOSING ZIPS IN ALLOCATION CROSSWALK WITH NO MATCHING ZIP IN CONDITION FILES
qc1 <- setdiff(crosswalk$ENCLOSINGZIP, cases_long$ENCLOSINGZIP)
qc_log("QC CHECK 1: ENCLOSINGZIPs in crosswalk with NO match in case files",
       if (length(qc1)) sort(qc1) else "None.",
       note = "Informational: ZIPs with no cases for any outcome/condition this year.")

#CONFIRM THAT ALL ZIPS IN CASE FILE HAVE A MATCHING ENCLOSING ZIP IN THE ALLOCATION CROSSWALK
# Cases in these ZIPs are NOT allocated to any geography (they drop out here).
qc2 <- setdiff(cases_long$ENCLOSINGZIP, crosswalk$ENCLOSINGZIP)
qc2_cases <- cases_long %>%
  filter(ENCLOSINGZIP %in% qc2, case_group == "total_case_sum") %>%
  group_by(ENCLOSINGZIP, OUTCOME) %>%
  summarise(total_cases_not_allocated = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_cases_not_allocated))
qc_check("QC CHECK 2: every ENCLOSINGZIP in the case files is in the crosswalk",
         length(qc2) == 0, details = qc2_cases, on_fail = "warn")

# END QC CHECK


# semi-join cases by enclosing zip to ensure blank rows are not created
crosswalk_filtered <- crosswalk %>%
  semi_join(cases_long, by = "ENCLOSINGZIP")

# left-join by enclosing zip: joining allocation crosswalk into cases
joined <- crosswalk_filtered %>%
  left_join(cases_long, by = "ENCLOSINGZIP", relationship = "many-to-many") %>%
  rename(Year = Year.y) %>%
  select(-Year.x)

#calculate adjusted cases and rename case_group to *_adj
adjusted <- joined %>%
  mutate(
    adjusted_cases = cases * pct,
    case_group     = gsub("_sum$", "_adj", case_group)
  )

# ===========================================================================*
# Step 6 — Aggregate to the final geography ---------------------------------
# ===========================================================================*
# NOTE: na.rm = FALSE is intentional — a missing ZIP-level value makes the whole
# geography total NA (fail-loud).
final_geo_agg <- adjusted %>%
  group_by(
    Geography, GeoType, GeoName, GeoID, Region, Year, SES, sourcevar, GeoName2,
    OUTCOME, CONDITION, case_group
  ) %>%
  summarize(total_adjusted_cases = sum(adjusted_cases, na.rm = FALSE), .groups = "drop")

# QC CHECK (fail-loud): any NA total means a missing ZIP-level count or pct fed
# into that geography. Stop here with the source rows listed.
na_totals <- final_geo_agg %>% filter(is.na(total_adjusted_cases))
qc_check("No NA total_adjusted_cases after aggregation", nrow(na_totals) == 0,
         details = adjusted %>%
           filter(is.na(adjusted_cases)) %>%
           count(OUTCOME, CONDITION, case_group, ENCLOSINGZIP,
                 cases_na = is.na(cases), pct_na = is.na(pct), name = "n_rows"))
qc_check("No negative total_adjusted_cases", !any(final_geo_agg$total_adjusted_cases < 0, na.rm = TRUE),
         details = final_geo_agg %>% filter(total_adjusted_cases < 0))

# QC CHECK: allocation does not create cases. Per GeoType, allocated total_case
# must not exceed the original total_case in crosswalk ZIPs.
alloc_conservation <- final_geo_agg %>%
  filter(case_group == "total_case_adj") %>%
  group_by(GeoType, OUTCOME) %>%
  summarise(allocated_total_case = sum(total_adjusted_cases), .groups = "drop") %>%
  left_join(
    cases_long %>%
      filter(case_group == "total_case_sum", ENCLOSINGZIP %in% crosswalk$ENCLOSINGZIP) %>%
      group_by(OUTCOME) %>%
      summarise(original_total_case = sum(cases, na.rm = TRUE), .groups = "drop"),
    by = "OUTCOME"
  ) %>%
  mutate(ratio = round(allocated_total_case / original_total_case, 4))
qc_log("Allocation conservation: allocated vs original total_case (all conditions) by GeoType",
       alloc_conservation,
       note = "ratio = 1 for GeoTypes that cover every ZIP (e.g. county-wide); below 1 only where a GeoType covers part of the county.")
qc_check("Allocation never creates cases (ratio <= 1 for every GeoType x OUTCOME)",
         all(alloc_conservation$ratio <= 1 + 1e-6, na.rm = TRUE),
         details = alloc_conservation %>% filter(ratio > 1 + 1e-6))

# ===========================================================================*
# Step 7 — Save the main adjusted output ------------------------------------
# ===========================================================================*

# SAVES FILES -> data/clean/final_adjusted/final_adjusted_cases.(rds|csv)
saveRDS(final_geo_agg, file.path(final_adjusted_dir, "final_adjusted_cases.rds"))
write_csv(final_geo_agg, file.path(final_adjusted_dir, "final_adjusted_cases.csv"))
cat("Final adjusted dataset saved to:", final_adjusted_dir, "\n")

# ===========================================================================*
# Step 8 — Save split copies BY CONDITION -----------------------------------
# ===========================================================================*
condition_dir <- file.path(final_adjusted_dir, "by_condition")
dir_create(condition_dir)

for (cond in unique(final_geo_agg$CONDITION)) {
  cond_data   <- final_geo_agg %>% filter(CONDITION == cond)
  cond_folder <- gsub("[^A-Za-z0-9_]+", "_", cond)          # safe folder name
  cond_path   <- file.path(condition_dir, cond_folder)
  dir_create(cond_path)
  
  # SAVES FILES -> .../by_condition/<cond>/final_adjusted_<cond>.(rds|csv)
  saveRDS(cond_data, file.path(cond_path, paste0("final_adjusted_", cond_folder, ".rds")))
  write_csv(cond_data, file.path(cond_path, paste0("final_adjusted_", cond_folder, ".csv")))
}

# ===========================================================================*
# Step 9 — Save split copies BY OUTCOME -------------------------------------
# ===========================================================================*
outcome_dir <- file.path(final_adjusted_dir, "by_outcome")
dir_create(outcome_dir)

for (outc in unique(final_geo_agg$OUTCOME)) {
  outc_data   <- final_geo_agg %>% filter(OUTCOME == outc)
  outc_folder <- gsub("[^A-Za-z0-9_]+", "_", outc)          # safe folder name
  outc_path   <- file.path(outcome_dir, outc_folder)
  dir_create(outc_path)
  
  # SAVES FILES -> .../by_outcome/<outcome>/final_adjusted_<outcome>.(rds|csv)
  saveRDS(outc_data, file.path(outc_path, paste0("final_adjusted_", outc_folder, ".rds")))
  write_csv(outc_data, file.path(outc_path, paste0("final_adjusted_", outc_folder, ".csv")))
}

cat("Saved by-condition and by-outcome splits.\n")


# ===========================================================================*
# QC CHECK — End-of-script checks and QC report ----
# ===========================================================================*
# Writes qc/qc_2_pivot_cases_long_allocate_aggregate_final_geog.txt: a cover page (what to confirm),
# any FAIL/WARN entries, a check summary, and every printout from this script.

# QC CHECK: one row per geography x outcome x condition x case_group.
dup_geo <- final_geo_agg %>%
  count(Geography, Year, OUTCOME, CONDITION, case_group, name = "n") %>%
  filter(n > 1)
qc_check("One row per Geography x Year x OUTCOME x CONDITION x case_group",
         nrow(dup_geo) == 0,
         details = c("Geography attributes (GeoType, GeoName, Region, ...) differ within a Geography in the crosswalk:",
                     .qc_capture(dup_geo)))

# PRINTOUTS: structure of the aggregated file.
qc_log("Final adjusted file: rows, geographies, and conditions by OUTCOME",
       final_geo_agg %>%
         group_by(OUTCOME) %>%
         summarise(n_rows = n(), n_geographies = n_distinct(Geography),
                   n_conditions = n_distinct(CONDITION), n_case_groups = n_distinct(case_group),
                   .groups = "drop"))
qc_log("Geographies by GeoType", final_geo_agg %>% distinct(GeoType, Geography) %>% count(GeoType, name = "n_geographies"))

# QC CHECK: outputs written this run.
qc_files_written(c(
  file.path(clean_dir, c("cases_long.rds", "cases_long.csv",
                         "small_cases_groups.rds", "small_cases_groups.csv")),
  file.path(final_adjusted_dir, c("final_adjusted_cases.rds", "final_adjusted_cases.csv"))
))
qc_check("A by_condition folder was written for every CONDITION",
         length(list.dirs(condition_dir, recursive = FALSE)) >= n_distinct(final_geo_agg$CONDITION),
         details = sprintf("folders = %s, conditions = %s",
                           length(list.dirs(condition_dir, recursive = FALSE)),
                           n_distinct(final_geo_agg$CONDITION)))
qc_check("A by_outcome folder was written for every OUTCOME",
         length(list.dirs(outcome_dir, recursive = FALSE)) >= n_distinct(final_geo_agg$OUTCOME),
         details = sprintf("folders = %s, outcomes = %s",
                           length(list.dirs(outcome_dir, recursive = FALSE)),
                           n_distinct(final_geo_agg$OUTCOME)))

qc_finish(
  confirm = c(
    "RUN STATUS is PASSED or PASSED WITH WARNINGS, and every WARN has a reviewer note.",
    "Pivot: the pivot row count check passed, so every wide cell became exactly one long row (main file plus set-aside file).",
    "Set-aside groups: no WARN about names not found (a typo would leave a small group in the main file).",
    "Crosswalk coverage by GeoType: pct sums and number of geographies per GeoType match the crosswalk design (e.g. the county GeoType covers every ZIP with pct_sum = 1).",
    "QC CHECK 2: if any case-file ZIPs are missing from the crosswalk, the listed cases not allocated are expected (e.g. 99999 unknown ZIP) and small.",
    "Allocation conservation: ratio is 1 for county-wide GeoTypes and at or below 1 for the rest.",
    "Final adjusted file by OUTCOME: all four outcomes present; geography and condition counts are in line with last year.",
    "Geographies by GeoType: counts match the expected geography list (target 74 geographies in total)."
  ),
  review_files = c(
    "data/clean/final_adjusted/final_adjusted_cases.csv: pick one CONDITION and OUTCOME; the county-level total_case_adj should equal total_case_sum for that condition in all_outcomes_merged.csv (minus any ZIPs listed in QC CHECK 2).",
    "data/clean/small_cases_groups.csv: contains only the set-aside small-population groups.",
    "data/clean/final_adjusted/by_condition/ and by_outcome/: one folder per condition and per outcome."
  )
)