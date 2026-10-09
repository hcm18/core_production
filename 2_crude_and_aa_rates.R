# ===========================================================================*
# 5_crude_and_aa_rates.R: Script Description --------------------------------
# ===========================================================================*
# PURPOSE
#   Compute crude and age-adjusted (AA) rates with confidence intervals, then run
#   the full suppression pipeline and write the final + QC outputs.
#
#   NONE of the rate formulas or suppression rules are changed here — only paths,
#   the CSV writer, output filenames, and comments differ from the raw version.
#
# INPUTS (read from disk)
#   - data/clean/final_adjusted/final_adjusted_ss_4rates.rds       (from step 4)
#   - scripts/suppression_config.R, scripts/suppression_functions.R
#   - scripts/backcalc_test_harness.R, scripts/backcalc_correctness_checker.R
#
# OUTPUTS (written to disk)  -- see "SAVES A FILE" markers
#   - data/output/df_with_aa.rds/.csv, aa_rate_summary.rds/.csv
#   - data/output/df_with_aa_by_outcome/df_with_aa_<outcome>.rds/.csv
#   - <ddi_dir>/df_with_suppression_<outcome>.rds/.csv and _all.(rds|csv)
#   - <ddi_dir>/qc/*.csv  (suppression + backcalc QC)
#   - rates/qc/qc_5_crude_and_aa_rates.txt  QC report (see QC CHECK section)
#   - rates/"for data check"/data_check_rates_<year>.xlsx  workbook for the team
#
# CSV note: final published CSVs are written with na = "NA" to exactly match the
#   previous base-R write.csv() output.
#
# PATHS: Defined in the CONFIGURATION step. This is a top-level script (not
#   sourced by step 1), so it defines its own full Step 0 configuration.
# --------------------------------------------------------------------------*

# ---- Step 0 — Configuration: EDIT HERE BEFORE RUNNING SCRIPT -----------------
# Main project root: UPDATE PROJECT ROOT FOLDER
project_root <- "D:/core_production"

rates_dir   <- file.path(project_root, "rates")
scripts_dir <- file.path(rates_dir, "scripts")
clean_dir   <- file.path(rates_dir, "data", "output", "cleaned cases")
output_dir  <- file.path(rates_dir, "data", "output")

# Input from step 4.
rates_input <- file.path(clean_dir, "final_adjusted", "final_adjusted_ss_4rates.rds")

# Suppression module scripts (sourced below).
suppression_config_script    <- file.path(scripts_dir, "suppression_config.R")
suppression_functions_script <- file.path(scripts_dir, "suppression_functions.R")
backcalc_harness_script       <- file.path(scripts_dir, "backcalc_test_harness.R")
backcalc_checker_script       <- file.path(scripts_dir, "backcalc_correctness_checker.R")

# Suppression ("ddi") output location.
ddi_dir <- file.path(output_dir, "ddi")
qc_dir  <- file.path(rates_dir, "qc", "ddi")

# Shared QC helper functions and the QC report folder. The report goes to the
# rates qc folder with the other step reports (qc_dir above holds suppression CSVs).
qc_functions_script <- file.path(project_root, "shared_scripts", "qc_functions.R")
qc_report_dir       <- file.path(rates_dir, "qc")

# Folder for the workbook that goes to the team for data checking.
data_check_dir <- file.path(rates_dir, "for data check")

# How many geographies the pipeline should produce. Used to confirm every
# condition, outcome, and demographic key has a row for every geography.
expected_geography_count <- 74

# Create this project's output directories if they don't exist.
for (d in c(output_dir, ddi_dir, qc_dir, qc_report_dir, data_check_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ---- Load Packages ---------------------------------------------------------
pacman::p_load(tidyverse, openxlsx)   # openxlsx writes the data check workbook

# ---- Start QC log ------------------------------------------------------------
# Checks and printouts below are written to rates/qc/qc_5_crude_and_aa_rates.txt
# by the QC CHECK section at the bottom. Any FAIL (or R error) stops the script
# AND writes the report.
if (!file.exists(qc_functions_script))
  stop("QC helper script not found: ", qc_functions_script)
source(qc_functions_script)
qc_start("5_crude_and_aa_rates.R", qc_report_dir, reset = TRUE)


# ---------------------------------------------------------------------------*
# PART A: CALCULATING RATES WORK FLOW ----
# ---------------------------------------------------------------------------*

# ===========================================================================*
# Step 1 — Load the sex-specific-adjusted input -----------------------------
# ===========================================================================*
qc_input_exists(rates_input, "final_adjusted_ss_4rates.rds (from step 4)")
df <- readRDS(rates_input)
qc_required_columns(df, c("Geography", "Year", "OUTCOME", "CONDITION", "demo_key",
                          "new_cases", "new_pop", "total_adjusted_cases"),
                    "final_adjusted_ss_4rates.rds")
qc_set_year(unique(df$Year))
n_rows_input <- nrow(df)

# ===========================================================================*
# Step 2 — Crude rate + crude CI --------------------------------------------
# ===========================================================================*

# Define the 10 official age-adjustment age groups to remove from demo_key.
aa_age_groups <- c(
  "age0_4","age5_14","age15_24","age25_34","age35_44",
  "age45_54","age55_64","age65_74","age75_84","age85plus"
)

# Extract the AA age group from demo_key.
df <- df %>%
  mutate(age_group = str_extract(demo_key, paste(aa_age_groups, collapse = "|")))

# Assign demo_group (which summary group each row rolls up to).
df <- df %>%
  mutate(
    demo_group = case_when(
      #total summary row
      demo_key == "total"                ~ "total",
      
      #total AA age rows
      demo_key %in% aa_age_groups        ~ "total",  
      
      #sex totals
      str_detect(demo_key, "^male_")     ~ "male_total",    
      str_detect(demo_key, "^female_")   ~ "female_total",
      
      #RE totals
      str_detect(demo_key, "^hisp_")     ~ "hisp_total",
      str_detect(demo_key, "^white_")    ~ "white_total",
      str_detect(demo_key, "^black_")    ~ "black_total",
      str_detect(demo_key, "^api_")      ~ "api_total",
      str_detect(demo_key, "^other_")    ~ "other_total",
      
      # sex and RE summary rows keep their label
      str_detect(demo_key, "_total$")    ~ demo_key,       
      TRUE                               ~ NA_character_
    )
  )

# Compute crude rate + CI (normal approximation on counts).
df <- df %>%
  mutate(
    crude_rate     = 100000 * (new_cases / new_pop),
    sq_counts      = sqrt(new_cases) * 1.96,
    counts_lower   = new_cases - sq_counts,
    counts_upper   = new_cases + sq_counts,
    unit_pop       = 100000 / new_pop,
    crude_ci_lower = unit_pop * counts_lower,
    crude_ci_upper = unit_pop * counts_upper
  )

# QC CHECK: denominators and crude rates.
qc_log("Rows by demo_group (NA = demo_key not rolled up to a summary group)",
       df %>% count(demo_group, name = "n_rows"))
bad_pop <- df %>% filter(is.na(new_pop) | new_pop <= 0)
qc_check("Every row has a population above 0 (new_pop)", nrow(bad_pop) == 0, on_fail = "warn",
         details = c("These rows get NA or infinite crude rates:",
                     .qc_capture(bad_pop %>% count(OUTCOME, CONDITION, new_pop_na = is.na(new_pop),
                                                   name = "n_rows"))))
qc_log("Crude rate: rows with non-finite values (NA, NaN, Inf)",
       df %>% group_by(OUTCOME) %>%
         summarise(rows = n(), crude_rate_not_finite = sum(!is.finite(crude_rate)),
                   new_cases_na = sum(is.na(new_cases)), .groups = "drop"))

# ===========================================================================*
# Step 3 — Age-adjusted rate + CI -------------------------------------------
# ===========================================================================*
# Standard-population weights for the 10 AA age groups.
age_weights <- data.frame(
  age_group = aa_age_groups,
  weight = c(
    0.069135,0.145565,0.138646,0.135573,
    0.162613,0.134834,0.087247,0.066037,
    0.044842,0.015508
  )
)

# QC CHECK: standard-population weights cover 10 groups and sum to 1.
qc_check("Age-adjustment weights: 10 groups summing to 1",
         nrow(age_weights) == 10 && abs(sum(age_weights$weight) - 1) < 1e-6,
         details = sprintf("groups = %s, sum of weights = %s", nrow(age_weights), sum(age_weights$weight)))

# Groups eligible for age adjustment.
aa_eligible_groups <- c(
  "total","male_total","female_total",
  "hisp_total","white_total","black_total",
  "api_total","other_total"
)

# Keep only AA-eligible summary groups x the 10 age bands, attach weights.
df_aa <- df %>%
  filter(demo_group %in% aa_eligible_groups, 
         age_group %in% aa_age_groups) %>%
  left_join(age_weights, by = "age_group")

# Compute age-adjusted rate + CI (only when all 10 age bands present).
aa_rate <- df_aa %>%
  group_by(Geography, CONDITION, OUTCOME, demo_group) %>%
  summarise(
    n_age       = n_distinct(age_group),
    total_cases = sum(new_cases, na.rm = TRUE),
    aa_rate = ifelse(
      n_age == length(aa_age_groups),
      sum((new_cases / new_pop) * 100000 * weight, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    aa_sq_counts = sqrt(total_cases),
    aa_se        = ifelse(total_cases > 0, aa_rate / aa_sq_counts, NA_real_),
    aa_ci_lower  = aa_rate - 1.96 * aa_se,
    aa_ci_upper  = aa_rate + 1.96 * aa_se
  )

# QC CHECK: one AA row per key (otherwise the join below multiplies rows).
dup_aa <- aa_rate %>% count(Geography, CONDITION, OUTCOME, demo_group, name = "n") %>% filter(n > 1)
qc_check("AA summary has one row per Geography x CONDITION x OUTCOME x demo_group",
         nrow(dup_aa) == 0, details = dup_aa)
qc_log("AA groups by number of age bands present (AA rate is NA unless all 10 are present)",
       aa_rate %>% count(n_age, name = "n_groups"))

# Attach AA rate/CI back, but only onto the summary rows.
df_with_aa <- df %>%
  left_join(aa_rate, by = c("Geography","CONDITION","OUTCOME","demo_group")) %>%
  mutate(
    aa_rate     = ifelse(demo_key == demo_group, aa_rate,     NA_real_),
    aa_se       = ifelse(demo_key == demo_group, aa_se,       NA_real_),
    aa_ci_lower = ifelse(demo_key == demo_group, aa_ci_lower, NA_real_),
    aa_ci_upper = ifelse(demo_key == demo_group, aa_ci_upper, NA_real_)
  )

qc_check("Row count unchanged by the AA join", nrow(df_with_aa) == n_rows_input,
         details = sprintf("input = %s rows, df_with_aa = %s rows", n_rows_input, nrow(df_with_aa)))
qc_log("AA rates attached to summary rows",
       df_with_aa %>% filter(demo_key %in% aa_eligible_groups) %>%
         group_by(demo_key) %>%
         summarise(rows = n(), aa_rate_present = sum(!is.na(aa_rate)),
                   aa_rate_na = sum(is.na(aa_rate)), .groups = "drop"))

# ===========================================================================*
# Step 4 — Export the rate outputs ------------------------------------------
# ===========================================================================*
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# SAVES FILES -> data/output/df_with_aa.(rds|csv) and aa_rate_summary.(rds|csv)
saveRDS(df_with_aa, file.path(output_dir, "df_with_aa.rds"))
write_csv(df_with_aa, file.path(output_dir, "df_with_aa.csv"), na = "NA")
saveRDS(aa_rate, file.path(output_dir, "aa_rate_summary.rds"))
write_csv(aa_rate, file.path(output_dir, "aa_rate_summary.csv"), na = "NA")

# SAVES FILES by Outcome -> data/output/df_with_aa_by_outcome/df_with_aa_<outcome>.(rds|csv)
by_outcome_dir <- file.path(output_dir, "df_with_aa_by_outcome")
dir.create(by_outcome_dir, recursive = TRUE, showWarnings = FALSE)
for (out in unique(df_with_aa$OUTCOME)) {
  df_out    <- df_with_aa %>% filter(OUTCOME == out)
  out_clean <- gsub("[^A-Za-z0-9]+", "_", out)
  saveRDS(df_out, file.path(by_outcome_dir, paste0("df_with_aa_", out_clean, ".rds")))
  write_csv(df_out, file.path(by_outcome_dir, paste0("df_with_aa_", out_clean, ".csv")), na = "NA")
}

stopifnot(exists("df_with_aa"))
cat("=== Finished: crude + AA + CI calculations ===\n")

# End of rates worklow.

# ----------------------------------------------------------------------------*
# PART B: SUPPRESSION WORK FLOW ----
# ----------------------------------------------------------------------------*

# ===========================================================================*
# Step 5 — Load suppression config + functions ------------------------------
# ===========================================================================*
qc_input_exists(suppression_config_script)
qc_input_exists(suppression_functions_script)
source(suppression_config_script)
source(suppression_functions_script)
qc_check("apply_all_suppression() loaded", exists("apply_all_suppression"),
         details = "suppression_functions.R did not define apply_all_suppression().")

# QC CHECK: every demographic group named in the suppression config exists in the
# data (a renamed demo_key would silently skip risk scoring or back-calculation).
config_groups_missing <- setdiff(unique(c(risk_groups, backcalc_groups)), unique(df_with_aa$demo_key))
qc_check("Every risk/backcalc group in suppression_config.R exists as a demo_key",
         length(config_groups_missing) == 0, on_fail = "warn",
         details = paste("Not found:", paste(config_groups_missing, collapse = ", ")))

# ===========================================================================*
# Step 6 — Run the full suppression pipeline --------------------------------
# ===========================================================================*
df_final <- apply_all_suppression(df_with_aa)
cat("Suppression completed. Rows in df_final: ", nrow(df_final), "\n")
qc_check("Row count unchanged by suppression", nrow(df_final) == nrow(df_with_aa),
         details = sprintf("df_with_aa = %s rows, df_final = %s rows", nrow(df_with_aa), nrow(df_final)))
qc_check("Suppression flags evaluated for every row (no NA supp_final)",
         !anyNA(df_final$supp_final), on_fail = "warn",
         details = c("Rows with NA new_cases get NA flags; their outputs are blank.",
                     .qc_capture(df_final %>% filter(is.na(supp_final)) %>%
                                   count(OUTCOME, CONDITION, demo_key, name = "n_rows"))))

# Outcomes present (drop NA).
unique_outcomes <- unique(df_final$OUTCOME)
unique_outcomes <- unique_outcomes[!is.na(unique_outcomes)]

#Round total cases, rates, and CI's. 
df_final <- df_final %>%
  dplyr::mutate(
    total_cases = floor(total_cases),
    total_adjusted_cases = floor(total_adjusted_cases),
    final_cases = floor(final_cases),
    final_crude_rate = round(final_crude_rate, 2),
    final_crude_ci_lower = round(final_crude_ci_lower, 2),
    final_crude_ci_upper = round(final_crude_ci_upper, 2),
    final_aa_rate = round(final_aa_rate, 2),
    final_aa_ci_lower = round(final_aa_ci_lower, 2),
    final_aa_ci_upper = round(final_aa_ci_upper, 2)
  )

# ===========================================================================*
# Step 7 — Write final suppressed files (per outcome + combined) ------------
# ===========================================================================*
# Filenames use a cleaned outcome (no spaces).
for (outcome in unique_outcomes) {
  df_out    <- df_final[df_final$OUTCOME == outcome, ]
  out_clean <- gsub("[^A-Za-z0-9]+", "_", outcome)
  base_name <- paste0("df_with_suppression_", out_clean)
  # SAVES FILES by outcome -> <ddi_dir>/df_with_suppression_<outcome>.(csv|rds)
  write_csv(df_out, file.path(ddi_dir, paste0(base_name, ".csv")), na = "NA")
  saveRDS(df_out, file.path(ddi_dir, paste0(base_name, ".rds")))
}

# SAVES FILES (all outcomes) -> <ddi_dir>/df_with_suppression_all.(csv|rds)
write_csv(df_final, file.path(ddi_dir, "df_with_suppression_all.csv"), na = "NA")
saveRDS(df_final, file.path(ddi_dir, "df_with_suppression_all.rds"))

# ===========================================================================*
# QC CHECK (Suppression QC summaries) ----------------------------------
# ===========================================================================*
# summarizes:
### -total rows in dataset, 
### -rows ddi for <11 cases,
### -rows w rates ddi for 11-19 cases, 
### -rows ddi based on risk-scoring, 
### -rows ddi to prevent back-calculation of small numbers,
### -rows with any ddi.

qc_summary <- df_final %>% summarise(
  full_count_supp = sum(supp_count_full, na.rm = TRUE),
  rate_only_supp  = sum(supp_rates_only, na.rm = TRUE),
  risk_supp       = sum(supp_risk, na.rm = TRUE),
  backcalc_supp   = sum(supp_backcalc, na.rm = TRUE),
  final_supp      = sum(supp_final, na.rm = TRUE)
)
# SAVES A FILE -> <ddi_dir>/qc/suppression_summary.csv
write_csv(qc_summary, file.path(qc_dir, "suppression_summary.csv"), na = "NA")

# Full risk-group QC file + per-outcome QC files.
df_qc_all <- df_final %>% filter(demo_key %in% risk_groups)
# SAVES A FILE -> <ddi_dir>/qc/backcalc_summary_all_riskgroups.csv
write_csv(df_qc_all, file.path(qc_dir, "backcalc_summary_all_riskgroups.csv"), na = "NA")

for (o in unique_outcomes) {
  o_clean <- gsub("[^A-Za-z0-9]+", "_", o)
  
  # SAVES A QC FILE per OUTCOME -> <ddi_dir>/qc/backcalc_summary_<outcome>.csv
  df_qc_all %>% filter(OUTCOME == o) %>%
    write_csv(file.path(qc_dir, paste0("backcalc_summary_", o_clean, ".csv")), na = "NA")
}
cat("All QC files written.\n")


# Backcalc validation: harness + correctness checker

# Harness = per-stratum summary of AGE/RACE backcalc behavior.
source(backcalc_harness_script)
harness <- run_backcalc_harness(df_final)
# SAVES A FILE -> <ddi_dir>/qc/backcalc_harness.csv
write_csv(harness, file.path(qc_dir, "backcalc_harness.csv"), na = "NA")

# Checker = flags strata where backcalc should/should not have fired but didn't/did.
source(backcalc_checker_script)
checker <- check_backcalc_correctness(df_final)
# SAVES FILES -> <ddi_dir>/qc/backcalc_correctness_(age|race).csv
write_csv(checker$age_backcalc_correctness,  file.path(qc_dir, "backcalc_correctness_age.csv"),  na = "NA")
write_csv(checker$race_backcalc_correctness, file.path(qc_dir, "backcalc_correctness_race.csv"), na = "NA")

cat("=== END RATES + SUPPRESSION WORKFLOW ===\n")

# Saving dataset to environment
df_with_supression_all <-readRDS(file.path(ddi_dir, "df_with_suppression_all.rds"))


# ===========================================================================*
# QC CHECK — Suppression verification and QC report ----
# ===========================================================================*
# Writes rates/qc/qc_5_crude_and_aa_rates.txt: a cover page (what to confirm),
# any FAIL/WARN entries, a check summary, and every printout from this script.
# The leak checks below re-test the published columns directly, independent of
# the suppression functions.

# QC CHECK: no counts published below the count threshold.
leak_counts <- df_final %>%
  filter(!is.na(final_cases), new_cases < count_suppress_cases_threshold)
qc_check(paste0("No final_cases published where new_cases < ", count_suppress_cases_threshold),
         nrow(leak_counts) == 0,
         details = leak_counts %>% select(Geography, OUTCOME, CONDITION, demo_key, new_cases, final_cases))

# QC CHECK: no rates published below the rate threshold.
leak_rates <- df_final %>%
  filter(new_cases < rate_suppress_cases_threshold,
         !is.na(final_crude_rate) | !is.na(final_crude_ci_lower) | !is.na(final_crude_ci_upper) |
           !is.na(final_aa_rate) | !is.na(final_aa_ci_lower) | !is.na(final_aa_ci_upper))
qc_check(paste0("No rates or CIs published where new_cases < ", rate_suppress_cases_threshold),
         nrow(leak_rates) == 0,
         details = leak_rates %>% select(Geography, OUTCOME, CONDITION, demo_key, new_cases,
                                         final_crude_rate, final_aa_rate))

# QC CHECK: risk-suppressed and back-calculation-suppressed rows are fully masked.
leak_flags <- df_final %>%
  filter(demo_key %in% risk_groups, (supp_risk %in% TRUE) | (supp_backcalc %in% TRUE),
         !is.na(final_cases) | !is.na(final_crude_rate) | !is.na(final_aa_rate))
qc_check("No values published for risk- or back-calculation-suppressed rows",
         nrow(leak_flags) == 0,
         details = leak_flags %>% select(Geography, OUTCOME, CONDITION, demo_key, new_cases,
                                         supp_risk, supp_backcalc, final_cases))

# QC CHECK: the team's backcalc correctness checker finds no mismatches.
age_mismatch  <- checker$age_backcalc_correctness  %>% filter(backcalc_mismatch %in% TRUE)
race_mismatch <- checker$race_backcalc_correctness %>% filter(backcalc_mismatch %in% TRUE)
qc_check("Age back-calculation: no strata where it fired incorrectly or failed to fire",
         nrow(age_mismatch) == 0, details = age_mismatch)
qc_check("Race back-calculation: no strata where it fired incorrectly or failed to fire",
         nrow(race_mismatch) == 0, details = race_mismatch)

# QC CHECK: confidence intervals. These are the bounds an epidemiologist would
# scan for anything odd, so they are checked here before the final files.
ci_backwards <- df_final %>%
  filter((!is.na(final_crude_rate) & !is.na(final_crude_ci_lower) & !is.na(final_crude_ci_upper) &
            (final_crude_ci_lower > final_crude_rate | final_crude_ci_upper < final_crude_rate)) |
           (!is.na(final_aa_rate) & !is.na(final_aa_ci_lower) & !is.na(final_aa_ci_upper) &
              (final_aa_ci_lower > final_aa_rate | final_aa_ci_upper < final_aa_rate)))
qc_check("Confidence intervals sit around their rate (lower <= rate <= upper)",
         nrow(ci_backwards) == 0,
         details = ci_backwards %>%
           select(Geography, OUTCOME, CONDITION, demo_key, new_cases,
                  final_crude_rate, final_crude_ci_lower, final_crude_ci_upper,
                  final_aa_rate, final_aa_ci_lower, final_aa_ci_upper))

negative_ci <- df_final %>%
  filter((!is.na(final_crude_ci_lower) & final_crude_ci_lower < 0) |
           (!is.na(final_aa_ci_lower) & final_aa_ci_lower < 0))
qc_check("No negative lower confidence limits", nrow(negative_ci) == 0, on_fail = "warn",
         details = c("The normal approximation can push a lower limit below 0 when counts are small.",
                     "Decide whether these should be floored at 0 before publication:",
                     .qc_capture(negative_ci %>%
                                   select(Geography, OUTCOME, CONDITION, demo_key, new_cases,
                                          final_crude_rate, final_crude_ci_lower) %>%
                                   head(25))))

wide_ci <- df_final %>%
  filter(!is.na(final_crude_rate), final_crude_rate > 0,
         (final_crude_ci_upper - final_crude_ci_lower) / final_crude_rate > 4) %>%
  select(Geography, OUTCOME, CONDITION, demo_key, new_cases, new_pop,
         final_crude_rate, final_crude_ci_lower, final_crude_ci_upper) %>%
  arrange(desc((final_crude_ci_upper - final_crude_ci_lower) / final_crude_rate))
qc_check("No unusually wide crude confidence intervals (width more than 4x the rate)",
         nrow(wide_ci) == 0, on_fail = "warn",
         details = head(wide_ci, 25))

qc_log("Confidence interval summary by OUTCOME",
       df_final %>%
         group_by(OUTCOME) %>%
         summarise(published_crude_rates = sum(!is.na(final_crude_rate)),
                   ci_pairs_present = sum(!is.na(final_crude_ci_lower) & !is.na(final_crude_ci_upper)),
                   negative_lower_limits = sum(final_crude_ci_lower < 0, na.rm = TRUE),
                   widest_ci_vs_rate = round(max((final_crude_ci_upper - final_crude_ci_lower) /
                                                   final_crude_rate, na.rm = TRUE), 1),
                   .groups = "drop"))

# PRINTOUT: rates by condition and outcome for the epidemiology review. County
# level, total row, so it can be read against last year's published rates.
qc_log("County 'total' rates by OUTCOME and CONDITION (for the epi review)",
       df_final %>%
         filter(demo_key == "total", tolower(as.character(GeoType)) == "county") %>%
         select(OUTCOME, CONDITION, new_cases, new_pop, final_crude_rate,
                final_crude_ci_lower, final_crude_ci_upper, final_aa_rate) %>%
         arrange(OUTCOME, CONDITION),
       max_rows = 500)

# PRINTOUTS: suppression summaries.
qc_log("Suppression summary (also saved as ddi/qc/suppression_summary.csv)",
       qc_summary %>% pivot_longer(everything(), names_to = "measure", values_to = "rows"))
qc_log("Suppression by OUTCOME",
       df_final %>% group_by(OUTCOME) %>%
         summarise(rows = n(),
                   full_count_supp = sum(supp_count_full, na.rm = TRUE),
                   rate_only_supp  = sum(supp_rates_only, na.rm = TRUE),
                   risk_supp       = sum(supp_risk, na.rm = TRUE),
                   backcalc_supp   = sum(supp_backcalc, na.rm = TRUE),
                   final_supp      = sum(supp_final, na.rm = TRUE),
                   pct_final_supp  = round(100 * mean(supp_final, na.rm = TRUE), 1),
                   .groups = "drop"))
qc_log("Back-calculation harness: strata where back-calculation fired",
       harness %>% summarise(strata = n(),
                             age_backcalc_strata  = sum(age_has_backcalc),
                             race_backcalc_strata = sum(race_has_backcalc)))
is_county  <- tolower(as.character(df_final$GeoType)) == "county"
county_geo <- if (any(is_county, na.rm = TRUE)) df_final$Geography[which(is_county)[1]] else df_final$Geography[1]
qc_log(paste0("County-level check: 'total' row rates by OUTCOME x CONDITION (Geography = ", county_geo, ")"),
       df_final %>%
         filter(demo_key == "total", Geography == county_geo) %>%
         select(Geography, OUTCOME, CONDITION, new_cases, new_pop, final_crude_rate, final_aa_rate) %>%
         arrange(OUTCOME, CONDITION),
       note = "Uses the first geography with GeoType 'County'; if none exists, the first Geography in the file.",
       max_rows = 500)

# ---------------------------------------------------------------------------*
# Data check workbook (to send out to team) ----
# ---------------------------------------------------------------------------*
# One workbook written to rates/"for data check", built from df_with_suppression_all
# (the object df_final). It is not the QC report: it holds the items the team
# asked to review by eye, one tab per item.
#   1. geographies
#   2. completeness: every condition, outcome, and demographic key
#   3. blanks and NAs
#   4. suppression (DDI) results
#   5. sex-specific conditions
#   6. case, rate, and CI formatting
#   7. conditions present

data_check_file <- file.path(data_check_dir,
                             paste0("data_check_rates_",
                                    paste(unique(df_final$Year), collapse = "_"), ".xlsx"))

# --- 1. Geographies -----------------------------------------------------------
geographies_tab <- df_final %>%
  distinct(Geography, GeoType, GeoName, GeoID, Region, SES, GeoName2) %>%
  arrange(GeoType, Geography)

geography_counts <- geographies_tab %>% 
  count(GeoType, name = "n_geographies")

qc_log("Geographies by GeoType", geography_counts)
qc_check(paste0("The expected ", expected_geography_count, " geographies are present"),
         n_distinct(df_final$Geography) == expected_geography_count, on_fail = "warn",
         details = c(sprintf("Geographies found: %s (expected %s). Change expected_geography_count in Step 0 if the geography list changed this year.",
                             n_distinct(df_final$Geography), expected_geography_count),
                     .qc_capture(geography_counts)))

# --- 2. Completeness ----------------------------------------------------------
# Sex-specific conditions have fewer rows on purpose: step 4 removes the
# opposite-sex demographic keys.
sex_specific_conditions <- unique(df_final$CONDITION[df_final$required_sex != "both"])
n_geo  <- n_distinct(df_final$Geography)
n_demo <- n_distinct(df_final$demo_key)

completeness_tab <- df_final %>%
  group_by(OUTCOME, CONDITION) %>%
  summarise(geographies = n_distinct(Geography), demo_keys = n_distinct(demo_key),
            rows = n(), .groups = "drop") %>%
  mutate(sex_specific  = CONDITION %in% sex_specific_conditions,
         expected_rows = ifelse(sex_specific, NA_real_, n_geo * n_demo),
         complete      = ifelse(sex_specific, NA, rows == expected_rows),
         note          = ifelse(sex_specific,
                                "sex-specific: opposite-sex demographic keys removed by design", "")) %>%
  arrange(OUTCOME, CONDITION)

geography_coverage <- df_final %>%
  group_by(OUTCOME, CONDITION, demo_key) %>%
  summarise(geographies = n_distinct(Geography), .groups = "drop") %>%
  filter(geographies != n_geo)

qc_check("Every condition and outcome has every geography and demographic key",
         all(completeness_tab$complete[!completeness_tab$sex_specific]), on_fail = "warn",
         details = completeness_tab %>% filter(!sex_specific, !complete))
qc_check("Every condition, outcome, and demographic key has a row for every geography",
         nrow(geography_coverage) == 0,
         details = c("These combinations are missing one or more geographies:",
                     .qc_capture(geography_coverage)))

# --- 3. Blanks and NAs --------------------------------------------------------
# Three kinds of column:
#   identity   never blank (a blank means a row lost its labels)
#   geography  blank is normal for some geography types (a municipality has no Region)
#   value      blank only where the value was suppressed
identity_cols  <- c("Geography", "Year", "OUTCOME", "CONDITION", "demo_key", "GeoType")
geography_cols <- c("GeoName", "GeoID", "Region", "SES", "GeoName2")
value_cols     <- c("final_cases", "final_crude_rate", "final_crude_ci_lower",
                    "final_crude_ci_upper", "final_aa_rate", "final_aa_ci_lower",
                    "final_aa_ci_upper")

blanks_tab <- bind_rows(
  tibble(column = identity_cols,  column_type = "identity (must never be blank)"),
  tibble(column = geography_cols, column_type = "geography attribute (blank is normal for some GeoTypes)"),
  tibble(column = value_cols,     column_type = "value (blank where suppressed)")
) %>%
  mutate(blank_rows = vapply(column, function(cc) {
           v <- df_final[[cc]]
           sum(is.na(v) | (is.character(v) & trimws(as.character(v)) == ""))
         }, numeric(1)),
         total_rows    = nrow(df_final),
         percent_blank = round(100 * blank_rows / nrow(df_final), 1))

identity_blanks <- blanks_tab %>%
  filter(column_type == "identity (must never be blank)", blank_rows > 0)
qc_check("No blanks in the identity columns", nrow(identity_blanks) == 0,
         details = identity_blanks)

geography_blanks_by_type <- df_final %>%
  group_by(GeoType) %>%
  summarise(across(all_of(geography_cols),
                   ~ sum(is.na(.x) | trimws(as.character(.x)) == "")),
            rows = n(), .groups = "drop")
qc_log("Blanks by column", blanks_tab,
       note = "Blanks in the geography attribute rows are expected: a municipality has no Region, and not every geography carries an SES value.")

# --- 4. Suppression (DDI) -----------------------------------------------------
suppression_tab <- df_final %>%
  group_by(OUTCOME) %>%
  summarise(rows = n(),
            cases_published = sum(!is.na(final_cases)),
            cases_suppressed = sum(is.na(final_cases)),
            percent_cases_suppressed = round(100 * mean(is.na(final_cases)), 1),
            crude_rates_published = sum(!is.na(final_crude_rate)),
            crude_rates_suppressed = sum(is.na(final_crude_rate)),
            aa_rates_published = sum(!is.na(final_aa_rate)),
            .groups = "drop")

backcalc_tab <- bind_rows(
  tibble(check = "age back-calculation",
         strata = nrow(checker$age_backcalc_correctness),
         fired = sum(checker$age_backcalc_correctness$age_has_backcalc %in% TRUE),
         mismatches = sum(checker$age_backcalc_correctness$backcalc_mismatch %in% TRUE)),
  tibble(check = "race back-calculation",
         strata = nrow(checker$race_backcalc_correctness),
         fired = sum(checker$race_backcalc_correctness$race_has_backcalc %in% TRUE),
         mismatches = sum(checker$race_backcalc_correctness$backcalc_mismatch %in% TRUE))
)

# --- 5. Sex-specific conditions ------------------------------------------------
sex_specific_tab <- df_final %>%
  filter(required_sex != "both") %>%
  group_by(CONDITION, required_sex, demo_key) %>%
  summarise(rows = n(),
            donor_population_used = sum(!is.na(new_pop) & !is.na(pop_counts) & new_pop != pop_counts),
            population_missing = sum(is.na(new_pop)),
            cases_missing = sum(is.na(new_cases)),
            .groups = "drop") %>%
  arrange(CONDITION, demo_key)

qc_log("Sex-specific conditions: rows using a donor population", sex_specific_tab, max_rows = 300)

# --- 6. Case, rate, and CI formatting ------------------------------------------
cases_not_whole <- df_final %>%
  filter(!is.na(final_cases), final_cases != floor(final_cases))
negative_values <- df_final %>%
  filter(if_any(all_of(value_cols), ~ !is.na(.x) & .x < 0))
rate_without_ci <- df_final %>%
  filter(!is.na(final_crude_rate) &
           (is.na(final_crude_ci_lower) | is.na(final_crude_ci_upper)))

formatting_tab <- tibble(
  check = c("Published cases are whole numbers",
            "Confidence intervals sit around their rate",
            "No negative cases, rates, or CI bounds",
            "Every published crude rate has both CI bounds",
            "Unusually wide crude CIs (width more than 4x the rate)"),
  rows_failing = c(nrow(cases_not_whole), nrow(ci_backwards), nrow(negative_values),
                   nrow(rate_without_ci), nrow(wide_ci))
) %>%
  mutate(result = ifelse(rows_failing == 0, "OK", "REVIEW"))

qc_check("Published cases are whole numbers", nrow(cases_not_whole) == 0,
         details = head(cases_not_whole %>%
                          select(Geography, OUTCOME, CONDITION, demo_key, new_cases, final_cases), 25))
qc_check("No negative cases, rates, or CI bounds", nrow(negative_values) == 0, on_fail = "warn",
         details = head(negative_values %>%
                          select(Geography, OUTCOME, CONDITION, demo_key, all_of(value_cols)), 25))
qc_check("Every published crude rate has both CI bounds", nrow(rate_without_ci) == 0,
         details = head(rate_without_ci %>%
                          select(Geography, OUTCOME, CONDITION, demo_key, new_cases,
                                 final_crude_rate, final_crude_ci_lower, final_crude_ci_upper), 25))

# --- 7. Conditions present ------------------------------------------------------
conditions_tab <- df_final %>%
  group_by(OUTCOME, CONDITION) %>%
  summarise(rows = n(),
            total_cases = sum(new_cases, na.rm = TRUE),
            geographies_with_cases = n_distinct(Geography[new_cases > 0]),
            .groups = "drop") %>%
  arrange(OUTCOME, CONDITION)

qc_check("Every condition has at least one case", all(conditions_tab$total_cases > 0),
         on_fail = "warn",
         details = conditions_tab %>% filter(total_cases == 0))

# --- Write the workbook ----  SAVES A FILE ->
data_check_intro <- c(
  paste0("DATA CHECK: rates and suppression, data year ",
         paste(unique(df_final$Year), collapse = ", ")),
  paste("Written:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " Prepared by:", Sys.info()[["user"]]),
  "",
  "Built from df_with_suppression_all: every geography, condition, outcome, and",
  "demographic key, after rates and suppression. One tab per item.",
  paste0("Cases under ", count_suppress_cases_threshold, " and rates under ",
         rate_suppress_cases_threshold, " cases are suppressed, so blank value cells are expected."),
  "",
  "WHAT TO CHECK:",
  "[ ] 1 Geographies: the geography list and GeoTypes are correct and complete.",
  "[ ] 2 Completeness: complete = TRUE on every row, and tab 2b is empty. Sex-specific",
  "      conditions are marked and have fewer rows by design.",
  "[ ] 3 Blanks: identity columns show 0 blanks. Geography attribute blanks (Region, SES)",
  "      are normal for some GeoTypes; check the pattern in tab 3b. Value blanks are",
  "      suppression only.",
  "[ ] 4 Suppression: percent suppressed is in line with last year, leaks are 0, and the",
  "      back-calculation mismatches are 0.",
  "[ ] 5 Sex-specific: prostate, maternal, and other sex-specific conditions use a donor",
  "      population, and nothing unexpected is missing a population or cases.",
  "[ ] 6 Formatting: every check reads OK.",
  "[ ] 7 Conditions present: every condition expected this year is listed with a plausible",
  "      case count.",
  "",
  "Reviewed by: ______________________   Date: ______________",
  "Notes:"
)

data_check_sheets <- list(
  "Cover page"            = data.frame(instructions = data_check_intro),
  "1 Geographies"         = geographies_tab,
  "1b Geography counts"   = geography_counts,
  "2 Completeness"        = completeness_tab,
  "2b Geography coverage" = if (nrow(geography_coverage) == 0)
                              data.frame(note = paste0("Every condition, outcome, and demographic key has all ",
                                                       n_geo, " geographies."))
                            else geography_coverage,
  "3 Blanks and NAs"      = blanks_tab,
  "3b Blanks by GeoType"  = geography_blanks_by_type,
  "4 Suppression"         = suppression_tab,
  "4b Back-calculation"   = backcalc_tab,
  "4c Suppression leaks"  = if (nrow(leak_counts) + nrow(leak_rates) == 0)
                              data.frame(note = "None. Nothing was published below the thresholds.")
                            else bind_rows(leak_counts, leak_rates) %>%
                              select(Geography, OUTCOME, CONDITION, demo_key, new_cases,
                                     final_cases, final_crude_rate),
  "5 Sex-specific"        = sex_specific_tab,
  "6 Formatting"          = formatting_tab,
  "6b Wide CIs"           = if (nrow(wide_ci) == 0) data.frame(note = "None flagged.")
                            else head(wide_ci, 500),
  "7 Conditions present"  = conditions_tab
)

write.xlsx(data_check_sheets, file = data_check_file, overwrite = TRUE,
           headerStyle = createStyle(textDecoration = "bold"),
           firstRow = TRUE, colWidths = "auto")
message("Data check workbook written: ", data_check_file)


# QC CHECK: outputs written this run.
out_names <- gsub("[^A-Za-z0-9]+", "_", unique_outcomes)
qc_files_written(c(
  file.path(output_dir, c("df_with_aa.rds", "df_with_aa.csv", "aa_rate_summary.rds", "aa_rate_summary.csv")),
  file.path(ddi_dir, paste0("df_with_suppression_", rep(out_names, each = 2), c(".rds", ".csv"))),
  file.path(ddi_dir, c("df_with_suppression_all.rds", "df_with_suppression_all.csv")),
  file.path(qc_dir, c("suppression_summary.csv", "backcalc_summary_all_riskgroups.csv",
                      "backcalc_harness.csv", "backcalc_correctness_age.csv",
                      "backcalc_correctness_race.csv")),
  data_check_file
))

qc_finish(
  confirm = c(
    "RUN STATUS is PASSED or PASSED WITH WARNINGS, and every WARN has a reviewer note.",
    "Data year in the header is the year you intended to process.",
    "Rows by demo_group: NA rows are only demo_keys that are not age-adjustment summary groups (e.g. 10-year age groups).",
    "Population above 0 check: passed, or every listed row is explained (these rows have no rate).",
    "AA groups by number of age bands: most groups have all 10 bands; groups with fewer have NA AA rates by design.",
    "Every risk/backcalc group in suppression_config.R exists in the data.",
    "All four leak checks passed (counts < 11, rates < 20, risk-suppressed, back-calc-suppressed).",
    "Age and race back-calculation checker: no mismatches.",
    "Suppression by OUTCOME: percent suppressed is in line with last year for each outcome.",
    "Confidence intervals: the lower <= rate <= upper check passed; any negative lower limit or unusually wide interval has been reviewed and a decision noted.",
    "Epi review: the county total rates by outcome and condition have been read by an epidemiologist against last year's published rates, and anything that moved has an explanation.",
    "County-level 'total' rows: crude and AA rates are plausible for a few familiar conditions and match last year's direction and scale.",
    "Geographies: the expected geography count matched, and every condition, outcome, and demographic key has a row for every geography.",
    "Completeness, blanks, formatting, and conditions-present checks passed, or each WARN is explained.",
    "Data check workbook: read every tab before sending it to the team."
  ),
  review_files = c(
    "ddi/df_with_suppression_all.csv: filter a small geography and confirm cells under 11 are blank and rates under 20 cases are blank.",
    "ddi/qc/backcalc_summary_all_riskgroups.csv: spot-check a stratum where one age or race group was suppressed; the next-smallest age group (or other_total) is also suppressed.",
    "ddi/qc/backcalc_correctness_age.csv and backcalc_correctness_race.csv: backcalc_mismatch is FALSE everywhere.",
    "data/output/aa_rate_summary.csv: spot-check one condition's AA rate against last year's published value.",
    "for data check/data_check_rates_<year>.xlsx: this is the workbook the team receives; read it through first."
  )
)
