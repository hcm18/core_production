# ===========================================================================*
# 4_sex_specific_processing.R: Script Description ---------------------------
# ===========================================================================*
# PURPOSE
#   For sex-specific conditions (e.g. prostate cancer = male only), the correct
#   denominator is the sex-specific population, not the combined one. This step
#   swaps in "donor" case and population values for those conditions so the rate
#   later uses, e.g., the male population for a male-only condition.
#
# INPUTS (read from disk)
#   - project_root/rates/project_root/rates/data/output/cleaned cases/final_adjusted/final_adjusted_cases_with_pop.rds   (from step 3)
#   - project_root/update_lists/demo_key_female_condition.csv                    donor lookups
#   - project_root/update_lists/demo_key_male_condition.csv
#
# OUTPUTS (written to disk)  -- see "SAVES A FILE" markers
#   -  project_root/rates/data/output/cleaned cases/final_adjusted/final_adjusted_ss_4rates.rds/.csv   input for step 5
#   -  project_root/rates/project_root/rates/qc/qc_sex_specific.csv and several project_root/rates/qc/*.csv summaries
#   -  project_root/rates/project_root/rates/qc/by_outcome/qc_<outcome>.csv
#   -  project_root/rates/qc/qc_sex_specific.csv and the other qc/*.csv summaries (always)
#
# QC: when sourced by 1_prep_cases_add_pops.R (the normal path) this script does
#   NOT open its own QC report; its checks and printouts go to that orchestrator's
#   single report, project_root/rates/qc/qc_1_prep_cases_add_pops.txt. Run on its
#   own, it opens and writes project_root/rates/qc/qc_1c_sex_specific_processing.txt.
#
# PATHS: Lives in project_root/rates/scripts/sourced scripts. Normally sourced by
#   1_prep_cases_add_pops.R (inherits Step 0 config). A minimal Step 0 below lets
#   it run on its own.
# --------------------------------------------------------------------------*

# ---- Step 0 — Configuration SET IN 1_prep cases add pops.R-----
#final_adjusted_dir <- file.path(clean_dir, "final_adjusted")
#qc_dir             <- file.path(project_root, "rates", "qc")
#update_lists_dir   <- file.path(project_root, "update_lists")
#qc_functions_script <- file.path(project_root, "shared_scripts", "qc_functions.R")

# ---- Load Packages ---------------------------------------------------------
#pacman::p_load(tidyverse, fs)
library(dplyr)
library(tidyr)
library(readr)
library(fs)
library(stringr)

# ---- Start QC log ------------------------------------------------------------
# When sourced by 1_prep_cases_add_pops.R a QC context is already open, so this
# script adds its checks and printouts to that single report. Only when run on
# its own (no open context) does it open and later finish its own report.
if (!exists("qc_start")) source(qc_functions_script)
qc_own_context_1c <- length(.qc_env$stack) == 0
if (qc_own_context_1c)
  qc_start("1c_sex_specific_processing.R", qc_dir, reset = TRUE)

# ===========================================================================*
# Step 1 — Load the cases+pop dataset ---------------------------------------
# ===========================================================================*
qc_input_exists(file.path(final_adjusted_dir, "final_adjusted_cases_with_pop.rds"))
df <- readRDS(file.path(final_adjusted_dir, "final_adjusted_cases_with_pop.rds"))
n_rows_input <- nrow(df)
qc_required_columns(df, c("Geography", "OUTCOME", "CONDITION", "demo_key",
                          "total_adjusted_cases", "pop_counts"),
                    "final_adjusted_cases_with_pop.rds")

# ===========================================================================*
# Step 2 — Sex-specific condition lists -------------------------------------
# ===========================================================================*
male_specific_conditions <- c(
  "Prostate Cancer",
  "Prostate Cancer (any mention)",
  "Prostate Cancer (multiple cause of death)"
)
female_specific_conditions <- c(
  "Female Breast Cancer",
  "Female Breast Cancer (any mention)",
  "Female Breast Cancer (multiple cause of death)",
  "Female Reproductive Cancer",
  "Female Reproductive Cancer (any mention)",
  "Female Reproductive Cancer (multiple cause of death)",
  "Maternal Complications",
  "Maternal Complications (multiple cause of death)"
)
# Normalize for robust matching (case / whitespace).
male_specific_clean   <- male_specific_conditions   %>% 
  str_squish() %>% 
  str_to_lower()
female_specific_clean <- female_specific_conditions %>% 
  str_squish() %>% 
  str_to_lower()

# ===========================================================================*
# Step 3 — Load donor lookup files ------------------------------------------
# ===========================================================================*
# Map a condition's demo_key to the donor demo_key it should draw from.
qc_input_exists(file.path(update_lists_dir, "demo_key_female_condition.csv"))
qc_input_exists(file.path(update_lists_dir, "demo_key_male_condition.csv"))
donor_female <- read_csv(file.path(update_lists_dir, "demo_key_female_condition.csv"),
                         show_col_types = FALSE)
donor_male   <- read_csv(file.path(update_lists_dir, "demo_key_male_condition.csv"),
                         show_col_types = FALSE)

# QC CHECK: donor files have the expected columns and one row per demo_key
# (a repeated demo_key would duplicate data rows in the join below).
qc_required_columns(donor_female, c("demo_key", "donor_female"), "demo_key_female_condition.csv")
qc_required_columns(donor_male,   c("demo_key", "donor_male"),   "demo_key_male_condition.csv")
qc_check("demo_key_female_condition.csv has one row per demo_key",
         !any(duplicated(donor_female$demo_key)),
         details = donor_female %>% filter(demo_key %in% demo_key[duplicated(demo_key)]))
qc_check("demo_key_male_condition.csv has one row per demo_key",
         !any(duplicated(donor_male$demo_key)),
         details = donor_male %>% filter(demo_key %in% demo_key[duplicated(demo_key)]))

donors <- full_join(donor_female, donor_male, by = "demo_key")

# QC CHECK: every donor key points at a demo_key that exists in the data.
donor_targets <- unique(na.omit(c(donors$donor_female, donors$donor_male)))
qc_check("Every donor key in the donor files exists as a demo_key in the data",
         all(donor_targets %in% df$demo_key), on_fail = "warn",
         details = paste("Not found:", paste(setdiff(donor_targets, df$demo_key), collapse = ", ")))

# ===========================================================================*
# Step 4 — Tag each row with the sex its condition requires -----------------
# ===========================================================================*
#clean the condition column
df <- df %>% 
  mutate(CONDITION_clean = CONDITION %>% 
           str_squish() %>% 
           str_to_lower())

#tag each row required by sex
df <- df %>%
  mutate(required_sex = case_when(
    CONDITION_clean %in% male_specific_clean   ~ "male",
    CONDITION_clean %in% female_specific_clean ~ "female",
    TRUE                                       ~ "both"
  ))
# ===========================================================================*
# ***QC CHECK**** (which sex-specific conditions were found in the data?)----
# ===========================================================================*

message("\n=== SEX-SPECIFIC CONDITIONS FOUND IN DATA ===")
message("Male-specific conditions found:")
print(df %>% filter(required_sex == "male")   %>% 
        distinct(CONDITION) %>% 
        pull())
message("\nFemale-specific conditions found:")
print(df %>% filter(required_sex == "female") %>% 
        distinct(CONDITION) %>% 
        pull())
message("=============================================\n")

qc_log("Sex-specific conditions tagged in the data",
       df %>% filter(required_sex != "both") %>% distinct(required_sex, CONDITION) %>%
         arrange(required_sex, CONDITION))

# QC CHECK: every listed condition name is found (a label change would silently
# turn a sex-specific condition into "both").
listed_not_found <- c(male_specific_conditions, female_specific_conditions)[
  !c(male_specific_clean, female_specific_clean) %in% unique(df$CONDITION_clean)]
qc_check("Every condition in the sex-specific lists is found in the data",
         length(listed_not_found) == 0, on_fail = "warn",
         details = c(paste("Not found:", paste(listed_not_found, collapse = "; ")),
                     "Expected only if that condition is not produced this year. Otherwise the label changed and the list needs updating."))

# QC CHECK: conditions whose names suggest a sex-specific condition but are not
# tagged (possible new or renamed condition missing from the lists).
sex_hint <- "prostate|testic|breast|cervi|uter|ovar|matern|pregnan|reproductive|obstet|gynec"
untagged_hint <- df %>%
  filter(required_sex == "both", str_detect(CONDITION_clean, sex_hint)) %>%
  distinct(CONDITION)
qc_check("No untagged conditions that look sex-specific", nrow(untagged_hint) == 0,
         on_fail = "warn",
         details = c("These names match sex-specific keywords but are treated as 'both'.",
                     "Add them to a list in Step 2 if they should use a sex-specific denominator:",
                     .qc_capture(untagged_hint)))

# ===========================================================================*
# Step 5 — Attach the donor key for each sex-specific row -------------------
# ===========================================================================*
df <- df %>%
  left_join(donors, by = "demo_key") %>%
  mutate(
    donor_female = if_else(required_sex == "both", NA_character_, donor_female),
    donor_male   = if_else(required_sex == "both", NA_character_, donor_male),
    donor_key = case_when(
      required_sex == "female" ~ donor_female,
      required_sex == "male"   ~ donor_male,
      TRUE                     ~ NA_character_
    )
  )

# ===========================================================================*
# Step 6 — Self-join to pull the donor group's cases + population -----------
# ===========================================================================*
df <- df %>%
  left_join(
    df %>% select(Geography, CONDITION_clean, OUTCOME, demo_key,
                  donor_cases = total_adjusted_cases,
                  donor_pop   = pop_counts),
    by = c("Geography", "CONDITION_clean", "OUTCOME", "donor_key" = "demo_key")
  )

# QC CHECK: the donor join and self-join did not add or drop rows.
qc_check("Row count unchanged by the donor joins", nrow(df) == n_rows_input,
         details = sprintf("input = %s rows, after joins = %s rows. Check for repeated keys (e.g. more than one Year or duplicate demo_key rows).",
                           n_rows_input, nrow(df)))

# ===========================================================================*
# Step 7 — Compute the new cases and population the rates step will use -----
# ===========================================================================*
df <- df %>%
  mutate(
    new_cases = case_when(
      required_sex == "both" ~ total_adjusted_cases,
      !is.na(donor_cases)    ~ donor_cases,
      TRUE                   ~ NA_real_
    ),
    new_pop = case_when(
      required_sex == "both" ~ pop_counts,
      !is.na(donor_pop)      ~ donor_pop,
      TRUE                   ~ NA_real_
    )
  )

# QC CHECK: sex-neutral conditions pass through unchanged.
same_value <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < 1e-9)
both_changed <- df %>%
  filter(required_sex == "both",
         !(same_value(new_cases, total_adjusted_cases) & same_value(new_pop, pop_counts)))
qc_check("Non-sex-specific rows: new_cases and new_pop equal the original values",
         nrow(both_changed) == 0, details = both_changed)
# 7A. Drop impossible sex-specific rows ------------------------------------*

# The complete grid built in step 3 intentionally created a row for EVERY
# demographic x condition, including impossible ones (e.g. a "female_*" row for a
# male-only condition like prostate cancer). Remove those opposite-sex rows here,
# BEFORE the rates step, so no rate is ever computed for e.g. female prostate.
#
#   - male-specific   condition -> drop demo_keys starting with "female_"
#   - female-specific condition -> drop demo_keys starting with "male_"
#
# Sex-neutral demo_keys (total, hisp_total, age0_4, ...) and same-sex demo_keys
# are KEPT: for a sex-specific condition, the sex-neutral keys already carry the
# donor-substituted (correct-sex) denominator from the steps above.
# NOTE: this relies on required_sex being tagged correctly, i.e. the condition
# lists in section 0 covering every sex-specific condition name in the data.

drop_sex_rows <- df %>%
  filter(
    (required_sex == "male"   & str_starts(demo_key, "female_")) |
      (required_sex == "female" & str_starts(demo_key, "male_"))
  )

# ***QC CHECK*** (confirm correct rows are being dropped)----
qc_log("Impossible sex-specific rows dropped (opposite-sex demo_keys)",
       if (nrow(drop_sex_rows) == 0) "None." else
         drop_sex_rows %>% count(CONDITION, OUTCOME, required_sex, name = "rows_dropped"),
       note = paste("Total rows to drop:", nrow(drop_sex_rows)))

# END QC CHECK

df <- df %>%
  filter(
    !((required_sex == "male"   & str_starts(demo_key, "female_")) |
        (required_sex == "female" & str_starts(demo_key, "male_")))
  )

# ===========================================================================*
# Step 8 — Save the file the rates step reads -------------------------------
# ===========================================================================*
dir_create(final_adjusted_dir)
if (!is.data.frame(df)) stop("'df' is not a data frame. Re-run earlier steps before saving.")

# SAVES FILES -> project_root/rates/data/output/cleaned cases/final_adjusted/final_adjusted_ss_4rates.(rds|csv)
saveRDS(df, file.path(final_adjusted_dir, "final_adjusted_ss_4rates.rds"))
write_csv(df, file.path(final_adjusted_dir, "final_adjusted_ss_4rates.csv"))
message("Saved final_adjusted_ss_4rates to: ", final_adjusted_dir)

# ===========================================================================*
# QC CHECK — QC flags and QC files ----
# ===========================================================================*
# Writes the detailed sex-specific QC CSVs below. The checks and printouts go to
# the orchestrator's single QC report (or, when this script is run on its own, to
# its own report, finished at the very bottom).

df <- df %>%
  mutate(
    donor_key_missing   = required_sex != "both" & is.na(donor_key),
    donor_missing_in_df = required_sex != "both" & !is.na(donor_key) & is.na(donor_cases)
  )

dir_create(qc_dir)

# SAVES A FILE -> project_root/rates/qc/qc_sex_specific.csv  (full tagged dataset)
write_csv(df, file.path(qc_dir, "qc_sex_specific.csv"))

# SAVES A FILE -> project_root/rates/qc/qc_summary_match_types.csv  (counts by match type)
qc_summary <- df %>%
  summarise(
    total_rows            = n(),
    sex_specific_rows     = sum(required_sex != "both"),
    non_sex_specific_rows = sum(required_sex == "both"),
    donor_used            = sum(!is.na(donor_key) & required_sex != "both"),
    copied                = sum(required_sex == "both"),
    donor_key_missing     = sum(donor_key_missing),
    donor_row_missing     = sum(donor_missing_in_df)
  )
write_csv(qc_summary, file.path(qc_dir, "qc_summary_match_types.csv"))

# SAVES FILES -> project_root/rates/qc/*.csv  (row-level QC slices)
df %>% filter(required_sex != "both", !is.na(donor_key), !is.na(donor_cases)) %>%
  write_csv(file.path(qc_dir, "qc_detailed_matches.csv"))
df %>% filter(required_sex == "both") %>%
  write_csv(file.path(qc_dir, "qc_copied_rows.csv"))
df %>% filter(donor_key_missing) %>%
  write_csv(file.path(qc_dir, "qc_missing_donor_keys.csv"))
df %>% filter(donor_missing_in_df) %>%
  write_csv(file.path(qc_dir, "qc_missing_donor_rows.csv"))

# QC CHECK: sex-specific rows with no usable donor. These rows get
# new_cases/new_pop = NA and will come out blank in step 5 (known open item:
# same-sex keys such as male_total for prostate with no donor-lookup entry).
qc_log("Sex-specific match summary (also saved as project_root/rates/qc/qc_summary_match_types.csv)",
       qc_summary %>% pivot_longer(everything(), names_to = "measure", values_to = "rows"))
qc_check("Every sex-specific row has a donor key in the donor files",
         sum(df$donor_key_missing) == 0, on_fail = "warn",
         details = c("Rows will have NA new_cases/new_pop (blank rates). See project_root/rates/qc/qc_missing_donor_keys.csv.",
                     .qc_capture(df %>% filter(donor_key_missing) %>%
                                   count(required_sex, demo_key, name = "n_rows"))))
qc_check("Every donor key finds its donor row in the data",
         sum(df$donor_missing_in_df) == 0, on_fail = "warn",
         details = c("See project_root/rates/qc/qc_missing_donor_rows.csv.",
                     .qc_capture(df %>% filter(donor_missing_in_df) %>%
                                   count(required_sex, demo_key, donor_key, name = "n_rows"))))
qc_log("Rows with NA new_cases by CONDITION (these produce blank rates in step 5)",
       df %>% filter(is.na(new_cases)) %>% count(OUTCOME, CONDITION, required_sex, name = "n_rows_na"))

# QC files split by OUTCOME ---------------------------------------*

qc_outcome_dir <- file.path(qc_dir, "by_outcome")
dir_create(qc_outcome_dir)
for (oc in df %>% distinct(OUTCOME) %>% pull()) {
  safe_name <- oc %>% str_replace_all("[^A-Za-z0-9]+", "_") %>% str_replace("^_|_$", "")
  # SAVES A FILE -> project_root/rates/project_root/rates/qc/by_outcome/qc_<outcome>.csv
  df %>% filter(OUTCOME == oc) %>%
    write_csv(file.path(qc_outcome_dir, paste0("qc_", safe_name, ".csv")))
}
message("Outcome-specific QC files written to: ", qc_outcome_dir)


# QC CHECK: no opposite-sex rows remain and outputs were written this run.
qc_check("No opposite-sex rows remain for sex-specific conditions",
         !any((df$required_sex == "male"   & str_starts(df$demo_key, "female_")) |
                (df$required_sex == "female" & str_starts(df$demo_key, "male_"))),
         details = "Opposite-sex rows are still present after Step 7A.")
qc_log("Rows by OUTCOME before and after sex-specific processing", tibble(
  measure = c("input rows (final_adjusted_cases_with_pop)", "rows dropped (opposite sex)", "output rows"),
  rows    = c(n_rows_input, nrow(drop_sex_rows), nrow(df))
))
qc_files_written(c(
  file.path(final_adjusted_dir, c("final_adjusted_ss_4rates.rds", "final_adjusted_ss_4rates.csv")),
  file.path(qc_dir, c("qc_sex_specific.csv", "qc_summary_match_types.csv",
                      "qc_detailed_matches.csv", "qc_copied_rows.csv",
                      "qc_missing_donor_keys.csv", "qc_missing_donor_rows.csv"))
))

# Only write a standalone report when this script opened its own context.
# When sourced, these items are rolled into the orchestrator's qc_finish().
if (qc_own_context_1c) {
  qc_finish(
    confirm = c(
      "RUN STATUS is PASSED or PASSED WITH WARNINGS, and every WARN has a reviewer note.",
      "Sex-specific conditions tagged: every prostate, female breast, female reproductive, and maternal condition in the data is listed under the right sex.",
      "No WARN for untagged conditions that look sex-specific (or each listed condition is confirmed as 'both').",
      "Impossible rows dropped: only opposite-sex demo_keys for sex-specific conditions; counts look reasonable.",
      "Sex-specific match summary: donor_key_missing and donor_row_missing are 0, or the rows are the known same-sex key open item.",
      "Rows with NA new_cases: only sex-specific conditions appear, and each is explained by the donor WARNs above."
    ),
    review_files = c(
      "project_root/rates/qc/qc_missing_donor_keys.csv: confirm which demo_keys lack a donor entry (e.g. male_total for prostate) and whether those breakdowns should be published.",
      "project_root/rates/qc/qc_detailed_matches.csv: spot-check a few rows; donor_pop is the male (or female) population for the matching group.",
      "project_root/rates/data/output/cleaned cases/final_adjusted/final_adjusted_ss_4rates.csv: for one prostate row with demo_key 'total', new_pop equals the male_total population for that Geography."
    )
  )
}
