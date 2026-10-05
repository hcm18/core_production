# ===========================================================================*
# Suppression_functions.R: Script Description -------------------------------
# ===========================================================================*
# PURPOSE
#   Defines the suppression functions used by 5_crude_and_aa_rates.R. Sourcing
#   this file only DEFINES functions; apply_all_suppression() is called by step 5.
# --------------------------------------------------------------------------*

library(dplyr)

`%||%` <- function(x, y) if (!is.null(x)) x else y
debug_print <- function(msg) cat("\n--- DEBUG:", msg, "\n")

# ============================================*
# Event Score Function ----
# ============================================*
calc_event_score <- function(df, event_table) {
  df <- df %>%
    mutate(new_cases = suppressWarnings(as.numeric(new_cases))) %>%
    rowwise() %>%
    mutate(event_score =
             if (is.na(new_cases) || new_cases == 0) 0
           else if (new_cases >= 1 && new_cases <= 10) 7
           else {
             matches <- event_table[
               new_cases >= event_table$min_cases &
                 new_cases <= event_table$max_cases,
               "event_score"
             ]
             if (length(matches) == 1) matches else 0
           }
    ) %>%
    ungroup()
  df
}

# ============================================*
# Demographic Scores Function ----
# ============================================*
add_demo_scores <- function(df, component_table) {
  df <- df %>%
    left_join(component_table, by = "demo_key") %>%
    mutate(
      sex_score         = ifelse(demo_key %in% risk_groups, sex_score, NA),
      age_score         = ifelse(demo_key %in% risk_groups, age_score, NA),
      re_score          = ifelse(demo_key %in% risk_groups, re_score, NA),
      interaction_score = ifelse(demo_key %in% risk_groups, interaction_score, NA)
    )
  df
}

# ============================================*
# Total Risk Score Function ----
# ============================================*
calc_total_risk_score <- function(df) {
  df <- df %>%
    mutate(risk_score_total =
             ifelse(demo_key %in% risk_groups,
                    event_score + sex_score + age_score +
                      re_score + interaction_score,
                    NA))
  df
}

# ============================================*
# Count Suppression Function ----
# ============================================*
apply_count_suppression <- function(df) {
  df <- df %>%
    mutate(
      supp_count_full = new_cases < count_suppress_cases_threshold,
      supp_rates_only = new_cases < rate_suppress_cases_threshold &
        new_cases >= count_suppress_cases_threshold
    )
  df
}

# ============================================*
# Risk Suppression Function ----
# ============================================*
apply_risk_suppression <- function(df, risk_groups, threshold) {
  df <- df %>%
    mutate(
      supp_risk =
        ifelse(demo_key %in% risk_groups,
               risk_score_total >= threshold,
               FALSE)
    )
  df
}

# ============================================*
# Race Backcalc (grouped) Function ----
# ============================================*
apply_secondary_race <- function(df) {
  
  df %>%
    group_by(Geography, OUTCOME, CONDITION) %>%
    mutate(
      primary_suppressed_race =
        (demo_key %in% backcalc_race_groups) &
        (supp_count_full | supp_risk),
      
      n_primary_race =
        sum(primary_suppressed_race, na.rm = TRUE),
      
      suppressed_nonzero_race =
        any(primary_suppressed_race & new_cases > 0, na.rm = TRUE),
      
      any_other_zero_race =
        any((demo_key %in% backcalc_race_groups) &
              new_cases == 0 &
              !primary_suppressed_race, na.rm = TRUE),
      
      race_trigger =
        (n_primary_race == 1 &
           suppressed_nonzero_race &
           !any_other_zero_race),
      
      supp_backcalc_race =
        ifelse(race_trigger & demo_key == "other_total",
               TRUE, FALSE)
    ) %>%
    ungroup()
}

# ============================================*
# Age Backcalc (grouped) ----
# ============================================*
apply_secondary_age <- function(df) {
  
  df %>%
    group_by(Geography, OUTCOME, CONDITION) %>%
    mutate(
      primary_suppressed_age =
        (demo_key %in% backcalc_age_groups) &
        (supp_count_full | supp_risk),
      
      n_primary_age =
        sum(primary_suppressed_age, na.rm = TRUE),
      
      suppressed_nonzero_age =
        any(primary_suppressed_age & new_cases > 0, na.rm = TRUE),
      
      any_other_zero_age =
        any((demo_key %in% backcalc_age_groups) &
              new_cases == 0 &
              !primary_suppressed_age, na.rm = TRUE),
      
      age_trigger =
        (n_primary_age == 1 &
           suppressed_nonzero_age &
           !any_other_zero_age),
      
      target_age_key =
        ifelse(age_trigger,
               {
                 suppressed_key <- demo_key[primary_suppressed_age]
                 candidates <- demo_key[
                   (demo_key %in% backcalc_age_groups) &
                     (new_cases > 0) &
                     (demo_key != suppressed_key)
                 ]
                 if (length(candidates) > 0) {
                   candidates[which.min(new_cases[
                     demo_key %in% candidates
                   ])]
                 } else NA
               },
               NA),
      
      supp_backcalc_age =
        ifelse(age_trigger & demo_key == target_age_key,
               TRUE, FALSE)
    ) %>%
    ungroup()
}

# ============================================*
# All suppression and diagnostics ----
# ============================================*
apply_all_suppression <- function(df) {
  
  df$supp_backcalc <- FALSE
  
  df <- calc_event_score(df, event_score_table)
  df <- add_demo_scores(df, demo_component_scores)
  df <- calc_total_risk_score(df)
  
  df <- apply_count_suppression(df)
  df <- apply_risk_suppression(df, risk_groups, risk_suppress_threshold)
  
  df <- apply_secondary_race(df)
  df <- apply_secondary_age(df)
  
  df <- df %>%
    mutate(
      supp_backcalc = (supp_backcalc_age | supp_backcalc_race),
      supp_risk     = ifelse(is.na(supp_risk), FALSE, supp_risk)
    )
  
  # ============================================*
  # Diagnostics ----
  # ============================================*
  df <- df %>%
    group_by(Geography, OUTCOME, CONDITION) %>%
    mutate(
      bc_is_age_group  = ifelse(demo_key %in% backcalc_age_groups, TRUE, NA),
      bc_is_race_group = ifelse(demo_key %in% backcalc_race_groups, TRUE, NA),
      
      bc_primary_suppressed =
        ifelse(demo_key %in% backcalc_groups,
               (supp_count_full | supp_risk),
               NA),
      
      bc_number_primary_suppressed_age =
        sum((demo_key %in% backcalc_age_groups) &
              (supp_count_full | supp_risk), na.rm = TRUE),
      
      bc_number_primary_suppressed_race =
        sum((demo_key %in% backcalc_race_groups) &
              (supp_count_full | supp_risk), na.rm = TRUE),
      
      bc_age_target =
        ifelse(supp_backcalc_age == TRUE, demo_key, NA),
      
      bc_race_target =
        ifelse(supp_backcalc_race == TRUE, demo_key, NA),
      
      bc_backcalc_triggered =
        ifelse(supp_backcalc == TRUE, TRUE, FALSE),
      
      bc_backcalc_reason =
        ifelse(
          supp_backcalc_age == TRUE, "age_backcalc_triggered",
          ifelse(
            supp_backcalc_race == TRUE, "race_backcalc_triggered",
            ifelse(
              bc_is_age_group == TRUE,
              ifelse(
                bc_number_primary_suppressed_age == 0, "age_zero_primary",
                ifelse(
                  bc_number_primary_suppressed_age > 1, "age_multiple_primary",
                  "age_not_triggered"
                )
              ),
              ifelse(
                bc_is_race_group == TRUE,
                ifelse(
                  bc_number_primary_suppressed_race == 0, "race_zero_primary",
                  ifelse(
                    bc_number_primary_suppressed_race > 1, "race_multiple_primary",
                    "race_not_triggered"
                  )
                ),
                NA
              )
            )
          )
        )
    ) %>%
    ungroup()
  
  # ============================================*
  # Final Masking ----
  # ============================================*
  df <- df %>%
    mutate(
      supp_final =
        ifelse(
          demo_key %in% risk_groups,
          (supp_count_full | supp_risk | supp_backcalc),
          supp_count_full
        ),
      
      final_cases = ifelse(supp_final, NA, new_cases),
      
      final_crude_rate     = ifelse(supp_final | supp_rates_only, NA, crude_rate),
      final_crude_ci_lower = ifelse(supp_final | supp_rates_only, NA, crude_ci_lower),
      final_crude_ci_upper = ifelse(supp_final | supp_rates_only, NA, crude_ci_upper),
      
      final_aa_rate        = ifelse(supp_final | supp_rates_only, NA, aa_rate),
      final_aa_ci_lower    = ifelse(supp_final | supp_rates_only, NA, aa_ci_lower),
      final_aa_ci_upper    = ifelse(supp_final | supp_rates_only, NA, aa_ci_upper)
    )
  df
}
