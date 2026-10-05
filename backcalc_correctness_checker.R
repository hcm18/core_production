###############################################################################
# BACKCALC CORRECTNESS CHECKER
# Identifies rows where AGE or RACE backcalc should have triggered but did not,
# and rows where backcalc triggered but rules say it should NOT have.
###############################################################################

check_backcalc_correctness <- function(df) {
  
  # AGE check
  age_check <- df %>%
    filter(demo_key %in% backcalc_age_groups) %>%
    group_by(Geography, OUTCOME, CONDITION) %>%
    summarise(
      # BACKCALC RULE CONDITIONS
      n_primary = sum((supp_count_full | supp_risk), na.rm = TRUE),
      suppressed_nonzero =
        any((supp_count_full | supp_risk) & new_cases > 0, na.rm = TRUE),
      any_other_zero =
        any(new_cases == 0 &
              !(supp_count_full | supp_risk), na.rm = TRUE),
      
      # OBSERVED BACKCALC RESULT
      has_backcalc =
        any(supp_backcalc == TRUE, na.rm = TRUE),
      
      # EXPECTED
      should_backcalc =
        (n_primary == 1 &
           suppressed_nonzero &
           !any_other_zero),
      
      # RESULTS
      backcalc_mismatch =
        ifelse(has_backcalc != should_backcalc, TRUE, FALSE),
      .groups = "drop"
    )
  
  # RACE check
  race_check <- df %>%
    filter(demo_key %in% backcalc_race_groups) %>%
    group_by(Geography, OUTCOME, CONDITION) %>%
    summarise(
      n_primary =
        sum((supp_count_full | supp_risk), na.rm = TRUE),
      suppressed_nonzero =
        any((supp_count_full | supp_risk) & new_cases > 0, na.rm = TRUE),
      any_other_zero =
        any(new_cases == 0 &
              !(supp_count_full | supp_risk), na.rm = TRUE),
      
      has_backcalc =
        any(supp_backcalc == TRUE &
              demo_key == "other_total", na.rm = TRUE),
      
      should_backcalc =
        (n_primary == 1 &
           suppressed_nonzero &
           !any_other_zero),
      
      backcalc_mismatch =
        ifelse(has_backcalc != should_backcalc, TRUE, FALSE),
      .groups = "drop"
    )
  
  list(
    age_backcalc_correctness = age_check,
    race_backcalc_correctness = race_check
  )
}