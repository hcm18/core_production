##############################################################
# BACKCALC TEST HARNESS
# Summarizes AGE and RACE backcalc behavior across dataset
##############################################################

run_backcalc_harness <- function(df) {
  df %>%
    group_by(Geography, OUTCOME, CONDITION) %>%
    summarise(
      age_primary =
        sum((demo_key %in% backcalc_age_groups) &
              (supp_count_full | supp_risk), na.rm = TRUE),
      
      race_primary =
        sum((demo_key %in% backcalc_race_groups) &
              (supp_count_full | supp_risk), na.rm = TRUE),
      
      any_age_zero =
        any((demo_key %in% backcalc_age_groups) &
              new_cases == 0, na.rm = TRUE),
      
      any_race_zero =
        any((demo_key %in% backcalc_race_groups) &
              new_cases == 0, na.rm = TRUE),
      
      age_has_backcalc =
        any((demo_key %in% backcalc_age_groups) &
              supp_backcalc == TRUE, na.rm = TRUE),
      
      race_has_backcalc =
        any((demo_key %in% backcalc_race_groups) &
              supp_backcalc == TRUE, na.rm = TRUE),
      
      age_backcalc_target =
        paste(
          demo_key[(demo_key %in% backcalc_age_groups) &
                     supp_backcalc == TRUE],
          collapse = ","
        ),
      
      race_backcalc_target =
        paste(
          demo_key[(demo_key %in% backcalc_race_groups) &
                     supp_backcalc == TRUE],
          collapse = ","
        ),
      
      .groups = "drop"
    )
}