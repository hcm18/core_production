#############################################
# suppression_config.R — UPDATED (2026-08-25)
# ------------------------------------------------------------
# This file defines thresholds, risk groups, and backcalc groups.
# Updated logic reflects user-verified rules:
#   * Primary suppression = count OR risk (NOT rate-only)
#   * AGE backcalc: trigger only if exactly one suppressed age group
#       AND that suppressed group is non-zero
#       AND all other age groups are non-zero
#       → secondary suppression = next-smallest non-zero age group
#   * RACE backcalc: trigger only if exactly one suppressed race group
#       AND that suppressed group is non-zero
#       AND all other race groups are non-zero
#       → secondary suppression = "other_total"
#   * "total" group never participates in backcalc
#############################################

##############################################################
# COUNT-BASED SUPPRESSION THRESHOLDS
##############################################################

# Full count suppression: < 11 new_cases
count_suppress_cases_threshold <- 11     

# Rate-only suppression: 11–19 new_cases (NOT primary suppression)
rate_suppress_cases_threshold  <- 20     


##############################################################
# RISK GROUPS — demographic groups where risk scoring applies
##############################################################

risk_groups <- c(
  "total",
  # AGE groups (risk scoring ON; backcalc only applies to these)
  "age0_9","age10_19","age20_29","age30_39",
  "age40_49","age50_59","age60_69","age70_79","age80plus",
  
  # RACE/ETHNICITY groups (risk scoring ON; backcalc only applies here)
  "api_total","black_total","hisp_total","other_total","white_total",
  
  # SEX groups (risk scoring ON; backcalc does NOT apply to these)
  "female_total","male_total"
)


##############################################################
# BACKCALC GROUPS — only AGE and RACE groups (NOT total, NOT sex)
# These must match AGE and RACE logic exactly.
##############################################################

# AGE groups included in AGE backcalc evaluation
backcalc_age_groups <- c(
  "age0_9","age10_19","age20_29","age30_39",
  "age40_49","age50_59","age60_69","age70_79","age80plus"
)

# RACE groups included in RACE backcalc evaluation
backcalc_race_groups <- c(
  "api_total","black_total","hisp_total","white_total","other_total"
)

# Full list of backcalc groups (for convenience)
backcalc_groups <- c(backcalc_age_groups, backcalc_race_groups)


##############################################################
# EVENT SCORE TABLE — used in risk scoring
# ------------------------------------------------------------
# user-confirmed logic:
#   0 cases      → event_score = 0
#   1–10 cases   → event_score = 7
#   11–99        → event_score = 5
#   100–999      → event_score = 3
#   >=1000       → event_score = 2
##############################################################

event_score_table <- data.frame(
  min_cases   = c(0,   1,   11,   100,  1000),
  max_cases   = c(0,   10,  99,   999,  Inf),
  event_score = c(0,    7,   5,     3,    2)
)


##############################################################
# DEMOGRAPHIC COMPONENT SCORES — applied ONLY to risk_groups
# All other demo_keys receive NA in suppression_functions.R
##############################################################

demo_component_scores <- data.frame(
  demo_key = c(
    "total",
    # AGE
    "age0_9","age10_19","age20_29","age30_39","age40_49",
    "age50_59","age60_69","age70_79","age80plus",
    # RACE
    "api_total","black_total","hisp_total","other_total","white_total",
    # SEX
    "female_total","male_total"
  ),
  
  # SEX SCORE
  sex_score = c(
    0,             # total
    rep(0,9),      # age groups
    0,0,0,0,0,     # race groups
    1,1            # female/male
  ),
  
  # AGE SCORE
  age_score = c(
    0,             # total
    rep(3,9),      # age groups
    0,0,0,0,0,     # race groups
    0,0            # sex groups
  ),
  
  # RACE SCORE
  re_score = c(
    0,             # total
    rep(0,9),      # age groups
    2,2,2,2,2,     # race groups
    0,0            # sex groups
  ),
  
  # INTERACTION SCORE
  interaction_score = c(
    -5,            # total
    rep(0,9),      # age groups
    0,0,0,0,0,     # race groups
    0,0            # sex groups
  )
)


##############################################################
# RISK SCORE SUPPRESSION THRESHOLD
# This value defines when risk_score_total triggers suppression.
##############################################################

risk_suppress_threshold <- 13


#############################################
# END suppression_config.R
#############################################