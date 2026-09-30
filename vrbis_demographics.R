
# ================================================================
# Case counters and crosstabs translated from SPSS to R for ori_core
# ================================================================
# Dependencies
library(dplyr)
library(tidyr)

# ------------------------------------------------
# Indicator mode: "NA" mimics SPSS IF (1/NA), "zero" yields 0/1
# ------------------------------------------------
INDICATOR_MODE <- "NA"   # change to "zero" if you prefer 0/1 variables

flag <- function(cond) {
  if (INDICATOR_MODE == "zero") as.integer(cond %in% TRUE) else ifelse(cond, 1L, NA_integer_)
}

# ------------------------------------------------
# Optional: ensure RCODE variables are character for AIAN checks
# ------------------------------------------------
ori_core <- ori_core %>%
  mutate(
    RCODE1 = as.character(RCODE1),
    RCODE2 = as.character(RCODE2),
    RCODE3 = as.character(RCODE3)
  )

# ================================================================
# ************* CREATE STANDARD CASE COUNTERS *************
# ================================================================

# Totals [checked]
ori_core <- ori_core %>%
  mutate(
    total_case = flag(CountyCases == 1),
    male_total_case = flag(CountyCases == 1 & Sex == 1),
    female_total_case = flag(CountyCases == 1 & Sex == 2)
  )

# 10-year age groups [checked]
ori_core <- ori_core %>%
  mutate(
    age0_9_case = flag(CountyCases == 1 & Age >= 0  & Age <= 9),
    age10_19_case = flag(CountyCases == 1 & Age >= 10 & Age <= 19),
    age20_29_case = flag(CountyCases == 1 & Age >= 20 & Age <= 29),
    age30_39_case = flag(CountyCases == 1 & Age >= 30 & Age <= 39),
    age40_49_case = flag(CountyCases == 1 & Age >= 40 & Age <= 49),
    age50_59_case = flag(CountyCases == 1 & Age >= 50 & Age <= 59),
    age60_69_case = flag(CountyCases == 1 & Age >= 60 & Age <= 69),
    age70_79_case = flag(CountyCases == 1 & Age >= 70 & Age <= 79),
    age80plus_case = flag(CountyCases == 1 & Age >= 80)
  )

# Extra needed set [checked]
ori_core <- ori_core %>%
  mutate(
    age65plus_case = flag(CountyCases == 1 & Age >= 65)
  )

# Race/Ethnicity case totals
# racecat8: 1 Hisp; 2 NH White; 3 NH Black; 4 NH AIAN; 5 NH Asian; 6 NH PI; 7 Other; 8 Two+; 9 Unknown
ori_core <- ori_core %>%
  mutate(
    hisp_total_case = flag(racecat8 == 1),
    white_total_case = flag(racecat8 == 2),
    black_total_case = flag(racecat8 == 3),
    api_total_case = flag(racecat8 %in% c(5, 6)),
    other_total_case = flag(racecat8 %in% c(4, 7, 8))
  )

# RE totals by Sex (for 2023dy)
ori_core <- ori_core %>%
  mutate(
    hisp_male_total_case = flag(CountyCases == 1 & racecat8 == 1 & Sex == 1),
    hisp_female_total_case = flag(CountyCases == 1 & racecat8 == 1 & Sex == 2),
    
    white_male_total_case = flag(CountyCases == 1 & racecat8 == 2 & Sex == 1),
    white_female_total_case = flag(CountyCases == 1 & racecat8 == 2 & Sex == 2),
    
    black_male_total_case = flag(CountyCases == 1 & racecat8 == 3 & Sex == 1),
    black_female_total_case = flag(CountyCases == 1 & racecat8 == 3 & Sex == 2),
    
    api_male_total_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Sex == 1),
    api_female_total_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Sex == 2),
    
    other_male_total_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Sex == 1),
    other_female_total_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Sex == 2)
  )

# ================================================================
# ************* AGE ADJUSTING *************
# ================================================================

# Age-adjusting totals [checked]
ori_core <- ori_core %>%
  mutate(
    age0_4_case = flag(CountyCases == 1 & Age < 5),
    age5_14_case = flag(CountyCases == 1 & Age >= 5  & Age <= 14),
    age15_24_case = flag(CountyCases == 1 & Age >= 15 & Age <= 24),
    age25_34_case = flag(CountyCases == 1 & Age >= 25 & Age <= 34),
    age35_44_case = flag(CountyCases == 1 & Age >= 35 & Age <= 44),
    age45_54_case = flag(CountyCases == 1 & Age >= 45 & Age <= 54),
    age55_64_case = flag(CountyCases == 1 & Age >= 55 & Age <= 64),
    age65_74_case = flag(CountyCases == 1 & Age >= 65 & Age <= 74),
    age75_84_case = flag(CountyCases == 1 & Age >= 75 & Age <= 84),
    age85plus_case = flag(CountyCases == 1 & Age >= 85)
  )

# Age-adjusting by Sex
# Male
ori_core <- ori_core %>%
  mutate(
    male_age0_4_case = flag(CountyCases == 1 & Sex == 1 & Age < 5),
    male_age5_14_case = flag(CountyCases == 1 & Sex == 1 & Age >= 5  & Age <= 14),
    male_age15_24_case = flag(CountyCases == 1 & Sex == 1 & Age >= 15 & Age <= 24),
    male_age25_34_case = flag(CountyCases == 1 & Sex == 1 & Age >= 25 & Age <= 34),
    male_age35_44_case = flag(CountyCases == 1 & Sex == 1 & Age >= 35 & Age <= 44),
    male_age45_54_case = flag(CountyCases == 1 & Sex == 1 & Age >= 45 & Age <= 54),
    male_age55_64_case = flag(CountyCases == 1 & Sex == 1 & Age >= 55 & Age <= 64),
    male_age65_74_case = flag(CountyCases == 1 & Sex == 1 & Age >= 65 & Age <= 74),
    male_age75_84_case = flag(CountyCases == 1 & Sex == 1 & Age >= 75 & Age <= 84),
    male_age85plus_case = flag(CountyCases == 1 & Sex == 1 & Age >= 85)
  )

# Female
ori_core <- ori_core %>%
  mutate(
    female_age0_4_case = flag(CountyCases == 1 & Sex == 2 & Age < 5),
    female_age5_14_case = flag(CountyCases == 1 & Sex == 2 & Age >= 5  & Age <= 14),
    female_age15_24_case = flag(CountyCases == 1 & Sex == 2 & Age >= 15 & Age <= 24),
    female_age25_34_case = flag(CountyCases == 1 & Sex == 2 & Age >= 25 & Age <= 34),
    female_age35_44_case = flag(CountyCases == 1 & Sex == 2 & Age >= 35 & Age <= 44),
    female_age45_54_case = flag(CountyCases == 1 & Sex == 2 & Age >= 45 & Age <= 54),
    female_age55_64_case = flag(CountyCases == 1 & Sex == 2 & Age >= 55 & Age <= 64),
    female_age65_74_case = flag(CountyCases == 1 & Sex == 2 & Age >= 65 & Age <= 74),
    female_age75_84_case = flag(CountyCases == 1 & Sex == 2 & Age >= 75 & Age <= 84),
    female_age85plus_case = flag(CountyCases == 1 & Sex == 2 & Age >= 85)
  )

# RE-specific age-adjusting (NOT gated by CountyCases per SPSS "all AA below checked...")
# Hispanic
ori_core <- ori_core %>%
  mutate(
    hisp_age0_4_case = flag(racecat8 == 1 & Age < 5),
    hisp_age5_14_case = flag(racecat8 == 1 & Age >= 5  & Age <= 14),
    hisp_age15_24_case = flag(racecat8 == 1 & Age >= 15 & Age <= 24),
    hisp_age25_34_case = flag(racecat8 == 1 & Age >= 25 & Age <= 34),
    hisp_age35_44_case = flag(racecat8 == 1 & Age >= 35 & Age <= 44),
    hisp_age45_54_case = flag(racecat8 == 1 & Age >= 45 & Age <= 54),
    hisp_age55_64_case = flag(racecat8 == 1 & Age >= 55 & Age <= 64),
    hisp_age65_74_case = flag(racecat8 == 1 & Age >= 65 & Age <= 74),
    hisp_age75_84_case = flag(racecat8 == 1 & Age >= 75 & Age <= 84),
    hisp_age85plus_case = flag(racecat8 == 1 & Age >= 85)
  )

# White
ori_core <- ori_core %>%
  mutate(
    white_age0_4_case = flag(racecat8 == 2 & Age < 5),
    white_age5_14_case = flag(racecat8 == 2 & Age >= 5  & Age <= 14),
    white_age15_24_case = flag(racecat8 == 2 & Age >= 15 & Age <= 24),
    white_age25_34_case = flag(racecat8 == 2 & Age >= 25 & Age <= 34),
    white_age35_44_case = flag(racecat8 == 2 & Age >= 35 & Age <= 44),
    white_age45_54_case = flag(racecat8 == 2 & Age >= 45 & Age <= 54),
    white_age55_64_case = flag(racecat8 == 2 & Age >= 55 & Age <= 64),
    white_age65_74_case = flag(racecat8 == 2 & Age >= 65 & Age <= 74),
    white_age75_84_case = flag(racecat8 == 2 & Age >= 75 & Age <= 84),
    white_age85plus_case = flag(racecat8 == 2 & Age >= 85)
  )

# Black
ori_core <- ori_core %>%
  mutate(
    black_age0_4_case = flag(racecat8 == 3 & Age < 5),
    black_age5_14_case = flag(racecat8 == 3 & Age >= 5  & Age <= 14),
    black_age15_24_case = flag(racecat8 == 3 & Age >= 15 & Age <= 24),
    black_age25_34_case = flag(racecat8 == 3 & Age >= 25 & Age <= 34),
    black_age35_44_case = flag(racecat8 == 3 & Age >= 35 & Age <= 44),
    black_age45_54_case = flag(racecat8 == 3 & Age >= 45 & Age <= 54),
    black_age55_64_case = flag(racecat8 == 3 & Age >= 55 & Age <= 64),
    black_age65_74_case = flag(racecat8 == 3 & Age >= 65 & Age <= 74),
    black_age75_84_case = flag(racecat8 == 3 & Age >= 75 & Age <= 84),
    black_age85plus_case = flag(racecat8 == 3 & Age >= 85)
  )

# API (NH Asian + NH Pacific Islander)
ori_core <- ori_core %>%
  mutate(
    api_age0_4_case = flag(racecat8 %in% c(5, 6) & Age < 5),
    api_age5_14_case = flag(racecat8 %in% c(5, 6) & Age >= 5  & Age <= 14),
    api_age15_24_case = flag(racecat8 %in% c(5, 6) & Age >= 15 & Age <= 24),
    api_age25_34_case = flag(racecat8 %in% c(5, 6) & Age >= 25 & Age <= 34),
    api_age35_44_case = flag(racecat8 %in% c(5, 6) & Age >= 35 & Age <= 44),
    api_age45_54_case = flag(racecat8 %in% c(5, 6) & Age >= 45 & Age <= 54),
    api_age55_64_case = flag(racecat8 %in% c(5, 6) & Age >= 55 & Age <= 64),
    api_age65_74_case = flag(racecat8 %in% c(5, 6) & Age >= 65 & Age <= 74),
    api_age75_84_case = flag(racecat8 %in% c(5, 6) & Age >= 75 & Age <= 84),
    api_age85plus_case = flag(racecat8 %in% c(5, 6) & Age >= 85)
  )

# Other (NH AIAN + Other + Two+ races)
ori_core <- ori_core %>%
  mutate(
    other_age0_4_case = flag(racecat8 %in% c(4, 7, 8) & Age < 5),
    other_age5_14_case = flag(racecat8 %in% c(4, 7, 8) & Age >= 5  & Age <= 14),
    other_age15_24_case = flag(racecat8 %in% c(4, 7, 8) & Age >= 15 & Age <= 24),
    other_age25_34_case = flag(racecat8 %in% c(4, 7, 8) & Age >= 25 & Age <= 34),
    other_age35_44_case = flag(racecat8 %in% c(4, 7, 8) & Age >= 35 & Age <= 44),
    other_age45_54_case = flag(racecat8 %in% c(4, 7, 8) & Age >= 45 & Age <= 54),
    other_age55_64_case = flag(racecat8 %in% c(4, 7, 8) & Age >= 55 & Age <= 64),
    other_age65_74_case = flag(racecat8 %in% c(4, 7, 8) & Age >= 65 & Age <= 74),
    other_age75_84_case = flag(racecat8 %in% c(4, 7, 8) & Age >= 75 & Age <= 84),
    other_age85plus_case = flag(racecat8 %in% c(4, 7, 8) & Age >= 85)
  )

# ================================================================
# ************* New age groups for dy2023 *************
# ================================================================
ori_core <- ori_core %>%
  mutate(
    age0_17_case = flag(CountyCases == 1 & Age >= 0  & Age <= 17),
    age18_24_case = flag(CountyCases == 1 & Age >= 18 & Age <= 24),
    age25_44_case = flag(CountyCases == 1 & Age >= 25 & Age <= 44),
    age45_64_case = flag(CountyCases == 1 & Age >= 45 & Age <= 64),
    age60plus_case = flag(CountyCases == 1 & Age >= 60)
  )

# Age_recode variable (for CTABLES-like outputs)
ori_core <- ori_core %>%
  mutate(
    Age_recode = case_when(
      Age <= 17                    ~ "0-17",
      Age >= 18 & Age <= 24        ~ "18-24",
      Age >= 25 & Age <= 44        ~ "25-44",
      Age >= 45 & Age <= 64        ~ "45-64",
      Age >= 60                    ~ "60+",
      TRUE ~ NA_character_
    )
  )

# Age groups x Sex (10-year + dy2023 sets + 60+/65+)
# Male
ori_core <- ori_core %>%
  mutate(
    male_age0_9_case = flag(CountyCases == 1 & Sex == 1 & Age >= 0  & Age <= 9),
    male_age10_19_case = flag(CountyCases == 1 & Sex == 1 & Age >= 10 & Age <= 19),
    male_age20_29_case = flag(CountyCases == 1 & Sex == 1 & Age >= 20 & Age <= 29),
    male_age30_39_case = flag(CountyCases == 1 & Sex == 1 & Age >= 30 & Age <= 39),
    male_age40_49_case = flag(CountyCases == 1 & Sex == 1 & Age >= 40 & Age <= 49),
    male_age50_59_case = flag(CountyCases == 1 & Sex == 1 & Age >= 50 & Age <= 59),
    male_age60_69_case = flag(CountyCases == 1 & Sex == 1 & Age >= 60 & Age <= 69),
    male_age70_79_case = flag(CountyCases == 1 & Sex == 1 & Age >= 70 & Age <= 79),
    male_age80plus_case = flag(CountyCases == 1 & Sex == 1 & Age >= 80),
    
    male_age0_17_case = flag(CountyCases == 1 & Sex == 1 & Age >= 0  & Age <= 17),
    male_age18_24_case = flag(CountyCases == 1 & Sex == 1 & Age >= 18 & Age <= 24),
    male_age25_44_case = flag(CountyCases == 1 & Sex == 1 & Age >= 25 & Age <= 44),
    male_age45_64_case = flag(CountyCases == 1 & Sex == 1 & Age >= 45 & Age <= 64),
    male_age60plus_case = flag(CountyCases == 1 & Sex == 1 & Age >= 60),
    male_age65plus_case = flag(CountyCases == 1 & Sex == 1 & Age >= 65)
  )

# Female
ori_core <- ori_core %>%
  mutate(
    female_age0_9_case = flag(CountyCases == 1 & Sex == 2 & Age >= 0  & Age <= 9),
    female_age10_19_case = flag(CountyCases == 1 & Sex == 2 & Age >= 10 & Age <= 19),
    female_age20_29_case = flag(CountyCases == 1 & Sex == 2 & Age >= 20 & Age <= 29),
    female_age30_39_case = flag(CountyCases == 1 & Sex == 2 & Age >= 30 & Age <= 39),
    female_age40_49_case = flag(CountyCases == 1 & Sex == 2 & Age >= 40 & Age <= 49),
    female_age50_59_case = flag(CountyCases == 1 & Sex == 2 & Age >= 50 & Age <= 59),
    female_age60_69_case = flag(CountyCases == 1 & Sex == 2 & Age >= 60 & Age <= 69),
    female_age70_79_case = flag(CountyCases == 1 & Sex == 2 & Age >= 70 & Age <= 79),
    female_age80plus_case = flag(CountyCases == 1 & Sex == 2 & Age >= 80),
    
    female_age0_17_case = flag(CountyCases == 1 & Sex == 2 & Age >= 0  & Age <= 17),
    female_age18_24_case = flag(CountyCases == 1 & Sex == 2 & Age >= 18 & Age <= 24),
    female_age25_44_case = flag(CountyCases == 1 & Sex == 2 & Age >= 25 & Age <= 44),
    female_age45_64_case = flag(CountyCases == 1 & Sex == 2 & Age >= 45 & Age <= 64),
    female_age60plus_case = flag(CountyCases == 1 & Sex == 2 & Age >= 60),
    female_age65plus_case = flag(CountyCases == 1 & Sex == 2 & Age >= 65)
  )

# RE-specific (with CountyCases gating)
# Hispanic
ori_core <- ori_core %>%
  mutate(
    hisp_age0_9_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 0  & Age <= 9),
    hisp_age10_19_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 10 & Age <= 19),
    hisp_age20_29_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 20 & Age <= 29),
    hisp_age30_39_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 30 & Age <= 39),
    hisp_age40_49_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 40 & Age <= 49),
    hisp_age50_59_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 50 & Age <= 59),
    hisp_age60_69_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 60 & Age <= 69),
    hisp_age70_79_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 70 & Age <= 79),
    hisp_age80plus_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 80),
    
    hisp_age0_17_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 0  & Age <= 17),
    hisp_age18_24_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 18 & Age <= 24),
    hisp_age25_44_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 25 & Age <= 44),
    hisp_age45_64_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 45 & Age <= 64),
    hisp_age60plus_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 60),
    hisp_age65plus_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 65)
  )

# White
ori_core <- ori_core %>%
  mutate(
    white_age0_9_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 0  & Age <= 9),
    white_age10_19_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 10 & Age <= 19),
    white_age20_29_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 20 & Age <= 29),
    white_age30_39_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 30 & Age <= 39),
    white_age40_49_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 40 & Age <= 49),
    white_age50_59_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 50 & Age <= 59),
    white_age60_69_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 60 & Age <= 69),
    white_age70_79_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 70 & Age <= 79),
    white_age80plus_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 80),
    
    white_age0_17_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 0  & Age <= 17),
    white_age18_24_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 18 & Age <= 24),
    white_age25_44_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 25 & Age <= 44),
    white_age45_64_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 45 & Age <= 64),
    white_age60plus_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 60),
    white_age65plus_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 65)
  )

# Black
ori_core <- ori_core %>%
  mutate(
    black_age0_9_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 0  & Age <= 9),
    black_age10_19_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 10 & Age <= 19),
    black_age20_29_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 20 & Age <= 29),
    black_age30_39_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 30 & Age <= 39),
    black_age40_49_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 40 & Age <= 49),
    black_age50_59_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 50 & Age <= 59),
    black_age60_69_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 60 & Age <= 69),
    black_age70_79_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 70 & Age <= 79),
    black_age80plus_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 80),
    
    black_age0_17_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 0  & Age <= 17),
    black_age18_24_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 18 & Age <= 24),
    black_age25_44_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 25 & Age <= 44),
    black_age45_64_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 45 & Age <= 64),
    black_age60plus_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 60),
    black_age65plus_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 65)
  )

# API
ori_core <- ori_core %>%
  mutate(
    api_age0_9_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 0  & Age <= 9),
    api_age10_19_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 10 & Age <= 19),
    api_age20_29_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 20 & Age <= 29),
    api_age30_39_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 30 & Age <= 39),
    api_age40_49_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 40 & Age <= 49),
    api_age50_59_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 50 & Age <= 59),
    api_age60_69_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 60 & Age <= 69),
    api_age70_79_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 70 & Age <= 79),
    api_age80plus_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 80),
    
    api_age0_17_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 0  & Age <= 17),
    api_age18_24_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 18 & Age <= 24),
    api_age25_44_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 25 & Age <= 44),
    api_age45_64_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 45 & Age <= 64),
    api_age60plus_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 60),
    api_age65plus_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 65)
  )

# Other
ori_core <- ori_core %>%
  mutate(
    other_age0_9_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 0  & Age <= 9),
    other_age10_19_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 10 & Age <= 19),
    other_age20_29_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 20 & Age <= 29),
    other_age30_39_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 30 & Age <= 39),
    other_age40_49_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 40 & Age <= 49),
    other_age50_59_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 50 & Age <= 59),
    other_age60_69_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 60 & Age <= 69),
    other_age70_79_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 70 & Age <= 79),
    other_age80plus_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 80),
    
    other_age0_17_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 0  & Age <= 17),
    other_age18_24_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 18 & Age <= 24),
    other_age25_44_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 25 & Age <= 44),
    other_age45_64_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 45 & Age <= 64),
    other_age60plus_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 60),
    other_age65plus_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 65)
  )

# ================================================================
# ************* Added 0–14 set *************
# ================================================================
ori_core <- ori_core %>%
  mutate(
    age0_14_case = flag(CountyCases == 1 & Age >= 0 & Age <= 14),
    male_age0_14_case = flag(CountyCases == 1 & Sex == 1 & Age >= 0 & Age <= 14),
    female_age0_14_case = flag(CountyCases == 1 & Sex == 2 & Age >= 0 & Age <= 14),
    
    hisp_age0_14_case = flag(CountyCases == 1 & racecat8 == 1 & Age >= 0 & Age <= 14),
    white_age0_14_case = flag(CountyCases == 1 & racecat8 == 2 & Age >= 0 & Age <= 14),
    black_age0_14_case = flag(CountyCases == 1 & racecat8 == 3 & Age >= 0 & Age <= 14),
    api_age0_14_case = flag(CountyCases == 1 & racecat8 %in% c(5, 6) & Age >= 0 & Age <= 14),
    other_age0_14_case = flag(CountyCases == 1 & racecat8 %in% c(4, 7, 8) & Age >= 0 & Age <= 14)
  )

# ================================================================
# ************* Non-Hispanic small populations *************
# ================================================================
ori_core <- ori_core %>%
  mutate(
    # NH Pacific Islander
    nhpi_total_case = flag(racecat8 == 6),
    nhpi_male_total_case = flag(racecat8 == 6 & Sex == 1),
    nhpi_female_total_case = flag(racecat8 == 6 & Sex == 2),
    
    # NH Asian
    asian_total_case = flag(racecat8 == 5),
    asian_male_total_case = flag(racecat8 == 5 & Sex == 1),
    asian_female_total_case = flag(racecat8 == 5 & Sex == 2),
    
    # NH Multiple race
    multiplerace_total_case = flag(racecat8 == 8),
    multiplerace_male_total_case = flag(racecat8 == 8 & Sex == 1),
    multiplerace_female_total_case = flag(racecat8 == 8 & Sex == 2)
  )

# Optional: "Asian + Other group" commented in SPSS—uncomment if needed
# ori_core <- ori_core %>%
#   mutate(
#     CASE_4AsianOther_TOTAL        = flag(racecat8 %in% c(4, 6, 7, 8)),
#     CASE_4AsianOther_Male_TOTAL   = flag(racecat8 %in% c(4, 6, 7, 8) & Sex == 1),
#     CASE_4AsianOther_Female_TOTAL = flag(racecat8 %in% c(4, 6, 7, 8) & Sex == 2)
#   )

# ================================================================
# ************* ALL_All_AIAN totals by age/gender *************
# (AIAN in any RCODE*, with CountyCases gate)
# ================================================================
ori_core <- ori_core %>%
  mutate(
    all_aian_total_case = flag(CountyCases == 1 & (RCODE1 == "30" | RCODE2 == "30" | RCODE3 == "30")),
    all_aian_male_total_case = flag(CountyCases == 1 & (RCODE1 == "30" | RCODE2 == "30" | RCODE3 == "30") & Sex == 1),
    all_aian_female_total_case = flag(CountyCases == 1 & (RCODE1 == "30" | RCODE2 == "30" | RCODE3 == "30") & Sex == 2)
  )

# ================================================================
# ************* Multiple race + some other (combined) *************
# ================================================================
ori_core <- ori_core %>%
  mutate(
    multiplerace_some_other_total_case = flag(racecat8 %in% c(7, 8)),
    multiplerace_some_other_male_total_case = flag(CountyCases == 1 & racecat8 %in% c(7, 8) & Sex == 1),
    multiplerace_some_other_female_total_case = flag(CountyCases == 1 & racecat8 %in% c(7, 8) & Sex == 2)
  )

# ================================================================
# ************* CTABLES equivalents (examples) *************
# ================================================================
# Age_recode × CountyCases × Sex counts
ct_age_cases_Sex <- ori_core %>%
  count(Age_recode, CountyCases, Sex, name = "count") %>%
  arrange(Age_recode, CountyCases, Sex)

# Age_recode × racecat8 counts
ct_age_re <- ori_core %>%
  count(Age_recode, racecat8, name = "count") %>%
  arrange(Age_recode, racecat8)

# ================================================================
# ************* Alias names for DESCRIPTIVES mismatches *************
# (SPSS list referenced e.g., CASE_hisp_25_44; create aliases from AG versions)
# ================================================================
# ori_core <- ori_core %>%
#   mutate(
#     CASE_hisp_25_44  = CASE_hisp_AG25_44,
#     CASE_white_25_44 = CASE_white_AG25_44,
#     CASE_black_25_44 = CASE_black_AG25_44,
#     CASE_API_25_44   = CASE_API_AG25_44,
#     CASE_Other_25_44 = CASE_Other_AG25_44
#   )

# ================================================================
# ************* DESCRIPTIVES SUM equivalent *************
# Sums of all CASE_ variables (na.rm = TRUE mimics SPSS sum over 1/NA)
# ================================================================
case_sums <- ori_core %>%
  summarise(across(starts_with("CASE_"), ~ sum(.x, na.rm = TRUE)))

# Preview: print key outputs (optional)
print(ct_age_cases_Sex)
print(ct_age_re)
print(case_sums)

# The updated data frame with all CASE_ variables is in `ori_core`
# ================================================================
