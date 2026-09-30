# ============================================================
# ICD code lists (same as your original)
# ============================================================

icd_codes_heat <- c("X30","T670", "T671", "T672", "T673", "T674",
  "T675", "T676", "T677", "T678", "T679")

# Non‑natural / man-made heat exposure:
icd_codes_nonnatural <- "W92"


# ============================================================
# FINAL HEAT ILLNESS/INJURY (MCOD) COMPUTATION
# Only saves heatillnessinjury_mcod
# ============================================================

ori_core <- ori_core %>%
  # Needed to allow c_across() row-by-row
  rowwise() %>%
  mutate(
    heatillnessinjury_mcod = {
      
      # -------------------------------------------------------
      # TEMP 1: Environmental heat (any ICD heat code)
      # Use na.rm = TRUE to prevent NA from RA1–RA20 causing
      # "missing value where TRUE/FALSE needed" error.
      # -------------------------------------------------------
      preHEAT <- any(c_across(RA1:RA20) %in% icd_codes_heat, na.rm = TRUE)
      
      # -------------------------------------------------------
      # TEMP 2: Non-natural heat exposure (W92)
      # Again, protect with na.rm = TRUE.
      # -------------------------------------------------------
      nonnatural <- any(c_across(RA1:RA20) %in% icd_codes_nonnatural, na.rm = TRUE)
      
      # -------------------------------------------------------
      # TEMP 3: CDC heat season (May–September)
      # Must force this to TRUE/FALSE, never NA.
      # If DMO is NA, default season = FALSE (safer).
      # -------------------------------------------------------
      season <- if_else(!is.na(DMO) & DMO >= 5 & DMO <= 9, TRUE, FALSE)
      
      # -------------------------------------------------------
      # FINAL CLASSIFICATION
      # Only condition saved to the dataset.
      # -------------------------------------------------------
      if (preHEAT && !nonnatural && season) {
        "Heat Illness/Injury"
      } else {
        "No condition"
      }
    }
  ) %>%
  ungroup()

# Show counts to check
ori_core %>% 
  count(heatillnessinjury_mcod, name = "n")