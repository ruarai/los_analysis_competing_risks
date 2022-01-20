
fit_meta <- tibble::tribble(
  ~ i_comp, ~ i_age_type,
  "onset_to_ward", "narrow", 
  "ward_to_discharge", "narrow",
  "ward_to_ICU", "narrow", 
  "ward_to_death", "wide",
  "ICU_to_discharge", "narrow",
  "ICU_to_death", "wide",
  "ICU_to_postICU", "narrow",
  "postICU_to_discharge", "narrow",
  "postICU_to_death", "wide"
)


data_subsets <- tibble::tribble(
  ~subset_name, ~date_start, ~date_end, ~LHD_filter,
  "omi_mix", "2021-12-01", NA_character_, NA_character_,
  
  "omi_HNE", "2021-12-01", NA_character_, "Hunter New England LHD",
  
  "delta", "2021-07-01", "2021-12-01", NA_character_
)
