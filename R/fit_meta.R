
fit_meta <- tibble::tribble(
  ~ i_comp, ~ i_age_type,
  "onset_to_ward", "narrow", 
  "ward_to_discharge", "narrow",
  "ward_to_ICU", "narrow", 
  "ward_to_death", "singular",
  "ICU_to_discharge", "narrow",
  "ICU_to_death", "wide",
  "ICU_to_postICU", "narrow",
  "postICU_to_discharge", "narrow",
  "postICU_to_death", "singular"
)

pretty_coding_name <- c(
  "onset_to_ward" = "onset-to-ward",
  "ward_to_discharge" = "ward-to-discharge",
  "ward_to_ICU" = "ward-to-ICU",
  "ward_to_death" = "ward-to-death",
  "ICU_to_discharge" = "ICU-to-discharge",
  "ICU_to_death" = "ICU-to-death",
  "ICU_to_postICU" = "ICU-to-post-ICU",
  "postICU_to_discharge" = "post-ICU-to-discharge",
  "postICU_to_death" = "post-ICU-to-death"
)

max_t_by_coding <- c(
  "onset_to_ward" = 21,
  "ward_to_discharge" = 30,
  "ward_to_ICU" = 14,
  "ward_to_death" = 50,
  "ICU_to_discharge" = 40,
  "ICU_to_death" = 40,
  "ICU_to_postICU" = 30,
  "postICU_to_discharge" = 30,
  "postICU_to_death" = 30
)

bad_fits <- tibble::tribble(
  ~coding, ~age_class, ~subset_name, 
  "ward_to_ICU", "0-39", "omi_HNE",
  "ICU_to_death", "0-69", "omi_HNE",
  "ICU_to_death", "70+", "omi_HNE",
  "postICU_to_death", NA, "omi_HNE",
) %>%
  mutate(bad_fit = TRUE)

remove_bad_fits <- . %>%
  left_join(bad_fits) %>%
  filter(is.na(bad_fit)) %>% 
  select(-bad_fit)



data_subsets <- tibble::tribble(
  ~subset_name, ~date_start, ~date_end, ~LHD_filter, ~do_remove_adm_delay, ~do_remove_episodes_sep,
  "omi_mix", "2021-12-15", NA_character_, NA_character_, TRUE, TRUE,

  "omi_HNE", "2021-12-15", NA_character_, "Hunter New England LHD", TRUE, TRUE,

  "delta", "2021-07-01", "2021-12-15", NA_character_, TRUE, TRUE,
)

# data_subsets <- tibble::tribble(
#   ~subset_name, ~date_start, ~date_end, ~LHD_filter, ~do_remove_adm_delay, ~do_remove_episodes_sep,
#   "om_filt_both", "2021-12-15", NA_character_, NA_character_, TRUE, TRUE,
#   "om_filt_delay", "2021-12-15", NA_character_, NA_character_, TRUE, FALSE,
#   "om_filt_eps", "2021-12-15", NA_character_, NA_character_, FALSE, TRUE,
#   "om_filt_none", "2021-12-15", NA_character_, NA_character_, FALSE, FALSE,
#   "d_filt_both", "2021-07-01", "2021-12-15", NA_character_, TRUE, TRUE,
#   "d_filt_delay", "2021-07-01", "2021-12-15", NA_character_, TRUE, FALSE,
#   "d_filt_eps", "2021-07-01", "2021-12-15", NA_character_, FALSE, TRUE,
#   "d_filt_none", "2021-07-01", "2021-12-15", NA_character_, FALSE, FALSE,
#   "h_filt_both", "2021-12-15", NA_character_, "Hunter New England LHD", TRUE, TRUE,
#   "h_filt_delay", "2021-12-15", NA_character_, "Hunter New England LHD", TRUE, FALSE,
#   "h_filt_eps", "2021-12-15", NA_character_, "Hunter New England LHD", FALSE, TRUE,
#   "h_filt_none", "2021-12-15", NA_character_, "Hunter New England LHD", FALSE, FALSE,
# )

