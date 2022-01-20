
ll_files <- c(
  "delta" = "results/NSW_delta_2021-11-25/linelist_filtering.csv",
  "omi_mix" = "results/NSW_omi_mix_2022-01-11/linelist_filtering.csv",
  "omi_HNE" = "results/NSW_omi_HNE_2022-01-11/linelist_filtering.csv"
)

names_nice <- c(
  "n_raw_ind" = "Individuals in data",
  "n_invalid_multi_episode_ind" = "Filtered for episode separation",
  "n_early_dated_ind" = "Filtered for excessively early admission date",
  "n_long_duration_ind" = "Filtered for excessively long admission duration",
  "n_adm_delay_ind" = "Filtered due to symptom onset after admission",
  "final_n_ind" = "Individuals included in analysis"
)

ll_filt_tbl <- ll_files %>%
  map_dfr(read_csv, show_col_types = FALSE, .id = "source") %>%
  
  pivot_wider(names_from = "source") %>%
  
  filter(
    name %in% names(names_nice)
  ) %>%
  
  mutate(name = names_nice[name])


require(kableExtra)

kbl(
  ll_filt_tbl,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  col.names = c("", "Delta", "Omicron/Delta mix", "HNE Omicron")
)
