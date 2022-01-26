require(kableExtra)

source("R/fit_meta.R")
all_means <- tar_read(all_means)
results_dir <- tar_read(results_dir)

round2 <- function(x) if_else(is.na(x), NA_character_, format(round(x, 2), nsmall = 2))

make_ci <- function(center, lower, upper) {
  if_else(
    is.na(center),
    "-",
    str_c(
      round2(center),
      str_c(" (", round2(lower) %>% str_trim(), ", ", round2(upper), ")")
    )
  )
  
}


split_age_class <- function(age_class) {
  case_when(
    is.na(age_class) ~ list(c("0-39", "40-69", "70+")),
    age_class == "0-69" ~ list(c("0-39", "40-69")),
    TRUE ~ list(age_class)
  )
}

mean_tbl_data <- all_means %>%
  left_join(
    fit_meta %>% mutate(keep = TRUE),
    by = c("coding" = "i_comp", "age_type" = "i_age_type")
  ) %>%
  drop_na(keep) %>%
  left_join(bad_fits) %>%
  mutate(bad_fit = replace_na(bad_fit, FALSE),
         mean = if_else(bad_fit, NA_real_, mean)) %>%
  
  rowwise() %>%
  mutate(age_class = split_age_class(age_class)) %>%
  unnest(age_class) %>%
  mutate(subset_name = factor(subset_name, levels = c("delta", "omi_mix", "omi_HNE"))) %>%
  
  
  select(-c(age_type, keep, bad_fit))


table_subset_names <- c(
  "delta" = "Delta",
  "omi_mix" = "Omicron/Delta",
  "omi_HNE" = "Omicron (HNE)"
)


summary_mean_tbl <- mean_tbl_data %>%
  filter(!str_detect(coding, "onset_to_")) %>%
  
  mutate(
    mean_upper = if_else(is.na(mean), NA_real_, mean_upper),
    mean_lower = if_else(is.na(mean), NA_real_, mean_lower),
  ) %>%
  
  mutate(mean_val = round2(mean),
         upper_val = round2(mean_upper),
         lower_val = round2(mean_lower),
         coding = factor(coding, levels = fit_meta$i_comp)) %>%
  
  select(-c(mean, mean_lower, mean_upper)) %>%
  
  pivot_wider(
    names_from = subset_name,
    values_from = c(n, mean_val, lower_val, upper_val)
  ) %>%
  
  mutate(
    across(starts_with("n_"), ~ replace_na(., 0)),
    across(starts_with("mean_val"), ~ replace_na(., "")),
    across(starts_with("lower_val"), ~ replace_na(., "")),
    across(starts_with("upper_val"), ~ replace_na(., ""))
  ) %>%
  
  arrange(coding, age_class) %>%
  
  relocate(
    c(coding, age_class, 
      n_delta, mean_val_delta, lower_val_delta, upper_val_delta,
      n_omi_mix, mean_val_omi_mix, lower_val_omi_mix, upper_val_omi_mix,
      n_omi_HNE, mean_val_omi_HNE, lower_val_omi_HNE, upper_val_omi_HNE)
  )

  
  
  
  
  
  

kbl(
  summary_mean_tbl,
  col.names = c("Pathway", "Age group", rep(c("n", "", "", ""), times = 3)),
  align = c("l", "l", rep(c("r", "d", "x", "y"), times = 3)),
  format = "latex",
  booktabs = TRUE,
  linesep = ""
) %>% 
  kable_styling(font_size = 8) %>%
  collapse_rows(1) %>%
  
  add_header_above(c("Pathway" = 1, "Age group" = 1, "Delta" = 4, "Omicron-Delta" = 4, "Omicron (HNE)" = 4),
                   align = "l")  %>%
  
  
  write_file(paste0(results_dir, "/tbl_summary_means_full.tex"))
