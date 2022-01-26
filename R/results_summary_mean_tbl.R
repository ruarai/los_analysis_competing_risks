

make_summary_mean_tbl <- function(
  all_means,
  results_dir
) {
  require(kableExtra)
  
  
  round2 <- function(x) format(round(x, 2), nsmall = 2)
  
  make_ci <- function(center, lower, upper) {
    str_c(
      round2(center),
      str_c(" (", round2(lower) %>% str_trim(), ", ", round2(upper), ")")
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
    "omi_mix" = "Omicron-Delta",
    "omi_HNE" = "Omicron (HNE)"
  )
  
  
  summary_mean_tbl <- mean_tbl_data %>%
    filter(coding %in% c("ward_to_discharge", "ward_to_ICU", "ICU_to_postICU")) %>%
    
    mutate(mean_val = make_ci(mean, mean_lower, mean_upper),
           coding = factor(coding, levels = c("ward_to_discharge", "ward_to_ICU", "ICU_to_postICU"))) %>%
    
    select(-c(mean, mean_lower, mean_upper)) %>%
    
    pivot_wider(names_from = coding,
                values_from = c(n, mean_val)) %>%
    
    arrange(subset_name, age_class) %>%
    
    mutate(subset_name = table_subset_names[subset_name]) %>%
    
    select(subset_name, age_class,
           n_ward_to_discharge, mean_val_ward_to_discharge,
           n_ward_to_ICU, mean_val_ward_to_ICU,
           n_ICU_to_postICU, mean_val_ICU_to_postICU) %>%
    
    mutate(across(starts_with("n_"), ~ replace_na(., "-")),
           across(starts_with("mean_val"), ~ if_else(str_detect(., "NA") | is.na(.), "-", .)))
  
  
  kbl(
    summary_mean_tbl,
    col.names = c("Period", "Age group", rep(c("n", "Mean"), times = 3)),
    align = c("l", "l", rep("r", times = 6)),
    format = "latex",
    booktabs = TRUE,
    linesep = ""
  ) %>% 
    kable_styling(font_size = 8) %>%
    collapse_rows(1) %>%
    
    add_header_above(c(" " = 2, "ward-to-discharge" = 2, "ward-to-ICU" = 2, "ICU-to-post-ICU" = 2),
                     italic = TRUE, align = "l")  %>%
    
    add_header_above(c(" " = 2, "Ward LoS (to discharge)" = 2, "Ward LoS (to ICU)" = 2, "ICU LoS" = 2),
                     line = FALSE, align = "l") %>%
    
    
    write_file(paste0(results_dir, "/tbl_summary_means.tex"))

}