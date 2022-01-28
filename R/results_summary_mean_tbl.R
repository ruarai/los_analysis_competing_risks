

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
           mean = if_else(bad_fit, NA_real_, mean),
           q90 = if_else(bad_fit, NA_real_, q90)) %>%
    
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
  
  coding_names = c(
    "ward_to_discharge" = "Ward LoS (to discharge)",
    "ward_to_ICU" = "Ward LoS (to ICU)",
    "ICU_to_postICU" = "ICU LoS"
  )
  
  
  summary_mean_tbl <- mean_tbl_data %>%
    filter(coding %in% c("ward_to_discharge", "ward_to_ICU", "ICU_to_postICU")) %>%
    
    mutate(mean_val = make_ci(mean, mean_lower, mean_upper),
           q90_val = make_ci(q90, q90_lower, q90_upper),
           coding = factor(coding, levels = c("ward_to_discharge", "ward_to_ICU", "ICU_to_postICU"))) %>%
    
    select(-c(mean, mean_lower, mean_upper, q90, q90_lower, q90_upper)) %>%
    
    arrange(coding, subset_name, age_class) %>%
    
    mutate(subset_name = table_subset_names[subset_name],
           coding = coding_names[coding]) %>%
    
    select(coding, subset_name, age_class, n, mean_val, q90_val) %>%
    
    mutate(n = replace_na(n, "-"),
           mean_val = if_else(str_detect(mean_val, "NA") | is.na(mean_val), "-", mean_val),
           q90_val = if_else(str_detect(q90_val, "NA") | is.na(q90_val), "-", q90_val))
  
  
  kbl(
    summary_mean_tbl,
    col.names = c("Coding", "Period", "Age group", "n", "Mean", "90% quantile"),
    align = c("l", "l", rep("r", times = 2)),
    format = "latex",
    booktabs = TRUE,
    linesep = "",
  ) %>% 
    kable_styling(font_size = 8, full_width = TRUE) %>%
    collapse_rows(c(1,2),
                  row_group_label_position = 'stack') %>%
    
    
    write_file(paste0(results_dir, "/tbl_summary_means.tex"))

}