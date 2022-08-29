

make_summary_mean_tbl <- function(all_means,
                                  results_dir) {
  require(kableExtra)


  round2 <- function(x) {
    if_else(is.na(x), NA_character_, format(round(x, 2), nsmall = 2))
  }
  round2trim <- function(x) str_trim(round2(x))


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
    mutate(
      bad_fit = replace_na(bad_fit, FALSE),
      mean = if_else(bad_fit, NA_real_, mean),
      q90 = if_else(bad_fit, NA_real_, q90)
    ) %>%
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

  coding_names <- c(
    "ward_to_discharge" = "Ward LoS (to discharge)",
    "ward_to_ICU" = "Ward LoS (to ICU)",
    "ICU_to_postICU" = "ICU LoS"
  )


  summary_mean_tbl <- mean_tbl_data %>%
    filter(coding %in% c("ward_to_discharge", "ward_to_ICU", "ICU_to_postICU")) %>%
    mutate(
      mean = round2(mean), mean_lower = str_c("(", round2trim(mean_lower), ","), mean_upper = str_c(round2(mean_upper), ")"),
      q90 = round2(q90), q90_lower = str_c("(", round2trim(q90_lower), ","), q90_upper = str_c(round2(q90_upper), ")"),
      coding = factor(coding, levels = c("ward_to_discharge", "ward_to_ICU", "ICU_to_postICU"))
    ) %>%
    arrange(coding, subset_name, age_class) %>%
    mutate(
      subset_name = table_subset_names[subset_name],
      coding = coding_names[coding]
    ) %>%
    select(coding, subset_name, age_class, n, mean, mean_lower, mean_upper, q90, q90_lower, q90_upper) %>%
    mutate(
      n = replace_na(n, "-"),
      mean_lower = if_else(is.na(mean), "", mean_lower),
      mean_upper = if_else(is.na(mean), "", mean_upper),
      q90_lower = if_else(is.na(q90), "", q90_lower),
      q90_upper = if_else(is.na(q90), "", q90_upper),
      mean = replace_na(mean, "-"),
      q90 = replace_na(q90, "-")
    )


  kbl(
    summary_mean_tbl,
    col.names = c(rep(" ", times = 10)),
    align = c("l", "l", "l", "r", "r", "wr{0.7cm}", "wr{0.5cm}", "r", "wr{0.7cm}", "wr{0.5cm}"),
    format = "latex",
    booktabs = TRUE,
    linesep = "",
  ) %>%
    kable_styling(font_size = 8) %>%
    collapse_rows(c(1, 2), row_group_label_position = "stack") %>%
    add_header_above(c(
      " " = 1, "Period" = 1, "Age group" = 1, "n" = 1,
      "Mean" = 3, "90% Quantile" = 3
    ),
    align = "l"
    ) %>%
    write_file(paste0(results_dir, "/tbl_summary_means.tex"))
}
