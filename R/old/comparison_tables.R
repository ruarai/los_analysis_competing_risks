

datasets <- list(
  "omi_mix" = "results/NSW_omi_mix_2022-01-11/los_estimates.csv",
  "delta" = "results/NSW_delta_2021-11-25/los_estimates.csv"#,
  #"omi_HNE" = "results/NSW_omi_HNE_2022-01-11/los_estimates.csv"
)

coding_order <- c("onset_to_ward", "ward_to_discharge", "ward_to_ICU", "ward_to_death", 
                  "ICU_to_discharge", "ICU_to_death", "ICU_to_postICU", "postICU_to_discharge", 
                  "postICU_to_death") %>% str_replace_all("_", "-")


round2 <- function(x) format(round(x, 2), nsmall = 2)


split_age_class <- function(age_class) {
  case_when(age_class == "0-69" ~ list(c("0-39", "40-69")),
            TRUE ~ list(age_class))
}

bad_fits <- tribble(
  ~source, ~coding,
  "omi_mix", "ICU_to_death",
  "omi_mix", "postICU_to_death",
  "omi_HNE", "ward_to_death",
  "omi_HNE", "ward_to_ICU",
  "omi_HNE", "ICU_to_discharge",
  "omi_HNE", "ICU_to_postICU",
  "omi_HNE", "ICU_to_death",
  "omi_HNE", "postICU_to_death",
  "omi_HNE", "postICU_to_discharge",
) %>% mutate(bad = TRUE)


all_data <- datasets %>%
  map(read_csv, show_col_types = FALSE) %>%
  map_dfr(identity, .id = "source") %>%
  
  mutate(
    var_est = shape_est / (rate_est ^ 2),
    std_est = sqrt(var_est),
    
    value = str_c(round2(mean_est), " (", round2(std_est) %>% str_trim(), ")")
  ) %>%
  
  select(source, coding, age_class, value) %>%
  
  left_join(bad_fits) %>%
  mutate(value = if_else(!is.na(bad), "-", value)) %>%
  select(-bad) %>%
  
  mutate(
    coding = coding  %>% str_replace_all("_", "-"),
    coding = factor(coding, levels = coding_order)
  )


# Split 0-69 into two age groups
all_data_age_split <- all_data  %>%
  rowwise() %>%
  mutate(age_class = split_age_class(age_class)) %>%
  unnest(age_class) %>%
  
  ungroup()


table_data <- all_data_age_split %>%
  filter(source %in% c("omi_mix", "delta")) %>%
  mutate(source = factor(source, levels = c("delta", "omi_mix", "omi_HNE"))) %>%
  
  arrange(coding, age_class, source) %>%
  pivot_wider(names_from = c(source, age_class))

require(kableExtra)


kbl(
  table_data,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Transition", rep(c("Delta", "Mixed"), times = 3)),
  align = c("l", rep("r", times = 6)),
  linesep = ""
) %>%
  kable_styling(
    font_size = 8,
  ) %>%
  
  column_spec(
    c(2, 4, 6),
    border_left = TRUE
  ) %>%
  column_spec(
    7,
    border_right = TRUE
  ) %>%
  
  add_header_above(c(" " = 1, "0-39" = 2, "40-69" = 2, "70+" = 2)) %>%
  
  write_file(paste0("results/comparison_table_omi_mix_delta.tex"))
