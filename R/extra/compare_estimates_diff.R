

library(tidyverse)
library(lubridate)

source("R/common.R")
source("R/surv_ward_to_next_static.R")
source("R/surv_ICU_to_next_static.R")



get_means <- function(linelist_data_path) {
  
  linelist_data <- read_csv(linelist_data_path)
  
  static_ward_omi_mix <- make_surv_ward_to_next(linelist_data, n_bootstraps_fit = 1000)
  static_ICU_omi_mix <- make_surv_ICU_to_next(linelist_data, n_bootstraps_fit = 1000)
  
  
  bind_rows(
    static_ward_omi_mix$fit_narrow %>%
      filter(coding == "ward_to_discharge" | coding == "ward_to_ICU") %>%
      mutate(subset_name = "omi_mix"),
    static_ICU_omi_mix$fit_narrow %>%
      filter(coding == "ICU_to_postICU") %>%
      mutate(subset_name = "omi_mix"),
  )
}


estimate_samples_asis <- read_csv("results/NSW_final_earlycutoff/omi_mix/estimate_samples_share_wide.csv")




means_samples_retro <- get_means("results/NSW_retro_earlycutoff/omi_mix/linelist_data.csv")


age_group_to_age_class <- function(age_group) {
  case_when(
    age_group == "0-9" ~ "0-39",
    age_group == "10-19" ~ "0-39",
    age_group == "20-29" ~ "0-39",
    age_group == "30-39" ~ "0-39",
    age_group == "40-49" ~ "40-69",
    age_group == "50-59" ~ "40-69",
    age_group == "60-69" ~ "40-69",
    age_group == "70-79" ~ "70+",
    age_group == "80+" ~ "70+",
  )
}

mean_samples_asis <- estimate_samples_asis %>%
  mutate(age_class_narrow = age_group_to_age_class(age_group),
         mean_ward_to_discharge = scale_ward_to_discharge * shape_ward_to_discharge,
         mean_ward_to_ICU = scale_ward_to_ICU * shape_ward_to_ICU,
         mean_ICU_to_postICU = scale_ICU_to_postICU * shape_ICU_to_postICU) %>%
  group_by(age_class_narrow, sample) %>%
  slice(1) %>%
  pivot_longer(starts_with("mean_"), names_to = "coding", values_to = "mean", names_prefix = "mean_") %>%
  
  ungroup() %>%
  
  rename(mean_asis = mean) %>%
  select(age_class_narrow, coding, bootstrap = sample, mean_asis)

data_diff <- mean_samples_asis %>%
  left_join(means_samples_retro) %>%
  drop_na(mean) %>%
  
  mutate(mean_diff = mean_asis - mean)

data_diff %>%
  
  ggplot() +
  
  geom_point(aes(x = age_class_narrow, y = mean_diff),
             position = position_jitter(width = 0.1),
             size = 0.4, alpha = 0.25) +
  
  stat_summary(aes(x = age_class_narrow, y = mean_diff),
               colour = "red",
               fun.min = ~ quantile(., 0.025), fun.max = ~quantile(., 0.975), fun = median) +
  
  facet_wrap(~coding) +
  
  theme_minimal()


ggplot() +
  
  geom_point(aes(x = age_class_narrow, y = mean),
             means_samples_retro,
              position = position_jitter(width = 0.1),
              size = 0.4, alpha = 0.25) +
 
 geom_point(aes(x = age_class_narrow, y = mean_asis),
            mean_samples_asis,
            position = position(width = 0.1),
            colour = "blue",
            size = 0.4, alpha = 0.25) +
  
  facet_wrap(~coding) +
  
  theme_minimal()


