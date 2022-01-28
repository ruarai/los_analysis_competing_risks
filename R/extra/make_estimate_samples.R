

surv_ward_to_next <- tar_read(surv_ward_to_next_omi_mix)

surv_ICU_to_next <- tar_read(surv_ICU_to_next_omi_mix)
surv_postICU_to_next <- tar_read(surv_postICU_to_next_omi_mix)


surv_fit <- surv_ward$fit_narrow


get_fit_samples <- function(
  surv_fit,
  age_classes,
  event_names
) {
  map_dfr(
    1:1000,
    function(i) {
      resampled_fit <- flexsurv:::resample_pars(surv_fit)
    
    
      bind_rows(
        map_dfr(
          event_names,
          function(i_event) {
            flexsurv:::get_basepars(
              resampled_fit,
              event = which(surv_fit$evnames == i_event),
              newdata = tibble(age_class_narrow = age_classes, age_class_wide = age_classes)
              
            ) %>%
              as_tibble() %>%
              mutate(age_class = age_classes,
                     coding = i_event)
          }
        ) %>%
          pivot_longer(cols = c(shape, scale))
        ,
        
        flexsurv:::get_probpars(
          resampled_fit,
          newdata = tibble(age_class_narrow = age_classes, age_class_wide = age_classes)
        ) %>%
          rename_with(~ case_when(. == "val" ~ "value",
                                  . == "age_class_narrow" ~ "age_class",
                                  . == "age_class_wide" ~ "age_class",
                                  . == "event" ~ "coding",
                                  TRUE ~ .)) %>%
          mutate(name = "prob")
      )
      
    },
    .id = "sample"
  ) %>%
    filter(coding %in% event_names) %>%
    mutate(age_class = replace_na(age_class, "all"))
  
}


narrow_ages <- get_narrow_age_table()$labels
wide_ages <- get_wide_age_table()$labels

samples <- bind_rows(
  get_fit_samples(surv_ward_to_next$fit_narrow, narrow_ages, c("ward_to_discharge", "ward_to_ICU")),
  get_fit_samples(surv_ward_to_next$fit_singular, "all", "ward_to_death"),
  
  get_fit_samples(surv_ICU_to_next$fit_wide, wide_ages, c("ICU_to_death", "ICU_to_discharge", "ICU_to_postICU")),
  
  get_fit_samples(surv_postICU_to_next$fit_narrow, narrow_ages, c("postICU_to_discharge")),
  get_fit_samples(surv_postICU_to_next$fit_singular, "all", "postICU_to_death"),
)


ggplot(samples %>% pivot_wider()) +
  geom_point(aes(x = shape, y = rate, color = age_class),
             size = 0.2, alpha = 0.2) +
  
  facet_wrap(~coding) +
  
  ggokabeito::scale_color_okabe_ito(order = 3:7, name = "Age group") +
  
  scale_x_log10() +
  scale_y_log10() +
  
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  
  theme_minimal() +
  theme(legend.position = "bottom") +
  
  ggtitle("Length of stay distribution parameter estimate samples") 

ggplot(samples %>% pivot_wider()) +
  geom_point(aes(x = age_class, y = prob),
             position = position_jitter(width = 0.5),
             size = 0.2, alpha = 0.2) +
  
  facet_wrap(~coding,
             scales = "free_x") +
  
  ggokabeito::scale_color_okabe_ito(order = 3:7, name = "Age group") +
  
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  
  theme_minimal() +
  theme(legend.position = "bottom") +
  
  ggtitle("Length of stay distribution parameter estimate samples") 




age_class_to_10yr <- function(age_class) {
  case_when(
    age_class == "0-39" ~ list(c("0-9", "10-19", "20-29", "30-39")),
    age_class == "40-69" ~ list(c("40-49", "50-59", "60-69")),
    age_class == "70+" ~ list(c("70-79", "80+")),
    age_class == "0-69" ~ list(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69")),
    
    age_class == "all" ~ list(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")),
  )
}


consistent_col_names <- c(
  "sample", "age_group", "pr_ward_to_discharge", "pr_ward_to_ICU", "pr_ICU_to_discharge", 
  "pr_ICU_to_postICU", "pr_postICU_to_death", "scale_onset_to_ward", 
  "scale_ward_to_discharge", "scale_ward_to_ICU", "scale_ward_to_death", 
  "scale_ICU_to_discharge", "scale_ICU_to_death", "scale_ICU_to_postICU", 
  "scale_postICU_to_discharge", "scale_postICU_to_death", "shape_onset_to_ward", 
  "shape_ward_to_discharge", "shape_ward_to_ICU", "shape_ward_to_death", 
  "shape_ICU_to_discharge", "shape_ICU_to_death", "shape_ICU_to_postICU", 
  "shape_postICU_to_discharge", "shape_postICU_to_death"
)

samples %>%
  mutate(age_class = age_class_to_10yr(age_class),
         name = if_else(name == "prob", "pr", name)) %>%
  unnest(age_class) %>%
  pivot_wider(names_from = c(name, coding)) %>%
  select(any_of(consistent_col_names)) %>%
  View()
  write_csv(paste0(results_dir, "/estimate_samples.csv"))
