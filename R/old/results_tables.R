

make_results_tables <- function(
  onset_fits, ward_fits, ICU_fits, postICU_fits,
  pathway_probabilities,
  
  age_table_narrow, age_table_wide,
  
  results_dir
) {
    
  
  require(kableExtra)
  
  round3 <- function(x) format(round(x, 3), nsmall = 3)
  round2 <- function(x) format(round(x, 2), nsmall = 2)
  
  pathway_results_tbl <- pathway_probabilities %>%
    mutate(est = str_c(round3(prop_pathway), " (", round3(lower_95), ", ", round3(upper_95), ")"),
           coding = coding %>% str_replace_all("_", "-")) %>%
    
    select(age_class, n, coding, x, est)
  
  pathway_results_latex <- kbl(
    pathway_results_tbl,
    format = "latex",
    booktabs = TRUE,
    col.names = c("Age class", "N", "Pathway", "n", "Probability (Mean and 95% CI)"),
    align = c("l", "r", "l", "r", "r")
  ) %>%
    collapse_rows(columns = c(1, 2)) %>%
    kable_styling(font_size = 8)
  
  write_file(pathway_results_latex, paste0(results_dir, "/pathway_probabilities.tex"))
  
  
  
  
  make_fit_summary <- function(surv_fit, age_classes, age_class_col, codings) {
    fit_vars <- expand_grid(
      coding = codings,
      age_class = age_classes
    ) %>%
      rename_with(~ if_else(. == "age_class", age_class_col, .)) %>%
      filter(coding != "censored", coding != "unknown")
    
    
    map_dfr(1:nrow(fit_vars), function(i) {
      fit_var_row <- fit_vars[i,]
      
      bind_cols(
        fit_var_row,
        
        summary(surv_fit, type = "mean", newdata = fit_var_row)[[1]] %>%
          rename_with(~ str_c("mean_", .)),
        
        summary(surv_fit, type = "link", newdata = fit_var_row)[[1]] %>%
          select(-time) %>%
          rename_with(~ str_c("rate_", .))
      )
      
      
    }) %>%
      
      mutate(shape_est = mean_est * rate_est,
             # Overestimates of CIs:
             shape_lcl = mean_lcl * rate_lcl,
             shape_ucl = mean_ucl * rate_ucl)
  }
  
  narrow_fits <- bind_rows(
    make_fit_summary(onset_fits$fit_narrow, age_table_narrow$labels, "age_class_narrow", "onset_to_ward"),
    make_fit_summary(ward_fits$fit_narrow, age_table_narrow$labels, "age_class_narrow", unique(ward_fits$data$coding)),
    make_fit_summary(ICU_fits$fit_narrow, age_table_narrow$labels, "age_class_narrow", unique(ICU_fits$data$coding)),
    make_fit_summary(postICU_fits$fit_narrow, age_table_narrow$labels, "age_class_narrow", unique(postICU_fits$data$coding))
  )
  
  wide_fits <- bind_rows(
    make_fit_summary(onset_fits$fit_wide, age_table_wide$labels, "age_class_wide", "onset_to_ward"),
    make_fit_summary(ward_fits$fit_wide, age_table_wide$labels, "age_class_wide", unique(ward_fits$data$coding)),
    make_fit_summary(ICU_fits$fit_wide, age_table_wide$labels, "age_class_wide", unique(ICU_fits$data$coding)),
    make_fit_summary(postICU_fits$fit_wide, age_table_wide$labels, "age_class_wide", unique(postICU_fits$data$coding))
  )
  
  
  plot_meta <- tribble(
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
  
  fits_final <- pmap_dfr(
    plot_meta,
    function(i_comp, i_age_type){
      if(i_age_type == "wide") {
        return(wide_fits %>% filter(coding == i_comp) %>% rename(age_class = age_class_wide))
      } else{
        return(narrow_fits %>% filter(coding == i_comp) %>% rename(age_class = age_class_narrow))
      }
    }
  )
  
  fits_final %>%
    write_csv(paste0(results_dir, "/los_estimates.csv"))
  
  
  
  
  diag_fitting_data <- map_dfr(
    list(onset_fits, ward_fits, ICU_fits, postICU_fits),
    ~ .$data
  )
  
  diag_data <- bind_rows(
    
    diag_fitting_data %>% 
      group_by(coding, age_class = age_class_wide) %>%
      summarise(n = n()) %>%
      filter(coding != "unknown", coding != "censored") %>%
      
      mutate(i_age_type = "wide"),
    
    diag_fitting_data %>% 
      group_by(coding, age_class = age_class_narrow) %>%
      summarise(n = n()) %>%
      filter(coding != "unknown", coding != "censored") %>%
      
      mutate(i_age_type = "narrow")
  ) %>%
    left_join(plot_meta %>% mutate(keep = TRUE),
              by = c("coding" = "i_comp", "i_age_type")) %>%
    drop_na(keep) %>% select(-c(keep, i_age_type))
  
  make_ci <- function(center, lower, upper) {
    str_c(
      round2(center),
      str_c(" (", round2(lower) %>% str_trim(), ", ", round2(upper), ")")
    )
  }
  
  fits_tbl <- fits_final %>%
    mutate(mean_ci = make_ci(mean_est, mean_lcl, mean_ucl),
           shape_ci = make_ci(shape_est, shape_lcl, shape_ucl),
           rate_ci = make_ci(rate_est, rate_lcl, rate_ucl)) %>%
    
    left_join(diag_data) %>%
    mutate(n = replace_na(n, 0)) %>%
    
    mutate(coding = coding  %>% str_replace_all("_", "-")) %>%
    
    select(coding, age_class, n, mean_ci, shape_ci, rate_ci)
  
  
  kbl(
    fits_tbl,
    format = "latex",
    booktabs = TRUE,
    col.names = c("Pathway", "Age class", "n", "Mean", "Shape", "Rate"),
    align = c("l", "l", "r", "r", "r", "r")
  ) %>%
    collapse_rows(columns = c(1, 2)) %>%
    kable_styling(font_size = 8) %>%
    write_file(paste0(results_dir, "/length_of_stay_estimates.tex"))
  
}
