
make_pathway_probabilities <- function(
  ward_fits, ICU_fits, postICU_fits,
  
  results_dir
) {

  
  binom_test_interval <- function(x, n) {
    x <- round(x)
    n <- round(n)
    
    if(x > n | n == 0){
      return(tibble(lower_95 = 0, upper_95 = 0, prop_wfh = 0, n = n))
    }
    
    test <- binom.test(x, n)
    
    tibble(lower_95 = test$conf.int[1],
           upper_95 = test$conf.int[2],
           prop_pathway = test$estimate[1],
           n = n,
           x = x)
  }
  
  make_prop_model <- function(fit_data) {
    fit_data %>% 
      ungroup() %>%
      
      filter(coding != "censored", coding != "unknown") %>%
      group_by(age_class = age_class_narrow) %>%
      
      mutate(n_age_class = n()) %>%
      
      group_by(age_class, coding) %>%
      
      summarise(prop_test = binom_test_interval(n(), first(n_age_class)),
                .groups = "drop") %>%
      unpack(prop_test)
  }
  
  
  prop_models <- bind_rows(
    ward_prop_model <- make_prop_model(ward_fits$data),
    ICU_prop_model <- make_prop_model(ICU_fits$data),
    postICU_prop_model <- make_prop_model(postICU_fits$data)
  ) %>%
    mutate(coding = factor(coding, levels = c("ward_to_discharge", "ward_to_ICU", "ward_to_death", 
                                              "ICU_to_discharge", "ICU_to_death", "ICU_to_postICU", "postICU_to_discharge", 
                                              "postICU_to_death")))
  
  
  
  ggplot(prop_models) +
    geom_linerange(aes(y = age_class, xmin = lower_95, xmax = upper_95)) +
    geom_point(aes(y = age_class, x = prop_pathway),
               pch = "|") +
    
    facet_wrap(~coding, ncol = 1) +
    
    xlab("Probability") + ylab(NULL) +
    
    theme_minimal()
  
  
  ggsave(paste0(results_dir, "/pathway_probs.png"),
         bg = 'white',
         width = 6,
         height = 8)
  
  
  prop_models %>%
    write_csv(paste0(results_dir, "/modelled_pathway_probs.csv"))

  prop_models
}
