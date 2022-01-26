
get_fit_aj <- function(
  surv_ward_to_next,
  surv_ICU_to_next,
  surv_postICU_to_next
) {
  
  surv_fits <- list(
    surv_ward_to_next,
    surv_ICU_to_next,
    surv_postICU_to_next
  )
  
  age_table_wide <- get_wide_age_table()
  age_table_narrow <- get_narrow_age_table()
  
  fix_result <- . %>%
    mutate(model = case_when(model == "Parametric mixture" ~ "parametric",
                             TRUE ~ "non-parametric")) %>%
    
    filter(coding != "Start")
    
  
  aj_fits <- surv_fits %>%
    map_dfr(function(fits_ls) {
      if(!is.null(fits_ls$fit_wide)) {
        aj_wide <- tryCatch(
          flexsurv::ajfit_flexsurvmix(
            fits_ls$fit_wide,
            
            B = 50,
            
            maxt = 120
          ) %>%
            rename(age_class = age_class_wide,
                   coding = state) %>%
            mutate(age_type = "wide") %>%
            fix_result(),
          error = function(e) {tibble()}
        )
        }
      else{
        aj_wide <- tibble()
      }
      
      if(!is.null(fits_ls$fit_narrow)) {
        aj_narrow <- flexsurv::ajfit_flexsurvmix(
          fits_ls$fit_narrow,
          
          B = 50,
          
          maxt = 120
        ) %>%
          rename(age_class = age_class_narrow,
                 coding = state) %>%
          mutate(age_type = "narrow") %>%
          fix_result()
      }
      else{
        aj_narrow <- tibble()
      }
      
      if(!is.null(fits_ls$fit_singular)) {
        aj_singular <- flexsurv::ajfit_flexsurvmix(
          fits_ls$fit_singular,
          
          B = 50,
          
          maxt = 120
        ) %>%
          rename(coding = state) %>%
          mutate(age_type = "singular") %>%
          fix_result()
      }
      else{
        aj_singular <- tibble()
      }
      
      bind_rows(aj_wide, aj_narrow, aj_singular)
    })
  
  aj_fits
  
}