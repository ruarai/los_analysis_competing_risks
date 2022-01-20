
source("R/fn_trunc_gamma_fitting.R")

make_trunc_onset_to_ward <- function(linelist_data, date_data_load) {

  require(rightTruncation)
  require(sizeSpectra)
  require(extraDistr)
  
  
  
  data_ward_fitting <- linelist_data %>%
    mutate(
      date_admission = as_date(dt_hosp_admission),
      
      LoS = as.numeric(date_admission - date_onset),
      
      age_class_narrow = cut_age(age, get_narrow_age_table()),
      
      coding = "onset_to_ward"
    ) %>%
    
    filter(LoS <= 31) %>%
    
    mutate(date_admission = as_date(dt_hosp_admission)) %>%
    
    select(age_class_narrow,
           reported_date = date_admission,
           symptom_onset_date = date_onset,
           time_to_report = LoS)
  
  
  get_fit_by_ageclass <- function(data_ward_fitting, i_age_class) {
    
    h_nr_tibble <- data_ward_fitting %>%
      filter(age_class_narrow == i_age_class) %>%
      
      rightTruncation::make_h_nr_tibble(day_N = date_data_load)
    
    
    MLE <- optim(f = neg_ll_gamma,
                 par = c(0,-1),
                 h_nr_tibble = h_nr_tibble)
    
    
    ests <- exp(MLE$par) %>%
      `names<-`(c("shape", "rate"))
    
    rate_confint <- sizeSpectra::profLike(
      neg_ll_gamma,
      MLE = ests["rate"],
      minNegLL = MLE$value,
      vecInc = 0.0001,
      vecDiff = 0.1,
      h_nr_tibble = h_nr_tibble,
      fixed_shape = ests["shape"]
    )
    
    shape_confint <- sizeSpectra::profLike(
      neg_ll_gamma,
      MLE = ests["shape"],
      minNegLL = MLE$value,
      vecInc = 0.0001,
      vecDiff = 0.2,
      h_nr_tibble = h_nr_tibble,
      fixed_rate = ests["rate"]
    )
    
    tibble(
      age_class = i_age_class,
      
      
      shape = ests["shape"],
      shape_lower = shape_confint[1],
      shape_upper = shape_confint[2],
      
      rate = ests["rate"],
      rate_lower = rate_confint[1],
      rate_upper = rate_confint[2],
      
      mean = ests["shape"] / ests["rate"]
    ) %>%
      mutate(mean_lower = shape_lower / rate_lower,
             mean_upper = shape_upper / rate_upper)
  }
  
  
  fit <- get_narrow_age_table()$labels %>%
    map_dfr(function(i_age_class) {
      
      get_fit_by_ageclass(
        data_ward_fitting,
        i_age_class
      )
    })
  
  list(
    fit = fit,
    data = data_ward_fitting
  )

}
