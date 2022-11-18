
read_NSW_data <- function(ll_raw, truncate14days = FALSE, no_postICU = FALSE) {
  
  did_patient_die <- function(discharge_description) {
    death_descriptors <- c("deceased", "death", "died", "dead")
    regex_match <- str_c("(?i)(", str_c(death_descriptors, collapse = "|"), ")")
    
    str_detect(discharge_description, regex_match) %>%
      replace_na(FALSE)
  }
  
  ll_selected <- ll_raw %>%
    
    mutate(episode_died = did_patient_die(AP_DISCHARGE_DISPOSITION_DESC),
           still_in_hospital = still_in_hosp == 1) %>% 
    
    select(
      person_id,
      age,
      dt_onset = ncims_calc_onset_dt,
      dt_admit = admit_date_dt,
      dt_discharge = discharge_date_dt,
      dt_ICU_admit = first_icu_date_dt,
      dt_ICU_discharge = last_icu_date_dt,
      
      still_in_hospital,
      
      episode_died
    )
  
  # Via https://stackoverflow.com/a/70394780
  print_filtered_rows <- function(dataframe, ...) {
    df <- dataframe
    vars = as.list(substitute(list(...)))[-1L]
    for(arg in vars) {
      dataframe <- df
      dataframe_new <- dataframe %>% filter(!!arg)
      rows_filtered <- nrow(df) - nrow(dataframe_new)
      cat(sprintf('Filtered out %s rows using: %s\n', rows_filtered, deparse(arg)))
      df = dataframe_new
    }
    return(dataframe_new)
  }
  
  ll_dated <- ll_selected %>%
    
    mutate(
      dt_admit = pmax(dt_onset, dt_admit),
      dt_ICU_admit = pmax(dt_onset, dt_ICU_admit)
    ) %>%
    
    print_filtered_rows(
      dt_discharge > dt_admit,
      is.na(dt_ICU_admit) | dt_ICU_admit > dt_admit,
      is.na(dt_ICU_discharge) | dt_ICU_discharge >= dt_ICU_admit
    )
  
  if(truncate14days) {
    ll_dated <- ll_dated %>%
      mutate(dt_discharge = pmin(dt_discharge, dt_onset + ddays(14)),
             dt_ICU_discharge = pmin(dt_discharge, dt_onset + ddays(14))) %>%
      
      print_filtered_rows(dt_admit < dt_discharge) %>% 
      
      mutate(
        ICU_lost = dt_ICU_admit >= dt_discharge,
        dt_ICU_admit = if_else(ICU_lost, NA_POSIXct_, dt_ICU_admit),
        dt_ICU_discharge = if_else(ICU_lost, NA_POSIXct_, dt_ICU_discharge),
      ) %>% 
      select(-ICU_lost)
  }
  
  
  time_diff_to_days <- function(time_diff) {
    as.numeric(time_diff / ddays(1))
  }
  
  
  
  
  
  
  split_ICU_episode <- function(x) {
    tribble(
      ~dt_admit, ~dt_discharge, ~group,
      x$dt_admit, x$dt_ICU_admit, "ward",
      x$dt_ICU_admit, x$dt_ICU_discharge, "ICU",
      x$dt_ICU_discharge, x$dt_discharge, "ward"
    ) %>%
      filter(
        dt_discharge > dt_admit
      ) %>%
      mutate(
        person_id = x$person_id,
        age = x$age,
        dt_onset = x$dt_onset,
        died = x$episode_died & row_number() == n(),
        still_in_hospital = c(rep(FALSE, n() - 1), x$still_in_hospital),
        .before = 1
      )
  }
  
  
  ll_ICU_eps <- ll_dated %>%
    filter(!is.na(dt_ICU_admit)) %>%
    rowwise() %>%
    do(
      split_ICU_episode(.)
    )
  
  ll_nonICU_eps <- ll_dated %>%
    filter(is.na(dt_ICU_admit)) %>%
    select(-c(dt_ICU_admit, dt_ICU_discharge)) %>%
    rename(died = episode_died) %>%
    mutate(group = "ward")
  
  
  ll_eps_all <- bind_rows(
    ll_ICU_eps,
    ll_nonICU_eps
  ) %>%
    arrange(person_id, dt_admit) %>%
    
    group_by(person_id) %>%
    
    mutate(
      in_transition = case_when(
        row_number() == 1 ~ "admit",
        TRUE ~ lag(group)
      ),
      out_transition = case_when(
        died ~ "death",
        row_number() == n() ~ "discharge",
        TRUE ~ lead(group)
      )
    ) %>%
    ungroup()
  
  
  ll_compartments <- ll_eps_all %>%
    group_by(person_id) %>%
    mutate(
      been_in_ICU = cumsum(group == "ICU") > 0,
      
      compartment = case_when(
        in_transition == "admit" ~ "ward",
        group == "ward" & !been_in_ICU ~ "ward",
        group == "ICU" ~ "ICU",
        group == "ward" & been_in_ICU ~ "postICU"
      )
    ) %>%
    
    select(-c(group, in_transition, been_in_ICU)) %>%
    
    group_by(
      person_id, compartment
    ) %>%
    summarise(
      person_id = first(person_id),
      age = first(age),
      dt_onset = first(dt_onset),
      los = sum(time_diff_to_days(dt_discharge - dt_admit)),
      
      dt_admit = min(dt_admit),
      
      out_transition = last(out_transition),
      still_in_hospital = any(still_in_hospital)
    ) 
  
  if(truncate14days) {
    print(str_c(sum(ll_compartments$los > 14), " rows with > 14 day length of stay despite processing"))
    print(str_c(sum(ll_compartments$los > 15), " rows with > 15 day length of stay despite processing"))
    
    
    # Sometimes get results slightly over 14, truncate these, but remove results > 15 days
    # The > 15 day stays seems to be a result of overlapping episode records (weird!)
    ll_compartments <- ll_compartments %>%
      filter(los < 15) %>%
      mutate(los = pmin(los, 14))
    
    ll_compartments$los <- pmin(ll_compartments$los, 14)
  }
  
  
  
  
  ll_out <- ll_compartments %>%
    group_by(person_id) %>%
    mutate(
      dt_onset = min(dt_onset),
      dt_admit = min(dt_admit)
    ) %>% 
    ungroup() %>%
    
    select(
      person_id,
      age,
      
      dt_onset,
      dt_admit,
      compartment,
      trans = out_transition,
      
      still_in_hospital,
      
      los
    )
  
  if(no_postICU) {
    ll_subset <- ll_out %>%
      filter(compartment != "postICU") %>%
      mutate(trans = if_else(compartment == "ICU" & trans == "ward", "discharge", trans))
    
    return(ll_subset)
    
  } else{
    return(ll_out)
  }
  
}



