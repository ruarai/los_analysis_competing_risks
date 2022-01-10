


library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  "tidyverse",
  "lubridate"
))

library(future.callr)
plan(callr)


source("../clinical_forecasting/R/state_data/NSW.R")
source("R/common.R")

source("R/surv_onset_to_ward.R")
source("R/surv_ward_to_next.R")
source("R/surv_ICU_to_next.R")
source("R/surv_postICU_to_next.R")

source("R/results_survival_plots.R")
source("R/pathway_probabilities.R")


list(
  tar_target(
    results_name, "NSW_omi_mix_2022-01-04"
  ),
  tar_target(
    results_dir, paste0("results/", results_name, "/")
  ),
  
  tar_target(
    linelist_path,
    
    "~/data_private/NSW/NSW_out_episode_2022_01_04.xlsx"
  ),
  
  tar_target(minimum_date, ymd("2021-11-15")),
  
  
  tar_target(
    linelist_raw,
    readxl::read_excel(linelist_path, sheet = 2)
  ),
  
  
  tar_target(
    linelist_data,
    
    read_NSW_linelist(linelist_raw) %>%
      filter(minimum_date >= ymd("2021-Jul-07"))
  ),
  
  
  tar_target(
    surv_onset_to_ward,
    make_surv_onset_to_ward(
      linelist_data,
      age_table_narrow = get_narrow_age_table(),
      age_table_wide = get_wide_age_table()
    )
  ),
  
  
  tar_target(
    surv_ward_to_next,
    make_surv_ward_to_next(
      linelist_data,
      age_table_narrow = get_narrow_age_table(),
      age_table_wide = get_wide_age_table()
    )
  ),
  
  tar_target(
    surv_ICU_to_next,
    make_surv_ICU_to_next(
      linelist_data,
      age_table_narrow = get_narrow_age_table(),
      age_table_wide = get_wide_age_table()
    )
  ),
  
  tar_target(
    surv_postICU_to_next,
    make_surv_postICU_to_next(
      linelist_data,
      age_table_narrow = get_narrow_age_table(),
      age_table_wide = get_wide_age_table()
    )
  ),
  
  tar_target(
    results_survival_plots,
    make_results_survival_plots(
      surv_onset_to_ward, surv_ward_to_next, surv_ICU_to_next, surv_postICU_to_next,
      age_table_narrow = get_narrow_age_table(),
      age_table_wide = get_wide_age_table(),
      results_dir
    )
  ),
  
  tar_target(
    pathway_probabilities,
    make_pathway_probabilities(
      surv_ward_to_next, surv_ICU_to_next, surv_postICU_to_next,
      results_dir
    )
  )
  
)

