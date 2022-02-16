


library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  "tidyverse",
  "lubridate"
))
library(tidyverse)
library(lubridate)
library(cowplot)

library(future.callr)
plan(callr)


source("../clinical_forecasting/R/state_data/NSW.R")
source("R/common.R")

source("R/fit_meta.R")

source("R/trunc_onset_to_ward.R")

source("R/surv_ward_to_next.R")
source("R/surv_ICU_to_next.R")
source("R/surv_postICU_to_next.R")

source("R/get_fit_aj.R")
source("R/get_fit_params.R")
source("R/get_fit_means.R")
source("R/get_fit_total_los.R")

source("R/export_fits.R")
source("R/extra/make_estimate_samples.R")

source("R/results_report_plots.R")
source("R/results_report_plots_2.R")
source("R/results_report_plots_3.R")
source("R/results_summary_mean_tbl.R")

source("R/clinical_burden.R")

pre_subsets <- list(
  tar_target(NSW_LHD_filter, NULL),
  
  tar_target(results_name_prefix, "NSW_2022-02-08"),
  tar_target(linelist_path,"~/data_private/NSW/NSW_out_episode_2022_02_08.xlsx"),
  
  
  
  tar_target(
    linelist_raw,
    {
      ll_data <- readxl::read_excel(linelist_path, sheet = 2)
    }
  ),
  tar_target(
    date_data_load,
    linelist_raw %>% pull(load_date) %>% first() %>% as_date()
  )
)


for_each_subset <- tar_map(
  values = data_subsets,
  unlist = FALSE,
  names = "subset_name",
  
  
  tar_target(
    results_dir, {
      dir <- paste0("results/", results_name_prefix, "_", subset_name, "/")
      dir.create(dir, showWarnings = FALSE)
      return(dir)
      }),

  
  tar_target(
    linelist_data,
    {
      ll_raw <- linelist_raw
      
      if(!is.na(LHD_filter)) {
        ll_raw <- ll_raw %>%
          filter(lhd_name == LHD_filter)
      }
      
      if(!is.na(date_start)) {
        ll_raw <- ll_raw %>%
          filter(admit_date_dt >= ymd(date_start))
      }
      
      if(!is.na(date_end)) {
        ll_raw <- ll_raw %>%
          filter(admit_date_dt < ymd(date_end))
      }
      
      
      ll_results <- read_NSW_linelist(
        ll_raw,
        remove_adm_delay = do_remove_adm_delay,
        remove_sep_episodes = do_remove_episodes_sep,
        
        return_diagnostics = TRUE
      ) 
      
      ll_data <- ll_results$data
      print(date_start)
      print(date_end)
      print(LHD_filter)
      
      ll_results$diagnostics %>%
        pivot_longer(everything()) %>%
        write_csv(paste0(results_dir, "/linelist_filtering.csv"))
      
      return(ll_data)
    }
    
  ),
  
  tar_target(
    trunc_onset_to_ward,
    make_trunc_onset_to_ward(
      linelist_data,
      date_data_load
    )
  ),
  
  tar_target(
    surv_ward_to_next,
    make_surv_ward_to_next(
      linelist_data
    )
  ),
  
  tar_target(
    surv_ICU_to_next,
    make_surv_ICU_to_next(
      linelist_data
    )
  ),
  
  tar_target(
    surv_postICU_to_next,
    make_surv_postICU_to_next(
      linelist_data
    )
  ),
  
  
  tar_target(
    fit_aj,
    get_fit_aj(
      surv_ward_to_next,
      surv_ICU_to_next,
      surv_postICU_to_next
    ) %>%
      mutate(subset_name = subset_name)
  ),
  
  tar_target(
    fit_params,
    get_fit_params(
      trunc_onset_to_ward,
      surv_ward_to_next,
      surv_ICU_to_next,
      surv_postICU_to_next
    ) %>%
      mutate(subset_name = subset_name)
  ),
  tar_target(
    fit_means,
    get_fit_means(
      surv_ward_to_next,
      surv_ICU_to_next,
      surv_postICU_to_next
    ) %>%
      mutate(subset_name = subset_name)
  ),
  tar_target(
    fit_total_los,
    get_fit_total_los(
      surv_ward_to_next,
      surv_ICU_to_next,
      surv_postICU_to_next
    ) %>%
      mutate(subset_name = subset_name)
  ),
  
  
  tar_target(
    fit_export_file,
    export_fits(
      fit_params,
      results_dir
    ),
    format = "file"
  ),
  
  tar_target(
    fit_samples_export,
    
    export_fit_samples(
      surv_ward_to_next,
      surv_ICU_to_next,
      surv_postICU_to_next,
      
      fit_means,
      
      results_dir
    )
  )
)

list(
  pre_subsets,
  for_each_subset,
  
  
  tar_combine(
    all_aj,
    for_each_subset[["fit_aj"]],
    
    command = dplyr::bind_rows(!!!.x)
  ),
  
  tar_combine(
    all_means,
    for_each_subset[["fit_means"]],
    
    command = dplyr::bind_rows(!!!.x)
  ),
  
  tar_combine(
    all_total_los,
    for_each_subset[["fit_total_los"]],
    
    command = dplyr::bind_rows(!!!.x)
  ),
  
  tar_target(
    results_dir, {
      dir <- paste0("results/", results_name_prefix, "_all", "/")
      dir.create(dir, showWarnings = FALSE)
      return(dir)
    }),
  
  tar_target(
    reporting_plots_1,
    make_reporting_plots_1(all_aj, results_dir)
  ),

  tar_target(
    reporting_plots_2,
    make_reporting_plots_2(all_means, results_dir)
  ),

  tar_target(
    reporting_summary_mean_tbl,
    make_summary_mean_tbl(all_means, results_dir)
  ),

  tar_target(
    reporting_burden,

    make_burden_figure(linelist_raw, date_data_load, ymd("2021-12-15"), results_dir)
  )
)



