


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


source("R/read_NSW_data_all.R")

source("R/common.R")

source("R/fit_meta.R")

source("R/make_surv_static.R")
source("R/export_static_14day_fits.R")

data_subsets <- tibble::tribble(
  ~subset_name, ~date_start, ~date_end, 
  "omi_BA5", "2022-06-01", "2022-11-01", 
)



pre_subsets <- list(
  tar_target(results_name_prefix, "NSW_2022-11-07_14day_2"),
  tar_target(linelist_path, "~/source/email_digester/downloads/hospital_linelist/NSW_out_episode_2022_11_07.xlsx"),
  tar_target(
    linelist_raw,
    {
      ll_data <- readxl::read_excel(linelist_path, sheet = 2)
    }
  ),
  tar_target(
    date_data_load,
    linelist_raw %>% pull(load_date) %>% first() %>% as_date()
  ),
  tar_target(
    results_dir,
    {
      dir <- paste0("results/", results_name_prefix, "/")
      dir.create(dir, showWarnings = FALSE)
      return(dir)
    }
  )
)


for_each_subset <- tar_map(
  values = data_subsets,
  unlist = FALSE,
  names = "subset_name",
  tar_target(
    results_dir,
    {
      dir <- paste0("results/", results_name_prefix, "/", subset_name, "/")
      dir.create(dir, showWarnings = FALSE, recursive = TRUE)
      return(dir)
    }
  ),
  tar_target(
    linelist_data,
    {
      ll_raw <- linelist_raw

      if (!is.na(date_start)) {
        ll_raw <- ll_raw %>%
          filter(admit_date_dt >= ymd(date_start))
      }

      if (!is.na(date_end)) {
        ll_raw <- ll_raw %>%
          filter(admit_date_dt < ymd(date_end))
      }
      
      
      ll_data <- read_NSW_data(
        ll_raw,
        truncate14days = TRUE,
        no_postICU = TRUE
      )
      
      return(ll_data)
    }
  ),
  
  tar_target(
    ward_fit,
    make_surv_static(linelist_data, "ward", date_data_load)
  ),
  
  tar_target(
    ICU_fit,
    make_surv_static(linelist_data, "ICU", date_data_load)
  ),
  
  tar_target(
    fit_exports,
    export_static_14day_fits(ward_fit, ICU_fit, results_dir)
  )
)

list(
  pre_subsets,
  for_each_subset
)
