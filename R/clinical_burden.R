

linelist_path <- "~/data_private/NSW/NSW_out_episode_2022_01_11.xlsx"
ll_data <- readxl::read_excel(linelist_path, sheet = 2)

source("../clinical_forecasting/R/state_data/NSW.R")


minimum_date <- ymd("2021-07-07")


ll_strict <- read_NSW_linelist(ll_data, strict = TRUE) %>%
  filter(dt_hosp_admission >= minimum_date)

ll_loose <- read_NSW_linelist(ll_data, strict = FALSE) %>%
  filter(dt_hosp_admission >= minimum_date)


make_ll_counts <- function(linelist) {
  days <- seq(
    ymd("2021-07-07"), ymd("2022-01-04"), by = "days"
  )
  
  tibble(date = days) %>%
    rowwise() %>%
    
    mutate(count_ward = linelist %>%
             filter(dt_hosp_discharge >= date | is.na(dt_hosp_discharge),
                    dt_hosp_admission < date,
                    
                    is.na(dt_first_icu) | dt_first_icu > date | dt_last_icu < date) %>%
             nrow(),
           
           count_ICU = linelist %>%
             drop_na(dt_first_icu) %>%
             filter(dt_last_icu >= date | is.na(dt_last_icu),
                    dt_first_icu <= date,) %>%
             nrow(),
           
           count_hosp = count_ward + count_ICU)# %>%
    # 
    # pivot_longer(cols = starts_with("count_"),
    #              names_prefix = "count_",
    #              values_to = "count",
    #              names_to = "group")
}

counts_strict <- make_ll_counts(ll_strict)
counts_loose <- make_ll_counts(ll_loose)

counts_loose %>%
  write_csv("~/temp/counts_loose.csv")
counts_strict %>%
  write_csv("~/temp/counts_strict.csv")

counts_occ <- counts_loose %>%
  filter(group == "ward") %>%
  select(date, ward_occ = count) %>%
  
  left_join(
    ll_loose %>% count(date = as_date(dt_hosp_admission)) %>%
      select(date, adm = n)
  )

ggplot() +
  geom_line(aes(x = date, y = count),
            counts_loose %>% filter(group == "ward")) +
  
  geom_line(aes(x = date, y = n),
            ll_loose %>% count(date = as_date(dt_hosp_admission)))

ggplot(counts_occ %>% filter(date >= ymd("2021-06-01"), date <= ymd("2022-01-02"))) +
  geom_point(aes(x = ward_occ, y = adm)) +
  
  xlab("Daily ward occupancy") +
  ylab("Daily admissions") +
  
  geom_smooth(aes(x = ward_occ, y = adm),
              method = "lm") +
  
  theme_minimal()



counts_all <- bind_rows(
  counts_strict %>% mutate(type = "strict"),
  counts_loose %>% mutate(type = "loose")
) %>%
  filter(group == "hosp")

ggplot(counts_all) +
  geom_line(aes(x = date, y = count, color = type)) +
  
  annotate("linerange", y = -30,
           xmin = ymd("2021-07-07"),
           xmax = ymd("2021-12-01"),
           color = "#e59f00",
           size = 1.1) +
  
  annotate("linerange", y = -30,
           xmin = ymd("2021-12-01"),
           xmax = ymd("2022-01-04"),
           color = "#56b4e9",
           size = 1.1) +
  
  theme_minimal() +
  
  xlab("Date") + ylab("Count") +
  ggtitle("Hospital occupancy", "With and without strict filtering of cases") +
  
  scale_x_date(
    date_breaks = "months",
    labels = scales::label_date_short()
  )
