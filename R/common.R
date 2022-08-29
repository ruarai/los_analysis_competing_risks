
time_diff_to_days <- function(time_diff) {
  as.numeric(time_diff / ddays(1))
}


get_narrow_age_table <- function() {
  list(
    breaks = c(0, 40, 70, Inf) - 1,
    labels = c("0-39", "40-69", "70+")
  )
}
get_wide_age_table <- function() {
  list(
    breaks = c(0, 70, Inf) - 1,
    labels = c("0-69", "70+")
  )
}

cut_age <- function(age, age_table) {
  cut(age, breaks = age_table$breaks, labels = age_table$labels)
}


est_cols <- c(
  "delta" = ggokabeito::palette_okabe_ito()[1],
  "omi_mix" = ggokabeito::palette_okabe_ito()[2],
  "omi_HNE" = ggokabeito::palette_okabe_ito()[3]
)
est_labels <- c(
  "delta" = "Delta epidemic          ",
  "omi_mix" = "Omicron/Delta mixed epidemic          ",
  "omi_HNE" = "Omicron epidemic (Hunter New England LHD)    "
)
