
example_data <-
    data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "days"),
               value_norm = rnorm(n = 365, mean = 10),
               value_norm_step = c(rnorm(n = 100, mean = 5), rnorm(n = 265, mean = 10)),
               value_zero = rep(0, 365),
               value_zero_norm = c(rep(0, 50), rnorm(n = 315, mean = 10)),
               value_na_norm = c(rep(NA, 200), rnorm(n = 165, mean = 10)),
               value_norm_na = c(rnorm(n = 165, mean = 10), rep(NA, 200)),
               value_norm_na_norm = c(rnorm(n = 165, mean = 10), rep(NA, 100), rnorm(n = 100, mean = 7)),
               stringsAsFactors = FALSE) |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("value_"), function(x){floor(x*10)}))


usethis::use_data(example_data, overwrite = TRUE)
