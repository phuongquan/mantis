#' Example data frame containing multiple time series in long format
#'
#' Simulated data to cover a range of different behaviours of time series
#'
#' @format ## `example_data`
#' A data frame with 3,903 rows and 3 columns:
#' \describe{
#'   \item{timepoint}{Dates for the time series}
#'   \item{item}{Labels to identify the different time series}
#'   \item{value}{Vaues for the time series}
#'   ...
#' }
"example_data"

example_data <-
    data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "days"),
               item_norm = rnorm(n = 365, mean = 10),
               item_norm_step = c(rnorm(n = 100, mean = 5), rnorm(n = 265, mean = 10)),
               item_zero = rep(0, 365),
               item_na = rep(NA, 365),
               item_zero_norm = c(rep(0, 50), rnorm(n = 315, mean = 10)),
               item_na_norm = c(rep(NA, 200), rnorm(n = 165, mean = 10)),
               item_norm_na = c(rnorm(n = 165, mean = 10), rep(NA, 200)),
               item_norm_na_norm = c(rnorm(n = 165, mean = 10), rep(NA, 100), rnorm(n = 100, mean = 7)),
               item_asc = 100 + cumsum(floor(rnorm(n = 365, mean = 5, sd = 2))),
               item_desc = 1000 - cumsum(floor(rnorm(n = 365, mean = 2, sd = 1))),
               stringsAsFactors = FALSE) |>
  dplyr::mutate(dplyr::across(dplyr::contains("norm"), function(x){floor(x*10)})) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("item_"),
                      names_prefix = "item_",
                      names_to = "item",
                      values_to = "value") |>
  dplyr::bind_rows(data.frame(
    timepoint = seq(as.Date("2022-03-01"), as.Date("2022-10-31"), by = "days"),
    item = "missing_norm_missing",
    value = rnorm(n = 245, mean = 10)
  )) |>
  dplyr::bind_rows(data.frame(
    timepoint = c(as.Date("2022-02-01"), as.Date("2022-05-30"), as.Date("2022-10-06"), as.Date("2022-11-01")),
    item = "sparse_1",
    value = c(1, 5, 3, 1)
  )) |>
  dplyr::bind_rows(data.frame(
    timepoint = c(as.Date("2022-04-01"), as.Date("2022-05-30"), as.Date("2022-09-06"), as.Date("2022-12-01")),
    item = "sparse_2",
    value = c(2, 3, 7, 4)
  ))

usethis::use_data(example_data, overwrite = TRUE)
