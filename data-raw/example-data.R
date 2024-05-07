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
               item_ascending = 100 + cumsum(floor(rnorm(n = 365, mean = 5, sd = 2))),
               item_descending = 1000 - cumsum(floor(rnorm(n = 365, mean = 2, sd = 1))),
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
  )) |>
  dplyr::mutate(group = dplyr::case_when(grepl("norm", item) ~ "norm",
                                         grepl("sparse", item) ~ "sparse",
                                         TRUE ~ "other"))

usethis::use_data(example_data, overwrite = TRUE)

example_prescription_numbers <-
  data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "days"),
             group = "SITE1",
             item_Coamoxiclav = c(rpois(n = 300, lambda = 50), rpois(n = 65, lambda = 50) + cumsum(floor(rnorm(n = 65, mean = 2, sd = 2)))),
             item_Gentamicin = rpois(n = 365, lambda = 35),
             item_Ceftriaxone = c(rpois(n = 100, lambda = 30), rpois(n = 265, lambda = 10)),
             item_Metronidazole = rpois(n = 365, lambda = 20),
             item_Meropenem = c(rpois(n = 365, lambda = 9)),
             item_Vancomycin = c(rpois(n = 165, lambda = 0.5), rep(NA, 100), rpois(n = 100, lambda = 0.5)),
             item_Clarithromycin = rpois(n = 365, lambda = 7),
             item_Linezolid = rpois(n = 365, lambda = 0.1),
             item_Amikacin = rpois(n = 365, lambda = 1),
             stringsAsFactors = FALSE) |>
  dplyr::bind_rows(data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "days"),
             group = "SITE2",
             item_Coamoxiclav = rep(NA, 365),
             item_Gentamicin = rep(NA, 365),
             item_Ceftriaxone = rep(NA, 365),
             item_Metronidazole = rep(NA, 365),
             item_Meropenem = rpois(n = 365, lambda = 5),
             item_Vancomycin = rep(NA, 365),
             item_Clarithromycin = rpois(n = 365, lambda = 7),
             item_Linezolid = rpois(n = 365, lambda = 0.1),
             item_Amikacin = rep(NA, 365),
             stringsAsFactors = FALSE)) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("item_"),
                      names_prefix = "item_",
                      names_to = "item",
                      values_to = "value") |>
  dplyr::select(PrescriptionDate = timepoint, Antibiotic = item, NumberOfPrescriptions = value, Location = group)

usethis::use_data(example_prescription_numbers, overwrite = TRUE)
