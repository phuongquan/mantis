test_that("prepare_table() avoids min/max warnings when all values in a group are NA", {
  df <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
               item_zero = rep(0, 10),
               item_na = rep(NA, 10),
               stringsAsFactors = FALSE) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("item_"),
                      names_prefix = "item_",
                      names_to = "item",
                      values_to = "value")

  expect_no_warning(prepare_table(df,
                         timepoint_col = "timepoint",
                         item_col = "item",
                         value_col = "value"))

})
