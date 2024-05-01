test_that("output_table_interactive() avoids min/max warnings when all values are NA", {
  df <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
                   item = "na",
                   value = rep(NA, 10),
                   stringsAsFactors = FALSE)

  table <- prepare_df(df,
                      timepoint_col = "timepoint",
                      item_col = "item",
                      value_col = "value") %>%
    prepare_table(plot_value_type = "value")

  expect_no_warning(output_table_interactive(table = table,
                                      item_label = "Item"))

})

test_that("output_table_interactive() avoids min/max warnings when all deltas are NA", {
  df <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
                   item = "sparse",
                   value = c(rep(NA, 5), 1, rep(NA, 4)),
                   stringsAsFactors = FALSE)

  table <- prepare_df(df,
                      timepoint_col = "timepoint",
                      item_col = "item",
                      value_col = "value") %>%
    prepare_table(plot_value_type = "delta")

  expect_no_warning(output_table_interactive(table = table,
                                      item_label = "Item"))

})
