test_that("prepare_table() avoids min/max warnings when all values in a group are NA", {
  df <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
               item_zero = rep(0, 10),
               item_na = rep(NA, 10),
               stringsAsFactors = FALSE) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("item_"),
                      names_prefix = "item_",
                      names_to = "item",
                      values_to = "value")

  inputspec <- inputspec(timepoint_col = "timepoint",
                         item_col = "item",
                         value_col = "value")

  prepared_df <- prepare_df(df,
                            inputspec = inputspec)

  expect_no_warning(
    prepare_table(prepared_df,
                  inputspec = inputspec)
    )

})

test_that("validate_df_to_inputspec() checks that duplicate column names in data not allowed", {
  df <- cbind(data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
                   item = rep(1, 10),
                   value = rep(3, 10),
                   stringsAsFactors = FALSE),
              data.frame(item = rep(2, 10),
                         stringsAsFactors = FALSE)
              )

  inputspec <- inputspec(timepoint_col = "timepoint",
                     item_col = "item",
                     value_col = "value")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})

test_that("validate_df_to_inputspec() checks that duplicate column names in inputspec not allowed", {
  df <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
                   item = rep(1, 10),
                   value = rep(3, 10),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                     item_col = "item",
                     value_col = "item")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  inputspec <- inputspec(timepoint_col = "timepoint",
                         item_col = c("item", "value"),
                         value_col = "value")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

})

test_that("validate_df_to_inputspec() checks that supplied colnames are present in df", {
  df <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
                   item = rep(1, 10),
                   value = rep(3, 10),
                   group = rep("a", 10),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint1",
                     item_col = "item1",
                     value_col = "value1")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  inputspec <- inputspec(timepoint_col = "timepoint",
                         item_col = c("item", "group1"),
                         value_col = "value",
                         tab_col = "group1")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  inputspec <- inputspec(timepoint_col = "timepoint",
                         item_col = c("item", "group1"),
                         value_col = "value")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

})


test_that("validate_df_to_inputspec() checks that timepoint_col is a datetime type", {
  df_orig <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
                   item = rep(1, 10),
                   value = rep(3, 10),
                   stringsAsFactors = FALSE)
  df <- df_orig

  inputspec <- inputspec(timepoint_col = "timepoint",
                     item_col = "item",
                     value_col = "value")

  # good types
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  df$timepoint <- as.POSIXct(df_orig$timepoint)
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  df$timepoint <- as.POSIXlt(df_orig$timepoint)
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  df$timepoint <- as.character(df_orig$timepoint)
  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  df$timepoint <- as.numeric(df_orig$timepoint)
  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

})

test_that("validate_df_to_inputspec() checks that value_col is a numeric type", {
  df_orig <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
                   item = rep(1, 10),
                   value = rep(3, 10),
                   stringsAsFactors = FALSE)
  df <- df_orig

  inputspec <- inputspec(timepoint_col = "timepoint",
                     item_col = "item",
                     value_col = "value")

  # good types
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  df$value <- as.integer(df_orig$value)
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  df$value <- as.character(df_orig$value)
  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  df$value <- as.Date(df_orig$value)
  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

})

test_that("validate_df_to_inputspec() checks that timepoint column doesn't contain NAs", {
  df <- data.frame(timepoint = c(seq(as.Date("2022-01-01"), as.Date("2022-01-09"), by = "days"), NA),
                   item = rep(1, 10),
                   value = rep(3, 10),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                     item_col = "item",
                     value_col = "value")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})

test_that("validate_df_to_inputspec() checks that item column can contain NA values", {
  df <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
                   item = c(NA, NA, NA, rep(1, 7)),
                   value = rep(3, 10),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                     item_col = "item",
                     value_col = "value")
mantis_report(df,inputspec)
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
      )
    )
})

test_that("validate_df_to_inputspec() checks that item column doesn't contain both NA values and 'NA' strings", {
  df <- data.frame(timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
                   item = c(NA, NA, "NA", rep(1, 7)),
                   value = rep(3, 10),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                     item_col = "item",
                     value_col = "value")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that duplicate timepoint-item combinations not allowed", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 5),
                   item = c(rep("a", 20), rep("b", 10), rep("c", 20)),
                   value = rep(3, 50),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                     item_col = "item",
                     value_col = "value")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

})

test_that("validate_df_to_inputspec() checks that duplicate timepoint-item-group combinations not allowed", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 5),
                   item = c(rep("a", 20), rep("b", 10), rep("c", 20)),
                   value = rep(3, 50),
                   group = c(rep("G1", 10), rep("G2", 10), rep("G1", 10), rep("G2", 20)),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                         item_col = c("item", "group"),
                         value_col = "value",
                         tab_col = "group")

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

})

test_that("validate_df_to_inputspec() allows duplicate timepoint-item combinations if timepoint-item-group combinations are unique", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 5),
                   item = c(rep("a", 20), rep("b", 10), rep("c", 20)),
                   value = rep(3, 50),
                   group = c(rep("G1", 10), rep("G2", 10), rep("G1", 10), rep("G1", 10), rep("G2", 10)),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                         item_col = c("item", "group"),
                         value_col = "value",
                         tab_col = "group")

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

})

test_that("history_to_list() doesn't convert xts date indexes to datetime indexes", {
  # daylight savings time conversions can move timepoint values to preceding day in the summer months
  xtslist <- history_to_list(value_for_history = c(1, 5),
                  timepoint = c(as.Date("2022-03-01"), as.Date("2022-04-01")),
                  plot_value_type = "value")

  expect_equal(
    xts::tclass(xtslist[[1]]), "Date"
  )
})


test_that("prepare_table() keeps original item order if sort_by not provided", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("c", 10), rep("b", 10), rep("a", 10)),
                   value = c(rep(3, 20), rep(1, 10)),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                         item_col = "item",
                         value_col = "value")

  prepared_df <- prepare_df(df,
                            inputspec = inputspec)

  prepared_table <- prepare_table(prepared_df = prepared_df,
                                  inputspec = inputspec,
                                  sort_by = NULL)

  expect_equal(prepared_table |>
                 dplyr::pull(.data[[prepared_df_item_cols(inputspec$item_col)]]),
               c("c", "b", "a"))

})

test_that("prepare_table() sorts by sort_by then original item_order", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("c", 10), rep("b", 10), rep("a", 10)),
                   value = c(rep(3, 20), rep(1, 10)),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                         item_col = "item",
                         value_col = "value")

  prepared_df <- prepare_df(df,
                            inputspec = inputspec)

  prepared_table <- prepare_table(prepared_df = prepared_df,
                                  inputspec = inputspec,
                                  sort_by = c("max_value"))

  expect_equal(prepared_table |>
                 dplyr::pull(.data[[prepared_df_item_cols(inputspec$item_col)]]),
               c("a", "c", "b"))

})

test_that("prepare_table() sorts by descending sort_by", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("c", 10), rep("b", 10), rep("a", 10)),
                   value = c(rep(3, 10), rep(4, 10), rep(1, 10)),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(timepoint_col = "timepoint",
                         item_col = "item",
                         value_col = "value")

  prepared_df <- prepare_df(df,
                            inputspec = inputspec)

  prepared_table <- prepare_table(prepared_df = prepared_df,
                                  inputspec = inputspec,
                                  sort_by = c("-max_value"))

  expect_equal(prepared_table |>
                 dplyr::pull(.data[[prepared_df_item_cols(inputspec$item_col)]]),
               c("b", "c", "a"))

})
