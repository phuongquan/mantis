test_that("prepare_table() avoids min/max warnings when all values in a group are NA", {
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item_zero = rep(0, 10),
    item_na = rep(NA, 10),
    stringsAsFactors = FALSE
  ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("item_"),
      names_prefix = "item_",
      names_to = "item",
      values_to = "value"
    )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  prepared_df <- prepare_df(df, inputspec = inputspec)

  expect_no_warning(
    prepare_table(prepared_df, inputspec = inputspec)
  )
})

test_that("validate_df_to_inputspec() checks that duplicate column names in data not allowed", {
  df <- cbind(
    data.frame(
      timepoint = seq(
        as.Date("2022-01-01"),
        as.Date("2022-01-10"),
        by = "days"
      ),
      item = rep(1, 10),
      value = rep(3, 10),
      stringsAsFactors = FALSE
    ),
    data.frame(item = rep(2, 10), stringsAsFactors = FALSE)
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})

test_that("validate_df_to_inputspec() checks that duplicate column names in inputspec not allowed", {
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "item"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = c("item", "value"),
    value_col = "value"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})

test_that("validate_df_to_inputspec() checks that supplied colnames are present in df", {
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = rep(1, 10),
    value = rep(3, 10),
    group = rep("a", 10),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint1",
    item_cols = "item1",
    value_col = "value1"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = c("item", "group1"),
    value_col = "value",
    tab_col = "group1"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = c("item", "group1"),
    value_col = "value"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that timepoint_col is a datetime type", {
  df_orig <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )
  df <- df_orig

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

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
  df_orig <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )
  df <- df_orig

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

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
  df <- data.frame(
    timepoint = c(
      seq(as.Date("2022-01-01"), as.Date("2022-01-09"), by = "days"),
      NA
    ),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that timepoint_col matches inputspec timepoint_unit of 'day'", {
  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value",
    timepoint_unit = "day"
  )

  # good types
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # with time portion (POSIXct)
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2022-01-01 12:00:00"),
      as.POSIXct("2022-01-10 12:00:00"),
      by = "days"
    ),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # with time portion (POSIXlt)
  df <- data.frame(
    timepoint = seq(
      as.POSIXlt("2022-01-01 12:00:00"),
      as.POSIXlt("2022-01-10 12:00:00"),
      by = "days"
    ),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  # different time each day
  df <- data.frame(
    timepoint = c(
      as.POSIXlt("2022-01-01 12:00:00"),
      as.POSIXlt("2022-01-02 13:00:00"),
      as.POSIXlt("2022-01-10 16:00:00")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that timepoint_col matches inputspec timepoint_unit of 'week'", {
  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value",
    timepoint_unit = "week"
  )

  # good types
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-03-05"), by = "week"),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # with gaps
  df <- data.frame(
    timepoint = c(
      as.Date("2022-01-01"),
      as.Date("2022-01-15"),
      as.Date("2022-01-22")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # with time portion
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2022-01-01 12:00:00"),
      as.POSIXct("2022-03-05 12:00:00"),
      by = "week"
    ),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  # different time each day
  df <- data.frame(
    timepoint = c(
      as.POSIXlt("2022-01-01 12:00:00"),
      as.POSIXlt("2022-01-08 13:00:00"),
      as.POSIXlt("2022-01-15 16:00:00")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  # nonweekly
  df <- data.frame(
    timepoint = c(
      as.Date("2022-01-01"),
      as.Date("2022-01-09"),
      as.Date("2022-01-15")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that timepoint_col matches inputspec timepoint_unit of 'month'", {
  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value",
    timepoint_unit = "month"
  )

  # good types
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-02"), as.Date("2022-10-02"), by = "month"),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # with time portion
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2022-01-02 12:00:00"),
      as.POSIXct("2022-10-02 12:00:00"),
      by = "month"
    ),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  # different time each day
  df <- data.frame(
    timepoint = c(
      as.POSIXlt("2022-01-01 12:00:00"),
      as.POSIXlt("2022-02-01 13:00:00"),
      as.POSIXlt("2022-03-01 16:00:00")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  # nonmonthly
  df <- data.frame(
    timepoint = c(
      as.Date("2022-01-01"),
      as.Date("2022-01-09"),
      as.Date("2022-01-15")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that timepoint_col matches inputspec timepoint_unit of 'quarter'", {
  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value",
    timepoint_unit = "quarter"
  )

  # good types
  df <- data.frame(
    timepoint = seq(
      as.Date("2022-01-02"),
      as.Date("2024-04-02"),
      by = "quarter"
    ),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # with time portion
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2022-01-02 12:00:00"),
      as.POSIXct("2024-04-02 12:00:00"),
      by = "quarter"
    ),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  # different time each day
  df <- data.frame(
    timepoint = c(
      as.POSIXlt("2022-01-01 12:00:00"),
      as.POSIXlt("2022-04-01 13:00:00"),
      as.POSIXlt("2022-07-01 16:00:00")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  # nonquarterly
  df <- data.frame(
    timepoint = c(
      as.Date("2022-01-01"),
      as.Date("2022-05-02"),
      as.Date("2022-06-01")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that timepoint_col matches inputspec timepoint_unit of 'year'", {
  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value",
    timepoint_unit = "year"
  )

  # good types
  df <- data.frame(
    timepoint = seq(as.Date("2013-01-02"), as.Date("2022-01-02"), by = "year"),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # with time portion
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2013-01-02 12:00:00"),
      as.POSIXct("2022-01-02 12:00:00"),
      by = "year"
    ),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  # different time each day
  df <- data.frame(
    timepoint = c(
      as.POSIXlt("2022-01-01 12:00:00"),
      as.POSIXlt("2023-01-01 13:00:00"),
      as.POSIXlt("2024-01-01 16:00:00")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  # nonyearly
  df <- data.frame(
    timepoint = c(
      as.Date("2022-01-01"),
      as.Date("2022-01-09"),
      as.Date("2022-01-15")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that timepoint_col matches inputspec timepoint_unit of 'hour'", {
  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value",
    timepoint_unit = "hour"
  )

  # good types
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2022-01-02 12:00:00"),
      as.POSIXct("2022-01-12 12:00:00"),
      by = "hour"
    ),
    item = rep(1, 241),
    value = rep(3, 241),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  # different time each hour
  df <- data.frame(
    timepoint = c(
      as.POSIXlt("2022-01-01 12:00:00"),
      as.POSIXlt("2022-01-01 13:01:00"),
      as.POSIXlt("2022-01-01 16:00:30")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that timepoint_col matches inputspec timepoint_unit of 'min'", {
  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value",
    timepoint_unit = "min"
  )

  # good types
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2022-01-02 12:00:00"),
      as.POSIXct("2022-01-03 12:00:00"),
      by = "min"
    ),
    item = rep(1, 1441),
    value = rep(3, 1441),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  # different second each minute
  df <- data.frame(
    timepoint = c(
      as.POSIXlt("2022-01-01 12:00:00"),
      as.POSIXlt("2022-01-01 13:01:00"),
      as.POSIXlt("2022-01-01 16:00:30")
    ),
    item = rep(1, 3),
    value = rep(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that timepoint_col matches inputspec timepoint_unit of 'sec'", {
  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value",
    timepoint_unit = "sec"
  )

  # good types
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2022-01-02 12:00:00"),
      as.POSIXct("2022-01-02 13:00:00"),
      by = "sec"
    ),
    item = rep(1, 3601),
    value = rep(3, 3601),
    stringsAsFactors = FALSE
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # bad types
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = rep(1, 10),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that item column can contain NA values", {
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = c(NA, NA, NA, rep(1, 7)),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )
})

test_that("validate_df_to_inputspec() checks that item column doesn't contain both NA values and 'NA' strings", {
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = c(NA, NA, "NA", rep(1, 7)),
    value = rep(3, 10),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})


test_that("validate_df_to_inputspec() checks that duplicate timepoint-item combinations not allowed", {
  # single item_col
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      5
    ),
    item = c(rep("a", 20), rep("b", 10), rep("c", 20)),
    value = rep(3, 50),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  # multi-item_cols
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      5
    ),
    item = c(rep("a", 20), rep("b", 10), rep("c", 20)),
    value = rep(3, 50),
    group = c(rep("G1", 10), rep("G2", 10), rep("G1", 10), rep("G2", 20)),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = c("item", "group"),
    value_col = "value"
  )

  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )
})

test_that("validate_df_to_inputspec() allows duplicate timepoint-item combinations if timepoint-item-group combinations are unique", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      5
    ),
    item = c(rep("a", 20), rep("b", 10), rep("c", 20)),
    value = rep(3, 50),
    group = c(
      rep("G1", 10),
      rep("G2", 10),
      rep("G1", 10),
      rep("G1", 10),
      rep("G2", 10)
    ),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = c("item", "group"),
    value_col = "value",
    tab_col = "group"
  )

  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )
})

test_that("history_to_list() doesn't convert xts date indexes to datetime indexes", {
  # daylight savings time conversions can move timepoint values to preceding day in the summer months
  xtslist <- history_to_list(
    value_for_history = c(1, 5),
    timepoint = c(as.Date("2022-03-01"), as.Date("2022-04-01")),
    plot_value_type = "value"
  )

  expect_equal(
    xts::tclass(xtslist[[1]]),
    "Date"
  )
})


test_that("prepare_table() keeps original item order if sort_by not provided", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      3
    ),
    item = c(rep("c", 10), rep("b", 10), rep("a", 10)),
    value = c(rep(3, 20), rep(1, 10)),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  prepared_df <- prepare_df(df, inputspec = inputspec)

  prepared_table <- prepare_table(
    prepared_df = prepared_df,
    inputspec = inputspec,
    sort_by = NULL
  )

  expect_equal(
    prepared_table |>
      dplyr::pull(.data[[item_cols_prefix(inputspec$item_cols)]]),
    c("c", "b", "a")
  )
})

test_that("prepare_table() sorts by sort_by then original item_order", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      3
    ),
    item = c(rep("c", 10), rep("b", 10), rep("a", 10)),
    value = c(rep(3, 20), rep(1, 10)),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  prepared_df <- prepare_df(df, inputspec = inputspec)

  prepared_table <- prepare_table(
    prepared_df = prepared_df,
    inputspec = inputspec,
    sort_by = c("max_value")
  )

  expect_equal(
    prepared_table |>
      dplyr::pull(.data[[item_cols_prefix(inputspec$item_cols)]]),
    c("a", "c", "b")
  )
})

test_that("prepare_table() sorts by descending sort_by", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      3
    ),
    item = c(rep("c", 10), rep("b", 10), rep("a", 10)),
    value = c(rep(3, 10), rep(4, 10), rep(1, 10)),
    stringsAsFactors = FALSE
  )

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  prepared_df <- prepare_df(df, inputspec = inputspec)

  prepared_table <- prepare_table(
    prepared_df = prepared_df,
    inputspec = inputspec,
    sort_by = c("-max_value")
  )

  expect_equal(
    prepared_table |>
      dplyr::pull(.data[[item_cols_prefix(inputspec$item_cols)]]),
    c("b", "c", "a")
  )
})

test_that("arrange_items() sorts by single item_order", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-09"), by = "days"),
      3
    ),
    item = c(rep("c", 9), rep("b", 9), rep("a", 9)),
    item2 = rep(c("z", "x", "y"), 9),
    value = rep(3, 27),
    stringsAsFactors = FALSE
  )

  expect_equal(
    arrange_items(df, item_order = list("item" = TRUE)) |>
      dplyr::pull(item) |>
      unique(),
    c("a", "b", "c")
  )

  expect_equal(
    arrange_items(df, item_order = list("item" = "b")) |>
      dplyr::pull(item) |>
      unique(),
    c("b", "a", "c")
  )
})

test_that("arrange_items() sorts by two item_orders", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-09"), by = "days"),
      3
    ),
    item = c(rep("c", 9), rep("b", 9), rep("a", 9)),
    item2 = rep(c("z", "x", "y"), 9),
    value = rep(3, 27),
    stringsAsFactors = FALSE
  )

  expect_equal(
    arrange_items(df, item_order = list("item" = TRUE, "item2" = TRUE)) |>
      dplyr::select(item, item2),
    data.frame(
      item = c(rep("a", 9), rep("b", 9), rep("c", 9)),
      item2 = rep(c(rep("x", 3), rep("y", 3), rep("z", 3)), 3)
    )
  )

  expect_equal(
    arrange_items(df, item_order = list("item2" = TRUE, "item" = TRUE)) |>
      dplyr::select(item, item2),
    data.frame(
      item = rep(c(rep("a", 3), rep("b", 3), rep("c", 3)), 3),
      item2 = c(rep("x", 9), rep("y", 9), rep("z", 9))
    )
  )

  expect_equal(
    arrange_items(df, item_order = list("item2" = TRUE, "item" = "b")) |>
      dplyr::select(item, item2),
    data.frame(
      item = rep(c(rep("b", 3), rep("a", 3), rep("c", 3)), 3),
      item2 = c(rep("x", 9), rep("y", 9), rep("z", 9))
    )
  )

  expect_equal(
    arrange_items(
      df,
      item_order = list("item2" = TRUE, "item" = c("b", "c"))
    ) |>
      dplyr::select(item, item2),
    data.frame(
      item = rep(c(rep("b", 3), rep("c", 3), rep("a", 3)), 3),
      item2 = c(rep("x", 9), rep("y", 9), rep("z", 9))
    )
  )

  expect_equal(
    arrange_items(
      df,
      item_order = list("item2" = c("x", "z"), "item" = c("b", "c"))
    ) |>
      dplyr::select(item, item2),
    data.frame(
      item = rep(c(rep("b", 3), rep("c", 3), rep("a", 3)), 3),
      item2 = c(rep("x", 9), rep("z", 9), rep("y", 9))
    )
  )
})
