test_that("prepare_table() avoids min/max warnings when all values in a group are NA", {
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item_zero = 0,
    item_na = NA,
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
      item = 1,
      value = 3,
      stringsAsFactors = FALSE
    ),
    data.frame(item = 2, stringsAsFactors = FALSE)
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
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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

  df$timepoint <- as.POSIXct(df_orig$timepoint, tz = "UTC")
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  df$timepoint <- as.POSIXlt(df_orig$timepoint, tz = "UTC")
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
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
      as.POSIXct("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2022-01-10 12:00:00", tz = "UTC"),
      by = "days"
    ),
    item = 1,
    value = 3,
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
      as.POSIXlt("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXlt("2022-01-10 12:00:00", tz = "UTC"),
      by = "days"
    ),
    item = 1,
    value = 3,
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
      as.POSIXlt("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXlt("2022-01-02 13:00:00", tz = "UTC"),
      as.POSIXlt("2022-01-10 16:00:00", tz = "UTC")
    ),
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
      as.POSIXct("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2022-03-05 12:00:00", tz = "UTC"),
      by = "week"
    ),
    item = 1,
    value = 3,
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
      as.POSIXlt("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXlt("2022-01-08 13:00:00", tz = "UTC"),
      as.POSIXlt("2022-01-15 16:00:00", tz = "UTC")
    ),
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
      as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
      as.POSIXct("2022-10-02 12:00:00", tz = "UTC"),
      by = "month"
    ),
    item = 1,
    value = 3,
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
      as.POSIXlt("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXlt("2022-02-01 13:00:00", tz = "UTC"),
      as.POSIXlt("2022-03-01 16:00:00", tz = "UTC")
    ),
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
      as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
      as.POSIXct("2024-04-02 12:00:00", tz = "UTC"),
      by = "quarter"
    ),
    item = 1,
    value = 3,
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
      as.POSIXlt("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXlt("2022-04-01 13:00:00", tz = "UTC"),
      as.POSIXlt("2022-07-01 16:00:00", tz = "UTC")
    ),
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
      as.POSIXct("2013-01-02 12:00:00", tz = "UTC"),
      as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
      by = "year"
    ),
    item = 1,
    value = 3,
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
      as.POSIXlt("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXlt("2023-01-01 13:00:00", tz = "UTC"),
      as.POSIXlt("2024-01-01 16:00:00", tz = "UTC")
    ),
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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
      as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
      as.POSIXct("2022-01-12 12:00:00", tz = "UTC"),
      by = "hour"
    ),
    item = 1,
    value = 3,
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
      as.POSIXlt("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXlt("2022-01-01 13:01:00", tz = "UTC"),
      as.POSIXlt("2022-01-01 16:00:30", tz = "UTC")
    ),
    item = 1,
    value = 3,
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
      as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
      as.POSIXct("2022-01-03 12:00:00", tz = "UTC"),
      by = "min"
    ),
    item = 1,
    value = 3,
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
      as.POSIXlt("2022-01-01 12:00:00", tz = "UTC"),
      as.POSIXlt("2022-01-01 13:01:00", tz = "UTC"),
      as.POSIXlt("2022-01-01 16:00:30", tz = "UTC")
    ),
    item = 1,
    value = 3,
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
      as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
      as.POSIXct("2022-01-02 13:00:00", tz = "UTC"),
      by = "sec"
    ),
    item = 1,
    value = 3,
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
    item = 1,
    value = 3,
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


test_that("validate_df_to_inputspec() checks that timepoint_col values are consistently in one timezone", {
  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value",
    timepoint_unit = "day"
  )

  # sequences are good, with or without explicit tz
  df <- data.frame(
    timepoint = seq(
      as.Date("2022-03-21"),
      as.Date("2022-04-01"),
      by = "day"
    ),
    item = 1,
    value = 3,
    stringsAsFactors = FALSE
  )
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # sequences are good, even without explicit tz
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2022-03-21"),
      as.POSIXct("2022-04-01"),
      by = "hour"
    ),
    item = 1,
    value = 3,
    stringsAsFactors = FALSE
  )
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value",
        timepoint_unit = "hour"
      )
    )
  )

  # this prints out with different timezones but the underlying values are consistent
  df <- data.frame(
    timepoint = seq(
      as.POSIXct("2022-03-21", tz = "Europe/London"),
      as.POSIXct("2022-04-01", tz = "Europe/London"),
      by = "day"
    ),
    item = 1,
    value = 3,
    stringsAsFactors = FALSE
  )
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # individual dates within a fixed timezone are good
  df <- data.frame(
    timepoint = c(
      as.POSIXct("2022-03-11", tz = "UTC"),
      as.POSIXct("2022-03-21", tz = "UTC"),
      as.POSIXct("2022-04-01", tz = "UTC"),
      as.POSIXct("2022-04-05", tz = "UTC")
    ),
    item = 1,
    value = 3,
    stringsAsFactors = FALSE
  )
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # individual times within a fixed timezone are good
  df <- data.frame(
    timepoint = c(
      as.POSIXct("2022-03-11 12:00:00", tz = "UTC"),
      as.POSIXct("2022-03-21 12:00:00", tz = "UTC"),
      as.POSIXct("2022-04-01 12:00:00", tz = "UTC"),
      as.POSIXct("2022-04-05 12:00:00", tz = "UTC")
    ),
    item = 1,
    value = 3,
    stringsAsFactors = FALSE
  )
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    )
  )

  # individual dates in a timezone with daylight savings are bad
  df <- data.frame(
    timepoint = c(
      as.POSIXct("2022-03-11", tz = "Europe/London"),
      as.POSIXct("2022-03-21", tz = "Europe/London"),
      as.POSIXct("2022-04-01", tz = "Europe/London"),
      as.POSIXct("2022-04-05", tz = "Europe/London")
    ),
    item = 1,
    value = 3,
    stringsAsFactors = FALSE
  )
  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  # individual times in a timezone with daylight savings are bad
  df <- data.frame(
    timepoint = c(
      as.POSIXct("2022-03-11 12:00:00", tz = "Europe/London"),
      as.POSIXct("2022-03-21 12:00:00", tz = "Europe/London"),
      as.POSIXct("2022-04-01 12:00:00", tz = "Europe/London"),
      as.POSIXct("2022-04-05 12:00:00", tz = "Europe/London")
    ),
    item = 1,
    value = 3,
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


test_that("validate_df_to_inputspec() checks that item column(s) can contain NA values", {
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = c(NA, NA, NA, rep(1, 7)),
    item2 = c(NA, NA, NA, rep(1, 7)),
    value = 3,
    stringsAsFactors = FALSE
  )

  # single item_col
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      )
    )
  )

  # multi item_cols
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "item2"),
        value_col = "value"
      )
    )
  )

})

test_that("validate_df_to_inputspec() checks that item column(s) don't contain both NA values and 'NA' strings", {
  df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item = c(NA, NA, "NA", rep(1, 7)),
    item2 = c(NA, NA, NA, rep(1, 7)),
    item3 = c("NA", "NA", "NA", rep(1, 7)),
    value = 3,
    stringsAsFactors = FALSE
  )

  # single item_col with NA and "NA"
  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      )
    ),
    class = "invalid_data"
  )

  # multi item_cols with NA and "NA"
  expect_error(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "item2"),
        value_col = "value"
      )
    ),
    class = "invalid_data"
  )

  # multi item_cols with NA and "NA" exclusively in separate cols
  expect_silent(
    validate_df_to_inputspec(
      df = df,
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item3", "item2"),
        value_col = "value"
      )
    )
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
    value = 3,
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
    value = 3,
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
    value = 3,
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
    value = 3,
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
    value = 3,
    stringsAsFactors = FALSE
  )

  expect_equal(
    arrange_items(df, item_order = list("item" = TRUE, "item2" = TRUE)) |>
      dplyr::select(item, item2),
    data.frame(
      item = c(rep("a", 9), rep("b", 9), rep("c", 9)),
      item2 = rep(c(rep("x", 3), rep("y", 3), rep("z", 3)), 3)
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    arrange_items(df, item_order = list("item2" = TRUE, "item" = TRUE)) |>
      dplyr::select(item, item2),
    data.frame(
      item = rep(c(rep("a", 3), rep("b", 3), rep("c", 3)), 3),
      item2 = c(rep("x", 9), rep("y", 9), rep("z", 9))
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    arrange_items(df, item_order = list("item2" = TRUE, "item" = "b")) |>
      dplyr::select(item, item2),
    data.frame(
      item = rep(c(rep("b", 3), rep("a", 3), rep("c", 3)), 3),
      item2 = c(rep("x", 9), rep("y", 9), rep("z", 9))
    ),
    ignore_attr = TRUE
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
    ),
    ignore_attr = TRUE
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
    ),
    ignore_attr = TRUE
  )
})


test_that("adjust_timepoint_limit() moves supplied limit appropriately for months", {
  # min_timepoint before first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "month", length.out = 5),
      timepoint_unit = "month",
      limit_type = "min"
    ),
    as.Date("2022-01-20")
  )

  # min_timepoint at first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-20"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "month", length.out = 5),
      timepoint_unit = "month",
      limit_type = "min"
    ),
    as.Date("2022-01-20")
  )

  # min_timepoint between timepoint_values with gaps
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-05-01"),
      timepoint_values = c(as.Date("2022-01-20"), as.Date("2023-01-20")),
      timepoint_unit = "month",
      limit_type = "min"
    ),
    as.Date("2022-05-20")
  )

  # min_timepoint after last timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2023-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "month", length.out = 5),
      timepoint_unit = "month",
      limit_type = "min"
    ),
    as.Date("2023-01-20")
  )

  # max_timepoint after last timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2023-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "month", length.out = 5),
      timepoint_unit = "month",
      limit_type = "max"
    ),
    as.Date("2022-12-20")
  )

  # max_timepoint between timepoint_values with gaps
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-05-01"),
      timepoint_values = c(as.Date("2022-01-20"), as.Date("2023-01-20")),
      timepoint_unit = "month",
      limit_type = "max"
    ),
    as.Date("2022-04-20")
  )

  # max_timepoint before first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "month", length.out = 5),
      timepoint_unit = "month",
      limit_type = "max"
    ),
    as.Date("2021-12-20")
  )

})

test_that("adjust_timepoint_limit() moves supplied limit appropriately for weeks", {
  # min_timepoint before first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "week", length.out = 5),
      timepoint_unit = "week",
      limit_type = "min"
    ),
    as.Date("2022-01-06")
  )

  # min_timepoint between timepoint_values with gaps
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-02-01"),
      timepoint_values = c(as.Date("2022-01-20"), as.Date("2022-02-17")),
      timepoint_unit = "week",
      limit_type = "min"
    ),
    as.Date("2022-02-03")
  )

  # min_timepoint after last timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-02-20"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "week", length.out = 5),
      timepoint_unit = "week",
      limit_type = "min"
    ),
    as.Date("2022-02-24")
  )

  # max_timepoint after last timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-02-20"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "week", length.out = 5),
      timepoint_unit = "week",
      limit_type = "max"
    ),
    as.Date("2022-02-17")
  )

  # max_timepoint between timepoint_values with gaps
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-02-01"),
      timepoint_values = c(as.Date("2022-01-20"), as.Date("2022-02-17")),
      timepoint_unit = "week",
      limit_type = "max"
    ),
    as.Date("2022-01-27")
  )

  # max_timepoint before first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "week", length.out = 5),
      timepoint_unit = "week",
      limit_type = "max"
    ),
    as.Date("2021-12-30")
  )

})

test_that("adjust_timepoint_limit() moves supplied limit appropriately for years", {
  # min_timepoint before first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "year", length.out = 5),
      timepoint_unit = "year",
      limit_type = "min"
    ),
    as.Date("2022-01-20")
  )

  # min_timepoint at first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-20"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "year", length.out = 5),
      timepoint_unit = "year",
      limit_type = "min"
    ),
    as.Date("2022-01-20")
  )

  # min_timepoint between timepoint_values with gaps
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-05-01"),
      timepoint_values = c(as.Date("2022-01-20"), as.Date("2027-01-20")),
      timepoint_unit = "year",
      limit_type = "min"
    ),
    as.Date("2023-01-20")
  )

  # min_timepoint after last timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2029-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "year", length.out = 5),
      timepoint_unit = "year",
      limit_type = "min"
    ),
    as.Date("2029-01-20")
  )

  # max_timepoint after last timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2029-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "year", length.out = 5),
      timepoint_unit = "year",
      limit_type = "max"
    ),
    as.Date("2028-01-20")
  )

  # max_timepoint between timepoint_values with gaps
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2023-05-01"),
      timepoint_values = c(as.Date("2022-01-20"), as.Date("2027-01-20")),
      timepoint_unit = "year",
      limit_type = "max"
    ),
    as.Date("2023-01-20")
  )

  # max_timepoint before first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "year", length.out = 5),
      timepoint_unit = "year",
      limit_type = "max"
    ),
    as.Date("2021-01-20")
  )

})

test_that("adjust_timepoint_limit() moves supplied limit appropriately for days", {
  # if everything is a Date then nothing changes
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-01"),
      timepoint_values = seq(as.Date("2022-01-20"), by = "quarter", length.out = 5),
      timepoint_unit = "day",
      limit_type = "max"
    ),
    as.Date("2022-01-01")
  )

  # limit and values are posixct
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.POSIXct("2022-01-02 08:30:00", tz = "UTC"),
      timepoint_values = seq(
        as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
        by = "day",
        length.out = 5
      ),
      timepoint_unit = "day",
      limit_type = "min"
    ),
    as.POSIXct("2022-01-02 12:00:00", tz = "UTC")
  )

  # limit is date, values are posixct
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.Date("2022-01-03"),
      timepoint_values = seq(
        as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
        by = "day",
        length.out = 5
      ),
      timepoint_unit = "day",
      limit_type = "min"
    ),
    as.POSIXct("2022-01-03 12:00:00", tz = "UTC")
  )

  # limit is posixct, values are date
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.POSIXct("2022-01-04 12:00:00", tz = "UTC"),
      timepoint_values = seq(
        as.Date("2022-01-02"),
        by = "day",
        length.out = 5
      ),
      timepoint_unit = "day",
      limit_type = "max"
    ),
    as.Date("2022-01-04")
  )

})

test_that("adjust_timepoint_limit() moves supplied limit appropriately for hours", {
  # min_timepoint before first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.POSIXct("2022-01-02 08:30:00", tz = "UTC"),
      timepoint_values = seq(
        as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
        as.POSIXct("2022-01-03 12:00:00", tz = "UTC"),
        by = "hour"
      ),
      timepoint_unit = "hour",
      limit_type = "min"
    ),
    as.POSIXct("2022-01-02 09:00:00", tz = "UTC")
  )

  # min_timepoint at first timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
      timepoint_values = seq(
        as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
        as.POSIXct("2022-01-03 12:00:00", tz = "UTC"),
        by = "hour"
      ),
      timepoint_unit = "hour",
      limit_type = "min"
    ),
    as.POSIXct("2022-01-02 12:00:00", tz = "UTC")
  )

  # min_timepoint between timepoint_values
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.POSIXct("2022-01-02 15:30:00", tz = "UTC"),
      timepoint_values = seq(
        as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
        as.POSIXct("2022-01-03 12:00:00", tz = "UTC"),
        by = "hour"
      ),
      timepoint_unit = "hour",
      limit_type = "min"
    ),
    as.POSIXct("2022-01-02 16:00:00", tz = "UTC")
  )

  # max_timepoint after last timepoint
  expect_equal(
    adjust_timepoint_limit(
      timepoint_limit = as.POSIXct("2022-01-03 15:30:00", tz = "UTC"),
      timepoint_values = seq(
        as.POSIXct("2022-01-02 12:00:00", tz = "UTC"),
        as.POSIXct("2022-01-03 12:00:00", tz = "UTC"),
        by = "hour"
      ),
      timepoint_unit = "hour",
      limit_type = "max"
    ),
    as.POSIXct("2022-01-03 15:00:00", tz = "UTC")
  )

})

test_that("align_data_timepoints() adjusts max value correctly when timepoint_limits[1] is NA", {
  prepared_df <- data.frame(
    timepoint = seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
    item.item = rep("a", 10),
    value = rep(1, 10),
    stringsAsFactors = FALSE
  )

  # limit is a Date
  aligned_timepoints <-
    align_data_timepoints(prepared_df = prepared_df,
                          inputspec = inputspec(
                            timepoint_col = "timepoint",
                            item_cols = "item",
                            value_col = "value",
                            timepoint_unit = "day"
                          ),
                          timepoint_limits = c(NA, as.Date("2022-01-12"))) |>
    dplyr::pull(timepoint) |>
    sort()

  expect_equal(aligned_timepoints,
               seq(as.POSIXct("2022-01-01", tz = "UTC"),
                   as.POSIXct("2022-01-12", tz = "UTC"),
                   by = "days"))

  # limit is a Posixct
  aligned_timepoints <-
    align_data_timepoints(prepared_df = prepared_df,
                          inputspec = inputspec(
                            timepoint_col = "timepoint",
                            item_cols = "item",
                            value_col = "value",
                            timepoint_unit = "hour"
                          ),
                          timepoint_limits = c(NA, as.POSIXct("2022-01-12", tz = "UTC"))) |>
    dplyr::pull(timepoint) |>
    sort()

  expect_equal(aligned_timepoints,
               seq(as.POSIXct("2022-01-01", tz = "UTC"),
                   as.POSIXct("2022-01-12", tz = "UTC"),
                   by = "hours"))

})
