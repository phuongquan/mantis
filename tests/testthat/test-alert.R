test_that("Valid alert_rules can be specified", {
  expect_s3_class(
    alert_rules(
      alert_missing(),
      alert_equals(rule_value = 0),
      alert_above(rule_value = 0),
      alert_below(rule_value = 0),
      alert_difference_above_perc(current_period = 1,
                                  previous_period = 2,
                                  rule_value = 10),
      alert_difference_below_perc(current_period = 1,
                                  previous_period = 2,
                                  rule_value = 10),
      alert_custom(
        short_name = "my_rule_doubled",
        description = "Last value is over double the first value",
        function_call = quote(rev(value)[1] > 2 * value[1])
      )
    ),
    "mantis_alert_rules"
  )
})

test_that("Invalid alert_rules cannot be specified", {
  expect_error(alert_rules(alert_missing(),
                           quote(rev(value)[1] > 2 * value[1]),
                           "hello"),
               class = "invalid_alert_rules")
})

test_that("alert_missing(all) returns condition correctly", {
  fc <- alert_missing(extent_type = "all")$function_call

  value = rep(NA, 10)
  expect_true(eval(fc))

  value = c(1, rep(NA, 10))
  expect_false(eval(fc))

})

test_that("alert_missing(any) returns condition correctly", {
  fc <- alert_missing(extent_type = "any", extent_value = 3)$function_call

  value = c(1, NA, 2, NA, 3, NA)
  expect_true(eval(fc))

  value = c(1, 2, NA, 3, NA)
  expect_false(eval(fc))

})

test_that("alert_missing(last) returns condition correctly", {
  fc <- alert_missing(extent_type = "last", extent_value = 3)$function_call

  value = c(1, 2, NA, NA, NA)
  expect_true(eval(fc))

  value = c(NA, 1, 2, NA, NA)
  expect_false(eval(fc))

})

test_that("alert_missing(consecutive) returns condition correctly", {
  fc <- alert_missing(extent_type = "consecutive", extent_value = 3)$function_call

  value = c(1, 1, NA, NA, NA, 1)
  expect_true(eval(fc))

  value = c(NA, 1, 1, NA, NA, 1)
  expect_false(eval(fc))

})


test_that("alert_equals(all) returns condition correctly", {
  fc <- alert_equals(extent_type = "all", rule_value = 0)$function_call

  value = rep(0, 10)
  expect_true(eval(fc))

  value = c(1, rep(0, 10))
  expect_false(eval(fc))

})

test_that("alert_equals(any) returns condition correctly", {
  fc <- alert_equals(extent_type = "any", extent_value = 3, rule_value = 0)$function_call

  value = c(1, 0, 2, 0, 3, 0)
  expect_true(eval(fc))

  value = c(1, 2, 0, 3, 0)
  expect_false(eval(fc))

  value = c(NA, 0, NA, 0, NA, 0)
  expect_true(eval(fc))

})

test_that("alert_equals(last) returns condition correctly", {
  fc <- alert_equals(extent_type = "last", extent_value = 3, rule_value = 0)$function_call

  value = c(1, 2, 0, 0, 0)
  expect_true(eval(fc))

  value = c(0, 1, 2, 0, 0)
  expect_false(eval(fc))

  value = c(1, 2, 0, NA, 0, 0)
  expect_true(eval(fc))

  value = c(1, 2, 0, NA, 0)
  expect_false(eval(fc))

  value = c(1, 2, NA, NA, NA)
  expect_false(eval(fc))

})

test_that("alert_equals(consecutive) returns condition correctly", {
  fc <- alert_equals(extent_type = "consecutive", extent_value = 3, rule_value = 0)$function_call

  value = c(1, 2, 0, 0, 0, 1)
  expect_true(eval(fc))

  value = c(0, 1, 1, 0, 0, 1)
  expect_false(eval(fc))

  # any NAs should be skipped
  value = c(0, NA, 0, 0, 1)
  expect_true(eval(fc))

})


test_that("alert_below(all) returns condition correctly", {
  fc <- alert_below(extent_type = "all", rule_value = 3)$function_call

  value = rep(0, 10)
  expect_true(eval(fc))

  value = c(2, rep(0, 10))
  expect_true(eval(fc))

  value = c(3, rep(0, 10))
  expect_false(eval(fc))

})

test_that("alert_below(any) returns condition correctly", {
  fc <- alert_below(extent_type = "any", extent_value = 3, rule_value = 3)$function_call

  value = c(1, 0, 2, 0, 3, 0)
  expect_true(eval(fc))

  value = c(3, 3, 0, 2, 0)
  expect_true(eval(fc))

  value = c(3, 3, 2, 1, 3)
  expect_false(eval(fc))

})

test_that("alert_below(last) returns condition correctly", {
  fc <- alert_below(extent_type = "last", extent_value = 3, rule_value = 3)$function_call

  value = c(1, 3, 0, 0, 0)
  expect_true(eval(fc))

  value = c(0, 1, 3, 0, 0)
  expect_false(eval(fc))

})

test_that("alert_below(consecutive) returns condition correctly", {
  fc <- alert_below(extent_type = "consecutive", extent_value = 3, rule_value = 3)$function_call

  value = c(10, 20, 2, 1, 2, 10)
  expect_true(eval(fc))

  value = c(10, 20, 20, 1, 2, 10)
  expect_false(eval(fc))

  # any NAs should be skipped
  value = c(0, NA, 0, 2, 21)
  expect_true(eval(fc))

})


test_that("alert_above(all) returns condition correctly", {
  fc <- alert_above(extent_type = "all", rule_value = 0)$function_call

  value = rep(2, 10)
  expect_true(eval(fc))

  value = rep(1, 10)
  expect_true(eval(fc))

  value = c(1, rep(0, 10))
  expect_false(eval(fc))

})

test_that("alert_above(any) returns condition correctly", {
  fc <- alert_above(extent_type = "any", extent_value = 3, rule_value = 0)$function_call

  value = c(1, 2, 0, 3, 0)
  expect_true(eval(fc))

  value = c(0, 0, 2, 0, 3, 0)
  expect_false(eval(fc))

})

test_that("alert_above(last) returns condition correctly", {
  fc <- alert_above(extent_type = "last", extent_value = 3, rule_value = 0)$function_call

  value = c(1, 0, 1, 3, 2)
  expect_true(eval(fc))

  value = c(0, 1, 2, 0, 0)
  expect_false(eval(fc))

})

test_that("alert_above(consecutive) returns condition correctly", {
  fc <- alert_above(extent_type = "consecutive", extent_value = 3, rule_value = 1)$function_call

  value = c(1, 20, 20, 10, 2, 10)
  expect_true(eval(fc))

  value = c(1, 20, 20, 1, 2, 10)
  expect_false(eval(fc))

  # any NAs should be skipped
  value = c(20, NA, 20, 2, 1)
  expect_true(eval(fc))

})

# TODO: more tests for equals/above/below with NAs in

test_that("alert_difference_above_perc() returns condition correctly", {
  fc <- alert_difference_above_perc(current_period = 1:2, previous_period = 3:6, rule_value = 50)$function_call

  value = c(100, 3, 3, 5, 5, 8, 6)
  expect_true(eval(fc))

  value = c(3, 3, 5, 5, 4, 6)
  expect_false(eval(fc))

  value = c(0, 0, 0, 0, 0, 0)
  expect_false(eval(fc))

  # includes NAs in period but excludes them from calculation
  value = c(3, 3, NA, NA, 5, 5, 8, 6)
  expect_false(eval(fc))

  # returns NA if either period is all NAs
  value = c(3, 3, 5, 5, 4, 6, NA, NA)
  expect_equal(eval(fc), NA)

  value = c(3, NA, NA, NA, NA, 4, 6)
  expect_equal(eval(fc), NA)

})

test_that("alert_difference_below_perc() returns condition correctly", {
  fc <- alert_difference_below_perc(current_period = 1:2, previous_period = 3:6, rule_value = 50)$function_call

  value = c(0, 10, 10, 10, 10, 4, 4)
  expect_true(eval(fc))

  value = c(10, 10, 10, 10, 8, 8)
  expect_false(eval(fc))

  value = rep(10, 6)
  expect_false(eval(fc))

  value = c(0, 0, 0, 0, 0, 0)
  expect_false(eval(fc))

  # includes NAs in period but excludes them from calculation
  value = c(10, 10, NA, NA, 10, 10, 8, 8)
  expect_false(eval(fc))

  # returns NA if either period is all NAs
  value = c(3, 3, 5, 5, 4, 6, NA, NA)
  expect_equal(eval(fc), NA)

  value = c(3, NA, NA, NA, NA, 4, 6)
  expect_equal(eval(fc), NA)
})


test_that("alert_custom() returns condition correctly", {
  fc <- alert_custom(
    short_name = "my_rule_doubled",
    description = "Last value is over double the first value",
    function_call = quote(rev(value)[1] > 2*value[1])
  )$function_call


  value = c(1, 0, 3)
  expect_true(eval(fc))

  value = c(1, 0, 1)
  expect_false(eval(fc))

  # returns NA if either value is NA
  value = c(3, 6, NA, NA)
  expect_equal(eval(fc), NA)
})


test_that("mantis_alerts() returns an empty df if supplied an empty df", {
  df <- data.frame(timepoint = as.Date(numeric()),
                   item = character(),
                   value = numeric(),
                   tab = character())

  results <- mantis_alerts(df,
                           inputspec = inputspec(timepoint_col = "timepoint",
                                                 item_cols = "item",
                                                 value_col = "value"),
                           alert_rules = alert_rules(alert_missing()),
                           timepoint_limits = c(as.Date("2022-01-01"), as.Date("2022-01-10")),
                           fill_with_zero = FALSE)

  expect_equal(nrow(results), 0)
})

test_that("mantis_alerts() returns an empty df if supplied an empty df with tab_col", {
  df <- data.frame(timepoint = as.Date(numeric()),
                   item = character(),
                   value = numeric(),
                   tab = character())

  results <- mantis_alerts(df,
                           inputspec = inputspec(timepoint_col = "timepoint",
                                                 item_cols = c("item", "tab"),
                                                 value_col = "value",
                                                 tab_col = "tab"),
                           alert_rules = alert_rules(alert_missing()),
                           timepoint_limits = c(as.Date("2022-01-01"), as.Date("2022-01-10")),
                           fill_with_zero = FALSE)

  expect_equal(nrow(results), 0)
})


test_that("restrict_items() doesn't filter prepared_df when no items specified", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 6),
                   item1 = c(rep("a", 20), rep("b", 20), rep("c", 20)),
                   item2 = rep(c(rep("x", 10), rep("y", 10)), 3),
                   item3 = c(rep("one", 40), rep("two", 20)),
                   value = rep(3, 60),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = c("item1", "item2", "item3"),
    value_col = "value"
  )

  prepared_df <-
    prepare_df(
      df,
      inputspec
    )

  expect_equal(
    prepared_df |>
      restrict_items(items = NULL) |>
      nrow(),
    nrow(prepared_df)
  )

})

test_that("restrict_items() filters prepared_df items correctly when items are specified", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 6),
                   item1 = c(rep("a", 20), rep("b", 20), rep("c", 20)),
                   item2 = rep(c(rep("x", 10), rep("y", 10)), 3),
                   item3 = c(rep("one", 40), rep("two", 20)),
                   value = rep(3, 60),
                   stringsAsFactors = FALSE)

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = c("item1", "item2", "item3"),
    value_col = "value"
  )

  prepared_df <-
    prepare_df(
      df,
      inputspec
    )

  # filter on item1 only
  expect_equal(
    prepared_df |>
      restrict_items(items = list("item1" = c("b", "c"))) |>
      dplyr::pull(.data[[item_cols_prefix("item1")]]) |>
      unique(),
    c("b", "c")
  )

  # filter on item1 AND item2 (two expectations)
  expect_equal(
    prepared_df |>
      restrict_items(items = list("item1" = c("b", "c"), "item2" = "x")) |>
      dplyr::pull(.data[[item_cols_prefix("item1")]]) |>
      unique(),
    c("b", "c")
  )
  expect_equal(
    prepared_df |>
      restrict_items(items = list("item1" = c("b", "c"), "item2" = "x")) |>
      dplyr::pull(.data[[item_cols_prefix("item2")]]) |>
      unique(),
    c("x")
  )

})

test_that("alert_rules items can be left unspecified", {

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = c("item1", "item2"),
    value_col = "value"
  )

  expect_silent(
    validate_alert_rules_to_inputspec(alert_rules = NULL,
                                      inputspec = inputspec)
  )

  expect_silent(
    validate_alert_rules_to_inputspec(
      alert_rules = alert_rules(alert_missing()),
      inputspec = inputspec
    )
  )

})

test_that("alert_rules items must match item_cols when specified", {

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = c("item1", "item2"),
    value_col = "value"
  )

  # one good item
  expect_silent(
    validate_alert_rules_to_inputspec(
      alert_rules = alert_rules(
        alert_missing(items = list("item1" = "a"))
      ),
      inputspec = inputspec
    )
  )

  # one bad item
  expect_error(
    validate_alert_rules_to_inputspec(
      alert_rules = alert_rules(
        alert_missing(items = list("otheritem" = "a"))
      ),
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  # one good one bad item in separate rules
  expect_error(
    validate_alert_rules_to_inputspec(
      alert_rules = alert_rules(
        alert_missing(items = list("item1" = "a")),
        alert_below(rule_value = 0,
                    items = list("otheritem" = "z"))
      ),
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  # one good one bad item in same rule
  expect_error(
    validate_alert_rules_to_inputspec(
      alert_rules = alert_rules(
        alert_missing(items = list("item1" = "a",
                                   "otheritem" = "z"))
      ),
      inputspec = inputspec
    ),
    class = "invalid_data"
  )

  # two items with same name in separate rules
  expect_silent(
    validate_alert_rules_to_inputspec(
      alert_rules = alert_rules(
        alert_missing(items = list("item1" = "a")),
        alert_below(rule_value = 0,
                    items = list("item1" = "z"))
      ),
      inputspec = inputspec
    )
  )

  })
