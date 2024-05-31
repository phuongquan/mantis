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
  fc <- alert_below(extent_type = "all", rule_value = 2)$function_call

  value = rep(0, 10)
  expect_true(eval(fc))

  value = c(2, rep(0, 10))
  expect_true(eval(fc))

  value = c(3, rep(0, 10))
  expect_false(eval(fc))

})

test_that("alert_below(any) returns condition correctly", {
  fc <- alert_below(extent_type = "any", extent_value = 3, rule_value = 2)$function_call

  value = c(1, 0, 2, 0, 3, 0)
  expect_true(eval(fc))

  value = c(3, 3, 0, 2, 0)
  expect_true(eval(fc))

  value = c(3, 3, 2, 1, 3)
  expect_false(eval(fc))

})

test_that("alert_below(last) returns condition correctly", {
  fc <- alert_below(extent_type = "last", extent_value = 3, rule_value = 2)$function_call

  value = c(1, 3, 0, 0, 0)
  expect_true(eval(fc))

  value = c(0, 1, 3, 0, 0)
  expect_false(eval(fc))

})

test_that("alert_below(consecutive) returns condition correctly", {
  fc <- alert_below(extent_type = "consecutive", extent_value = 3, rule_value = 2)$function_call

  value = c(10, 20, 2, 1, 2, 10)
  expect_true(eval(fc))

  value = c(10, 20, 20, 1, 2, 10)
  expect_false(eval(fc))

  # any NAs should be skipped
  value = c(0, NA, 0, 2, 21)
  expect_true(eval(fc))

})


test_that("alert_above(all) returns condition correctly", {
  fc <- alert_above(extent_type = "all", rule_value = 1)$function_call

  value = rep(2, 10)
  expect_true(eval(fc))

  value = rep(1, 10)
  expect_true(eval(fc))

  value = c(1, rep(0, 10))
  expect_false(eval(fc))

})

test_that("alert_above(any) returns condition correctly", {
  fc <- alert_above(extent_type = "any", extent_value = 3, rule_value = 1)$function_call

  value = c(1, 2, 0, 3, 0)
  expect_true(eval(fc))

  value = c(0, 0, 2, 0, 3, 0)
  expect_false(eval(fc))

})

test_that("alert_above(last) returns condition correctly", {
  fc <- alert_above(extent_type = "last", extent_value = 3, rule_value = 1)$function_call

  value = c(1, 0, 1, 3, 2)
  expect_true(eval(fc))

  value = c(0, 1, 2, 0, 0)
  expect_false(eval(fc))

})

test_that("alert_above(consecutive) returns condition correctly", {
  fc <- alert_above(extent_type = "consecutive", extent_value = 3, rule_value = 2)$function_call

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
  fc <- alert_difference_above_perc(current_period = 2, previous_period = 4, rule_value = 50)$function_call

  value = c(100, 3, 3, 5, 5, 8, 6)
  expect_true(eval(fc))

  value = c(3, 3, 5, 5, 4, 6)
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
  fc <- alert_difference_below_perc(current_period = 2, previous_period = 4, rule_value = 50)$function_call

  value = c(0, 10, 10, 10, 10, 5, 5)
  expect_true(eval(fc))

  value = c(10, 10, 10, 10, 8, 8)
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

