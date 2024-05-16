test_that("alert_missing(all) returns condition correctly", {
  fc <- alert_missing(missing_extent_type = "all")$function_call

  value = rep(NA, 10)
  expect_true(eval(fc))

  value = c(1, rep(NA, 10))
  expect_false(eval(fc))

})

test_that("alert_missing(any) returns condition correctly", {
  fc <- alert_missing(missing_extent_type = "any", missing_extent_value = 3)$function_call

  value = c(1, NA, 2, NA, 3, NA)
  expect_true(eval(fc))

  value = c(1, 2, NA, 3, NA)
  expect_false(eval(fc))

})

test_that("alert_missing(last) returns condition correctly", {
  fc <- alert_missing(missing_extent_type = "last", missing_extent_value = 3)$function_call

  value = c(1, 2, NA, NA, NA)
  expect_true(eval(fc))

  value = c(NA, 1, 2, NA, NA)
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

})

test_that("alert_equals(last) returns condition correctly", {
  fc <- alert_equals(extent_type = "last", extent_value = 3, rule_value = 0)$function_call

  value = c(1, 2, 0, 0, 0)
  expect_true(eval(fc))

  value = c(0, 1, 2, 0, 0)
  expect_false(eval(fc))

})


test_that("alert_lt(all) returns condition correctly", {
  fc <- alert_lt(extent_type = "all", rule_value = 2)$function_call

  value = rep(0, 10)
  expect_true(eval(fc))

  value = c(2, rep(0, 10))
  expect_false(eval(fc))

})

test_that("alert_lt(any) returns condition correctly", {
  fc <- alert_lt(extent_type = "any", extent_value = 3, rule_value = 2)$function_call

  value = c(1, 0, 2, 0, 3, 0)
  expect_true(eval(fc))

  value = c(2, 2, 0, 3, 0)
  expect_false(eval(fc))

})

test_that("alert_lt(last) returns condition correctly", {
  fc <- alert_lt(extent_type = "last", extent_value = 3, rule_value = 2)$function_call

  value = c(1, 2, 0, 0, 0)
  expect_true(eval(fc))

  value = c(0, 1, 2, 0, 0)
  expect_false(eval(fc))

})


test_that("alert_gt(all) returns condition correctly", {
  fc <- alert_gt(extent_type = "all", rule_value = 1)$function_call

  value = rep(2, 10)
  expect_true(eval(fc))

  value = c(2, rep(0, 10))
  expect_false(eval(fc))

})

test_that("alert_gt(any) returns condition correctly", {
  fc <- alert_gt(extent_type = "any", extent_value = 3, rule_value = 1)$function_call

  value = c(2, 2, 0, 3, 0)
  expect_true(eval(fc))

  value = c(1, 0, 2, 0, 3, 0)
  expect_false(eval(fc))

})

test_that("alert_gt(last) returns condition correctly", {
  fc <- alert_gt(extent_type = "last", extent_value = 3, rule_value = 1)$function_call

  value = c(1, 0, 2, 3, 2)
  expect_true(eval(fc))

  value = c(0, 1, 2, 0, 0)
  expect_false(eval(fc))

})

# TODO: test equals/lt/gt with NAs in

