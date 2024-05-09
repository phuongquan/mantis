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

