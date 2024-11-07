# NOTE: testfn_params_required() and testfn_params_type() defined in utilities.R as devtools::test() can't find them when defined here
# also integrates better with RStudio when placed in R/ rather than in tests/testthat/helper.R

test_that("validate_params_required() checks is silent if required params are supplied", {
  expect_silent(testfn_params_required(1, 2))
  expect_silent(testfn_params_required(p2 = 1, p1 = 2))
})

test_that("validate_params_required() checks returns an error if any required params are not supplied", {
  expect_error(testfn_params_required(),
               class = "invalid_param_missing"
  )
  expect_error(testfn_params_required(1),
               class = "invalid_param_missing"
  )
  expect_error(testfn_params_required(p2 = 1),
               class = "invalid_param_missing"
  )
})

test_that("validate_params_required() allows arbitrary additional params to be supplied via ...", {
  expect_silent(testfn_params_required(p2 = 1, p1 = 2, passthrough = 1))
})


test_that("validate_params_type() is silent if all params are of correct type", {
  # all default args are valid
  expect_silent(testfn_params_type())
})

test_that("validate_params_type() checks df params are of correct type", {
  # more careful validation is done by validate_df_to_inputspec()
  expect_error(testfn_params_type(df = c("Fieldname" = 123)),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks inputspec params are of correct type", {
  expect_error(testfn_params_type(inputspec = 1),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks outputspec params are of correct type", {
  expect_error(testfn_params_type(outputspec = 1),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks alert_rules params are of correct type", {
  expect_error(testfn_params_type(alert_rules = 1),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks dataset_description params are of correct type", {
  expect_silent(testfn_params_type(dataset_description = ""))
  expect_silent(testfn_params_type(dataset_description = NULL))
  expect_error(testfn_params_type(dataset_description = 123),
               class = "invalid_param_type"
  )
  expect_error(testfn_params_type(dataset_description = c("col1", "col2")),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks report_title params are of correct type", {
  expect_silent(testfn_params_type(report_title = ""))
  expect_silent(testfn_params_type(report_title = NULL))
  expect_error(testfn_params_type(report_title = 123),
               class = "invalid_param_type"
  )
  expect_error(testfn_params_type(report_title = c("col1", "col2")),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks save_directory params are of correct type", {
  # Real dir
  expect_silent(testfn_params_type(save_directory = test_path()))
  # Real dir with trailing slash
  expect_silent(testfn_params_type(save_directory = paste0(test_path(), "/")))
  # Fake dir
  expect_error(testfn_params_type(save_directory = "fakedir"),
               class = "invalid_param_type"
  )
  # Dir includes filename
  expect_error(testfn_params_type(save_directory = test_path("test_utilities.R")),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks save_filename params are allowed to contain alphanumerics, - and _", {
  expect_silent(testfn_params_type(save_filename = "alpha123"))
  expect_silent(testfn_params_type(save_filename = "alpha-123"))
  expect_silent(testfn_params_type(save_filename = "alpha_123"))
})

test_that("validate_params_type() checks save_filename params are allowed to be NULL", {
  expect_silent(testfn_params_type(save_filename = NULL))
})

test_that("validate_params_type() checks save_filename params are not allowed to contain the extension", {
  expect_error(testfn_params_type(save_filename = "badname.html"),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks save_filename params are not allowed to contain punctuation other than - and _", {
  expect_error(testfn_params_type(save_filename = "bad.name"),
               class = "invalid_param_type"
  )
  expect_error(testfn_params_type(save_filename = "badname&"),
               class = "invalid_param_type"
  )
  expect_error(testfn_params_type(save_filename = "badname*"),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks show_progress params are of correct type", {
  expect_error(testfn_params_type(show_progress = 1),
               class = "invalid_param_type"
  )
})

test_that("validate_params_type() works with package prefix", {
  expect_error(mantis::inputspec(timepoint_col = 1, item_col = "a", value_col = "b"),
               class = "invalid_param_type"
  )
})

