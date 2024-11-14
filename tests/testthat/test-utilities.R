# NOTE: testfn_params_required() and testfn_params_type() defined in utilities.R as devtools::test() can't find them when defined here
# also integrates better with RStudio when placed in R/ rather than in tests/testthat/helper.R

test_that("validate_params_required() checks is silent if required params are supplied", {
  expect_silent(testfn_params_required(1, 2))
  expect_silent(testfn_params_required(p2 = 1, p1 = 2))
})

test_that("validate_params_required() checks returns an error if any required params are not supplied", {
  expect_error(testfn_params_required(),
               class = "invalid_param_missing")
  expect_error(testfn_params_required(1),
               class = "invalid_param_missing")
  expect_error(testfn_params_required(p2 = 1),
               class = "invalid_param_missing")
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
               class = "invalid_param_type")
  expect_error(testfn_params_type(df = NULL),
               class = "invalid_param_type")
})


test_that("validate_params_type() checks inputspec params are of correct type", {
  expect_error(testfn_params_type(inputspec = 1),
               class = "invalid_param_type")
  expect_error(testfn_params_type(inputspec = NULL),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks timepoint_col params are of correct type", {
  expect_silent(testfn_params_type(timepoint_col = "col1"))

  expect_error(testfn_params_type(timepoint_col = ""),
               class = "invalid_param_type")
  expect_error(testfn_params_type(timepoint_col = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(timepoint_col = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(timepoint_col = c("col1", "col2")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks item_col params are of correct type", {
  expect_silent(testfn_params_type(item_col = "col1"))

  expect_error(testfn_params_type(item_col = ""),
               class = "invalid_param_type")
  expect_error(testfn_params_type(item_col = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(item_col = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(item_col = c("col1", "col2")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks value_col params are of correct type", {
  expect_silent(testfn_params_type(value_col = "col1"))

  expect_error(testfn_params_type(value_col = ""),
               class = "invalid_param_type")
  expect_error(testfn_params_type(value_col = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(value_col = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(value_col = c("col1", "col2")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks tab_col params are of correct type", {
  expect_silent(testfn_params_type(tab_col = "col1"))
  expect_silent(testfn_params_type(tab_col = NULL))

  expect_error(testfn_params_type(tab_col = ""),
               class = "invalid_param_type")
  expect_error(testfn_params_type(tab_col = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(tab_col = c("col1", "col2")),
               class = "invalid_param_type")
})


test_that("validate_params_type() checks outputspec params are of correct type", {
  expect_silent(testfn_params_type(outputspec = NULL))
  expect_error(testfn_params_type(outputspec = 1),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks plot_label params are of correct type", {
  expect_silent(testfn_params_type(plot_label = ""))
  expect_silent(testfn_params_type(plot_label = NULL))

  expect_error(testfn_params_type(plot_label = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(plot_label = c("col1", "col2")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks item_label params are of correct type", {
  expect_silent(testfn_params_type(item_label = ""))
  expect_silent(testfn_params_type(item_label = NULL))

  expect_error(testfn_params_type(item_label = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(item_label = c("col1", "col2")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks fill_colour params are of correct type", {
  expect_silent(testfn_params_type(fill_colour = "blue"))

  expect_error(testfn_params_type(fill_colour = ""),
               class = "invalid_param_type")
  expect_error(testfn_params_type(fill_colour = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(fill_colour = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(fill_colour = c("blue", "red")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks y_label params are of correct type", {
  expect_silent(testfn_params_type(y_label = ""))
  expect_silent(testfn_params_type(y_label = NULL))

  expect_error(testfn_params_type(y_label = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(y_label = c("col1", "col2")),
               class = "invalid_param_type")
})


test_that("validate_params_type() checks alert_rules params are of correct type", {
  expect_silent(testfn_params_type(alert_rules = NULL))
  expect_error(testfn_params_type(alert_rules = 1),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks dataset_description params are of correct type", {
  expect_silent(testfn_params_type(dataset_description = ""))
  expect_silent(testfn_params_type(dataset_description = NULL))

  expect_error(testfn_params_type(dataset_description = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(dataset_description = c("col1", "col2")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks report_title params are of correct type", {
  expect_silent(testfn_params_type(report_title = ""))
  expect_silent(testfn_params_type(report_title = NULL))

  expect_error(testfn_params_type(report_title = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(report_title = c("col1", "col2")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks save_directory params are of correct type", {
  # Real dir
  expect_silent(testfn_params_type(save_directory = test_path()))
  # Real dir with trailing slash
  expect_silent(testfn_params_type(save_directory = paste0(test_path(), "/")))

  # Fake dir
  expect_error(testfn_params_type(save_directory = "fakedir"),
               class = "invalid_param_type")
  # Dir includes filename
  expect_error(testfn_params_type(save_directory = test_path("test_utilities.R")),
               class = "invalid_param_type")
  # Multiple dirs
  expect_error(testfn_params_type(save_directory = c(".", test_path())),
               class = "invalid_param_type")
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
               class = "invalid_param_type")
})

test_that("validate_params_type() checks save_filename params are not allowed to contain punctuation other than - and _", {
  expect_error(testfn_params_type(save_filename = "bad.name"),
               class = "invalid_param_type")
  expect_error(testfn_params_type(save_filename = "badname&"),
               class = "invalid_param_type")
  expect_error(testfn_params_type(save_filename = "badname*"),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks save_filename params are scalar", {
  expect_error(testfn_params_type(save_filename = c("file1", "file2")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks show_progress params are of correct type", {
  expect_silent(testfn_params_type(show_progress = TRUE))
  expect_silent(testfn_params_type(show_progress = FALSE))

  expect_error(testfn_params_type(show_progress = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(show_progress = 1),
               class = "invalid_param_type")
  expect_error(testfn_params_type(show_progress = c(TRUE, FALSE)),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks plot_value_type params are of correct type", {
  expect_silent(testfn_params_type(plot_value_type = "value"))
  expect_silent(testfn_params_type(plot_value_type = "delta"))

  expect_error(testfn_params_type(plot_value_type = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(plot_value_type = 1),
               class = "invalid_param_type")
  expect_error(testfn_params_type(plot_value_type = "hello"),
               class = "invalid_param_type")
  expect_error(testfn_params_type(plot_value_type = c("value", "delta")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks plot_type params are of correct type", {
  expect_silent(testfn_params_type(plot_type = "bar"))
  expect_silent(testfn_params_type(plot_type = "line"))

  expect_error(testfn_params_type(plot_type = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(plot_type = 1),
               class = "invalid_param_type")
  expect_error(testfn_params_type(plot_type = "hello"),
               class = "invalid_param_type")
  expect_error(testfn_params_type(plot_type = c("bar", "line")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks period params are of correct type", {
  expect_silent(testfn_params_type(period = "day"))
  expect_silent(testfn_params_type(period = "week"))
  expect_silent(testfn_params_type(period = "month"))
  expect_silent(testfn_params_type(period = "quarter"))
  expect_silent(testfn_params_type(period = "year"))

  expect_error(testfn_params_type(period = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(period = 123),
               class = "invalid_param_type")
  expect_error(testfn_params_type(period = c("day", "week")),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks item_order params are either TRUE or all strings", {
  expect_silent(testfn_params_type(item_order = "a"))
  expect_silent(testfn_params_type(item_order = c("a", "b")))
  expect_silent(testfn_params_type(item_order = TRUE))
  expect_silent(testfn_params_type(item_order = NULL))

  expect_error(testfn_params_type(item_order = FALSE),
               class = "invalid_param_type")
  expect_error(testfn_params_type(item_order = 1:3),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks summary_cols params contain valid values", {
  expect_silent(testfn_params_type(summary_cols = "max_value"))
  expect_silent(testfn_params_type(summary_cols = c("max_value",
                                                    "last_value",
                                                    "last_value_nonmissing",
                                                    "last_timepoint",
                                                    "mean_value")))
  expect_silent(testfn_params_type(summary_cols = NULL))

  expect_error(testfn_params_type(summary_cols = FALSE),
               class = "invalid_param_type")
  expect_error(testfn_params_type(summary_cols = "hello"),
               class = "invalid_param_type")
  expect_error(testfn_params_type(summary_cols = c("max_value", "hello")),
               class = "invalid_param_type")
  expect_error(testfn_params_type(summary_cols = 1:3),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks sort_by params contain valid values", {
  expect_silent(testfn_params_type(sort_by = "max_value"))
  expect_silent(testfn_params_type(sort_by = c("max_value",
                                               "-mean_value")))
  expect_silent(testfn_params_type(sort_by = NULL))

  # NOTE: if they provide a colname that doesn't exist, just ignore it, as you may want to
  # supply a standard superset for everything
  expect_silent(testfn_params_type(sort_by = "hello"))

  expect_error(testfn_params_type(sort_by = FALSE),
               class = "invalid_param_type")
  expect_error(testfn_params_type(sort_by = 1:3),
               class = "invalid_param_type")
})


test_that("validate_params_type() checks filter_results params contain valid values", {
  expect_silent(testfn_params_type(filter_results = "PASS"))
  expect_silent(testfn_params_type(filter_results = c("PASS",
                                                    "FAIL",
                                                    "NA")))

  expect_error(testfn_params_type(filter_results = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(filter_results = NA),
               class = "invalid_param_type")
  expect_error(testfn_params_type(filter_results = "hello"),
               class = "invalid_param_type")
  expect_error(testfn_params_type(filter_results = c("PASS", "hello")),
               class = "invalid_param_type")
  expect_error(testfn_params_type(filter_results = 1:3),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks timepoint_limits params contain valid values", {
  expect_silent(testfn_params_type(timepoint_limits = c(NA, NA)))
  expect_silent(testfn_params_type(timepoint_limits = c(NA, as.Date("2024-01-01"))))
  expect_silent(testfn_params_type(timepoint_limits = c(as.Date("2024-01-01"), NA)))
  expect_silent(testfn_params_type(timepoint_limits = c(as.Date("2024-01-01"), as.Date("2024-02-01"))))

  expect_error(testfn_params_type(timepoint_limits = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(timepoint_limits = NA),
               class = "invalid_param_type")
  expect_error(testfn_params_type(timepoint_limits = "hello"),
               class = "invalid_param_type")
  expect_error(testfn_params_type(timepoint_limits = c("2024-01-01", "2024-02-01")),
               class = "invalid_param_type")
  expect_error(testfn_params_type(timepoint_limits = 1:3),
               class = "invalid_param_type")
})

test_that("validate_params_type() checks fill_with_zero params are of correct type", {
  expect_silent(testfn_params_type(fill_with_zero = TRUE))
  expect_silent(testfn_params_type(fill_with_zero = FALSE))

  expect_error(testfn_params_type(fill_with_zero = NULL),
               class = "invalid_param_type")
  expect_error(testfn_params_type(fill_with_zero = 1),
               class = "invalid_param_type")
  expect_error(testfn_params_type(fill_with_zero = c(TRUE, FALSE)),
               class = "invalid_param_type")
})



test_that("validate_params_type() works with package prefix", {
  expect_error(mantis::inputspec(timepoint_col = 1, item_col = "a", value_col = "b"),
               class = "invalid_param_type")
})

