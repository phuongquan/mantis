# NOTE: testfn_params_required() and testfn_params_type() defined in utilities.R
# as devtools::test() can't find them when defined here
# Also integrates better with RStudio when placed in R/ rather than
# in tests/testthat/helper.R

test_that("validate_params_required() is silent if required params are supplied", {
  expect_silent(testfn_params_required(1, 2))
  expect_silent(testfn_params_required(p2 = 1, p1 = 2))
})

test_that("validate_params_required() returns an error if any required params are not supplied", {
  expect_error(
    testfn_params_required(),
    class = "invalid_param_missing"
  )
  expect_error(
    testfn_params_required(1),
    class = "invalid_param_missing"
  )
  expect_error(
    testfn_params_required(p2 = 1),
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
  expect_error(
    testfn_params_type(df = c("Fieldname" = 123)),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(df = NULL),
    class = "invalid_param_type"
  )
})


test_that("validate_params_type() checks file params contain a valid directory (or no directory)", {
  # Real dir
  expect_silent(testfn_params_type(file = file.path(test_path(), "filename.html")))
  # No dir
  expect_silent(testfn_params_type(file = "filename.html"))

  # Fake dir
  expect_error(
    testfn_params_type(file = file.path("fakedir", "filename.html")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks file params are scalar", {
  expect_error(
    testfn_params_type(file = file.path(c("filename.html", "filename2.html"))),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks file params are not allowed to be empty", {
  expect_error(testfn_params_type(file = NULL))
  expect_error(testfn_params_type(file = ""))
  expect_error(testfn_params_type(file = NA))
  expect_error(testfn_params_type(file = character(0)))
})

test_that("validate_params_type() checks file name of file params includes valid extension", {
  # Valid
  expect_silent(testfn_params_type(file = file.path(test_path(), "filename.html")))
  expect_silent(testfn_params_type(file = "filename.html"))
  expect_silent(testfn_params_type(file = file.path(test_path(), "filename.htm")))
  expect_silent(testfn_params_type(file = "filename.htm"))

  # Invalid
  expect_error(testfn_params_type(file = file.path(test_path(), "filename.css")))
  expect_error(testfn_params_type(file = file.path(test_path(), "filename")))
  expect_error(testfn_params_type(file = "filename.css"))
  expect_error(testfn_params_type(file = "filename"))
})

test_that("validate_params_type() checks file name of file params can contain alphanumerics, - and _", {
  expect_silent(testfn_params_type(file = "alpha123.html"))
  expect_silent(testfn_params_type(file = "alpha-123.html"))
  expect_silent(testfn_params_type(file = "alpha_123.html"))
  expect_silent(testfn_params_type(file = file.path(test_path(), "alpha123.html")))
  expect_silent(testfn_params_type(file = file.path(test_path(), "alpha-123.html")))
  expect_silent(testfn_params_type(file = file.path(test_path(), "alpha_123.html")))
})

test_that("validate_params_type() checks file name of file params are not allowed to contain punctuation other than - and _", {
  expect_error(
    testfn_params_type(file = "bad.name.html"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(file = "badname&.html"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(file = "badname*.html"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(file = file.path(test_path(), "bad.name.html")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(file = file.path(test_path(), "badname&.html")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(file = file.path(test_path(), "badname*.html")),
    class = "invalid_param_type"
  )
})


test_that("validate_params_type() checks inputspec params are of correct type", {
  expect_error(
    testfn_params_type(inputspec = 1),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(inputspec = NULL),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks timepoint_col params are of correct type", {
  expect_silent(testfn_params_type(timepoint_col = "col1"))

  expect_error(
    testfn_params_type(timepoint_col = ""),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_col = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_col = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_col = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks item_cols params are of correct type", {
  expect_silent(testfn_params_type(item_cols = "col1"))
  expect_silent(testfn_params_type(item_cols = c("col1", "col2")))

  expect_error(
    testfn_params_type(item_cols = ""),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(item_cols = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(item_cols = 123),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks value_col params are of correct type", {
  expect_silent(testfn_params_type(value_col = "col1"))

  expect_error(
    testfn_params_type(value_col = ""),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(value_col = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(value_col = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(value_col = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks tab_col params are of correct type", {
  expect_silent(testfn_params_type(tab_col = "col1"))
  expect_silent(testfn_params_type(tab_col = NULL))

  expect_error(
    testfn_params_type(tab_col = ""),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_col = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_col = c("col1", "col2")),
    class = "invalid_param_type"
  )
})


test_that("validate_params_type() checks outputspec params are of correct type", {
  expect_silent(testfn_params_type(outputspec = NULL))
  expect_error(
    testfn_params_type(outputspec = 1),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks plot_label params are of correct type", {
  expect_silent(testfn_params_type(plot_label = ""))
  expect_silent(testfn_params_type(plot_label = NULL))

  expect_error(
    testfn_params_type(plot_label = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(plot_label = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks item_labels params are of correct type", {
  expect_silent(testfn_params_type(item_labels = NULL))
  expect_silent(testfn_params_type(
    item_labels = c("col1" = "col 1", "col2" = "col 2")
  ))

  expect_error(
    testfn_params_type(item_labels = ""),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(item_labels = c("col1", "col2")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(item_labels = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(item_labels = c("col1" = 123, "col2" = 1)),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks fill_colour params are of correct type", {
  expect_silent(testfn_params_type(fill_colour = "blue"))

  expect_error(
    testfn_params_type(fill_colour = ""),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(fill_colour = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(fill_colour = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(fill_colour = c("blue", "red")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks y_label params are of correct type", {
  expect_silent(testfn_params_type(y_label = ""))
  expect_silent(testfn_params_type(y_label = NULL))

  expect_error(
    testfn_params_type(y_label = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(y_label = c("col1", "col2")),
    class = "invalid_param_type"
  )
})


test_that("validate_params_type() checks alertspec params are of correct type", {
  expect_silent(testfn_params_type(alertspec = NULL))
  expect_error(
    testfn_params_type(alertspec = 1),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks alert_rules params are of correct type", {
  expect_error(
    testfn_params_type(alert_rules = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(alert_rules = 1),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks dataset_description params are of correct type", {
  expect_silent(testfn_params_type(dataset_description = ""))
  expect_silent(testfn_params_type(dataset_description = NULL))

  expect_error(
    testfn_params_type(dataset_description = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(dataset_description = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks report_title params are of correct type", {
  expect_silent(testfn_params_type(report_title = ""))
  expect_silent(testfn_params_type(report_title = NULL))

  expect_error(
    testfn_params_type(report_title = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(report_title = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks show_progress params are of correct type", {
  expect_silent(testfn_params_type(show_progress = TRUE))
  expect_silent(testfn_params_type(show_progress = FALSE))

  expect_error(
    testfn_params_type(show_progress = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(show_progress = 1),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(show_progress = c(TRUE, FALSE)),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks plot_value_type params are of correct type", {
  expect_silent(testfn_params_type(plot_value_type = "value"))
  expect_silent(testfn_params_type(plot_value_type = "delta"))

  expect_error(
    testfn_params_type(plot_value_type = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(plot_value_type = 1),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(plot_value_type = "hello"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(plot_value_type = c("value", "delta")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks plot_type params are of correct type", {
  expect_silent(testfn_params_type(plot_type = "bar"))
  expect_silent(testfn_params_type(plot_type = "line"))

  expect_error(
    testfn_params_type(plot_type = NULL),
    class = "invalid_param_type"
  )
  expect_error(testfn_params_type(plot_type = 1), class = "invalid_param_type")
  expect_error(
    testfn_params_type(plot_type = "hello"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(plot_type = c("bar", "line")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks timepoint_unit params are of correct type", {
  expect_silent(testfn_params_type(timepoint_unit = "day"))
  expect_silent(testfn_params_type(timepoint_unit = "week"))
  expect_silent(testfn_params_type(timepoint_unit = "month"))
  expect_silent(testfn_params_type(timepoint_unit = "quarter"))
  expect_silent(testfn_params_type(timepoint_unit = "year"))
  expect_silent(testfn_params_type(timepoint_unit = "sec"))
  expect_silent(testfn_params_type(timepoint_unit = "min"))
  expect_silent(testfn_params_type(timepoint_unit = "hour"))

  expect_error(
    testfn_params_type(timepoint_unit = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_unit = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(extent_type = "hello"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_unit = c("day", "week")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks item_order params are a named list containing either TRUE or a vector of strings", {
  expect_silent(testfn_params_type(item_order = NULL))
  expect_silent(testfn_params_type(item_order = list("a" = "a")))
  expect_silent(testfn_params_type(item_order = list("b" = c("a", "b"))))
  expect_silent(testfn_params_type(item_order = list("a" = TRUE)))
  expect_silent(testfn_params_type(
    item_order = list("a" = TRUE, "b" = c("a", "b"))
  ))

  # not a list
  expect_error(
    testfn_params_type(item_order = TRUE),
    class = "invalid_param_type"
  )
  # not a list
  expect_error(
    testfn_params_type(item_order = c("a", "b")),
    class = "invalid_param_type"
  )
  # non-char vector
  expect_error(
    testfn_params_type(item_order = list("a" = 1:3)),
    class = "invalid_param_type"
  )
  # unnamed list
  expect_error(
    testfn_params_type(item_order = list("a")),
    class = "invalid_param_type"
  )
  # unnamed list
  expect_error(
    testfn_params_type(item_order = list(c("a", "b"))),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks summary_cols params contain valid values", {
  expect_silent(testfn_params_type(summary_cols = "max_value"))
  expect_silent(testfn_params_type(
    summary_cols = c(
      "max_value",
      "last_value",
      "last_value_nonmissing",
      "last_timepoint",
      "mean_value"
    )
  ))
  expect_silent(testfn_params_type(summary_cols = NULL))

  expect_error(
    testfn_params_type(summary_cols = FALSE),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(summary_cols = "hello"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(summary_cols = c("max_value", "hello")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(summary_cols = 1:3),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks sort_by params contain valid values", {
  expect_silent(testfn_params_type(sort_by = "max_value"))
  expect_silent(testfn_params_type(
    sort_by = c(
      "max_value",
      "-mean_value"
    )
  ))
  expect_silent(testfn_params_type(sort_by = NULL))

  # NOTE: if they provide a colname that doesn't exist, just ignore it, as you may want to
  # supply a standard superset for everything
  expect_silent(testfn_params_type(sort_by = "hello"))

  expect_error(
    testfn_params_type(sort_by = FALSE),
    class = "invalid_param_type"
  )
  expect_error(testfn_params_type(sort_by = 1:3), class = "invalid_param_type")
})


test_that("validate_params_type() checks filter_results params contain valid values", {
  expect_silent(testfn_params_type(filter_results = "PASS"))
  expect_silent(testfn_params_type(
    filter_results = c(
      "PASS",
      "FAIL",
      "NA"
    )
  ))

  expect_error(
    testfn_params_type(filter_results = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(filter_results = NA),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(filter_results = "hello"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(filter_results = c("PASS", "hello")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(filter_results = 1:3),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks show_tab_results params contain valid values", {
  expect_silent(testfn_params_type(show_tab_results = "PASS"))
  expect_silent(testfn_params_type(
    show_tab_results = c(
      "PASS",
      "FAIL",
      "NA"
    )
  ))
  expect_silent(testfn_params_type(show_tab_results = NULL))

  expect_error(
    testfn_params_type(show_tab_results = NA),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(show_tab_results = "hello"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(show_tab_results = c("PASS", "hello")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(show_tab_results = 1:3),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks timepoint_limits params contain valid values", {
  expect_silent(
    testfn_params_type(
      timepoint_limits = c(NA, NA)
    )
  )
  expect_silent(
    testfn_params_type(
      timepoint_limits = c(NA, as.Date("2024-01-01"))
    )
  )
  expect_silent(
    testfn_params_type(
      timepoint_limits = c(as.Date("2024-01-01"), NA)
    )
  )
  expect_silent(
    testfn_params_type(
      timepoint_limits = c(as.Date("2024-01-01"), as.Date("2024-02-01"))
    )
  )
  expect_silent(
    testfn_params_type(
      timepoint_limits = c(as.POSIXct("2024-01-01"), as.POSIXct("2024-02-01"))
    )
  )
  expect_silent(
    testfn_params_type(
      timepoint_limits = c(as.POSIXlt("2024-01-01"), as.POSIXlt("2024-02-01"))
    )
  )

  expect_error(
    testfn_params_type(timepoint_limits = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_limits = NA),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_limits = "hello"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_limits = c("2024-01-01", "2024-02-01")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_limits = c(1, 10)),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(timepoint_limits = 1:3),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks fill_with_zero params are of correct type", {
  expect_silent(testfn_params_type(fill_with_zero = TRUE))
  expect_silent(testfn_params_type(fill_with_zero = FALSE))

  expect_error(
    testfn_params_type(fill_with_zero = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(fill_with_zero = 1),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(fill_with_zero = c(TRUE, FALSE)),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks sync_axis_range params are of correct type", {
  expect_silent(testfn_params_type(sync_axis_range = TRUE))
  expect_silent(testfn_params_type(sync_axis_range = FALSE))

  expect_error(
    testfn_params_type(sync_axis_range = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(sync_axis_range = 1),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(sync_axis_range = c(TRUE, FALSE)),
    class = "invalid_param_type"
  )
})


test_that("validate_params_type() checks tab_name params are of correct type", {
  expect_silent(testfn_params_type(tab_name = ""))
  expect_silent(testfn_params_type(tab_name = NULL))

  expect_error(
    testfn_params_type(tab_name = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_name = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks tab_group_name params are of correct type", {
  expect_silent(testfn_params_type(tab_group_name = ""))
  expect_silent(testfn_params_type(tab_group_name = NULL))

  expect_error(
    testfn_params_type(tab_group_name = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_group_name = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks tab_level params are of correct type", {
  expect_silent(testfn_params_type(tab_level = 1))
  expect_silent(testfn_params_type(tab_level = 5))

  expect_error(
    testfn_params_type(tab_level = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_level = "col1"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_level = 0),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_level = 1.5),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_level = c(1, "col2")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_level = c(1, 5)),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks tab_group_level params are of correct type", {
  expect_silent(testfn_params_type(tab_group_level = 1))
  expect_silent(testfn_params_type(tab_group_level = 5))

  expect_error(
    testfn_params_type(tab_group_level = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_group_level = "col1"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_group_level = 0),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_group_level = 1.5),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_group_level = c(1, "col2")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(tab_group_level = c(1, 5)),
    class = "invalid_param_type"
  )
})


test_that("validate_params_type() checks extent_type params are of correct type", {
  expect_silent(testfn_params_type(extent_type = "all"))
  expect_silent(testfn_params_type(extent_type = "any"))
  expect_silent(testfn_params_type(extent_type = "last"))
  expect_silent(testfn_params_type(extent_type = "consecutive"))

  expect_error(
    testfn_params_type(extent_type = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(extent_type = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(extent_type = "hello"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(extent_type = c("any", "all")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks extent_value params are of correct type", {
  expect_silent(testfn_params_type(extent_value = 1))
  expect_silent(testfn_params_type(extent_value = 5))

  expect_error(
    testfn_params_type(extent_value = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(extent_value = "col1"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(extent_value = 0),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(extent_value = 1.5),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(extent_value = c(1, "col2")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(extent_value = c(1, 5)),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks rule_value params are of correct type", {
  expect_silent(testfn_params_type(rule_value = -1))
  expect_silent(testfn_params_type(rule_value = 1))
  expect_silent(testfn_params_type(rule_value = 2.5))

  expect_error(
    testfn_params_type(rule_value = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(rule_value = "col1"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(rule_value = c(1, "col2")),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(rule_value = c(1, 5)),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks items params contain valid values", {
  # NOTE: if they provide values that don't exist in the data, just ignore them, as you may want to
  # supply a standard superset for everything
  expect_silent(testfn_params_type(items = NULL))
  expect_silent(testfn_params_type(items = list("item1" = c("this", "that"))))
  expect_silent(testfn_params_type(
    items = list(
      "item1" = c("this", "that"),
      "item2" = "theother"
    )
  ))

  expect_error(testfn_params_type(items = FALSE), class = "invalid_param_type")
  expect_error(testfn_params_type(items = 1:3), class = "invalid_param_type")
  expect_error(
    testfn_params_type(items = list(c("this", "that"))),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks current_period params are of correct type", {
  expect_silent(testfn_params_type(current_period = 1))
  expect_silent(testfn_params_type(current_period = 1:5))

  expect_error(
    testfn_params_type(current_period = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(current_period = "col1"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(current_period = 1.7),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(current_period = c(1, "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks previous_period params are of correct type", {
  expect_silent(testfn_params_type(previous_period = 1))
  expect_silent(testfn_params_type(previous_period = 1:5))

  expect_error(
    testfn_params_type(previous_period = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(previous_period = "col1"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(previous_period = 1.7),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(previous_period = c(1, "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks description params are of correct type", {
  expect_silent(testfn_params_type(description = "short desc"))

  expect_error(
    testfn_params_type(description = ""),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(description = NULL),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(description = 123),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(description = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks expression params are of correct type", {
  expect_silent(testfn_params_type(expression = quote(all(is.na(value)))))

  expect_error(
    testfn_params_type(expression = "all(is.na(value))"),
    class = "invalid_param_type"
  )
  expect_error(
    testfn_params_type(expression = NULL),
    class = "invalid_param_type"
  )
})


# See https://github.com/ropensci/daiquiri/issues/10
test_that("validate_params_required() works with package prefix", {
  expect_error(
    mantis::inputspec(),
    class = "invalid_param_missing"
  )
})

# See https://github.com/ropensci/daiquiri/issues/10
test_that("validate_params_type() works with package prefix", {
  expect_error(
    mantis::inputspec(timepoint_col = 1, item_cols = "a", value_col = "b"),
    class = "invalid_param_type"
  )
})
