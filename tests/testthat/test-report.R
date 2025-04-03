test_that("mantis_report() creates single-tab interactive report and returns path successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 2),
                   item = c(rep("a", 10), rep("b", 10)),
                   value = rep(c(3, -3), 10),
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                        item_cols = "item",
                        value_col = "value"),
      outputspec = outputspec_interactive(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
      )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates multi-tab interactive report and returns path successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
                   value = rep(3, 30),
                   tab = c(rep("one", 20), rep("two", 10)),
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                        item_cols = c("item", "tab"),
                        value_col = "value",
                        tab_col = "tab"),
      outputspec = outputspec_interactive(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
      )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates heatmap report and returns path successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
                   value = rep(3, 30),
                   tab = c(rep("one", 20), rep("two", 10)),
                   stringsAsFactors = FALSE)

  # single-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = "item",
                            value_col = "value"),
      outputspec = outputspec_static_heatmap(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_type(reportpath, "character")
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = c("item", "tab"),
                            value_col = "value",
                            tab_col = "tab"),
      outputspec = outputspec_static_heatmap(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_type(reportpath, "character")
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates multipanel report and returns path successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
                   value = rep(3, 30),
                   tab = c(rep("one", 20), rep("two", 10)),
                   stringsAsFactors = FALSE)

  # single-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = "item",
                            value_col = "value"),
      outputspec = outputspec_static_multipanel(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_type(reportpath, "character")
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = c("item", "tab"),
                            value_col = "value",
                            tab_col = "tab"),
      outputspec = outputspec_static_multipanel(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_type(reportpath, "character")
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates interactive report with alerts successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
                   value = rep(3, 30),
                   tab = c(rep("one", 20), rep("two", 10)),
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = c("item", "tab"),
                            value_col = "value",
                            tab_col = "tab"),
      outputspec = outputspec_interactive(),
      alertspec = alertspec(
        alert_rules = alert_rules(
          alert_missing(
            extent_type = "all",
            items = list("item" = c("b", "c"))
          ),
          alert_above(
            extent_type = "any",
            extent_value = 1,
            rule_value = 5,
            items = list("item" = c("b", "c"))
          )
        )
      ),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})


test_that("mantis_report() creates interactive report even if supplied an empty df", {
  df <- data.frame(timepoint = as.Date(numeric()),
                   item = character(),
                   value = numeric(),
                   tab = character())

  # single-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = "item",
                            value_col = "value"),
      outputspec = outputspec_interactive(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = c("item", "tab"),
                            value_col = "value",
                            tab_col = "tab"),
      outputspec = outputspec_interactive(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates interactive report with alerts even if supplied an empty df", {
  df <- data.frame(timepoint = as.Date(numeric()),
                   item = character(),
                   value = numeric(),
                   tab = character())

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = c("item", "tab"),
                            value_col = "value",
                            tab_col = "tab"),
      outputspec = outputspec_interactive(),
      alertspec = alertspec(
        alert_rules = alert_rules(
          alert_missing(
            extent_type = "all",
            items = list("item" = c("b", "c"))
          ),
          alert_above(
            extent_type = "any",
            extent_value = 1,
            rule_value = 5,
            items = list("item" = c("b", "c"))
          )
        )
      ),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates heatmap report even if supplied an empty df", {
  df <- data.frame(timepoint = as.Date(numeric()),
                   item = character(),
                   value = numeric(),
                   tab = character())

  # single-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = "item",
                            value_col = "value"),
      outputspec = outputspec_static_heatmap(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = c("item", "tab"),
                            value_col = "value",
                            tab_col = "tab"),
      outputspec = outputspec_static_heatmap(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates multipanel report even if supplied an empty df", {
  df <- data.frame(timepoint = as.Date(numeric()),
                   item = character(),
                   value = numeric(),
                   tab = character())

  # single-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = "item",
                            value_col = "value"),
      outputspec = outputspec_static_multipanel(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = c("item", "tab"),
                            value_col = "value",
                            tab_col = "tab"),
      outputspec = outputspec_static_multipanel(),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates multi-item_cols interactive report and returns path successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
                   value = rep(3, 30),
                   tab = c(rep("one", 20), rep("two", 10)),
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_cols = c("item", "tab"),
                            value_col = "value"),
      outputspec = outputspec_interactive(item_labels = c("item" = "My item")),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})


test_that("mantis_report() creates interactive report with spaces in item_cols names", {
  df <- data.frame("m timepoint" = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   "m item" = c(rep("a", 10), rep("b", 10), rep("c", 10)),
                   "m value" = rep(3, 30),
                   "m tab" = c(rep("one", 20), rep("two", 10)),
                   check.names = FALSE,
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "m timepoint",
                            item_cols = c("m item", "m tab"),
                            value_col = "m value",
                            tab_col = "m tab"),
      outputspec = outputspec_interactive(),
      alertspec = alertspec(
        alert_rules = alert_rules(
          alert_missing(
            extent_type = "all",
            items = list("m item" = c("b", "c"))
          ),
          alert_above(
            extent_type = "any",
            extent_value = 1,
            rule_value = 5,
            items = list("m item" = c("b", "c"))
          )
        )
      ),
      save_directory = tempdir(),
      save_filename = "mantis_testthatreport",
      show_progress = FALSE
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

