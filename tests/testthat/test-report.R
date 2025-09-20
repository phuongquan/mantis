test_that("mantis_report() creates single-tab interactive report and returns path successfully", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      2
    ),
    item = c(rep("a", 10), rep("b", 10)),
    value = rep(c(3, -3), 10),
    stringsAsFactors = FALSE
  )

  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      ),
      outputspec = outputspec_interactive(),
      show_progress = FALSE
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates multi-tab interactive report and returns path successfully", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      3
    ),
    item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
    value = 3,
    tab = c(rep("one", 20), rep("two", 10)),
    stringsAsFactors = FALSE
  )

  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "tab"),
        value_col = "value",
        tab_col = "tab"
      ),
      outputspec = outputspec_interactive(),
      show_progress = FALSE
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates heatmap report and returns path successfully", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      3
    ),
    item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
    value = 3,
    tab = c(rep("one", 20), rep("two", 10)),
    stringsAsFactors = FALSE
  )

  # single-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      ),
      outputspec = outputspec_static_heatmap(),
      show_progress = FALSE
    )
  expect_type(reportpath, "character")
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "tab"),
        value_col = "value",
        tab_col = "tab"
      ),
      outputspec = outputspec_static_heatmap(),
      show_progress = FALSE
    )
  expect_type(reportpath, "character")
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates multipanel report and returns path successfully", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      3
    ),
    item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
    value = 3,
    tab = c(rep("one", 20), rep("two", 10)),
    stringsAsFactors = FALSE
  )

  # single-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      ),
      outputspec = outputspec_static_multipanel(),
      show_progress = FALSE
    )
  expect_type(reportpath, "character")
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "tab"),
        value_col = "value",
        tab_col = "tab"
      ),
      outputspec = outputspec_static_multipanel(),
      show_progress = FALSE
    )
  expect_type(reportpath, "character")
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates interactive report with alerts successfully", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      3
    ),
    item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
    value = 3,
    tab = c(rep("one", 20), rep("two", 10)),
    stringsAsFactors = FALSE
  )

  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "tab"),
        value_col = "value",
        tab_col = "tab"
      ),
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
      show_progress = FALSE
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})


test_that("mantis_report() creates interactive report even if supplied an empty df", {
  df <- data.frame(
    timepoint = as.Date(numeric()),
    item = character(),
    value = numeric(),
    tab = character()
  )

  # single-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      ),
      outputspec = outputspec_interactive(),
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "tab"),
        value_col = "value",
        tab_col = "tab"
      ),
      outputspec = outputspec_interactive(),
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates interactive report with alerts even if supplied an empty df", {
  df <- data.frame(
    timepoint = as.Date(numeric()),
    item = character(),
    value = numeric(),
    tab = character()
  )

  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "tab"),
        value_col = "value",
        tab_col = "tab"
      ),
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
      show_progress = FALSE
    )

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates heatmap report even if supplied an empty df", {
  df <- data.frame(
    timepoint = as.Date(numeric()),
    item = character(),
    value = numeric(),
    tab = character()
  )

  # single-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      ),
      outputspec = outputspec_static_heatmap(),
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "tab"),
        value_col = "value",
        tab_col = "tab"
      ),
      outputspec = outputspec_static_heatmap(),
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates multipanel report even if supplied an empty df", {
  df <- data.frame(
    timepoint = as.Date(numeric()),
    item = character(),
    value = numeric(),
    tab = character()
  )

  # single-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      ),
      outputspec = outputspec_static_multipanel(),
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))

  # multi-tab
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "tab"),
        value_col = "value",
        tab_col = "tab"
      ),
      outputspec = outputspec_static_multipanel(),
      show_progress = FALSE
    )
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates multi-item_cols interactive report and returns path successfully", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      3
    ),
    item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
    value = 3,
    tab = c(rep("one", 20), rep("two", 10)),
    stringsAsFactors = FALSE
  )

  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = c("item", "tab"),
        value_col = "value"
      ),
      outputspec = outputspec_interactive(item_labels = c("item" = "My item")),
      show_progress = FALSE
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})


test_that("mantis_report() creates interactive report with spaces in item_cols names", {
  df <- data.frame(
    "m timepoint" = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      3
    ),
    "m item" = c(rep("a", 10), rep("b", 10), rep("c", 10)),
    "m value" = rep(3, 30),
    "m tab" = c(rep("one", 20), rep("two", 10)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "m timepoint",
        item_cols = c("m item", "m tab"),
        value_col = "m value",
        tab_col = "m tab"
      ),
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
      show_progress = FALSE
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})


test_that("format_filename() appends explict timestamp correctly when a path and name is supplied", {
  timestamp_string <- format(Sys.time(), "_%Y%m%d%_%H%M%S")
  orig_file <- file.path(test_path(), "filename.html")
  new_file <- file.path(test_path(), paste0("filename", timestamp_string, ".html"))

  # appends to filename
  expect_equal(
    basename(format_filename(file = orig_file,
                             append_string = timestamp_string)),
    basename(new_file)
  )
  # but does not change path
  expect_equal(
    normalizePath(dirname(format_filename(file = orig_file,
                             append_string = timestamp_string))),
    normalizePath(dirname(new_file))
  )
})


test_that("format_filename() appends explict timestamp correctly when only a name is supplied", {
  timestamp_string <- format(Sys.time(), "_%Y%m%d%_%H%M%S")
  orig_file <- "filename.html"
  new_file <- paste0("filename", timestamp_string, ".html")

  # appends to filename
  expect_equal(
    basename(format_filename(file = orig_file,
                             append_string = timestamp_string)),
    basename(new_file)
  )
  # but does not change path
  expect_equal(
    normalizePath(dirname(format_filename(file = orig_file,
                                          append_string = timestamp_string))),
    normalizePath(dirname(new_file))
  )
})


test_that("mantis_report() appends timestamp to filename appropriately", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      2
    ),
    item = c(rep("a", 10), rep("b", 10)),
    value = rep(c(3, -3), 10),
    stringsAsFactors = FALSE
  )

  # include timestamp
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      ),
      outputspec = outputspec_interactive(),
      add_timestamp = TRUE,
      show_progress = FALSE
    )

  expect_true(grepl("^mantis_testthatreport_[0-9]{8}_[0-9]{6}\\.html$", basename(reportpath)))

  # clean up
  expect_true(file.remove(reportpath))

  # exclude timestamp
  reportpath <-
    mantis_report(
      df,
      file = file.path(tempdir(), "mantis_testthatreport.html"),
      inputspec = inputspec(
        timepoint_col = "timepoint",
        item_cols = "item",
        value_col = "value"
      ),
      outputspec = outputspec_interactive(),
      add_timestamp = FALSE,
      show_progress = FALSE
    )

  expect_equal(basename(reportpath), "mantis_testthatreport.html")

  # clean up
  expect_true(file.remove(reportpath))
})


test_that("mantis_report() creates report in wd when no path supplied", {
  df <- data.frame(
    timepoint = rep(
      seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"),
      2
    ),
    item = c(rep("a", 10), rep("b", 10)),
    value = rep(c(3, -3), 10),
    stringsAsFactors = FALSE
  )

  withr::with_dir(
    tempdir(),
    reportpath <-
      mantis_report(
        df,
        file = "mantis_testthatreport.html",
        inputspec = inputspec(
          timepoint_col = "timepoint",
          item_cols = "item",
          value_col = "value"
        ),
        outputspec = outputspec_interactive(),
        add_timestamp = TRUE,
        show_progress = FALSE
      )
  )

  # clean up
  expect_true(file.remove(reportpath))
})
