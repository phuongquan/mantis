test_that("mantis_report() creates single-tab interactive report and returns path successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 2),
                   item = c(rep("a", 10), rep("b", 10)),
                   value = rep(3, 20),
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      colspec = colspec(timepoint_col = "timepoint",
                        item_col = "item",
                        value_col = "value"),
      outputspec = outputspec_interactive()
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
      colspec = colspec(timepoint_col = "timepoint",
                        item_col = "item",
                        value_col = "value",
                        tab_col = "tab"),
      outputspec = outputspec_interactive()
      )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates single-tab heatmap report and returns path successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 2),
                   item = c(rep("a", 10), rep("b", 10)),
                   value = rep(3, 20),
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      colspec = colspec(timepoint_col = "timepoint",
                        item_col = "item",
                        value_col = "value"),
      outputspec = outputspec_static_heatmap()
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates single-tab multipanel report and returns path successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 2),
                   item = c(rep("a", 10), rep("b", 10)),
                   value = rep(3, 20),
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      colspec = colspec(timepoint_col = "timepoint",
                        item_col = "item",
                        value_col = "value"),
      outputspec = outputspec_static_multipanel()
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

