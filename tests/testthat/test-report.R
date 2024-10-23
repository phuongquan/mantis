test_that("mantis_report() creates single-tab interactive report and returns path successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 2),
                   item = c(rep("a", 10), rep("b", 10)),
                   value = rep(3, 20),
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
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
      inputspec = inputspec(timepoint_col = "timepoint",
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
      inputspec = inputspec(timepoint_col = "timepoint",
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
      inputspec = inputspec(timepoint_col = "timepoint",
                        item_col = "item",
                        value_col = "value"),
      outputspec = outputspec_static_multipanel()
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates interactive report with alerts successfully", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
                   value = c(rep(3, 20), rep(NA, 10)),
                   stringsAsFactors = FALSE)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                        item_col = "item",
                        value_col = "value"),
      outputspec = outputspec_interactive(),
      alert_rules = alert_rules(
        alert_missing(
          extent_type = "all",
          items = c("b", "c")
        ),
        alert_above(
          extent_type = "any",
          extent_value = 1,
          rule_value = 5,
          items = c("b", "c")
        )
      )
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})


test_that("mantis_report() creates single-tab interactive report even if supplied an empty df", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 2),
                   item = c(rep("a", 10), rep("b", 10)),
                   value = rep(3, 20),
                   stringsAsFactors = FALSE) %>%
    dplyr::slice(0)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_col = "item",
                            value_col = "value"),
      outputspec = outputspec_interactive()
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates multi-tab interactive report even if supplied an empty df", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("a", 10), rep("b", 10), rep("c", 10)),
                   value = rep(3, 30),
                   tab = c(rep("one", 20), rep("two", 10)),
                   stringsAsFactors = FALSE) %>%
    dplyr::slice(0)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_col = "item",
                            value_col = "value",
                            tab_col = "tab"),
      outputspec = outputspec_interactive()
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates interactive report with alerts even if supplied an empty df", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 3),
                   item = c(rep("c", 10), rep("b", 10), rep("a", 10)),
                   value = c(rep(3, 20), rep(1, 10)),
                   stringsAsFactors = FALSE) %>%
    dplyr::slice(0)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_col = "item",
                            value_col = "value"),
      outputspec = outputspec_interactive(),
      alert_rules = alert_rules(
        alert_missing(
          extent_type = "all",
          items = c("b", "c")
        ),
        alert_above(
          extent_type = "any",
          extent_value = 1,
          rule_value = 5,
          items = c("b", "c")
        )
      )
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates single-tab heatmap report even if supplied an empty df", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 2),
                   item = c(rep("a", 10), rep("b", 10)),
                   value = rep(3, 20),
                   stringsAsFactors = FALSE) %>%
    dplyr::slice(0)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_col = "item",
                            value_col = "value"),
      outputspec = outputspec_static_heatmap()
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("mantis_report() creates single-tab multipanel report even if supplied an empty df", {
  df <- data.frame(timepoint = rep(seq(as.Date("2022-01-01"), as.Date("2022-01-10"), by = "days"), 2),
                   item = c(rep("a", 10), rep("b", 10)),
                   value = rep(3, 20),
                   stringsAsFactors = FALSE) %>%
    dplyr::slice(0)

  reportpath <-
    mantis_report(
      df,
      inputspec = inputspec(timepoint_col = "timepoint",
                            item_col = "item",
                            value_col = "value"),
      outputspec = outputspec_static_multipanel()
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

