test_that("bespoke_rmd_initialise_widgets() runs without error", {
  expect_no_error(bespoke_rmd_initialise_widgets(plot_type = "bar"))
  expect_visible(bespoke_rmd_initialise_widgets(plot_type = "bar"))
})


test_that("rmd_fig_height() returns suitable height for heatmap plots", {
  # each row in the heatmap = 0.6 inches
  # fig heights must be same in all tabs so use highest number of rows across all tabs

  quick_df <- function(items, tabs = NA) {
      data.frame(
        timepoint = seq(
          as.Date("2022-01-01"),
          as.Date("2022-01-01") + length(items) - 1,
          by = "days"
        ),
        item = items,
        value = items,
        tab = tabs
      )
  }

  inputspec_notab <- inputspec(timepoint_col = "timepoint",
                         item_cols = "item",
                         value_col = "value")

  inputspec_tab <- inputspec(timepoint_col = "timepoint",
                         item_cols = c("item", "tab"),
                         value_col = "value",
                         tab_col = "tab")
  # 0 rows
  expect_null(
    rmd_fig_height(df = data.frame(timepoint = as.Date(numeric()), item = character(), value = character()),
                   inputspec = inputspec_notab,
                   outputspec = outputspec_static_heatmap())
  )
  expect_null(
    rmd_fig_height(df = data.frame(timepoint = as.Date(numeric()), item = character(), value = character(), tab = character()),
                   inputspec = inputspec_tab,
                   outputspec = outputspec_static_heatmap())
  )

  # 1 item, no tabs
  expect_equal(
    rmd_fig_height(df = quick_df(items = 1),
                   inputspec = inputspec_notab,
                   outputspec = outputspec_static_heatmap()),
    0.6
  )

  # 10 items, no tabs
  expect_equal(
    rmd_fig_height(df = quick_df(items = 1:10),
                   inputspec = inputspec_notab,
                   outputspec = outputspec_static_heatmap()),
    0.6 * 10
  )

  # 10 items, 3 in tab_a 7 in tab_b, no overlap, maxrows=7
  expect_equal(
    rmd_fig_height(df = quick_df(items = 1:10, tabs = c(rep("a", 3), rep("b", 7))),
                   inputspec = inputspec_tab,
                   outputspec = outputspec_static_heatmap()),
    0.6 * 7
  )

  # 10 items, 3 in tab_a 7 in tab_b, two in both, maxrows=7
  expect_equal(
    rmd_fig_height(df = quick_df(items = c(1:3, 2:8), tabs = c(rep("a", 3), rep("b", 7))),
                   inputspec = inputspec_tab,
                   outputspec = outputspec_static_heatmap()),
    0.6 * 7
  )

})

test_that("rmd_fig_height() returns suitable height for multipanel plots", {
  # each row in the multipanel = 0.8 inches
  # fig heights must be same in all tabs so use highest number of rows across all tabs

  quick_df <- function(items, tabs = NA) {
      data.frame(
        timepoint = seq(
          as.Date("2022-01-01"),
          as.Date("2022-01-01") + length(items) - 1,
          by = "days"
        ),
        item = items,
        value = items,
        tab = tabs
      )
  }

  inputspec_notab <- inputspec(timepoint_col = "timepoint",
                         item_cols = "item",
                         value_col = "value")

  inputspec_tab <- inputspec(timepoint_col = "timepoint",
                             item_cols = c("item", "tab"),
                             value_col = "value",
                             tab_col = "tab")
  # 0 rows
  expect_null(
    rmd_fig_height(df = data.frame(timepoint = as.Date(numeric()), item = character(), value = character()),
                   inputspec = inputspec_notab,
                   outputspec = outputspec_static_multipanel())
  )
  expect_null(
    rmd_fig_height(df = data.frame(timepoint = as.Date(numeric()), item = character(), value = character(), tab = character()),
                   inputspec = inputspec_tab,
                   outputspec = outputspec_static_multipanel())
  )

  # 1 item, no tabs
  expect_equal(
    rmd_fig_height(df = quick_df(items = 1),
                   inputspec = inputspec_notab,
                   outputspec = outputspec_static_multipanel()),
    0.8
  )

  # 10 items, no tabs
  expect_equal(
    rmd_fig_height(df = quick_df(items = 1:10),
                   inputspec = inputspec_notab,
                   outputspec = outputspec_static_multipanel()),
    0.8 * 10
  )

  # 10 items, 3 in tab_a 7 in tab_b, no overlap, maxrows=7
  expect_equal(
    rmd_fig_height(df = quick_df(items = 1:10, tabs = c(rep("a", 3), rep("b", 7))),
                   inputspec = inputspec_tab,
                   outputspec = outputspec_static_multipanel()),
    0.8 * 7
  )

  # 10 items, 3 in tab_a 7 in tab_b, two in both, maxrows=7
  expect_equal(
    rmd_fig_height(df = quick_df(items = c(1:3, 2:8), tabs = c(rep("a", 3), rep("b", 7))),
                   inputspec = inputspec_tab,
                   outputspec = outputspec_static_multipanel()),
    0.8 * 7
  )

})
