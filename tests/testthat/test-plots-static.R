test_that("plot_heatmap_static() looks how it should", {
  df <- mantis::example_data

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  prepared_df <- prepare_df(df, inputspec = inputspec)

  p <-
    plot_heatmap_static(
      prepared_df,
      inputspec,
      fill_colour = "blue",
      y_label = "Item"
    )

  vdiffr::expect_doppelganger(
    title = "example_data_heatmap_static",
    fig = p
  )
})


test_that("plot_multipanel_static() looks how it should", {
  df <- mantis::example_data

  inputspec <- inputspec(
    timepoint_col = "timepoint",
    item_cols = "item",
    value_col = "value"
  )

  prepared_df <- prepare_df(df, inputspec = inputspec)

  p <-
    plot_multipanel_static(
      prepared_df,
      inputspec,
      sync_axis_range = FALSE,
      y_label = "Item"
    )

  vdiffr::expect_doppelganger(
    title = "example_data_multipanel_static",
    fig = p
  )
})


test_that("empty_plot_static() looks how it should", {
  p <- empty_plot_static()

  vdiffr::expect_doppelganger(
    title = "example_data_empty_static",
    fig = p
  )
})
