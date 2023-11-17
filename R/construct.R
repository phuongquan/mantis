
#' Initialise HTML widgets
#'
#' If the output is being constructed in 'asis' chunks, there must also be at least one standard chunk that
#' contains the relevant widgets, otherwise they won't render. dyGraph also needs to be rendered with appropriate plot_type
#' @param plot_type "`bar`" or "`line`", depending on what will be used in real tables.
#'
#' @return A (mostly) invisible html widget
#' @noRd
initialise_widgets <- function(plot_type){
  # https://stackoverflow.com/questions/63534247/recommended-way-to-initialize-js-renderer-in-asis-r-markdown-chunk
  # Currently appears like a line break when rendered. Could try harder to make it invisible but
  # people can always put it at the end of the file if required
  dummy_df <- data.frame(a = as.Date("2023-01-01"), b = "item", c = 1)

  prepare_table(dummy_df,
                timepoint_col = "a",
                item_col = "b",
                value_col = "c") %>%
    output_table_interactive(
      plot_type = plot_type,
      summary_cols = "",
      height = 0,
      bordered = FALSE
    )

}


#' Dynamically generate a single tab for an rmd chunk
#'
#' Chunk options must contain `results = 'asis'`.
#' Function writes directly to the chunk using side-effects
#'
#' @param df Data frame containing time series in long format
#' @param timepoint_col Column to be used for x-axes
#' @param item_col Column containing categorical values identifying distinct time series
#' @param value_col Column containing the time series values which will be used for the y-axes.
#' @param tab_name Character string to appear on parent tab
#' @param tab_level Child level for tab. Value of 1 creates a tab with rmd level "##".
#' @param plot_value_type "value" or "delta"
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param item_order Optional vector containing values from item_col in desired order of display down the table
#' @param item_label String label to use for the "item" column in the table.
#' @param summary_cols Summary data to include as columns in the report. Options are `c("max_value",
#'   "last_value", "last_timepoint", "mean_value", "mean_value_last14")`.
#' @param plot_type Display the time series as a "`bar`" or "`line`" chart.
#' @param plot_label String label to use for the time series column in the table.
#' @param sync_axis_range Set the y-axis to be the same range for all time series in a table.
#'   X-axes are always synced.
#'
#' @return (invisibly) the supplied df
#' @noRd
construct_rmd_tab_item <- function(df,
                              timepoint_col,
                              item_col,
                              value_col,
                              tab_name = NULL,
                              tab_level = 1,
                              plot_value_type = "value",
                              timepoint_limits = c(NA, NA),
                              fill_with_zero = FALSE,
                              item_order = NULL,
                              item_label = "Item",
                              summary_cols = c("max_value"),
                              plot_type = "bar",
                              plot_label = "History",
                              sync_axis_range = FALSE) {

  construct_tab_label(tab_name = tab_name,
                      tab_level = tab_level)

  p <-
    prepare_table(
      df,
      timepoint_col,
      item_col,
      value_col,
      plot_value_type = plot_value_type,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero,
      item_order = item_order
    ) %>%
    output_table_interactive(
      item_label = item_label,
      plot_label = plot_label,
      summary_cols = summary_cols,
      plot_type = plot_type,
      sync_axis_range = sync_axis_range
    )
  # NOTE: a regular print() doesn't render the widget
  cat(knitr::knit_print(p))
  cat("\n")

  invisible(df)
}

#' Dynamically generate a group of tabs for an rmd chunk
#'
#' Chunk options must contain `results = 'asis'`.
#' Function writes directly to the chunk using side-effects
#'
#' @param tab_name Character string to appear on parent tab
#' @param df Data frame containing time series in long format
#' @param timepoint_col Name of column to be used for x-axes
#' @param item_col Name of column containing categorical values identifying distinct time series
#' @param value_col Name of column containing the time series values which will be used for the y-axes.
#' @param tab_col Name of column containing categorical values which will be used to group the time series into different tabs.
#' @param tab_order Optional vector containing values from tab_col in desired order of display
#' @param plot_value_type "value" or "delta"
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param item_order Optional vector containing values from item_col in desired order of display down the table
#' @param item_label String label to use for the "item" column in the table.
#' @param summary_cols Summary data to include as columns in the report. Options are `c("max_value",
#'   "last_value", "last_timepoint", "mean_value", "mean_value_last14")`.
#' @param plot_type Display the time series as a "`bar`" or "`line`" chart.
#' @param plot_label String label to use for the time series column in the table.
#' @param sync_axis_range Set the y-axis to be the same range for all time series in a table.
#'   X-axes are always synced.
#'
#' @return (invisibly) the supplied df
#' @noRd
construct_rmd_tab_group <- function(df,
                                timepoint_col,
                                item_col,
                                value_col,
                                tab_col,
                                tab_order = NULL,
                                tab_group_name = NULL,
                                tab_group_level = 1,
                                plot_value_type = "value",
                                timepoint_limits = c(NA, NA),
                                fill_with_zero = FALSE,
                                item_order = NULL,
                                item_label = "Item",
                                plot_label = "History",
                                summary_cols = c("max_value"),
                                plot_type = "bar",
                                sync_axis_range = FALSE) {

  tab_names <- unique(df[tab_col] %>%
                        dplyr::pull())

  if (!is.null(tab_order)) {
    tab_names <-
      tab_names[order(match(tab_names, tab_order))]
  }

  construct_tab_label(tab_name = tab_group_name,
                      tab_level = tab_group_level,
                      has_child_tabs = TRUE)

  for (i in seq_along(tab_names)) {
    dftab <-
      df %>% dplyr::filter(.data[[tab_col]] == tab_names[i])

    construct_rmd_tab_item(
      df = dftab,
      timepoint_col = timepoint_col,
      item_col = item_col,
      value_col = value_col,
      tab_name = tab_names[i],
      tab_level = tab_group_level + 1,
      plot_value_type = plot_value_type,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero,
      item_order = item_order,
      item_label = item_label,
      summary_cols = summary_cols,
      plot_type = plot_type,
      sync_axis_range = sync_axis_range
    )
  }

  invisible(df)
}

construct_tab_label <- function(tab_name, tab_level, has_child_tabs = FALSE){
  if (!is.null(tab_name)) {
    cat("\n",
        paste0(rep("#", tab_level + 1), collapse = ""),
        " ", tab_name,
        ifelse(has_child_tabs," {.tabset}", ""),
        "\n",
        sep = "")
  }
}
