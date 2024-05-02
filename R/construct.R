
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

  prepare_df(
    dummy_df,
    timepoint_col = "a",
    item_col = "b",
    value_col = "c"
  ) %>%
    prepare_table() %>%
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
#' @param outputspec Specification for display of tab contents
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param item_order vector of values contained in item_col, for ordering the items in the table. Any values not mentioned are included alphabetically at the end. If NULL, the original order as given by unique(item_col) will be used.
#' @param tab_name Character string to appear on parent tab
#' @param tab_level Child level for tab. Value of 1 creates a tab with rmd level "##".
#'
#' @return (invisibly) the supplied df
#' @noRd
construct_rmd_tab_item <- function(df,
                              timepoint_col,
                              item_col,
                              value_col,
                              outputspec,
                              timepoint_limits = c(NA, NA),
                              fill_with_zero = FALSE,
                              item_order = TRUE,
                              tab_name = NULL,
                              tab_level = 1) {

  construct_tab_label(tab_name = tab_name,
                      tab_level = tab_level)

  prepared_df <-
    prepare_df(
      df,
      timepoint_col = timepoint_col,
      item_col = item_col,
      value_col = value_col,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero,
      item_order = item_order
    )

  if (is_outputspec_static_heatmap(outputspec)) {
      plot_heatmap_static(prepared_df = prepared_df,
                          fill_colour = outputspec$fill_colour,
                          y_label = outputspec$y_label) %>%
      print()
  } else if (is_outputspec_static_multipanel(outputspec)) {
      plot_multipanel_static(prepared_df = prepared_df,
                             sync_axis_range = outputspec$sync_axis_range,
                             y_label = outputspec$y_label) %>%
      print()
  } else if (is_outputspec_interactive(outputspec)) {
    p <-
      prepare_table(
        prepared_df,
        plot_value_type = outputspec$plot_value_type
      ) %>%
      output_table_interactive(
        item_label = outputspec$item_label,
        plot_label = outputspec$plot_label,
        summary_cols = outputspec$summary_cols,
        plot_type = outputspec$plot_type,
        sync_axis_range = outputspec$sync_axis_range
      )
    # NOTE: a regular print() doesn't render the widget
    cat(knitr::knit_print(p))
  }

  cat("\n")

  invisible(df)
}

#' Dynamically generate a group of tabs for an rmd chunk
#'
#' Chunk options must contain `results = 'asis'`.
#' Function writes directly to the chunk using side-effects
#'
#' @param df Data frame containing time series in long format
#' @param timepoint_col Name of column to be used for x-axes
#' @param item_col Name of column containing categorical values identifying distinct time series
#' @param value_col Name of column containing the time series values which will be used for the y-axes.
#' @param tab_col Name of column containing categorical values which will be used to group the time series into different tabs.
#' @param outputspec Specification for display of tab contents
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param item_order vector of values contained in item_col, for ordering the items in the table. Any values not mentioned are included alphabetically at the end. If NULL, the original order as given by unique(item_col) will be used.
#' @param tab_order Optional vector containing values from tab_col in desired order of display
#' @param tab_group_name Character string to appear on parent tab
#' @param tab_group_level integer specifying the nesting level of the parent tab. Value of 1 equates to rmd level "##".
#'
#' @return (invisibly) the supplied df
#' @noRd
#' @importFrom dplyr .data
construct_rmd_tab_group <- function(df,
                                timepoint_col,
                                item_col,
                                value_col,
                                tab_col,
                                outputspec,
                                timepoint_limits = c(NA, NA),
                                fill_with_zero = FALSE,
                                item_order = TRUE,
                                tab_order = NULL,
                                tab_group_name = NULL,
                                tab_group_level = 1) {

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
      outputspec = outputspec,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero,
      item_order = item_order,
      tab_name = tab_names[i],
      tab_level = tab_group_level + 1
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
