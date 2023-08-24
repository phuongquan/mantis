
#' Dynamically generate a single tab for an rmd chunk
#'
#' Chunk options must contain `results = 'asis'`.
#' Function writes directly to the chunk using side-effects
#'
#' @param tab_name
#' @param df
#' @param timepoint_col
#' @param item_col
#' @param value_col
#' @param tab_col
#' @param tab_order
#' @param history_type
#' @param timepoint_limits
#' @param fill_with_zero
#' @param item_order
#' @param item_label
#' @param summary_cols
#' @param history_style
#' @param sync_axis_range
#'
#' @return
#' @export
construct_rmd_tab_item <- function(df,
                              timepoint_col,
                              item_col,
                              value_col,
                              tab_name = NULL,
                              history_type = "value",
                              timepoint_limits = c(NA, NA),
                              fill_with_zero = FALSE,
                              item_order = NULL,
                              item_label = "Item",
                              summary_cols = c("max_value"),
                              history_style = "bar",
                              sync_axis_range = FALSE) {

  # TODO: consider allowing tab level to be passed in as a param
  if (!is.null(tab_name)) {
    cat("\n###", tab_name, "\n")
  }

  p <-
    prepare_table(
      df,
      timepoint_col,
      item_col,
      value_col,
      history_type = history_type,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero,
      item_order = item_order
    ) %>%
    output_table_html(
      item_label = item_label,
      summary_cols = summary_cols,
      history_style = history_style,
      sync_axis_range = sync_axis_range
    )
  # NOTE: a regular print() doesn't render the widget
  cat(knitr::knit_print(p))
  cat("\n")

}

#' Dynamically generate a group of tabs for an rmd chunk
#'
#' Chunk options must contain `results = 'asis'`.
#' Function writes directly to the chunk using side-effects
#'
#' @param tab_name
#' @param df
#' @param timepoint_col
#' @param item_col
#' @param value_col
#' @param tab_col
#' @param tab_order
#' @param history_type
#' @param timepoint_limits
#' @param fill_with_zero
#' @param item_order
#' @param item_label
#' @param summary_cols
#' @param history_style
#' @param sync_axis_range
#'
#' @return
#' @export
construct_rmd_tab_group <- function(df,
                                timepoint_col,
                                item_col,
                                value_col,
                                tab_col,
                                tab_order = NULL,
                                tab_group_name = NULL,
                                history_type = "value",
                                timepoint_limits = c(NA, NA),
                                fill_with_zero = FALSE,
                                item_order = NULL,
                                item_label = "Item",
                                summary_cols = c("max_value"),
                                history_style = "bar",
                                sync_axis_range = FALSE) {

  tab_names <- unique(df[tab_col] %>%
                        dplyr::pull())

  if (!is.null(tab_order)) {
    tab_names <-
      tab_names[order(match(tab_names, tab_order))]
  }

  # TODO: consider allowing tab level to be passed in as a param
  if (!is.null(tab_group_name)) {
    cat("\n##", tab_group_name, " {.tabset}\n")
  }

  for (i in seq_along(tab_names)) {
    dftab <-
      df %>% dplyr::filter(.data[[tab_col]] == tab_names[i])

    construct_rmd_tab_item(
      df = dftab,
      timepoint_col = timepoint_col,
      item_col = item_col,
      value_col = value_col,
      tab_name = tab_names[i],
      history_type = history_type,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero,
      item_order = item_order,
      item_label = item_label,
      summary_cols = summary_cols,
      history_style = history_style,
      sync_axis_range = sync_axis_range
    )
  }

}
