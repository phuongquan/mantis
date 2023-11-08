
#' Initialise HTML widgets
#'
#' If the output is being constructed in 'asis' chunks, there must also be at least one standard chunk that
#' contains the relevant widgets, otherwise they won't render. dyGraph also needs to be rendered with appropriate plot_type
#'
#' @return
#' @export
#'
#' @examples
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
#' @param tab_name
#' @param df
#' @param timepoint_col
#' @param item_col
#' @param value_col
#' @param tab_col
#' @param tab_order
#' @param plot_value_type
#' @param timepoint_limits
#' @param fill_with_zero
#' @param item_order
#' @param item_label
#' @param summary_cols
#' @param plot_type
#' @param sync_axis_range
#'
#' @return
#' @export
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
#' @param plot_value_type
#' @param timepoint_limits
#' @param fill_with_zero
#' @param item_order
#' @param item_label
#' @param summary_cols
#' @param plot_type
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
