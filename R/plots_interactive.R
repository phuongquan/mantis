#' Create html table from prepared data frame
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param plot_value_type "value" or "delta"
#' @param sync_axis_range sync all history graphs to same y-axis range
#' @param item_label Label for first column
#' @param summary_cols vector of which summary columns to include
#' @param plot_type "bar" or "line"
#'
#' @return html table
#' @noRd
output_table_interactive <- function(prepared_df,
                                     plot_value_type = "value",
                                     item_label = "Item",
                                     plot_label = "History",
                                     summary_cols = c("max_value"),
                                     plot_type = "bar",
                                     sync_axis_range = FALSE,
                                     bordered = TRUE,
                                     ...) {

  # initialise column names to avoid R CMD check Notes
  timepoint <- item <- value <- value_for_history <- NULL

  # TODO: validate inputs

  table <- prepare_table(prepared_df = prepared_df,
                         plot_value_type = plot_value_type)

  reactable::reactable(
    table,
    sortable = TRUE,
    filterable = FALSE,
    searchable = FALSE,
    rownames = FALSE,
    pagination = FALSE,
    striped = TRUE,
    highlight = TRUE,
    fullWidth = TRUE,
    bordered = bordered,
    compact = TRUE,
    columns = list(
      item = reactable::colDef(name = item_label,
                               searchable = TRUE,
                               filterable = TRUE),
      last_timepoint = reactable::colDef(name = "Last timepoint",
                                         show = "last_timepoint" %in% summary_cols),
      last_value = reactable::colDef(name = "Last value",
                                     show = "last_value" %in% summary_cols),
      last_value_nonmissing = reactable::colDef(name = "Last non-missing value",
                                     show = "last_value_nonmissing" %in% summary_cols),
      max_value = reactable::colDef(name = "Max value",
                                     show = "max_value" %in% summary_cols),
      mean_value = reactable::colDef(name = "Mean",
                               show = "mean_value" %in% summary_cols),
      # cell argument accepts a function with cell _values_, row _index_, and/or column _names_ as arguments, below just uses _values_
      history = reactable::colDef(
        name = plot_label,
        width = 410,
        cell = function(value) {
          dy <- dygraphs::dygraph(value,
                                  height = 40,
                                  width = 400) %>%
            dygraphs::dyOptions(
              drawXAxis = FALSE,
              drawYAxis = FALSE,
              drawGrid = FALSE,
              drawPoints = FALSE
            ) %>%
            dygraphs::dyAxis(name = "y",
                             valueRange = if (sync_axis_range) {
                               value_range_from_history(value_history = table$history)
                             } else {
                               value_range_from_history(value_history = value)
                             }) %>%
            dygraphs::dyAxis(
              name = "x",
              rangePad = 10,
              labelHeight = 0,
              axisHeight = 0
            )
          if (plot_type == "bar") {
            dy <-
              dy %>%
              dygraphs::dyBarChart()
          }
          dy
        }
        )
      ),
    ...
    )

}


#' Set y-axis range based on plot_value_type and values
#'
#' @param value_history lists of history values
#'
#' @return vector of min and max values
#' @noRd
value_range_from_history <- function(value_history) {
  # NOTE: add 1 to max otherwise the tooltip doesn't appear
  # NOTE: min/max return warnings when all values are NA
  suppressWarnings(
    c(min(unlist(lapply(value_history, function(x) {
      min(c(0, min(x, na.rm = TRUE)))
    }))),
      max(unlist(lapply(value_history, function(x) {
      max(c(0, max(x, na.rm = TRUE)))
    }))) + 1)
  )
}

