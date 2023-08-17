#' Create html table from prepared data frame
#'
#' @param table from prepare_table()
#' @param sync_axis_range sync all history graphs to same y-axis range
#' @param item_label Label for first column
#' @param summary_cols vector of which summary columns to include
#' @param history_style "bar" or "line"
#'
#' @return html table
#' @export
output_table_html <- function(table,
                         item_label,
                         summary_cols = c("last_value", "mean_last14"),
                         history_style = "bar",
                         sync_axis_range = FALSE) {

  # initialise column names to avoid R CMD check Notes
  item <- last_timepoint <- last_value <- history <- NULL

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
    bordered = TRUE,
    compact = TRUE,
    columns = list(
      item = reactable::colDef(name = item_label,
                               searchable = TRUE,
                               filterable = TRUE),
      last_timepoint = reactable::colDef(name = "Last timepoint",
                                         show = "last_timepoint" %in% summary_cols),
      last_value = reactable::colDef(name = "Last value",
                                     show = "last_value" %in% summary_cols),
      mean = reactable::colDef(name = "Mean",
                               show = "mean" %in% summary_cols),
      mean_last14 = reactable::colDef(name = "Recent Mean",
                                      show = "mean_last14" %in% summary_cols),
      # cell argument accepts a function with cell _values_, row _index_, and/or column _names_ as arguments, below just uses _values_
      history = reactable::colDef(
        name = "History",
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
          if (history_style == "bar") {
            dy <-
              dy %>%
              dygraphs::dyBarChart()
          }
          dy
        }
        )
      )
    )

}


#' Set y-axis range based on history_type and values
#'
#' @param value_history lists of history values
#'
#' @return vector of min and max values
#' @noRd
value_range_from_history <- function(value_history) {
  # NOTE: add 1 to max otherwise the tooltip doesn't appear
  c(min(unlist(lapply(value_history, function(x) {
    min(c(0, min(x, na.rm = TRUE)))
  }))),
    max(unlist(lapply(value_history, function(x) {
    max(c(0, max(x, na.rm = TRUE)))
  }))) + 1)
}

