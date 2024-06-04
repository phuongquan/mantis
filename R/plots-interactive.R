#' Create html table from prepared data frame
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param plot_value_type "value" or "delta"
#' @param sync_axis_range sync all history graphs to same y-axis range
#' @param alert_results `alert_results` object returned from `run_alerts()`
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
                                     alert_results = NULL,
                                     sort_cols = NULL,
                                     bordered = TRUE,
                                     ...) {

  # initialise column names to avoid R CMD check Notes
  timepoint <- item <- value <- value_for_history <- NULL

  # TODO: validate inputs

  table <- prepare_table(prepared_df = prepared_df,
                         plot_value_type = plot_value_type,
                         alert_results = alert_results,
                         sort_cols = sort_cols)

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
      item = reactable::colDef(
        name = item_label,
        searchable = TRUE,
        filterable = TRUE
      ),
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
      ),
      alert_overall = reactable::colDef(
        name = "Alerts",
        show = !is.null(alert_results),
        searchable = TRUE,
        filterable = TRUE,
        cell = function(value) {
          if (!is.na(value)) {
            paste0(ifelse(grepl("PASS", value), "\u2714\ufe0f", "\u274c\ufe0f"), " ", value)
          } else{
            "-"
          }
        },
        details = function(index) {
          alert_details <- table[index, "alert_details"][[1]][[1]]
          if (!is.null(alert_details)) {
            htmltools::div(
              style = "padding: 1rem",
              reactable::reactable(
                alert_details,
                columns = list(
                  alert_description = reactable::colDef(name = "Rule"),
                  alert_result = reactable::colDef(name = "Result")
                ),
                outlined = TRUE,
                highlight = TRUE,
                fullWidth = TRUE
              )
            )
          }
        }
      ),
      alert_details = reactable::colDef(show = FALSE)
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

