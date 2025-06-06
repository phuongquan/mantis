# -----------------------------------------------------------------------------
#' Create html table from prepared data frame
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param inputspec Specification of data in df
#' @param plot_value_type "value" or "delta"
#' @param item_labels Named vector containing string label(s) to use for the "item" column(s) in the
#'   report. The names should correspond to the `item_cols`, and the values should contain the
#'   desired labels. If NULL, the original columns name(s) will be used.
#' @param plot_label Label for History column
#' @param summary_cols vector of which summary columns to include
#' @param plot_type "bar" or "line"
#' @param sync_axis_range sync all history graphs to same y-axis range
#' @param alert_results `alert_results` object returned from `run_alerts()`
#' @param sort_by column in output table to sort by. Can be one of `item`, `alert_overall`, or one
#'   of the summary columns. Append a minus sign to sort in descending order e.g. `-max_value`.
#'   Secondary ordering will be based on `item_order`.
#' @param bordered this param is needed so that we can initialise an "invisible" widget
#'
#' @return html table
#' @noRd
output_table_interactive <- function(prepared_df,
                                     inputspec,
                                     plot_value_type = "value",
                                     item_labels = NULL,
                                     plot_label = NULL,
                                     summary_cols = c("max_value"),
                                     plot_type = "bar",
                                     sync_axis_range = FALSE,
                                     alert_results = NULL,
                                     sort_by = NULL,
                                     bordered = TRUE,
                                     ...) {
  table <- prepare_table(
    prepared_df = prepared_df,
    inputspec = inputspec,
    plot_value_type = plot_value_type,
    alert_results = alert_results,
    sort_by = sort_by
  )

  # generate the item column(s)
  item_cols <- inputspec$item_cols
  item_colDefs <- list()
  for (i in seq_along(item_cols)) {
    # if the item_col is used for a tabset, hide the column from the table
    if (item_cols[i] %in% inputspec$tab_col) {
      item_colDefs[[item_cols_prefix(item_cols[i])]] <-
        reactable::colDef(show = FALSE)
    } else {
      # NOTE: if item_labels contains names that don't match the data, just ignore them
      item_colDefs[[item_cols_prefix(item_cols[i])]] <-
        reactable::colDef(
          name = ifelse(item_cols[i] %in% names(item_labels),
            item_labels[item_cols[i]],
            item_cols[i]
          ),
          filterable = TRUE
        )
    }
  }

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
    columns = c(
      item_colDefs,
      list(
        last_timepoint = reactable::colDef(
          name = "Last timepoint",
          show = "last_timepoint" %in% summary_cols
        ),
        last_value = reactable::colDef(
          name = "Last value",
          show = "last_value" %in% summary_cols
        ),
        last_value_nonmissing = reactable::colDef(
          name = "Last non-missing value",
          show = "last_value_nonmissing" %in% summary_cols
        ),
        max_value = reactable::colDef(
          name = "Max value",
          show = "max_value" %in% summary_cols
        ),
        mean_value = reactable::colDef(
          name = "Mean",
          show = "mean_value" %in% summary_cols
        ),
        # cell argument accepts a function with cell _values_, row _index_, and/or column _names_ as arguments, below just uses _values_
        history = reactable::colDef(
          name = ifelse(is.null(plot_label), inputspec$value_col, plot_label),
          width = 410,
          filterable = FALSE,
          cell = function(value) {
            dy <- dygraphs::dygraph(value,
              height = 40,
              width = 400
            ) |>
              dygraphs::dyOptions(
                drawXAxis = FALSE,
                drawYAxis = FALSE,
                drawGrid = FALSE,
                drawPoints = FALSE
              ) |>
              dygraphs::dyAxis(
                name = "y",
                valueRange = if (sync_axis_range) {
                  value_range_from_history(value_history = table$history)
                } else {
                  value_range_from_history(value_history = value)
                }
              ) |>
              dygraphs::dyAxis(
                name = "x",
                rangePad = 10,
                labelHeight = 0,
                axisHeight = 0
              )
            if (plot_type == "bar") {
              dy <-
                dy |>
                dygraphs::dyBarChart()
            }
            dy
          }
        ),
        alert_overall = reactable::colDef(
          name = "Alerts",
          show = !is.null(alert_results),
          filterable = TRUE,
          cell = function(value) {
            if (!is.na(value)) {
              img_src <- ifelse(grepl("PASS", value),
                knitr::image_uri(system.file(
                  "images",
                  "tick.png",
                  package = utils::packageName(),
                  mustWork = FALSE
                )),
                knitr::image_uri(system.file(
                  "images",
                  "cross.png",
                  package = utils::packageName(),
                  mustWork = FALSE
                ))
              )
              image <- htmltools::img(src = img_src, style = "height: 14px; padding: 0 3px 2px 0; vertical-align: middle;", alt = "")
              htmltools::tagList(
                htmltools::div(style = "display: inline-block;", image),
                value
              )
            } else {
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
      )
    ),
    ...
  )
}


# -----------------------------------------------------------------------------
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
    c(
      min(unlist(lapply(value_history, function(x) {
        min(c(0, min(x, na.rm = TRUE)))
      }))),
      max(unlist(lapply(value_history, function(x) {
        max(c(0, max(x, na.rm = TRUE)))
      }))) + 1
    )
  )
}
