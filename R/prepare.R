#' Convert supplied df into required format for generating tables/plots
#'
#' Supplied df needs to be long (at least for now)
#'
#' @param df
#' @param timepoint_col
#' @param item_col
#' @param value_col
#' @param plot_value_type "value" or "delta"
#' @param timepoint_limits set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero replace any missing or NA values with 0? Useful when value_col is a record count
#' @param item_order vector of values contained in item_col, for ordering the items in the table. Any values not mentioned are included alphabetically at the end
#'
#' @return data frame
#' @noRd
#'
#' @examples
prepare_table <-
  function(df,
           timepoint_col,
           item_col,
           value_col,
           plot_value_type = "value",
           timepoint_limits = c(NA, NA),
           fill_with_zero = FALSE,
           item_order = NULL) {

  # TODO: Validate supplied colnames against df
  # TODO: allow df to be passed in wide with vector of value_cols?

  # initialise column names to avoid R CMD check Notes
  timepoint <- item <- value <- value_for_history <- NULL

  # rename cols for ease. may want to figure out how to keep original colnames
  table_df <-
    df %>%
    dplyr::rename(timepoint = all_of(timepoint_col),
                  item = all_of(item_col),
                  value = all_of(value_col))

  table_df <-
    align_data_timepoints(df = table_df,
                          timepoint_limits = timepoint_limits)

  if (fill_with_zero) {
    table_df <-
      table_df %>%
      tidyr::replace_na(list(value = 0))
  }

  # add history column
  table_df <-
    table_df %>%
    dplyr::group_by(item) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::mutate(
      value_for_history = dplyr::case_when(
        plot_value_type == "value" ~ as.numeric(value),
        plot_value_type == "delta" ~ as.numeric(value) - dplyr::lag(as.numeric(value))
      )
    ) %>%
    dplyr::summarise(
      last_timepoint = suppressWarnings(max(timepoint[!is.na(value)])),
      last_value = rev(value)[1],
      # TODO: add last_value_nonmissing
      max_value = suppressWarnings(max(value, na.rm = TRUE)),
      # TODO: match precision to values
      mean_value = round(mean(value, na.rm = TRUE),
                   digits = 1),
      mean_value_last14 = round(mean(rev(value)[1:14], na.rm = TRUE),
                          digits = 1),
      history = history_to_list(value_for_history,
                                timepoint,
                                plot_value_type),
      .groups = "drop"
    )

  if (!is.null(item_order)) {
    table_df <-
      table_df %>%
      dplyr::arrange(factor(item, levels = item_order))
  }

  table_df
}


#' Specify relevant columns in the source data frame
#'
#' @param timepoint_col String denoting the (datetime) column which will be used for the x-axes.
#' @param item_col String denoting the (character) column containing categorical values identifying
#'   distinct time series.
#' @param value_col String denoting the (numeric) column containing the time series values which
#'   will be used for the y-axes.
#' @param group_col Optional. String denoting the (character) column containing categorical values
#'   which will be used to group the time series into different tabs on the report.
#'
#' @return A `colspec()` object
#' @export
colspec <- function(timepoint_col,
                    item_col,
                    value_col,
                    group_col = NULL){
  structure(
    list(timepoint_col = timepoint_col,
       item_col = item_col,
       value_col = value_col,
       group_col = group_col),
    class = "tinduck_colspec")
}

#' Specify output options for the report
#'
#' @param plot_value_type Display the raw "`value`" for the time series or display the calculated
#'   "`delta`" between consecutive values.
#' @param plot_type Display the time series as a "`bar`" or "`line`" chart.
#' @param item_label String label to use for the "item" column in the report.
#' @param plot_label String label to use for the time series column in the report.
#' @param summary_cols Summary data to include as columns in the report. Options are `c("max_value",
#'   "last_value", "last_timepoint", "mean_value", "mean_value_last14")`.
#' @param sync_axis_range Set the y-axis to be the same range for all time series in a table.
#'   X-axes are always synced.
#'
#' @return An `outputspec()` object
#' @export
outputspec <- function(plot_value_type = "value",
                       plot_type = "bar",
                       item_label = "Item",
                       plot_label = "History",
                       summary_cols = c("max_value"),
                       sync_axis_range = FALSE){

  structure(
    list(plot_value_type = plot_value_type,
         plot_type = plot_type,
         item_label = item_label,
         plot_label = plot_label,
         summary_cols = summary_cols,
         sync_axis_range = sync_axis_range),
    class = "tinduck_outputspec")
}

history_to_list <-
  function(value_for_history,
           timepoint,
           plot_value_type) {

    ts <-
      data.frame(value_for_history,
                 timepoint,
                 row.names = 2) %>%
      xts::as.xts() %>%
      list()
    names(ts[[1]]) <- plot_value_type
    ts
  }


#' Align the timepoint values across all items
#'
#' @param df
#' @param timepoint_limits
#'
#' Ensure timepoint values are the same for all items, for consistency down the table.
#' Also can restrict/expand data to a specified period here as cannot set xlimits in dygraphs.
#' # TODO: THIS CURRENTLY ONLY WORKS FOR DAILY TIMEPOINTS
#'
#' @return
#' @noRd
align_data_timepoints <-
  function(df,
           timepoint_limits = c(NA, NA)) {

  # TODO: Need to work out correct limits to use based on df
  #  in case supplied limits don't match df granularity
  if (is.na(timepoint_limits[1])) {
    min_timepoint <- min(df$timepoint)
  } else{
    min_timepoint <- timepoint_limits[1]
  }
  if (is.na(timepoint_limits[2])) {
    max_timepoint <- max(df$timepoint)
  } else{
    max_timepoint <- timepoint_limits[2]
  }

  # TODO: Need to work out correct granularity to use based on df
  #  as don't want to insert unnecessary rows
  all_timepoints <- seq(min_timepoint, max_timepoint, by = "day")

  df_out <-
    df %>%
    tidyr::pivot_wider(names_from = item,
                       values_from = value,
                       names_prefix = "piv_") %>%
    # insert any missing timepoint values
    dplyr::full_join(data.frame("timepoint" = all_timepoints), by = "timepoint") %>%
    # restrict to specified limits
    dplyr::filter(timepoint >= min_timepoint & timepoint <= max_timepoint) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("piv_"),
                        names_to = "item",
                        names_prefix = "piv_")

  df_out

}
