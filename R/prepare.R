#' Convert supplied df into required format for generating tables/plots
#'
#' Supplied df needs to be long (at least for now)
#'
#' @param df A data frame containing multiple time series in long format
#' @param timepoint_col Name of column to be used for x-axes
#' @param item_col Name of column containing categorical values identifying distinct time series
#' @param value_col Name of column containing the time series values which will be used for the y-axes.
#' @param plot_value_type "value" or "delta"
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param item_order vector of values contained in item_col, for ordering the items in the table. Any values not mentioned are included alphabetically at the end. If NULL, the original order as given by unique(item_col) will be used.
#'
#' @return data frame
#' @noRd
prepare_df <-
  function(df,
           timepoint_col,
           item_col,
           value_col,
           timepoint_limits = c(NA, NA),
           fill_with_zero = FALSE,
           item_order = NULL) {

  # initialise column names to avoid R CMD check Notes
  item <- NULL

  # keep only relevant cols and rename for ease. may want to figure out how to keep original colnames
  prepared_df <-
    df %>%
    dplyr::select(timepoint = dplyr::all_of(timepoint_col),
                  item = dplyr::all_of(item_col),
                  value = dplyr::all_of(value_col))

  prepared_df <-
    align_data_timepoints(df = prepared_df,
                          timepoint_limits = timepoint_limits)

  if (fill_with_zero) {
    prepared_df <-
      prepared_df %>%
      tidyr::replace_na(list(value = 0))
  }

  if (!is.null(item_order)) {
    prepared_df <-
      prepared_df %>%
      dplyr::arrange(item) %>%
      dplyr::arrange(factor(item, levels = item_order))
  }

  prepared_df
}

#' Convert supplied df into required format for generating tables/plots
#'
#' Supplied df needs to be long (at least for now)
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param plot_value_type "value" or "delta"
#'
#' @return data frame
#' @noRd
prepare_table <-
  function(prepared_df,
           plot_value_type = "value") {

  # TODO: allow df to be passed in wide with vector of value_cols?

  # initialise column names to avoid R CMD check Notes
  timepoint <- item <- value <- value_for_history <- NULL

  # validate inputs

  # store order as later group_by will alphabetise
  item_order_final <- unique(prepared_df$item)

  # add history column
  table_df <-
    prepared_df %>%
    dplyr::group_by(item) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::mutate(
      value_for_history = dplyr::case_when(
        plot_value_type == "value" ~ as.numeric(value),
        plot_value_type == "delta" ~ as.numeric(value) - dplyr::lag(as.numeric(value))
      )
    ) %>%
    dplyr::summarise(
      last_timepoint = max_else_na(timepoint[!is.na(value)]),
      last_value = rev(value)[1],
      last_value_nonmissing = rev(value[!is.na(value)])[1],
      max_value = max_else_na(value),
      # TODO: match precision to values
      mean_value = round(mean(value, na.rm = TRUE),
                   digits = 1),
      history = history_to_list(value_for_history,
                                timepoint,
                                plot_value_type),
      .groups = "drop"
    )

  table_df <-
    table_df %>%
    dplyr::arrange(factor(item, levels = item_order_final))

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
#'   "last_value", "last_value_nonmissing", "last_timepoint", "mean_value")`.
#' @param sync_axis_range Set the y-axis to be the same range for all time series in a table.
#'   X-axes are always synced.
#'
#' @return An `outputspec()` object
#' @export
outputspec_interactive <- function(plot_value_type = "value",
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
    class = c("tinduck_outputspec", "tinduck_outputspec_interactive")
  )
}

#' Specify output options for a static report containing heatmaps
#'
#' @param fill_colour colour to use for the tiles
#' @param y_label string for y-axis label. Optional
#'
#' @return An `outputspec()` object
#' @export
outputspec_heatmap_static <- function(fill_colour = "blue",
                                      y_label = NULL) {

  structure(
    list(fill_colour = fill_colour,
         y_label = y_label),
    class = c("tinduck_outputspec", "tinduck_outputspec_static", "tinduck_outputspec_heatmap_static")
    )
}

#' Specify output options for a static report containing grids of plots.
#'
#' Currently only creates scatter plots
#'
#' @param sync_axis_range Set the y-axis to be the same range for all the plots.
#'   X-axes are always synced.
#' @param y_label string for y-axis label. Optional
#'
#' @return An `outputspec()` object
#' @export
outputspec_multiplot_static <- function(sync_axis_range = FALSE,
                                        y_label = NULL) {

  structure(
    list(sync_axis_range = sync_axis_range,
         y_label = y_label),
    class = c("tinduck_outputspec", "tinduck_outputspec_static", "tinduck_outputspec_multiplot_static")
  )
}

#-----------------------------------------------------------------------
#' Test if object is an outputspec
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec <- function(x) inherits(x, "tinduck_outputspec")

#' Test if object is an outputspec_interactive
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec_interactive <- function(x) inherits(x, "tinduck_outputspec_interactive")

#' Test if object is an outputspec_static
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec_static <- function(x) inherits(x, "tinduck_outputspec_static")

#' Test if object is an outputspec_heatmap_static
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec_heatmap_static <- function(x) inherits(x, "tinduck_outputspec_heatmap_static")

#' Test if object is an outputspec_multiplot_static
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec_multiplot_static <- function(x) inherits(x, "tinduck_outputspec_multiplot_static")


#-----------------------------------------------------------------------

#' Create a time series as a list so it can be stored in a df cell
#'
#' @param value_for_history vector of values
#' @param timepoint vector of dates
#' @param plot_value_type "value" or "delta". Indicates the type of values in the time series
#'
#' @return list
#' @noRd
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
#' @param df Data frame with 3 columns: timepoint, item, and value
#' @param timepoint_limits Vector containing min and max dates for the x-axes. Use Date type.
#'
#' Ensure timepoint values are the same for all items, for consistency down the table.
#' Also can restrict/expand data to a specified period here as cannot set xlimits in dygraphs.
#' # TODO: THIS CURRENTLY ONLY WORKS FOR DAILY TIMEPOINTS
#'
#' @return Data frame with consistent timepoints
#' @noRd
align_data_timepoints <-
  function(df,
           timepoint_limits = c(NA, NA)) {

  # initialise column names to avoid R CMD check Notes
  timepoint <- item <- value <- NULL

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


#' Wrapper for max function
#'
#' Returns NA (instead of Inf) if all values are NA. Retains datatype and avoids using suppressWarnings.
#'
#' @param x vector of values
#'
#' @return Maximum value excluding NAs
#' @noRd
max_else_na <- function(x){
  if (all(is.na(x))) {
    if ("Date" %in% class(x)) {
      as.Date(NA)
    } else{
      NA_real_
    }
  } else{
    max(x, na.rm = TRUE)
  }
}

#' Validate the supplied df against the supplied colspec
#'
#' If there are any validation errors, these are all compiled before calling a
#' single stop()
#'
#' @param df user supplied df
#' @param colspec user supplied colspec
#'
#' @noRd
validate_df_to_colspec <- function(df,
                                   colspec){

  # validate - collect all errors together and return only once
  err_validation <- character()

  err_validation <-
    append(err_validation,
           validate_df_to_colspec_names(df, colspec))

  # only do the following checks if the colspec and df names are valid
  if (length(err_validation) == 0) {
    err_validation <-
      append(err_validation,
             validate_df_to_colspec_duplicate_timepoints(df, colspec))
  }

  # call stop() if there are any validation errors
  if (length(err_validation) > 0) {
    stop_custom(
      .subclass = "invalid_data",
      message = paste0(
        "Invalid data or column names supplied.\n",
        paste(err_validation, collapse = "\n")
      )
    )
  }

}

#' Check names in supplied df and colspec
#'
#' @param df user supplied df
#' @param colspec user supplied colspec
#'
#' @return character string containing any error messages
#' @noRd
validate_df_to_colspec_names <- function(df,
                                         colspec){

  err_validation <- character()

  # drop any items in the colspec that are NULL
  colspec_vector <- unlist(colspec)
  # ignore any columns in df that are not in specification
  dfnames <- names(df)[names(df) %in% colspec_vector]

  # check for duplicate names in df
  if (anyDuplicated(dfnames) > 0) {
    err_validation <-
      append(
        err_validation,
        paste(
          "Duplicate column names in data: [",
          paste(dfnames[duplicated(dfnames)], collapse = ", "),
          "]"
        )
      )
  }
  # check for duplicate names in colspec
  if (anyDuplicated(colspec_vector) > 0) {
    err_validation <-
      append(
        err_validation,
        paste(
          "Duplicate column names in colspec: [",
          paste(colspec_vector[duplicated(colspec_vector)], collapse = ", "),
          "]. Each colspec parameter must refer to a different column in the df "
        )
      )
  }
  # check supplied colnames are present in df
  for (i in seq_along(colspec_vector)){
    if (!colspec_vector[i] %in% dfnames) {
      err_validation <-
        append(
          err_validation,
          paste(
            names(colspec_vector)[i],
            "specified to be [",
            colspec_vector[i],
            "] but column is not present in the df"
          )
        )
    }
  }

  err_validation
}

#' Check supplied df has only one timepoint per item or item-group
#'
#' This assumes that the names in colspec and the df have already been check and are valid
#'
#' @param df user supplied df
#' @param colspec user supplied colspec
#'
#' @return character string containing any error message
#' @noRd
validate_df_to_colspec_duplicate_timepoints <- function(df,
                                                        colspec){

  # initialise column names to avoid R CMD check Notes
  baditem <- NULL

  err_validation <- character()

  duplicate_timepoints <-
    df %>%
    dplyr::group_by(dplyr::pick(dplyr::any_of(c(
      colspec$item_col,
      colspec$group_col
    )))) %>%
    dplyr::summarise(
      duplicate_timepoints = anyDuplicated(dplyr::pick(dplyr::all_of(
        colspec$timepoint_col))),
      .groups = "drop") %>%
    dplyr::filter(duplicate_timepoints > 0) %>%
    tidyr::unite(baditem,
                 dplyr::any_of(c(colspec$item_col,
                                 colspec$group_col)),
                 sep = ":")

  if (nrow(duplicate_timepoints) > 0) {
    err_validation <-
        paste(
          "Duplicate timepoints for items: [",
          paste(duplicate_timepoints$baditem, collapse = ", "),
          "]. Each timepoint-item-group combination must only appear once in the df"
        )
  }

  err_validation
}
