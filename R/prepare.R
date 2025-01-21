#' Convert supplied df into required format for generating tables/plots
#'
#' Supplied df needs to be long (at least for now)
#'
#' @param df A data frame containing multiple time series in long format
#' @param inputspec Specification of data in df
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param item_order named list with names corresponding to columns in the supplied `df`. List members are
#'  either `TRUE` for ascending order, or a character vector of values contained in the named
#'  column for explicit ordering. If `item_order = NULL`, the original order will be kept.
#'
#' @return data frame
#' @noRd
prepare_df <-
  function(df,
           inputspec,
           period = "day",
           timepoint_limits = c(NA, NA),
           fill_with_zero = FALSE,
           item_order = NULL) {
  # TODO: Can this function be rewritten to include the tab_col?

  # initialise column names to avoid R CMD check Notes
  item <- NULL

  # keep only relevant cols and rename for ease. prefix item_cols with "item." to ensure uniqueness
  prepared_df <-
    df |>
    dplyr::select(dplyr::all_of(
      c(
        inputspec$item_col,
        inputspec$timepoint_col,
        inputspec$value_col
      )
    )) |>
    dplyr::rename(
      timepoint = dplyr::all_of(inputspec$timepoint_col),
      value = dplyr::all_of(inputspec$value_col)
    ) |>
    dplyr::rename_with(
      .cols = dplyr::all_of(inputspec$item_col),
      .fn = function(x){paste0("item.", x)}
      )

  # if there is no data, return the formatted (empty) df
  if(nrow(prepared_df) == 0){
    return(prepared_df)
  }

  prepared_df <-
    align_data_timepoints(df = prepared_df,
                          inputspec = inputspec,
                          timepoint_limits = timepoint_limits)

  if (fill_with_zero) {
    prepared_df <-
      prepared_df |>
      tidyr::replace_na(list(value = 0))
  }

  if (!is.null(item_order)){
    # df has item columns renamed so pass in renamed item_order
    item_order_renamed <- item_order
    names(item_order_renamed) <- paste0("item.", names(item_order))

    prepared_df <-
      arrange_items(df = prepared_df,
                    item_order = item_order_renamed)
  }

  prepared_df
}

#' Convert supplied df into required format for generating tables/plots
#'
#' Supplied df needs to be long (at least for now)
#'
#' @param prepared_df data frame returned from prepare_df()
#' @param inputspec Specification of data in df
#' @param plot_value_type "value" or "delta"
#' @param alert_results `alert_results` object returned from `run_alerts()`
#' @param sort_by column in output table to sort by. Can be one of `alert_overall`, or one
#'   of the summary columns. Append a minus sign to sort in descending order e.g. `-max_value`.
#'   Secondary ordering will be based on `item_order`.
#'
#' @return data frame
#' @noRd
prepare_table <-
  function(prepared_df,
           inputspec,
           plot_value_type = "value",
           alert_results = NULL,
           sort_by = NULL) {

  # TODO: allow df to be passed in wide with vector of value_cols?

  # initialise column names to avoid R CMD check Notes
  timepoint <- value <- value_for_history <- alert_description <- alert_result <- NULL

  # TODO: validate inputs

  table_df <-
    prepared_df |>
    # store original sort order as later group_by will alphabetise
    dplyr::mutate(item_order_final = dplyr::row_number()) |>
    # remember prepared_df has had its item_cols renamed to ensure uniqueness
    dplyr::group_by(dplyr::across(dplyr::all_of(paste0("item.", inputspec$item_col)))) |>
    dplyr::arrange(timepoint) |>
    dplyr::mutate(
      value_for_history = dplyr::case_when(
        plot_value_type == "value" ~ as.numeric(value),
        plot_value_type == "delta" ~ as.numeric(value) - dplyr::lag(as.numeric(value))
      )
    ) |>
    dplyr::summarise(
      item_order_final = min(item_order_final),
      # summary columns
      # TODO: only generate these if requested
      last_timepoint = max_else_na(timepoint[!is.na(value)]),
      last_value = rev(value)[1],
      last_value_nonmissing = rev(value[!is.na(value)])[1],
      max_value = max_else_na(value),
      # TODO: match precision to values
      mean_value = round(mean(value, na.rm = TRUE),
                   digits = 1),
      # history column
      history = history_to_list(value_for_history,
                                timepoint,
                                plot_value_type),
      .groups = "drop"
    )

  # add alerts column
  if (!is.null(alert_results)) {
    table_df <-
      table_df |>
      dplyr::left_join(
        alert_results |>
          dplyr::group_by(dplyr::across(dplyr::all_of(item_col_renamed))) |>
          dplyr::summarise(
            alert_overall = ifelse(
              any(alert_result == "FAIL"),
              paste0("FAIL (", sum(alert_result == "FAIL"), "/", dplyr::n(), ")"),
              paste0("PASS (", dplyr::n(), ")")
            ),
            alert_details = list(data.frame(
              alert_description, alert_result, stringsAsFactors = FALSE
            )),
            .groups = "drop"
          ),
        by = dplyr::all_of(item_col_renamed),
      )

  } else{
    table_df$alert_overall <- NA
    table_df$alert_details <- list(NULL)
  }

  # sort table
  if (is.null(sort_by)){
    table_df <-
      table_df |>
      dplyr::arrange(item_order_final)
  } else if (substring(sort_by, 1, 1) == "-"){
    table_df <-
      table_df |>
      dplyr::arrange(dplyr::across(dplyr::any_of(substring(sort_by, 2)), dplyr::desc),
                     item_order_final)
  } else{
    table_df <-
      table_df |>
      dplyr::arrange(dplyr::pick(dplyr::any_of(sort_by)),
                     item_order_final)
  }

  table_df |>
    dplyr::select(-item_order_final)
}


# -----------------------------------------------------------------------------
#' Specify relevant columns in the source data frame
#'
#' @param timepoint_col String denoting the (date) column which will be used for the x-axes.
#' @param item_col String denoting the (character) column containing categorical values identifying
#'   distinct time series. Multiple columns that together identify a time series can be provided as a vector
#' @param value_col String denoting the (numeric) column containing the time series values which
#'   will be used for the y-axes.
#' @param tab_col Optional. String denoting the (character) column containing categorical values
#'   which will be used to group the time series into different tabs on the report.
#' @param period periodicity of the timepoint_col values. "day"/"month"/"year"
#'
#' @return A `inputspec()` object
#' @export
inputspec <- function(timepoint_col,
                    item_col,
                    value_col,
                    tab_col = NULL,
                    period = "day"){

  validate_params_required(match.call())
  validate_params_type(match.call(),
                       timepoint_col = timepoint_col,
                       item_col = item_col,
                       value_col = value_col,
                       tab_col = tab_col,
                       period = period
  )

  structure(
    list(timepoint_col = timepoint_col,
       item_col = item_col,
       value_col = value_col,
       tab_col = tab_col,
       period = period),
    class = "mantis_inputspec")
}

# -----------------------------------------------------------------------------
#' Test if object is an inputspec object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_inputspec <- function(x) inherits(x, "mantis_inputspec")


# -----------------------------------------------------------------------------
#' Specify output options for the report
#'
#' @param plot_value_type Display the raw "`value`" for the time series or display the calculated
#'   "`delta`" between consecutive values.
#' @param plot_type Display the time series as a "`bar`" or "`line`" chart.
#' @param item_label String label(s) to use for the "item" column(s) in the report. If NULL, the original columns name(s) will be used.
#' @param plot_label String label to use for the time series column in the report. If NULL, the original `value_col` name will be used.
#' @param summary_cols Summary data to include as columns in the report. Options are
#'   `c("max_value", "last_value", "last_value_nonmissing", "last_timepoint", "mean_value")`.
#' @param sync_axis_range Set the y-axis to be the same range for all time series in a table.
#'   X-axes are always synced.
#' @param item_order named list corresponding to `item_col` columns for ordering the
#'   items in the output. List values are either `TRUE` for ascending order, or a character vector
#'   of values contained in the named column for explicit ordering. If `item_order = NULL`, the
#'   original order will be kept. See Details.
#' @param sort_by column in output table to sort by. Can be one of `alert_overall`, or one
#'   of the summary columns. Append a minus sign to sort in descending order e.g. `-max_value`.
#'   Secondary ordering will be based on `item_order`.
#'
#' @section Details: For `item_order`, the names of the list members should correspond to the
#'   `item_col` columns previously specified in the `inputspec`. Any names that don't match will be
#'   ignored. When multiple columns are specified, they are sorted together, in the same order of
#'   priority as the list. If a list member is `TRUE` then that column is sorted in ascending order.
#'   If a list member is a character vector then that column is sorted in the order of the vector
#'   first, with any remaining values included alphabetically at the end.
#'
#' @return An `outputspec()` object
#' @export
outputspec_interactive <- function(plot_value_type = "value",
                       plot_type = "bar",
                       item_label = NULL,
                       plot_label = NULL,
                       summary_cols = c("max_value"),
                       sync_axis_range = FALSE,
                       item_order = NULL,
                       sort_by = NULL){

  validate_params_required(match.call())
  validate_params_type(match.call(),
                       plot_value_type = plot_value_type,
                       plot_type = plot_type,
                       item_label = item_label,
                       plot_label = plot_label,
                       summary_cols = summary_cols,
                       sync_axis_range = sync_axis_range,
                       item_order = item_order,
                       sort_by = sort_by
                       )

  structure(
    list(plot_value_type = plot_value_type,
         plot_type = plot_type,
         item_label = item_label,
         plot_label = plot_label,
         summary_cols = summary_cols,
         sync_axis_range = sync_axis_range,
         item_order = item_order,
         sort_by = sort_by),
    class = c("mantis_outputspec", "mantis_outputspec_interactive")
  )
}

#' Specify output options for a static report containing heatmaps
#'
#' @param fill_colour colour to use for the tiles
#' @param y_label string for y-axis label. Optional
#' @param item_order named list corresponding to `item_col` columns for ordering the
#'   items in the output. List values are either `TRUE` for ascending order, or a character vector
#'   of values contained in the named column for explicit ordering. If `item_order = NULL`, the
#'   original order will be kept. See Details.
#'
#' @section Details: For `item_order`, the names of the list members should correspond to the
#'   `item_col` columns previously specified in the `inputspec`. Any names that don't match will be
#'   ignored. When multiple columns are specified, they are sorted together, in the same order of
#'   priority as the list. If a list member is `TRUE` then that column is sorted in ascending order.
#'   If a list member is a character vector then that column is sorted in the order of the vector
#'   first, with any remaining values included alphabetically at the end.
#'
#' @return An `outputspec()` object
#' @export
outputspec_static_heatmap <- function(fill_colour = "blue",
                                      y_label = NULL,
                                      item_order = NULL) {

  structure(
    list(fill_colour = fill_colour,
         y_label = y_label,
         item_order = item_order),
    class = c("mantis_outputspec", "mantis_outputspec_static", "mantis_outputspec_static_heatmap")
    )
}

#' Specify output options for a static report containing grids of plots.
#'
#' Currently only creates scatter plots
#'
#' @param sync_axis_range Set the y-axis to be the same range for all the plots.
#'   X-axes are always synced.
#' @param y_label string for y-axis label. Optional
#' @param item_order named list corresponding to `item_col` columns for ordering the
#'   items in the output. List values are either `TRUE` for ascending order, or a character vector
#'   of values contained in the named column for explicit ordering. If `item_order = NULL`, the
#'   original order will be kept. See Details.
#'
#' @section Details: For `item_order`, the names of the list members should correspond to the
#'   `item_col` columns previously specified in the `inputspec`. Any names that don't match will be
#'   ignored. When multiple columns are specified, they are sorted together, in the same order of
#'   priority as the list. If a list member is `TRUE` then that column is sorted in ascending order.
#'   If a list member is a character vector then that column is sorted in the order of the vector
#'   first, with any remaining values included alphabetically at the end.
#'
#' @return An `outputspec()` object
#' @export
outputspec_static_multipanel <- function(sync_axis_range = FALSE,
                                        y_label = NULL,
                                        item_order = NULL) {

  structure(
    list(sync_axis_range = sync_axis_range,
         y_label = y_label,
         item_order = item_order),
    class = c("mantis_outputspec", "mantis_outputspec_static", "mantis_outputspec_static_multipanel")
  )
}

#-----------------------------------------------------------------------
#' Test if object is an outputspec
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec <- function(x) inherits(x, "mantis_outputspec")

#' Test if object is an outputspec_interactive
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec_interactive <- function(x) inherits(x, "mantis_outputspec_interactive")

#' Test if object is an outputspec_static
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec_static <- function(x) inherits(x, "mantis_outputspec_static")

#' Test if object is an outputspec_static_heatmap
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec_static_heatmap <- function(x) inherits(x, "mantis_outputspec_static_heatmap")

#' Test if object is an outputspec_static_multipanel
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_outputspec_static_multipanel <- function(x) inherits(x, "mantis_outputspec_static_multipanel")


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
      xts::xts(x = value_for_history,
               order.by = timepoint) |>
      list()

    if (length(ts[[1]]) > 0){
      names(ts[[1]]) <- plot_value_type
    }

    ts
  }


#' Align the timepoint values across all items
#'
#' @param df Data frame with 2 columns named timepoint and value, plus the item_cols
#' @param inputspec Specification of data in df
#' @param timepoint_limits Vector containing min and max dates for the x-axes. Use Date type.
#'
#' Ensure timepoint values are the same for all items, for consistency down the table.
#' Also can restrict/expand data to a specified period here as cannot set xlimits in dygraphs.
#'
#' @return Data frame with consistent timepoints
#' @noRd
align_data_timepoints <-
  function(df,
           inputspec = inputspec,
           timepoint_limits = c(NA, NA)) {

  # initialise column names to avoid R CMD check Notes
  timepoint <- value <- NULL

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
  all_timepoints <- seq(min_timepoint, max_timepoint, by = inputspec$period)

  # NOTE: uses an unusual separator :~: to separate multiple item_cols,
  # assuming the string won't appear in the column names
  item_cols <- paste0("item.", inputspec$item_col)

  df_out <-
    df |>
    tidyr::pivot_wider(names_from = dplyr::all_of(item_cols),
                       values_from = value,
                       names_glue = paste0("piv_{", paste(item_cols, collapse = "}:~:{"), "}")) |>
    # insert any missing timepoint values
    dplyr::full_join(data.frame("timepoint" = all_timepoints), by = "timepoint") |>
    # restrict to specified limits
    dplyr::filter(timepoint >= min_timepoint & timepoint <= max_timepoint) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("piv_"),
                        names_to = item_cols,
                        names_pattern = paste0("piv_?(.*)", rep(":~:(.*)", length(item_cols) - 1)))


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

#' Validate the supplied df against the supplied inputspec
#'
#' If there are any validation errors, these are all compiled before calling a
#' single stop()
#'
#' @param df user supplied df
#' @param inputspec user supplied inputspec
#'
#' @noRd
validate_df_to_inputspec <- function(df,
                                     inputspec){

  # validate - collect all errors together and return only once
  err_validation <- character()

  # check the inputspec and df names are valid first
  err_validation <-
    append(err_validation,
           validate_df_to_inputspec_col_names(df, inputspec))

  # only do the following checks if the inputspec and df names are valid
  if (length(err_validation) == 0) {
    err_validation <-
      append(err_validation,
             validate_df_to_inputspec_col_types(df, inputspec))
    err_validation <-
      append(err_validation,
             validate_df_to_inputspec_duplicate_timepoints(df, inputspec))
  }

  # TODO: check periodicity

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

#' Check names in supplied df and inputspec
#'
#' @param df user supplied df
#' @param inputspec user supplied inputspec
#'
#' @return character string containing any error messages
#' @noRd
validate_df_to_inputspec_col_names <- function(df,
                                               inputspec){

  err_validation <- character()

  # only keep the cols params
  # and drop any items that are NULL using the unlist()
  colspec_vector <- unlist(inputspec[endsWith(names(inputspec), "_col")])

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
  # check for duplicate names in inputspec
  if (anyDuplicated(colspec_vector) > 0) {
    err_validation <-
      append(
        err_validation,
        paste(
          "Duplicate column names in inputspec: [",
          paste(colspec_vector[duplicated(colspec_vector)], collapse = ", "),
          "]. Each inputspec parameter must refer to a different column in the df "
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

#' Check datatypes in supplied df and inputspec
#'
#' Only run if supplied df and inputspec column names are valid
#'
#' @param df user supplied df
#' @param inputspec user supplied inputspec
#'
#' @return character string containing any error messages
#' @noRd
validate_df_to_inputspec_col_types <- function(df,
                                               inputspec){

  err_validation <- character()

  # check timepoint col is datetime type
  timepoint_vals <- df |> dplyr::pull(dplyr::all_of(inputspec$timepoint_col))
  if (!is_datetime(timepoint_vals)) {
    err_validation <-
      append(
        err_validation,
        paste(
          "The timepoint_col column [",
          inputspec$timepoint_col,
          "] must be a Date or POSIXt type. Instead found [",
          paste(class(timepoint_vals), collapse = ", "),
          "]"
        )
      )
  }
  # check no missing timepoints
  if (any(is.na(timepoint_vals))) {
    err_validation <-
      append(
        err_validation,
        paste(
          "The timepoint_col column [",
          inputspec$timepoint_col,
          "] must not contain missing values. Found [",
          sum(is.na(timepoint_vals)),
          "] NAs"
        )
      )
  }

  # item col will be coerced to character type
  # Check it doesn't contain both NA values and string "NA" values
  # TODO: Confirm this is valid for multi-item_cols
  for (col in inputspec$item_col) {
    item_vals <- df |> dplyr::pull(dplyr::all_of(col))
    if (any(is.na(item_vals)) && any(item_vals == "NA", na.rm = TRUE)) {
      err_validation <-
        append(
          err_validation,
          paste(
            "The item_col column [",
            col,
            '] cannot contain both NA values and "NA" strings. Found [',
            sum(is.na(item_vals)),
            "] NA values and [",
            sum(item_vals == "NA", na.rm = TRUE),
            '] "NA" strings'
          )
        )
    }
  }

  # check value col is numeric type
  # Note: Infs and NaNs can be left in, they should be treated like NAs
  value_vals <- df |> dplyr::pull(dplyr::all_of(inputspec$value_col))
  if (!is.numeric(value_vals)) {
    err_validation <-
      append(
        err_validation,
        paste(
          "The value_col column [",
          inputspec$value_col,
          "] must be a numeric type. Instead found [",
          paste(class(value_vals), collapse = ", "),
          "]"
        )
      )
  }

  err_validation
}

#' Check supplied df has only one timepoint per item or item-tab
#'
#' This assumes that the names in inputspec and the df have already been check and are valid
#'
#' @param df user supplied df
#' @param inputspec user supplied inputspec
#'
#' @return character string containing any error message
#' @noRd
validate_df_to_inputspec_duplicate_timepoints <- function(df,
                                                        inputspec){

  # initialise column names to avoid R CMD check Notes
  baditem <- NULL

  err_validation <- character()

  duplicate_timepoints <-
    df |>
    dplyr::group_by(dplyr::pick(dplyr::any_of(c(
      inputspec$item_col,
      inputspec$tab_col
    )))) |>
    dplyr::summarise(
      duplicate_timepoints = anyDuplicated(dplyr::pick(dplyr::all_of(
        inputspec$timepoint_col))),
      .groups = "drop") |>
    dplyr::filter(duplicate_timepoints > 0) |>
    tidyr::unite(baditem,
                 dplyr::any_of(c(inputspec$item_col,
                                 inputspec$tab_col)),
                 sep = ":")

  if (nrow(duplicate_timepoints) > 0) {
    err_validation <-
        paste(
          "Duplicate timepoints for items: [",
          paste(duplicate_timepoints$baditem, collapse = ", "),
          "]. Each timepoint-item-tab combination must only appear once in the df"
        )
  }

  err_validation
}

#'Arrange/sort a df based on a list of items
#'
#'@param df df to arrange
#'@param item_order named list with names corresponding to columns in the supplied `df`. List members are
#'  either `TRUE` for ascending order, or a character vector of values contained in the named
#'  column for explicit ordering. If `item_order = NULL`, the original order will be kept. See Details.
#'
#'@section Details: For `item_order`, the names of the list members should correspond to the column
#'  names in the `df`. Any names that don't match will be ignored. When multiple columns are
#'  specified, they are sorted together, in the same order as the list. If a list item is `TRUE` then
#'  that column is sorted in ascending order. If a list item is a character vector then that column
#'  is sorted in the order of the vector first, with any remaining values included alphabetically at
#'  the end
#'
#'@return data frame
arrange_items <- function(df, item_order = NULL){
  if (is.null(item_order)) {
    return(df)
  }

  # keep only the items which match a column in the df
  items <- item_order[which(names(item_order) %in% names(df))]

  df <-
    df |>
    # sort ascending on all named columns (if present) first
    dplyr::arrange(dplyr::across(dplyr::any_of(names(items))))

  # then sort by any named values
  item_factors <- items[vapply(items, function(x){all(is.character(x))}, FUN.VALUE = TRUE)]
  # this is super ugly but temporarily just limit it to 3 factors until find better way
  if (length(item_factors) == 1){
    df <-
      df |>
      dplyr::arrange(
        factor(.data[[names(item_factors)[1]]], levels = item_factors[1][[1]])
      )
  } else if (length(item_factors) == 2){
    df <-
      df |>
      dplyr::arrange(
        factor(.data[[names(item_factors)[1]]], levels = item_factors[1][[1]]),
        factor(.data[[names(item_factors)[2]]], levels = item_factors[2][[1]])
      )
  } else if (length(item_factors) == 3){
    df <-
      df |>
      dplyr::arrange(
        factor(.data[[names(item_factors)[1]]], levels = item_factors[1][[1]]),
        factor(.data[[names(item_factors)[2]]], levels = item_factors[2][[1]]),
        factor(.data[[names(item_factors)[3]]], levels = item_factors[3][[1]])
      )
  }

  df

}
