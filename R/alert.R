#' Create set of alert rules
#'
#' @param ... alerts to apply to the time series
#' @return An `alert_rules` object
#' @export
alert_rules <- function(...) {

  ars <- list(...)

  # TODO: validation
  # check only one rule of each type

  structure(ars, class = "mantis_alert_rules")
}

alert_rule <- function(type,
                       function_call,
                       short_name,
                       description,
                       items = "ALL") {

  structure(
    list(
      type = type,
      function_call = function_call,
      short_name = short_name,
      description = description,
      items = items
    ),
    class = c(paste0("mantis_alert_rule_", type), "mantis_alert_rule")
  )
}

#' alert_missing
#'
#' @param extent_type "all", "any", "last", "consecutive"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means alert if there are 5 or more missing values in any position
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#' @return An `alert_rule` object
#' @export
alert_missing <- function(extent_type = "all",
                          extent_value = 1,
                          items = "ALL") {

  # TODO: consider allowing different extent_values for different alert severities
  # TODO: "consecutive" type. rle doesn't handle runs of NAs so will need a wrapper for it

  rule_short_name <- paste0("missing_", extent_type, ifelse(extent_type == "all", "", paste0("_", extent_value)))

  if (extent_type == "all"){
    function_call <- quote(all(is.na(value)))
    rule_description <- "All values are missing"
  } else if(extent_type == "any"){
    function_call <- substitute(sum(is.na(value)) >= x, list(x = extent_value))
    rule_description <- paste0("At least ", extent_value, " values are missing in total")
  } else if(extent_type == "last"){
    function_call <- substitute(all(is.na(rev(value)[1:x])),
                                list(x = extent_value))
    rule_description <- paste0("The last ", extent_value, " or more values are missing")
  } else if(extent_type == "consecutive"){
    function_call <- substitute(
      {run_lengths <- rle(is.na(value));
      any(run_lengths$lengths[run_lengths$values] >= x)},
      list(x = extent_value))
    rule_description <- paste0(extent_value, " or more values in a row are missing")
  }

  alert_rule(
    type = "missing",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}

#' alert_equals
#'
#' @param extent_type "all", "any", "last", "consecutive"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means alert if there are 5 or more values that satisfy the condition, in any position
#' @param rule_value value to test against. e.g. `rule_value=0` means alert if value == 0
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#'
#' @return An `alert_rule` object
#' @export
alert_equals <- function(extent_type = "all",
                     extent_value = 1,
                     rule_value,
                     items = "ALL") {

  # TODO: NEED TO THINK ABOUT WHAT TO DO WITH NAs - currently just removing them but may want them in the extent_value
  # TODO: Need to check datatypes are numeric
  # TODO: consider allowing different extent_values for different alert severities

  rule_short_name <- paste0("equals_", rule_value, "_", extent_type, ifelse(extent_type == "all", "", paste0("_", extent_value)))

  if (extent_type == "all"){
    function_call <- substitute(all(value[!is.na(value)] == rv), list(rv = rule_value))
    rule_description <- paste0("All (non-missing) values are equal to ", rule_value)
  } else if(extent_type == "any"){
    function_call <- substitute(sum(value[!is.na(value)] == rv) >= x, list(x = extent_value, rv = rule_value))
    rule_description <- paste0("At least ", extent_value, " (non-missing) values are equal to ", rule_value)
  } else if(extent_type == "last"){
    function_call <- substitute(all(rev(value[!is.na(value)])[1:x] == rv),
                                list(x = extent_value, rv = rule_value))
    rule_description <- paste0("The last ", extent_value, " or more (non-missing) values are equal to ", rule_value)
  } else if(extent_type == "consecutive"){
    function_call <- substitute(
      {run_lengths <- rle(value[!is.na(value)]);
      any(run_lengths$lengths[run_lengths$values == rv] >= x)},
      list(x = extent_value, rv = rule_value))
    rule_description <- paste0(extent_value, " or more (non-missing) values in a row are are equal to ", rule_value)
  }

  alert_rule(
    type = "equals",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}



#' alert_below
#'
#' Less than
#'
#' @param extent_type "all", "any", "last", "consecutive"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means alert if there are 5 or more values that satisfy the condition, in any position
#' @param rule_value value to test against. e.g. `rule_value=1` means alert if value is less than 1
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#'
#' @return An `alert_rule` object
#' @export
alert_below <- function(extent_type = "all",
                     extent_value = 1,
                     rule_value,
                     items = "ALL") {

  # TODO: consider allowing different extent_values for different alert severities

  rule_short_name <- paste0("below_", rule_value, "_", extent_type, ifelse(extent_type == "all", "", paste0("_", extent_value)))

  if (extent_type == "all"){
    function_call <- substitute(all(value[!is.na(value)] < rv), list(rv = rule_value))
    rule_description <- paste0("All (non-missing) values are < ", rule_value)
  } else if(extent_type == "any"){
    function_call <- substitute(sum(value[!is.na(value)] < rv) >= x, list(x = extent_value, rv = rule_value))
    rule_description <- paste0("At least ", extent_value, " (non-missing) values are < ", rule_value)
  } else if(extent_type == "last"){
    function_call <- substitute(all(rev(value[!is.na(value)])[1:x] < rv),
                                list(x = extent_value, rv = rule_value))
    rule_description <- paste0("The last ", extent_value, " or more (non-missing) values are < ", rule_value)
  } else if(extent_type == "consecutive"){
    function_call <- substitute(
      {run_lengths <- rle(value[!is.na(value)] < rv);
      any(run_lengths$lengths[run_lengths$values] >= x)},
      list(x = extent_value, rv = rule_value))
    rule_description <- paste0(extent_value, " or more (non-missing) values in a row are are < ", rule_value)
  }

  alert_rule(
    type = "below",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}


#' alert_above
#'
#' Greater than
#'
#' @param extent_type "all", "any", "last", "consecutive"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means alert if there are 5 or more values that satisfy the condition, in any position
#' @param rule_value value to test against. e.g. `rule_value=1` means alert if value is greater than 1
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#'
#' @return An `alert_rule` object
#' @export
alert_above <- function(extent_type = "all",
                     extent_value = 1,
                     rule_value,
                     items = "ALL") {

  # TODO: consider allowing different extent_values for different alert severities

  rule_short_name <- paste0("above_", rule_value, "_", extent_type, ifelse(extent_type == "all", "", paste0("_", extent_value)))

  if (extent_type == "all"){
    function_call <- substitute(all(value[!is.na(value)] > rv), list(rv = rule_value))
    rule_description <- paste0("All (non-missing) values are > ", rule_value)
  } else if(extent_type == "any"){
    function_call <- substitute(sum(value[!is.na(value)] > rv) >= x, list(x = extent_value, rv = rule_value))
    rule_description <- paste0("At least ", extent_value, " (non-missing) values are > ", rule_value)
  } else if(extent_type == "last"){
    function_call <- substitute(all(rev(value[!is.na(value)])[1:x] > rv),
                                list(x = extent_value, rv = rule_value))
    rule_description <- paste0("The last ", extent_value, " or more (non-missing) values are > ", rule_value)
  } else if(extent_type == "consecutive"){
    function_call <- substitute(
      {run_lengths <- rle(value[!is.na(value)] > rv);
      any(run_lengths$lengths[run_lengths$values] >= x)},
      list(x = extent_value, rv = rule_value))
    rule_description <- paste0(extent_value, " or more (non-missing) values in a row are are > ", rule_value)
  }

  alert_rule(
    type = "above",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}

#' Alert when there is a percentage increase in latest values
#'
#' Check if latest values are greater than in a previous period, over a particular percentage
#'
#' Based on the mean of values in the two periods.
#'
#' @param current_period vector containing positions from end of time series to use for comparison
#' @param previous_period vector containing positions from end of time series to use for comparison. Can overlap with `current_period` if desired.
#' @param rule_value value to test against. e.g. `rule_value=5` means alert if percentage change is greater than 5
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#'
#' @return An `alert_rule` object
#' @export
alert_difference_above_perc <- function(current_period,
                                        previous_period,
                                        rule_value,
                                        items = "ALL") {

  # TODO: consider allowing different rule_values for different alert severities

  rule_short_name <- paste0("diff_above_perc_", rule_value,
                            "_period_", ifelse(length(current_period) == 1,
                                               current_period,
                                               paste0(current_period[1], "-", rev(current_period)[1])),
                            "_v_", ifelse(length(previous_period) == 1,
                                          previous_period,
                                          paste0(previous_period[1], "-", rev(previous_period)[1])))

  function_call <- substitute(mean(rev(value)[cp], na.rm = TRUE) > (1 + rv/100) * mean(rev(value)[pp], na.rm = TRUE),
                              list(cp = current_period, pp = previous_period, rv = rule_value))
  rule_description <- paste0(ifelse(length(current_period) == 1,
                                    paste0("Value ", current_period, " from end is"),
                                    paste0("Values ", current_period[1], "-", rev(current_period)[1], " from end are")),
                             " over ", rule_value, "% greater than ",
                             ifelse(length(previous_period) == 1,
                                    paste0("value ", previous_period),
                                    paste0("values ", previous_period[1], "-", rev(previous_period)[1])),
                             " from end")

  alert_rule(
    type = "diff_above_perc",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}

#' Alert when there is a percentage drop in latest values
#'
#' Check if latest values are lower than in the previous period, over a particular percentage
#'
#' Based on the mean of values in the two periods.
#'
#' @param current_period vector containing positions from end of time series to use for comparison
#' @param previous_period vector containing positions from end of time series to use for comparison. Can overlap with `current_period` if desired.
#' @param rule_value value to test against. e.g. `rule_value=5` means alert if percentage change is greater than 5
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#'
#' @return An `alert_rule` object
#' @export
alert_difference_below_perc <- function(current_period,
                                        previous_period,
                                        rule_value,
                                        items = "ALL") {

  # TODO: consider allowing different rule_values for different alert severities

  rule_short_name <- paste0("diff_below_perc_", rule_value,
                            "_period_", ifelse(length(current_period) == 1,
                                               current_period,
                                               paste0(current_period[1], "-", rev(current_period)[1])),
                            "_v_", ifelse(length(previous_period) == 1,
                                          previous_period,
                                          paste0(previous_period[1], "-", rev(previous_period)[1])))

  function_call <- substitute(mean(rev(value)[cp], na.rm = TRUE) < (1 - rv/100) * mean(rev(value)[pp], na.rm = TRUE),
                              list(cp = current_period, pp = previous_period, rv = rule_value))
  rule_description <- paste0(ifelse(length(current_period) == 1,
                                    paste0("Value ", current_period, " from end is"),
                                    paste0("Values ", current_period[1], "-", rev(current_period)[1], " from end are")),
                             " over ", rule_value, "% less than ",
                             ifelse(length(previous_period) == 1,
                                    paste0("value ", previous_period),
                                    paste0("values ", previous_period[1], "-", rev(previous_period)[1])),
                             " from end")

  alert_rule(
    type = "diff_below_perc",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}

#' Create a custom alert rule
#'
#' @param short_name short name to uniquely identify the rule. Avoid spaces and special characters.
#' @param description description of what the rule checks for
#' @param function_call expression to be evaluated per item. See details.
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#'
#' The function_call expression is called using `eval()` within a `dplyr::summarise()` after grouping by the `item_col`.
#' Column names that can be used explicitly in the expression are: `value`, `item`, `timepoint`
#'
#' @return An `alert_rule` object
#' @export
#'
#' @examples
#' alert_custom(
#'   short_name = "my_rule_combo",
#'   description = "Over 3 missing values and max value is > 10",
#'   function_call = quote(sum(is.na(value)) > 3 && max(value, na.rm = TRUE) > 10)
#' )
#'
#' alert_custom(
#'   short_name = "my_rule_doubled",
#'   description = "Last value is over double the first value",
#'   function_call = quote(rev(value)[1] > 2*value[1])
#' )
alert_custom <- function(short_name,
                         description,
                         function_call,
                         items = "ALL") {

  # TODO: validation

  structure(
    list(
      type = "custom",
      function_call = function_call,
      short_name = short_name,
      description = description,
      items = items
    ),
    class = c(paste0("mantis_alert_rule_", "custom"), "mantis_alert_rule")
  )
}


#' Run all alert rules and return results
#'
#' @param prepared_df prepared_df
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#' @param filter_results only return rows where the alert result is in this vector of values
#'
#' @return tibble
#' @noRd
run_alerts <- function(prepared_df,
                       alert_rules,
                       filter_results = c("PASS", "FAIL", "NA")) {
  alert_result <- NULL

  # if there is no data, return a formatted (empty) df
  if(nrow(prepared_df) == 0){
    return(dplyr::tibble(
      item = character(),
      alert_name = character(),
      alert_description = character(),
      alert_result = character()
    ))
  }

  results <-
    lapply(alert_rules, FUN = run_alert, prepared_df = prepared_df) |>
    purrr::reduce(dplyr::bind_rows)

  results |>
    dplyr::mutate(alert_result = tidyr::replace_na(alert_result, "NA")) |>
    dplyr::filter(alert_result %in% filter_results)
}

#' Run alert rule and return results
#'
#' @param prepared_df prepared_df
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#'
#' @return tibble
#' @noRd
run_alert <- function(prepared_df, alert_rule){
  item <- timepoint <- NULL

  # TODO: if it's a custom rule, wrap it in some error handling

  prepared_df |>
    dplyr::filter(item %in% alert_rule$items | all(alert_rule$items == "ALL")) |>
    dplyr::group_by(item) |>
    dplyr::arrange(timepoint) |>
    dplyr::summarise(alert_name = alert_rule$short_name,
                     alert_description = alert_rule$description,
                     alert_result = ifelse(eval(alert_rule$function_call), "FAIL", "PASS"))

}


#' Generate a data frame containing alert results
#'
#' @param df A data frame containing multiple time series in long format. See Details.
#' @param inputspec [`inputspec()`] object specifying which columns in the supplied `df` represent the
#'   "timepoint", "item", "value"  and (optionally) "tab" for the time series. Each "item-tab" represents an individual time series
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#' @param filter_results only return rows where the alert result is in this vector of values
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#'
#' @return tibble
#' @export
#' @importFrom rlang :=
mantis_alerts <- function(df,
                          inputspec,
                          alert_rules,
                          filter_results = c("PASS", "FAIL", "NA"),
                          timepoint_limits = c(NA, NA),
                          fill_with_zero = FALSE) {
  item <- NULL

  validate_df_to_inputspec(df, inputspec)

  # if there is no data, return a formatted (empty) df, only needed when tab_col is specified
  if(nrow(df) == 0){
    return(dplyr::tibble(
      item = character(),
      alert_name = character(),
      alert_description = character(),
      alert_result = character()
    ))
  }

  if (is.null(inputspec$tab_col)) {
    prepared_df <-
      prepare_df(
        df,
        inputspec = inputspec,
        timepoint_limits = timepoint_limits,
        fill_with_zero = fill_with_zero
      )
    results <-
      run_alerts(prepared_df, alert_rules, filter_results = filter_results) |>
      dplyr::rename("{inputspec$item_col}" := item)
  } else{
    resultslist <- list()

    tab_names <- unique(df[inputspec$tab_col] |>
                          dplyr::pull())

    for (i in seq_along(tab_names)) {
      dftab <-
        df |> dplyr::filter(.data[[inputspec$tab_col]] == tab_names[i])

      # TODO: Would be nice if prepare_df could include tab_col
      prepared_df <-
        prepare_df(
          dftab,
          inputspec = inputspec,
          timepoint_limits = timepoint_limits,
          fill_with_zero = fill_with_zero
        )

      resultslist[[i]] <-
        run_alerts(prepared_df, alert_rules, filter_results = filter_results) |>
        dplyr::rename("{inputspec$item_col}" := item) |>
        dplyr::mutate("{inputspec$tab_col}" := tab_names[i])
    }

    results <- purrr::reduce(resultslist, dplyr::bind_rows) |>
      dplyr::select(dplyr::all_of(c(inputspec$tab_col, inputspec$item_col)), dplyr::everything())

  }
  results
}
