#' Create set of alert rules
#'
#' @param ... alerts to apply to the time series
#' @return An `alert_rules` object
#' @export
alert_rules <- function(...) {

  ars <- list(...)

  # validation
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
#' @param extent_type "all", "any", "last"
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
#' @param extent_type "all", "any", "last"
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
    function_call <- substitute(all(value == rv, na.rm = TRUE), list(rv = rule_value))
    rule_description <- paste0("All values are equal to ", rule_value)
  } else if(extent_type == "any"){
    function_call <- substitute(sum(value == rv, na.rm = TRUE) >= x, list(x = extent_value, rv = rule_value))
    rule_description <- paste0("At least ", extent_value, " values are equal to ", rule_value)
  } else if(extent_type == "last"){
    function_call <- substitute(all(rev(value)[1:x] == rv, na.rm = TRUE),
                                list(x = extent_value, rv = rule_value))
    rule_description <- paste0("The last ", extent_value, " or more values are equal to ", rule_value)
  }

  alert_rule(
    type = "equals",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}



#' alert_lt
#'
#' Less than
#'
#' @param extent_type "all", "any", "last"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means alert if there are 5 or more values that satisfy the condition, in any position
#' @param rule_value value to test against. e.g. `rule_value=1` means alert if value is less than 1
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#'
#' @return An `alert_rule` object
#' @export
alert_lt <- function(extent_type = "all",
                     extent_value = 1,
                     rule_value,
                     items = "ALL") {

  # TODO: consider allowing different extent_values for different alert severities

  rule_short_name <- paste0("lt_", rule_value, "_", extent_type, ifelse(extent_type == "all", "", paste0("_", extent_value)))

  if (extent_type == "all"){
    function_call <- substitute(all(value < rv, na.rm = TRUE), list(rv = rule_value))
    rule_description <- paste0("All values are less than ", rule_value)
  } else if(extent_type == "any"){
    function_call <- substitute(sum(value < rv, na.rm = TRUE) >= x, list(x = extent_value, rv = rule_value))
    rule_description <- paste0("At least ", extent_value, " values are less than ", rule_value)
  } else if(extent_type == "last"){
    function_call <- substitute(all(rev(value)[1:x] < rv, na.rm = TRUE),
                                list(x = extent_value, rv = rule_value))
    rule_description <- paste0("The last ", extent_value, " or more values are less than ", rule_value)
  }

  alert_rule(
    type = "lt",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}


#' alert_gt
#'
#' Greater than
#'
#' @param extent_type "all", "any", "last"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means alert if there are 5 or more values that satisfy the condition, in any position
#' @param rule_value value to test against. e.g. `rule_value=1` means alert if value is greater than 1
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#'
#' @return An `alert_rule` object
#' @export
alert_gt <- function(extent_type = "all",
                     extent_value = 1,
                     rule_value,
                     items = "ALL") {

  # TODO: consider allowing different extent_values for different alert severities

  rule_short_name <- paste0("gt_", rule_value, "_", extent_type, ifelse(extent_type == "all", "", paste0("_", extent_value)))

  if (extent_type == "all"){
    function_call <- substitute(all(value > rv, na.rm = TRUE), list(rv = rule_value))
    rule_description <- paste0("All values are greater than ", rule_value)
  } else if(extent_type == "any"){
    function_call <- substitute(sum(value > rv, na.rm = TRUE) >= x, list(x = extent_value, rv = rule_value))
    rule_description <- paste0("At least ", extent_value, " values are greater than ", rule_value)
  } else if(extent_type == "last"){
    function_call <- substitute(all(rev(value)[1:x] > rv, na.rm = TRUE),
                                list(x = extent_value, rv = rule_value))
    rule_description <- paste0("The last ", extent_value, " or more values are greater than ", rule_value)
  }

  alert_rule(
    type = "gt",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}

#' Alert when there is a percentage change in latest values
#'
#' Check if latest values are greater than in the previous period, over a particular percentage
#'
#' Based on the mean of values in the two periods.
#'
#' @param current_period number of values from end to use for comparison
#' @param previous_period number of values back from `current_period` to use for comparison
#' @param rule_value value to test against. e.g. `rule_value=5` means alert if percentage change is greater or equal to 5
#' @param items vector of values in item_col that the rule should be applied to. Or "ALL" to apply it to all items.
#'
#' @return An `alert_rule` object
#' @export
alert_difference_above_perc <- function(current_period,
                                        previous_period,
                                        rule_value,
                                        items = "ALL") {

  # TODO: consider allowing different rule_values for different alert severities

  rule_short_name <- paste0("diff_above_perc_", rule_value)

  function_call <- substitute(mean(rev(value)[1:cp], na.rm = TRUE) >= (1 + rv/100) * mean(rev(value)[ppstart:ppend], na.rm = TRUE),
                              list(cp = current_period, ppstart = current_period + 1, ppend = current_period + previous_period, rv = rule_value))
  rule_description <- paste0("The last ", current_period, " non-missing values are over ", rule_value, "% greater than the previous ", previous_period, " non-missing values")

  alert_rule(
    type = "diff_above_perc",
    function_call = function_call,
    items = items,
    short_name = rule_short_name,
    description = rule_description
  )
}


run_alerts <- function(prepared_df,
                       alert_rules,
                       filter_results = c(TRUE, FALSE)) {
  alert_result <- NULL

  results <-
    lapply(alert_rules, FUN = run_alert, prepared_df = prepared_df) %>%
    purrr::reduce(dplyr::bind_rows)

  results %>%
    dplyr::filter(alert_result %in% filter_results)
}

run_alert <- function(prepared_df, alert_rule){
  item <- timepoint <- NULL

  prepared_df %>%
    dplyr::filter(item %in% alert_rule$items | all(alert_rule$items == "ALL")) %>%
    dplyr::group_by(item) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::summarise(alert_name = alert_rule$short_name,
                     alert_description = alert_rule$description,
                     alert_result = eval(alert_rule$function_call))

}


#' Generate a data frame containing alert results
#'
#' @param df A data frame containing multiple time series in long format. See Details.
#' @param colspec [`colspec()`] object specifying which columns in the supplied `df` represent the
#'   "timepoint", "item", "value"  and (optionally) "tab" for the time series. Each "item-tab" represents an individual time series
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#' @param filter_results only return rows where the alert result is in this vector of values
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#'
#' @return tibble
#' @export
mantis_alerts <- function(df,
                          colspec,
                          alert_rules,
                          filter_results = c(TRUE, FALSE),
                          timepoint_limits = c(NA, NA),
                          fill_with_zero = FALSE) {
  item <- NULL

  validate_df_to_colspec(df, colspec)

  if (is.null(colspec$tab_col)) {
    prepared_df <-
      prepare_df(
        df,
        timepoint_col = colspec$timepoint_col,
        item_col = colspec$item_col,
        value_col = colspec$value_col,
        timepoint_limits = timepoint_limits,
        fill_with_zero = fill_with_zero
      )
    results <-
      run_alerts(prepared_df, alert_rules, filter_results = filter_results) %>%
      dplyr::rename("{colspec$item_col}" := item)
  } else{
    resultslist <- list()

    tab_names <- unique(df[colspec$tab_col] %>%
                          dplyr::pull())

    for (i in seq_along(tab_names)) {
      dftab <-
        df %>% dplyr::filter(.data[[colspec$tab_col]] == tab_names[i])

      # TODO: Would be nice if prepare_df could include tab_col
      prepared_df <-
        prepare_df(
          dftab,
          timepoint_col = colspec$timepoint_col,
          item_col = colspec$item_col,
          value_col = colspec$value_col,
          timepoint_limits = timepoint_limits,
          fill_with_zero = fill_with_zero
        )

      resultslist[[i]] <-
        run_alerts(prepared_df, alert_rules, filter_results = filter_results) %>%
        dplyr::rename("{colspec$item_col}" := item) %>%
        dplyr::mutate("{colspec$tab_col}" := tab_names[i])
    }

    results <- purrr::reduce(resultslist, dplyr::bind_rows) %>%
      dplyr::select(dplyr::all_of(c(colspec$tab_col, colspec$item_col)), dplyr::everything())

  }
  results
}
