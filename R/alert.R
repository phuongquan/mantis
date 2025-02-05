# -----------------------------------------------------------------------------
#' Create set of alert rules
#'
#' Note: this functionality is currently only implemented for interactive outputs and will be ignored for static outputs.
#'
#' @param ... alerts to apply to the time series
#' @return An `alert_rules` object
#' @export
#' @examples
#' # alert if any values are NA
#' # or if all values are zero
#' ars <- alert_rules(
#'   alert_missing(extent_type = "any", extent_value = 1),
#'   alert_equals(extent_type = "all", rule_value = 0)
#' )
alert_rules <- function(...) {

  ars <- list(...)

  # TODO: check only one rule of each type

  err_validation <- character()
  is_alert_rule <- vapply(ars, is_alert_rule, logical(1))
  if (any(!is_alert_rule)) {
    err_validation <-
      append(
        err_validation,
        paste(
          "Unrecognised alert rule(s) in positions: [",
          paste(which(!is_alert_rule), collapse = ", "),
          "]",
          ": Found [ class =",
          paste(vapply(ars[which(!is_alert_rule)], class, character(1)), collapse = ", "),
          "]"
        )
      )
  }

  if (length(err_validation) > 0) {
    stop_custom(
      .subclass = "invalid_alert_rules",
      message = paste0(
        "Invalid items passed into alert_rules() specification.\n",
        paste(err_validation, collapse = "\n")
      )
    )
  }

  structure(ars, class = "mantis_alert_rules")
}

# -----------------------------------------------------------------------------
#' Test if object is an alert_rules object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_alert_rules <- function(x) inherits(x, "mantis_alert_rules")


# -----------------------------------------------------------------------------
#' Constructor for an alert_rule
#'
#' @param type type of rule
#' @param function_call expression to pass to `eval()`, that returns either `TRUE` or `FALSE`.
#'   Return value of `TRUE` means alert result is FAIL
#' @param short_name a short computer-friendly name to uniquely identify the rule
#' @param description brief but user-friendly explanation of why the rule result is FAIL
#' @param items named list with names corresponding to members of `item_cols`. List members are
#'   character vectors of values contained in the named column that the rule should be applied to.
#'   If `items = NULL` the rule will be applied to all items. See Details.
#'
#' @section Details: Use `items` to restrict the rule to be applied only to specified items.
#'   `items` can either be NULL or a named list of character vectors. If `NULL`, the rule will be
#'   applied to all items. If a named list, the names must match members of the `item_cols`
#'   parameter in the `inputspec`, (as well as column names in the `df`), though can be a subset.
#'   If an `item_col` is not named in the list, the rule will apply to all its members. If an
#'   `item_col` is named in the list, the rule will only be applied when the `item_col`'s value is
#'   contained in the corresponding character vector. When multiple `item_col`s are
#'   specified, the rule will be applied only to items that satisfy all the conditions.
#'
#' @noRd
alert_rule <- function(type,
                       function_call,
                       short_name,
                       description,
                       items = NULL) {

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

# -----------------------------------------------------------------------------
#' Test if object is an alert_rule object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_alert_rule <- function(x) inherits(x, "mantis_alert_rule")


# -----------------------------------------------------------------------------
#' Test for missing values
#'
#' Configure a rule to test the time series for the presence of NA values. Tolerance can be
#' adjusted using the `extent_type` and `extent_value` parameters, see Examples for details.
#'
#' @param extent_type "all", "any", "last", "consecutive"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means
#'   alert if there are 5 or more missing values in any position
#' @param items named list with names corresponding to members of `item_cols`. List members are
#'   character vectors of values contained in the named column that the rule should be applied to.
#'   If `items = NULL` the rule will be applied to all items. See Details.
#'
#' @section Details: Use `items` to restrict the rule to be applied only to specified items.
#'   `items` can either be NULL or a named list of character vectors. If `NULL`, the rule will be
#'   applied to all items. If a named list, the names must match members of the `item_cols`
#'   parameter in the `inputspec`, (as well as column names in the `df`), though can be a subset.
#'   If an `item_col` is not named in the list, the rule will apply to all its members. If an
#'   `item_col` is named in the list, the rule will only be applied when the `item_col`'s value is
#'   contained in the corresponding character vector. When multiple `item_col`s are
#'   specified, the rule will be applied only to items that satisfy all the conditions.
#'
#' @return An `alert_rule` object
#' @seealso [alert_rules()], [alert_equals()]
#' @examples
#' # alert if all values are NA
#' ars <- alert_rules(alert_missing(extent_type = "all"))
#'
#' # alert if there are 10 or more missing values in total
#' # or if the last 3 or more values are missing
#' # or if 5 or more values in a row are missing
#' ars <- alert_rules(
#'   alert_missing(extent_type = "any", extent_value = 10),
#'   alert_missing(extent_type = "last", extent_value = 3),
#'   alert_missing(extent_type = "consecutive", extent_value = 5)
#' )
#' @export
alert_missing <- function(extent_type = "all",
                          extent_value = 1,
                          items = NULL) {

  validate_params_type(match.call(),
                       extent_type = extent_type,
                       extent_value = extent_value,
                       items = items
  )

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

#' Test for specific values
#'
#' Configure a rule to test the time series for the presence of specific values. Tolerance can be
#' adjusted using the `extent_type` and `extent_value` parameters, see Examples for details.
#'
#' @param extent_type "all", "any", "last", "consecutive"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means
#'   alert if there are 5 or more values that satisfy the condition, in any position
#' @param rule_value value to test against. e.g. `rule_value=0` means alert if value == 0
#' @param items named list with names corresponding to members of `item_cols`. List members are
#'   character vectors of values contained in the named column that the rule should be applied to.
#'   If `items = NULL` the rule will be applied to all items. See Details.
#'
#' @section Details: Use `items` to restrict the rule to be applied only to specified items.
#'   `items` can either be NULL or a named list of character vectors. If `NULL`, the rule will be
#'   applied to all items. If a named list, the names must match members of the `item_cols`
#'   parameter in the `inputspec`, (as well as column names in the `df`), though can be a subset.
#'   If an `item_col` is not named in the list, the rule will apply to all its members. If an
#'   `item_col` is named in the list, the rule will only be applied when the `item_col`'s value is
#'   contained in the corresponding character vector. When multiple `item_col`s are
#'   specified, the rule will be applied only to items that satisfy all the conditions.
#'
#' @return An `alert_rule` object
#' @examples
#' # alert if all values are zero
#' ars <- alert_rules(alert_equals(extent_type = "all", rule_value = 0))
#'
#' # alert if there are 10 or more zero values in total
#' # or if the last 3 or more values are zero
#' # or if 5 or more values in a row are zero
#' ars <- alert_rules(
#'   alert_equals(extent_type = "any", extent_value = 10, rule_value = 0),
#'   alert_equals(extent_type = "last", extent_value = 3, rule_value = 0),
#'   alert_equals(extent_type = "consecutive", extent_value = 5, rule_value = 0)
#' )
#' @export
alert_equals <- function(extent_type = "all",
                     extent_value = 1,
                     rule_value,
                     items = NULL) {

  # TODO: NEED TO THINK ABOUT WHAT TO DO WITH NAs - currently just removing them but may want them in the extent_value
  validate_params_required(match.call())
  validate_params_type(match.call(),
                       extent_type = extent_type,
                       extent_value = extent_value,
                       rule_value = rule_value,
                       items = items
  )

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



#' Test for values less than a specific value
#'
#' Configure a rule to test the time series for the presence of values less than a specific value.
#' Tolerance can be adjusted using the `extent_type` and `extent_value` parameters, see Examples
#' for details.
#'
#' @param extent_type "all", "any", "last", "consecutive"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means
#'   alert if there are 5 or more values that satisfy the condition, in any position
#' @param rule_value value to test against. e.g. `rule_value=1` means alert if value is less than 1
#' @param items named list with names corresponding to members of `item_cols`. List members are
#'   character vectors of values contained in the named column that the rule should be applied to.
#'   If `items = NULL` the rule will be applied to all items. See Details.
#'
#' @section Details: Use `items` to restrict the rule to be applied only to specified items.
#'   `items` can either be NULL or a named list of character vectors. If `NULL`, the rule will be
#'   applied to all items. If a named list, the names must match members of the `item_cols`
#'   parameter in the `inputspec`, (as well as column names in the `df`), though can be a subset.
#'   If an `item_col` is not named in the list, the rule will apply to all its members. If an
#'   `item_col` is named in the list, the rule will only be applied when the `item_col`'s value is
#'   contained in the corresponding character vector. When multiple `item_col`s are
#'   specified, the rule will be applied only to items that satisfy all the conditions.
#'
#' @return An `alert_rule` object
#' @examples
#' # alert if all values are less than 2
#' ars <- alert_rules(alert_below(extent_type = "all", rule_value = 2))
#'
#' # alert if there are 10 or more values that are less than 7
#' # or if the last 3 or more values are less than 2
#' # or if 5 or more values in a row are less than 7
#' ars <- alert_rules(
#'   alert_below(extent_type = "any", extent_value = 10, rule_value = 7),
#'   alert_below(extent_type = "last", extent_value = 3, rule_value = 2),
#'   alert_below(extent_type = "consecutive", extent_value = 5, rule_value = 7)
#' )
#' @export
alert_below <- function(extent_type = "all",
                     extent_value = 1,
                     rule_value,
                     items = NULL) {

  validate_params_required(match.call())
  validate_params_type(match.call(),
                       extent_type = extent_type,
                       extent_value = extent_value,
                       rule_value = rule_value,
                       items = items
  )

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


#' Test for values greater than a specific value
#'
#' Configure a rule to test the time series for the presence of values greater than a specific
#' value. Tolerance can be adjusted using the `extent_type` and `extent_value` parameters, see
#' Examples for details.
#'
#' @param extent_type "all", "any", "last", "consecutive"
#' @param extent_value lower limit of extent. e.g. `extent_type="any"` and `extent_value=5` means
#'   alert if there are 5 or more values that satisfy the condition, in any position
#' @param rule_value value to test against. e.g. `rule_value=1` means alert if value is greater
#'   than 1
#' @param items named list with names corresponding to members of `item_cols`. List members are
#'   character vectors of values contained in the named column that the rule should be applied to.
#'   If `items = NULL` the rule will be applied to all items. See Details.
#'
#' @section Details: Use `items` to restrict the rule to be applied only to specified items.
#'   `items` can either be NULL or a named list of character vectors. If `NULL`, the rule will be
#'   applied to all items. If a named list, the names must match members of the `item_cols`
#'   parameter in the `inputspec`, (as well as column names in the `df`), though can be a subset.
#'   If an `item_col` is not named in the list, the rule will apply to all its members. If an
#'   `item_col` is named in the list, the rule will only be applied when the `item_col`'s value is
#'   contained in the corresponding character vector. When multiple `item_col`s are
#'   specified, the rule will be applied only to items that satisfy all the conditions.
#'
#' @return An `alert_rule` object
#' @examples
#' # alert if all values are greater than 50
#' ars <- alert_rules(alert_below(extent_type = "all", rule_value = 50))
#'
#' # alert if there are 10 or more values that are greater than 50
#' # or if the last 3 or more values are greater than 60
#' # or if 5 or more values in a row are greater than 40
#' ars <- alert_rules(
#'   alert_below(extent_type = "any", extent_value = 10, rule_value = 50),
#'   alert_below(extent_type = "last", extent_value = 3, rule_value = 60),
#'   alert_below(extent_type = "consecutive", extent_value = 5, rule_value = 40)
#' )
#' @export
alert_above <- function(extent_type = "all",
                     extent_value = 1,
                     rule_value,
                     items = NULL) {

  validate_params_required(match.call())
  validate_params_type(match.call(),
                       extent_type = extent_type,
                       extent_value = extent_value,
                       rule_value = rule_value,
                       items = items
  )

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

#' Test for when there is a percentage increase in latest values
#'
#' Check if latest values are greater than in a previous period, over a particular percentage.
#'
#' Based on the mean of values in the two periods. Ranges should be contiguous, and denote
#' positions from the end of the time series.
#'
#' @param current_period vector containing positions from end of time series to use for comparison
#' @param previous_period vector containing positions from end of time series to use for
#'   comparison. Can overlap with `current_period` if desired.
#' @param rule_value value to test against. e.g. `rule_value=5` means alert if percentage increase
#'   is greater than 5
#' @param items named list with names corresponding to members of `item_cols`. List members are
#'   character vectors of values contained in the named column that the rule should be applied to.
#'   If `items = NULL` the rule will be applied to all items. See Details.
#'
#' @section Details: Use `items` to restrict the rule to be applied only to specified items.
#'   `items` can either be NULL or a named list of character vectors. If `NULL`, the rule will be
#'   applied to all items. If a named list, the names must match members of the `item_cols`
#'   parameter in the `inputspec`, (as well as column names in the `df`), though can be a subset.
#'   If an `item_col` is not named in the list, the rule will apply to all its members. If an
#'   `item_col` is named in the list, the rule will only be applied when the `item_col`'s value is
#'   contained in the corresponding character vector. When multiple `item_col`s are
#'   specified, the rule will be applied only to items that satisfy all the conditions.
#'
#' @return An `alert_rule` object
#' @examples
#' # alert if mean of last 3 values is over 20% greater than mean of the previous 12 values
#' ars <- alert_rules(
#'   alert_difference_above_perc(current_period = 1:3, previous_period = 4:15, rule_value = 20)
#' )
#' @export
alert_difference_above_perc <- function(current_period,
                                        previous_period,
                                        rule_value,
                                        items = NULL) {

  validate_params_required(match.call())
  validate_params_type(match.call(),
                       current_period = current_period,
                       previous_period = previous_period,
                       rule_value = rule_value,
                       items = items
  )

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

#' Test for when there is a percentage drop in latest values
#'
#' Check if latest values are lower than in the previous period, over a particular percentage.
#'
#' Based on the mean of values in the two periods. Ranges should be contiguous, and denote
#' positions from the end of the time series.
#'
#' @param current_period vector containing positions from end of time series to use for comparison
#' @param previous_period vector containing positions from end of time series to use for
#'   comparison. Can overlap with `current_period` if desired.
#' @param rule_value value to test against. e.g. `rule_value=5` means alert if percentage drop is
#'   greater than 5
#' @param items named list with names corresponding to members of `item_cols`. List members are
#'   character vectors of values contained in the named column that the rule should be applied to.
#'   If `items = NULL` the rule will be applied to all items. See Details.
#'
#' @section Details: Use `items` to restrict the rule to be applied only to specified items.
#'   `items` can either be NULL or a named list of character vectors. If `NULL`, the rule will be
#'   applied to all items. If a named list, the names must match members of the `item_cols`
#'   parameter in the `inputspec`, (as well as column names in the `df`), though can be a subset.
#'   If an `item_col` is not named in the list, the rule will apply to all its members. If an
#'   `item_col` is named in the list, the rule will only be applied when the `item_col`'s value is
#'   contained in the corresponding character vector. When multiple `item_col`s are
#'   specified, the rule will be applied only to items that satisfy all the conditions.
#'
#' @return An `alert_rule` object
#' @examples
#' # alert if mean of last 3 values is over 20% lower than mean of the previous 12 values
#' ars <- alert_rules(
#'   alert_difference_below_perc(current_period = 1:3, previous_period = 4:15, rule_value = 20)
#' )
#' @export
alert_difference_below_perc <- function(current_period,
                                        previous_period,
                                        rule_value,
                                        items = NULL) {

  validate_params_required(match.call())
  validate_params_type(match.call(),
                       current_period = current_period,
                       previous_period = previous_period,
                       rule_value = rule_value,
                       items = items
  )

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
#'   The supplied `function_call` is passed to `eval()` within a `dplyr::summarise()` after grouping
#'   by the `item_cols` and ordering by the `timepoint_col`. Column names that can be used explicitly
#'   in the expression are `value` and `timepoint`, and which refer to the values in the
#'   `value_col` and `timepoint_col` columns of the data respectively.
#'
#'
#' @param short_name short name to uniquely identify the rule. Only include alphanumeric, '-', and
#'   '_' characters.
#' @param description short description of what the rule checks for
#' @param function_call call to be evaluated per item, that returns either `TRUE` or `FALSE`.
#'   Return value of `TRUE` means alert result is "FAIL". See Details.
#' @param items named list with names corresponding to members of `item_cols`. List members are
#'   character vectors of values contained in the named column that the rule should be applied to.
#'   If `items = NULL` the rule will be applied to all items. See Details.
#'
#' @section Details: Use `items` to restrict the rule to be applied only to specified items.
#'   `items` can either be NULL or a named list of character vectors. If `NULL`, the rule will be
#'   applied to all items. If a named list, the names must match members of the `item_cols`
#'   parameter in the `inputspec`, (as well as column names in the `df`), though can be a subset.
#'   If an `item_col` is not named in the list, the rule will apply to all its members. If an
#'   `item_col` is named in the list, the rule will only be applied when the `item_col`'s value is
#'   contained in the corresponding character vector. When multiple `item_col`s are
#'   specified, the rule will be applied only to items that satisfy all the conditions.
#'
#' @return An `alert_rule` object
#' @export
#'
#' @examples
#' ars <- alert_rules(
#'   alert_custom(
#'     short_name = "my_rule_combo",
#'     description = "Over 3 missing values and max value is > 10",
#'     function_call = quote(sum(is.na(value)) > 3 && max(value, na.rm = TRUE) > 10)
#'   ),
#'   alert_custom(
#'     short_name = "my_rule_doubled",
#'     description = "Last value is over double the first value",
#'     function_call = quote(rev(value)[1] > 2*value[1])
#'   )
#' )
alert_custom <- function(short_name,
                         description,
                         function_call,
                         items = NULL) {

  # TODO: Consider renaming function_call to quoted_expression

  validate_params_required(match.call())
  validate_params_type(match.call(),
                       short_name = short_name,
                       description = description,
                       function_call = function_call,
                       items = items
  )

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
#' @param inputspec Specification of data in df
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#' @param filter_results only return rows where the alert result is in this vector of values
#'
#' @return tibble
#' @noRd
run_alerts <- function(prepared_df,
                       inputspec,
                       alert_rules,
                       filter_results = c("PASS", "FAIL", "NA")) {
  alert_result <- NULL

  # if there is no data, return a formatted (empty) df
  if(nrow(prepared_df) == 0){
    return(
      prepared_df |>
        dplyr::select(dplyr::all_of(item_cols_prefix(inputspec$item_cols))) |>
        dplyr::mutate(
          alert_name = character(),
          alert_description = character(),
          alert_result = character()
        )
    )
  }

  results <-
    lapply(alert_rules, FUN = run_alert, prepared_df = prepared_df, inputspec = inputspec) |>
    purrr::reduce(dplyr::bind_rows)

  results |>
    dplyr::mutate(alert_result = tidyr::replace_na(alert_result, "NA")) |>
    dplyr::filter(alert_result %in% filter_results)
}

#' Run alert rule and return results
#'
#' @param prepared_df prepared_df
#' @param inputspec Specification of data in df
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#'
#' @return tibble
#' @noRd
run_alert <- function(prepared_df, inputspec, alert_rule){
  item <- timepoint <- NULL

  # TODO: if it's a custom rule, wrap it in some error handling

  prepared_df |>
    restrict_items(items = alert_rule$items) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(item_cols_prefix(inputspec$item_cols)))) |>
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
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col. Can be either Date values or NAs.
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#'
#' @return tibble
#' @export
mantis_alerts <- function(df,
                          inputspec,
                          alert_rules,
                          filter_results = c("PASS", "FAIL", "NA"),
                          timepoint_limits = c(NA, NA),
                          fill_with_zero = FALSE) {
  item <- NULL

  validate_params_required(match.call())
  # TODO: alert_rules are required here, but optional in mantis_report()
  validate_params_type(match.call(),
                       df = df,
                       inputspec = inputspec,
                       alert_rules = alert_rules,
                       filter_results = filter_results,
                       timepoint_limits = timepoint_limits,
                       fill_with_zero = fill_with_zero
                       )

  validate_df_to_inputspec(df, inputspec)
  validate_alert_rules_to_inputspec(alert_rules, inputspec)

  prepared_df <-
    prepare_df(
      df,
      inputspec = inputspec,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero
    )

  results <-
    run_alerts(
      prepared_df = prepared_df,
      inputspec = inputspec,
      alert_rules = alert_rules,
      filter_results = filter_results
    )

  results |>
    dplyr::rename_with(.fn = item_cols_unprefix,
                       .cols = dplyr::all_of(item_cols_prefix(inputspec$item_cols)))
}


#' Restrict prepared_df to specified items
#'
#' @param prepared_df prepared_df
#' @param items named list of character vectors
#'
#' @return tibble
#' @noRd
restrict_items <- function(prepared_df,
                           items = NULL){
  # Note: items param is validated at an earlier stage

  if (is.null(items)){
    return(prepared_df)
  }

  prepared_df |>
    dplyr::filter(eval(parse(text = items_list_to_condition_str(items))))

}


#' Convert items parameter into a filter condition
#'
#' @param items named list of character vectors
#'
#' @return character string
#' @noRd
items_list_to_condition_str <- function(items = NULL){

  condition_str <-
    paste(mapply(
      FUN = function(x, y) {
        paste0("`", item_cols_prefix(x), "`",
               " %in% c('",
               paste(y, collapse = "', '"),
               "') ")
      },
      names(items),
      items
    ),
    collapse = " & ")

  condition_str

}

#' Validate the supplied alert_rules against the supplied inputspec
#'
#' If there are any validation errors, these are all compiled before calling a
#' single stop()
#'
#' @param alert_rules user supplied alert_rules
#' @param inputspec user supplied inputspec
#'
#' @noRd
validate_alert_rules_to_inputspec <- function(alert_rules,
                                     inputspec){

  # Note: item_cols should already have been validated against the df so don't need to do it again for items

  # validate - collect all errors together and return only once
  err_validation <- character()

  if (is.null(alert_rules)){
    return(invisible())
  }

  # check all alert_rules items match item_cols
  all_items <-
    unlist(lapply(alert_rules, FUN = function(x){names(x$items)}))
  if (!is.null(all_items) && !all(all_items %in% inputspec$item_cols)){
    err_validation <-
      append(
        err_validation,
        paste(
          "alert_rule item(s) [",
          paste(all_items[!all_items %in% inputspec$item_cols], collapse = ", "),
          "] not in item_cols [",
          paste(inputspec$item_cols, collapse = ", "),
          "]. Named items must match one of the values in item_cols"
        )
      )
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

