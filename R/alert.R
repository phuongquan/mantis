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
#'
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

run_alerts <- function(prepared_df,
                       alert_rules){

  results <-
    lapply(alert_rules, FUN = run_alert, prepared_df = prepared_df) %>%
    purrr::reduce(dplyr::full_join, by = "item")

  results
}

run_alert <- function(prepared_df, alert_rule){
  item <- timepoint <- NULL

  prepared_df %>%
    dplyr::filter(item %in% alert_rule$items | all(alert_rule$items == "ALL")) %>%
    dplyr::group_by(item) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::summarise("{alert_rule$short_name}" := eval(alert_rule$function_call))

  # TODO: is it more efficient if the data is wide? Might be able to use tidyselect for items then
  # bycol <- prepared_df %>%
  #   tidyr::pivot_wider(id_cols = "timepoint",
  #                      names_from = "item")

}
