#' Create set of alert rules
#'
#' @param ... alerts to apply to the time series
#' @return An `alert_rules` object
#' @export
alert_rules <- function(...) {

  ars <- list(...)

  structure(ars, class = "mantis_alert_rules")
}

alert_rule <- function(type,
                       function_call,
                       friendly_name_short,
                       items = "ALL") {

  structure(
    list(
      type = type,
      function_call = function_call,
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
#' @return
#' @export
alert_missing <- function(extent_type = "all",
                          extent_value = 1,
                          items = "ALL") {

  # TODO: consider allowing different extent_values for different alert severities
  # TODO: "consecutive" type. rle doesn't handle runs of NAs so will need a wrapper for it

  function_call <-
    switch (
      missing_extent_type,
      all = quote(all(is.na(value))),
      any = substitute(sum(is.na(value)) >= x, list(x = missing_extent_value)),
      last = substitute(all(is.na(rev(value)[1:x])),
                        list(x = missing_extent_value))
    )

  alert_rule(
    type = "missing",
    function_call = function_call,
    friendly_name_short = paste0("missing_", missing_extent_type, ifelse(missing_extent_type == "all", "", paste0("_", missing_extent_value))),
    items = items
  )
}


run_alerts <- function(prepared_df,
                       alert_rules){

  # bycol <- prepared_df %>%
  #   tidyr::pivot_wider(id_cols = "timepoint",
  #                      names_from = "item")

  results <-
    prepared_df %>%
    dplyr::group_by(item) %>%
    dplyr::arrange(timepoint) %>%
    dplyr::summarise(missing_all = eval(alert_rules[[1]]$function_call))

  results
}
