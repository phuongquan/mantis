# =============================================================================
# FUNCTIONS FOR PARAMETER VALIDATION

# -----------------------------------------------------------------------------
#' Check all required params have been passed in to the calling function
#'
#' Should be called at start of all exported functions to check user has
#' supplied all required arguments If any are missing, execution is stopped
#'
#' @param call call from the function being checked
#' @noRd
validate_params_required <- function(call) {
  # get the required arguments from function definition
  params_defined <-
    formals(utils::tail(as.character(call[[1]]), n = 1))
  params_required <- names(which(vapply(params_defined, is.symbol, logical(1))))
  # exclude ... param
  params_required <- params_required[which(params_required != "...")]
  # get the arguments passed into the parent call
  params_passed <- names(as.list(call)[-1])

  if (any(!params_required %in% params_passed)) {
    stop_custom(
      .subclass = "invalid_param_missing",
      message = paste(
        "Required argument(s) missing:",
        paste(setdiff(params_required, params_passed),
              collapse = ", "
        )
      )
    )
  }
}


# -----------------------------------------------------------------------------
#' Check all params that have been passed in to the calling function are of
#' correct type/class
#'
#' Should be called at start of all exported functions to check user has
#' supplied all arguments correctly. Any that are invalid are collated and then
#' execution is stopped
#'
#' @param call call from the function being checked
#' @param ... the parameters that were actually passed into the function being
#'   checked, with names
#' @noRd
validate_params_type <- function(call, ...) {
  params_defined <-
    names(formals(utils::tail(as.character(call[[1]]), n = 1)))
  # exclude ... param
  params_defined <- params_defined[which(params_defined != "...")]
  params_passed <- list(...)
  params_names <- names(params_passed)

  # check internal usage is correct
  if (length(which(params_names != "")) != length(params_passed)) {
    stop_custom(
      .subclass = "invalid_call",
      message = "Invalid call for function. Params must be passed in with names"
    )
  }
  if (!setequal(params_defined, params_names)) {
    stop_custom(
      .subclass = "invalid_call",
      message = paste0(
        "Invalid call for function. Different set of params in parent function
        definition than were passed in to validate_params_type().",
        "\n",
        "In validate_params_type() but not in parent function: ",
        paste(setdiff(params_names, params_defined), collapse = ", "),
        "\n",
        "In parent function but not in validate_params_type(): ",
        paste(setdiff(params_defined, params_names), collapse = ", ")
      )
    )
  }

  # validate user-supplied params - collect all errors together and return only once
  err_validation <- character()
  for (i in seq_along(params_names)) {
    err_validation <- append(
        err_validation,
        validate_param_byname(param_name = params_names[i],
                              param_value = params_passed[[i]]))
  }

  if (length(err_validation) > 0) {
    stop_custom(
      .subclass = "invalid_param_type",
      message = paste0(
        "Invalid argument(s) supplied.\n",
        paste(err_validation, collapse = "\n")
      )
    )
  }
}

#' Select and apply type validation rules based on the param name
#'
#' @param param_name name of the parameter
#' @param param_value object passed into the parameter
#'
#' @return character
#' @noRd
validate_param_byname <- function(param_name, param_value){
  switch(param_name,
    "df" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = FALSE,
      validation_function = is.data.frame,
      error_message = "Expected a data frame",
      error_contents_max_length = 100
    ),
    "inputspec" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = FALSE,
      validation_function = is_inputspec,
      error_message = "Expected an inputspec object",
      error_contents_max_length = 100
    ),
    "outputspec" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = FALSE,
      validation_function = is_outputspec,
      error_message = "Expected an outputspec object",
      error_contents_max_length = 100
    ),
    "alertspec" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = FALSE,
      validation_function = is_alertspec,
      error_message = "Expected an alertspec object",
      error_contents_max_length = 100
    ),
    "alert_rules" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = FALSE,
      validation_function = is_alert_rules,
      error_message = "Expected an alert_rules object",
      error_contents_max_length = 100
    ),
    "fill_with_zero" = ,
    "sync_axis_range" = ,
    "show_progress" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = is.logical,
      error_message = "Expected TRUE/FALSE",
      error_contents_max_length = 100
    ),
    "save_directory" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = function(x) {
        is.character(x) && dir.exists(x)
      },
      error_message = "Directory not found. Expected a single path to an existing directory",
      error_contents_max_length = 255
    ),
    "save_filename" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = TRUE,
      validation_function = function(x) {
        !grepl("[^a-zA-Z0-9_-]", x) && nchar(x) > 0
      },
      error_message = "Filename can only contain alphanumeric, '-', and '_' characters, and should not include the file extension",
      error_contents_max_length = 255
    ),
    "tab_group_name" = ,
    "tab_name" = ,
    "dataset_description" = ,
    "report_title" = ,
    "y_label" = ,
    "plot_label" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = TRUE,
      validation_function = is.character,
      error_message = "Expected a character string",
      error_contents_max_length = 500
    ),
    "description" = ,
    "fill_colour" = ,
    "timepoint_col" = ,
    "value_col" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = function(x) {
        is.character(x) && nchar(x) > 0
      },
      error_message = "Expected a non-empty character string",
      error_contents_max_length = 100
    ),
    "item_cols" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = FALSE,
      validation_function = function(x) {
        all(is.character(x)) && all(nchar(x) > 0)
      },
      error_message = "Expected a vector of non-empty character strings",
      error_contents_max_length = 100
    ),
    "tab_col" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = TRUE,
      validation_function = function(x) {
        is.character(x) && nchar(x) > 0
      },
      error_message = "Expected a non-empty character string",
      error_contents_max_length = 100
    ),
    "plot_value_type" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = function(x) {
        x %in% c("value", "delta")
      },
      error_message = "Values allowed are: value, delta",
      error_contents_max_length = 100
    ),
    "plot_type" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = function(x) {
        x %in% c("bar", "line")
      },
      error_message = "Values allowed are: bar, line",
      error_contents_max_length = 100
    ),
    "period" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = function(x) {
        x %in% c("sec", "min", "hour", "day", "week", "month", "quarter", "year")
      },
      error_message = "Values allowed are: sec, min, hour, day, week, month, quarter, year",
      error_contents_max_length = 100
    ),
    # TODO: if they provide a colname that doesn't exist, is it better to error or ignore?
    "summary_cols" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = FALSE,
      validation_function = function(x) {
        all(
          x %in% c(
            "max_value",
            "last_value",
            "last_value_nonmissing",
            "last_timepoint",
            "mean_value"
          )
        )
      },
      error_message = 'Expected a subset of c("max_value", "last_value", "last_value_nonmissing", "last_timepoint", "mean_value")',
      error_contents_max_length = 500
    ),
    "item_order" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = FALSE,
      validation_function = function(x) {
        all(is.list(x),
            length(names(x)) == length(x),
            unlist(lapply(x,
                          function(x) {
                            (length(x) == 1 && x == TRUE) || is.character(x)
                          })))
      },
      error_message = "Expected a named list with each item either TRUE or a vector of character strings",
      error_contents_max_length = 500
    ),
    "items" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = FALSE,
      validation_function = function(x) {
        all(is.list(x),
            length(names(x)) == length(x),
            unlist(lapply(x, is.character)))
      },
      error_message = "Expected a named list with each item a vector of character strings",
      error_contents_max_length = 500
    ),
    # NOTE: if they provide names that don't match the data, just ignore them
    "item_labels" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = FALSE,
      validation_function = function(x) {
        all(!is.list(x),
            length(names(x)) == length(x),
            is.character(x))
      },
      error_message = "Expected a named vector of character strings",
      error_contents_max_length = 500
    ),
    # NOTE: if they provide values that don't exist in the data, just ignore them, as you may want to
    # supply a standard superset for everything
    "sort_by" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = FALSE,
      validation_function = is.character,
      error_message = "Expected a vector of character strings",
      error_contents_max_length = 500
    ),
    "filter_results" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = FALSE,
      validation_function = function(x) {
        all(x %in% c("PASS", "FAIL", "NA"))
      },
      error_message = 'Expected a subset of c("PASS", "FAIL", "NA")',
      error_contents_max_length = 100
    ),
    "show_tab_results" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = TRUE,
      expect_scalar = FALSE,
      validation_function = function(x) {
        all(x %in% c("PASS", "FAIL", "NA"))
      },
      error_message = 'Expected a subset of c("PASS", "FAIL", "NA")',
      error_contents_max_length = 100
    ),
    "timepoint_limits" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = FALSE,
      validation_function = function(x) {
        # NOTE: can't just test for Date class as if first value is NA, second value gets converted to numeric implicitly
        length(x) == 2 && all(is_date_or_time(x) | is.numeric(x) | is.na(x))
      },
      error_message = 'Expected a vector of two Dates or NAs',
      error_contents_max_length = 100
    ),
    "extent_value" = ,
    "tab_group_level" = ,
    "tab_level" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = function(x){is.numeric(x) && x == as.integer(x) && x >= 1},
      error_message = "Expected an integer >= 1",
      error_contents_max_length = 100
    ),
    "extent_type" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = function(x){
        x %in% c("all", "any", "last", "consecutive")
        },
      error_message = "Values allowed are: all, any, last, consecutive",
      error_contents_max_length = 100
    ),
    "rule_value" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = is.numeric,
      error_message = "Expected a numeric value",
      error_contents_max_length = 100
    ),
    "current_period" = ,
    "previous_period" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = FALSE,
      validation_function = function(x){all(is.numeric(x)) && all(x == as.integer(x))},
      error_message = "Expected a vector of integers",
      error_contents_max_length = 100
    ),
    "short_name" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = TRUE,
      validation_function = function(x) {
        !grepl("[^a-zA-Z0-9_-]", x) && nchar(x) > 0
      },
      error_message = "Should only contain alphanumeric, '-', and '_' characters",
      error_contents_max_length = 255
    ),
    "function_call" = validate_param(
      param_name = param_name,
      param_value = param_value,
      allow_null = FALSE,
      expect_scalar = FALSE,
      validation_function = is.call,
      error_message = "Expected a call",
      error_contents_max_length = 100
    ),
  )

}

#' Perform specified type validation
#'
#' If if fails, return an error message, if it succeeds, return a zero-length character vector
#'
#' @param param_name name of the parameter
#' @param param_value object passed into the parameter
#' @param allow_null TRUE/FALSE. Specifies if a value of NULL is allowed
#' @param expect_scalar TRUE/FALSE. Specifies if only a single value allowed
#' @param validation_function function to use to check if param_value is appropriate
#' @param error_message user-friendly string describing the problem
#' @param error_contents_max_length max chars to display from the param_value object
#'
#' @return character
#' @noRd
validate_param <- function(param_name,
                           param_value,
                           allow_null,
                           expect_scalar,
                           validation_function,
                           error_message,
                           error_contents_max_length) {

  # null value supplied
  if (is.null(param_value)) {
    if (allow_null) {
      return(character())
    } else{
      return(
        validation_failed_message(
          param_name = param_name,
          param_value = param_value,
          error_message = error_message,
          error_contents_max_length = error_contents_max_length
        )
      )
    }
  }

  # a vector has been supplied when only a single value is expected
  if (expect_scalar && length(param_value) > 1) {
    return(
      validation_failed_message(
        param_name = param_name,
        param_value = param_value,
        error_message = error_message,
        error_contents_max_length = error_contents_max_length
      )
    )
  }

  # else do the validation
  if (validation_function(param_value)) {
    return(character())
  } else{
    return(
      validation_failed_message(
        param_name = param_name,
        param_value = param_value,
        error_message = error_message,
        error_contents_max_length = error_contents_max_length
      )
    )
  }
}

#' Construct validation failed message
#'
#' @param param_name name of the parameter
#' @param param_value object passed into the parameter
#' @param error_message user-friendly string describing the problem
#' @param error_contents_max_length max chars to display from the param_value object
#'
#' @return character
#' @noRd
validation_failed_message <- function(param_name,
                                      param_value,
                                      error_message,
                                      error_contents_max_length) {
  paste0(
    param_name,
    ": ",
    error_message,
    ": Found [ class = ",
    paste(class(param_value), collapse = ";"),
    "; contents = ",
    substr(toString(param_value), 1, error_contents_max_length),
    "]."
  )
}

# -----------------------------------------------------------------------------
# DUMMY FUNCTIONS SET UP PURELY FOR UNIT TESTING
# integrates better with RStudio when placed here rather than in testthat/helper.R

#' Dummy function to assist unit testing of validate_params_required()
#'
#' @param p1 required param
#' @param p2 required param
#' @param p3 optional param
#' @param ... optional param
#' @noRd
testfn_params_required <- function(p1, p2, p3 = NULL, ...) {
  validate_params_required(match.call())
}

#' Dummy function to assist unit testing of validate_params_type()
#'
#' This should contain every parameter defined in an exported function
#'
#' @noRd
testfn_params_type <- function(df,
                               inputspec,
                               outputspec,
                               alertspec,
                               alert_rules,
                               dataset_description = "",
                               report_title = "mantis report",
                               save_directory = ".",
                               save_filename = "filename",
                               show_progress = TRUE,
                               timepoint_col = "timepoint_col",
                               item_cols = "item_cols",
                               value_col = "value_col",
                               tab_col = NULL,
                               period = "day",
                               plot_value_type = "value",
                               plot_type = "bar",
                               item_labels = NULL,
                               plot_label = NULL,
                               summary_cols = c("max_value"),
                               sync_axis_range = FALSE,
                               item_order = NULL,
                               sort_by = NULL,
                               fill_colour = "blue",
                               y_label = NULL,
                               filter_results = c("PASS", "FAIL", "NA"),
                               show_tab_results = c("PASS", "FAIL", "NA"),
                               timepoint_limits = c(NA, NA),
                               fill_with_zero = FALSE,
                               tab_name = NULL,
                               tab_level = 1,
                               tab_order = NULL,
                               tab_group_name = NULL,
                               tab_group_level = 1,
                               extent_type = "all",
                               extent_value = 1,
                               rule_value = 0,
                               items = NULL,
                               current_period = 1:3,
                               previous_period = 4:9,
                               short_name = "my_rule",
                               description = "rule_description",
                               function_call = quote(all(is.na(value)))
) {
  if (missing(df)) {
    df <- data.frame("Fieldname" = 123)
  }
  if (missing(inputspec)) {
    inputspec <-
      structure(list(rule = NA), class = "mantis_inputspec")

  }
  if (missing(outputspec)) {
    outputspec <-
      structure(list(rule = NA), class = "mantis_outputspec")
  }
  if (missing(alertspec)) {
    alertspec <-
      structure(list(rule = NA), class = "mantis_alertspec")
  }
  if (missing(alert_rules)) {
    alert_rules <-
      structure(list(rule = NA), class = "mantis_alert_rules")
  }

  validate_params_type(
    match.call(),
    df = df,
    inputspec = inputspec,
    outputspec = outputspec,
    alertspec = alertspec,
    alert_rules = alert_rules,
    dataset_description = dataset_description,
    report_title = report_title,
    save_directory = save_directory,
    save_filename = save_filename,
    show_progress = show_progress,
    timepoint_col = timepoint_col,
    item_cols = item_cols,
    value_col = value_col,
    tab_col = tab_col,
    period = period,
    plot_value_type = plot_value_type,
    plot_type = plot_type,
    item_labels = item_labels,
    plot_label = plot_label,
    summary_cols = summary_cols,
    sync_axis_range = sync_axis_range,
    item_order = item_order,
    sort_by = sort_by,
    fill_colour = fill_colour,
    y_label = y_label,
    filter_results = filter_results,
    show_tab_results = show_tab_results,
    timepoint_limits = timepoint_limits,
    fill_with_zero = fill_with_zero,
    tab_name = tab_name,
    tab_level = tab_level,
    tab_order = tab_order,
    tab_group_name = tab_group_name,
    tab_group_level = tab_group_level,
    extent_type = extent_type,
    extent_value = extent_value,
    rule_value = rule_value,
    items = items,
    current_period = current_period,
    previous_period = previous_period,
    short_name = short_name,
    description = description,
    function_call = function_call
  )
}


# =============================================================================
# MISCELLANEOUS

# -----------------------------------------------------------------------------
#' Raise a custom error with a class that can be tested for
#'
#' Copied from https://adv-r.hadley.nz/conditions.html#custom-conditions
#'
#' @param .subclass category of error
#' @param message error message
#' @param call calling function
#' @param ... other items to pass to condition object
#' @noRd
stop_custom <- function(.subclass, message, call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}


#' Test if object is Date or POSIXt class
#'
#' @param x object to test
#'
#' @return logical(1)
#' @noRd
is_date_or_time <- function(x){
  inherits(x, what = "Date") || inherits(x, what = "POSIXt")
}
