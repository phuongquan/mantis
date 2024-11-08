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
    param_name <- params_names[i]
    param_value <- params_passed[[i]]

    if (param_name == "df") {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = FALSE,
                       expect_scalar = FALSE,
                       validation_function = is.data.frame,
                       error_message = "Expected a data frame",
                       error_contents_max_length = 100)
      )
    } else if (param_name == "inputspec") {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = FALSE,
                       expect_scalar = FALSE,
                       validation_function = is_inputspec,
                       error_message = "Expected an inputspec object",
                       error_contents_max_length = 100)
      )
    } else if (param_name == "outputspec") {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = TRUE,
                       expect_scalar = FALSE,
                       validation_function = is_outputspec,
                       error_message = "Expected an outputspec object",
                       error_contents_max_length = 100)
      )
    } else if (param_name == "alert_rules") {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = TRUE,
                       expect_scalar = FALSE,
                       validation_function = is_alert_rules,
                       error_message = "Expected an alert_rules object",
                       error_contents_max_length = 100)
      )
    } else if (param_name %in% c("show_progress")) {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = FALSE,
                       expect_scalar = TRUE,
                       validation_function = is.logical,
                       error_message = "Expected TRUE/FALSE",
                       error_contents_max_length = 100)
      )
    } else if (param_name %in% c("save_directory")) {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = FALSE,
                       expect_scalar = TRUE,
                       validation_function = function(x){is.character(x) && dir.exists(x)},
                       error_message = "Directory not found. Expected a single path to an existing directory",
                       error_contents_max_length = 255)
      )
    } else if (param_name %in% c("save_filename")) {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = TRUE,
                       expect_scalar = TRUE,
                       validation_function = function(x){!grepl("[^a-zA-Z0-9_-]", x) && nchar(x) > 0},
                       error_message = "Filename can only contain alphanumeric, '-', and '_' characters, and should not include the file extension",
                       error_contents_max_length = 255)
      )
    } else if (param_name %in% c("dataset_description",
                                 "report_title",
                                 "tab_col")) {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = TRUE,
                       expect_scalar = TRUE,
                       validation_function = is.character,
                       error_message = "Expected a character string",
                       error_contents_max_length = 500)
      )
    } else if (param_name %in% c("timepoint_col",
                                 "item_col",
                                 "value_col",
                                 "plot_value_type",
                                 "plot_type",
                                 "item_label",
                                 "plot_label")) {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = FALSE,
                       expect_scalar = TRUE,
                       validation_function = is.character,
                       error_message = "Expected a character string",
                       error_contents_max_length = 100)
      )
    } else if (param_name %in% c("period")) {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = FALSE,
                       expect_scalar = TRUE,
                       validation_function = function(x){x %in% c("day", "week", "month", "quarter", "year")},
                       error_message = "Values allowed are: day, week, month, quarter, year",
                       error_contents_max_length = 100)
      )
    } else if (param_name == "summary_cols") {
      # TODO: if they provide a colname that doesn't exist, is it better to error or ignore?
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = TRUE,
                       expect_scalar = FALSE,
                       validation_function = function(x){all(x %in% c("max_value",
                                                                      "last_value",
                                                                      "last_value_nonmissing",
                                                                      "last_timepoint",
                                                                      "mean_value"))},
                       error_message = "Expected a subset of c('max_value', 'last_value', 'last_value_nonmissing', 'last_timepoint', 'mean_value')",
                       error_contents_max_length = 500)
      )
    } else if (param_name %in% c("item_order")) {
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = TRUE,
                       expect_scalar = FALSE,
                       validation_function = function(x){(length(x) == 1 && x == TRUE) || is.character(x)},
                       error_message = "Expected either TRUE or a vector of character strings",
                       error_contents_max_length = 500)
      )
    } else if (param_name == "sort_by") {
      # NOTE: if they provide a colname that doesn't exist, just ignore it, as you may want to
      # supply a standard superset for everything
      err_validation <- append(
        err_validation,
        validate_param(param_name = param_name,
                       param_value = param_value,
                       allow_null = TRUE,
                       expect_scalar = FALSE,
                       validation_function = is.character,
                       error_message = "Expected a vector of character strings",
                       error_contents_max_length = 500)
      )
    }
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
                               alert_rules,
                               dataset_description = "shortdesc",
                               report_title = "title",
                               save_directory = ".",
                               save_filename = "filename",
                               show_progress = TRUE,
                               timepoint_col = "timepoint_col",
                               item_col = "item_col",
                               value_col = "value_col",
                               tab_col = "tab_col",
                               period = "day",
                               plot_value_type = "value",
                               plot_type = "bar",
                               item_label = "Item",
                               plot_label = "History",
                               summary_cols = c("max_value"),
                               sync_axis_range = FALSE,
                               item_order = NULL,
                               sort_by = NULL
                               ) {
  if (missing(df)) {
    df <- data.frame("Fieldname" = 123)
  }
  if (missing(inputspec)) {
    inputspec <- mantis::inputspec(timepoint_col = "timepoint",
                                   item_col = "item",
                                   value_col = "value")
  }
  if (missing(outputspec)) {
    outputspec <- mantis::outputspec_interactive()
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
    alert_rules = alert_rules,
    dataset_description = dataset_description,
    report_title = report_title,
    save_directory = save_directory,
    save_filename = save_filename,
    show_progress = show_progress,
    timepoint_col = timepoint_col,
    item_col = item_col,
    value_col = value_col,
    tab_col = tab_col,
    period = period,
    plot_value_type = plot_value_type,
    plot_type = plot_type,
    item_label = item_label,
    plot_label = plot_label,
    summary_cols = summary_cols,
    sync_axis_range = sync_axis_range,
    item_order = item_order,
    sort_by = sort_by
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

