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
      if (!is.data.frame(param_value)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Expected a data frame but found: [ class = ",
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 100),
            "]."
          )
        )
      }
    } else if (param_name == "inputspec") {
      if (!is_inputspec(param_value)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Expected an inputspec object but found: [ class = ",
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 100),
            "]"
          )
        )
      }
    } else if (param_name == "outputspec") {
      if (!is.null(param_value) &&
          !is_outputspec(param_value)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Expected an outputspec object but found: [ class = ",
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 100),
            "]"
          )
        )
      }
    } else if (param_name == "alert_rules") {
      if (!is.null(param_value) &&
          !is_alert_rules(param_value)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Expected an alert_rules object but found: [ class = ",
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 100),
            "]"
          )
        )
      }
    } else if (param_name %in% c("show_progress")) {
      if (!is.logical(param_value)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Expected TRUE/FALSE but found: [ class = ",
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 100),
            "]"
          )
        )
      }
    } else if (param_name %in% c("save_directory")) {
      if (!is.character(param_value) ||
          !dir.exists(param_value)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Directory not found: [ class = ",
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 255),
            "]"
          )
        )
      }
    } else if (param_name %in% c("save_filename")) {
      if (!is.null(param_value) &&
          (grepl("[^a-zA-Z0-9_-]", param_value) ||
           nchar(param_value) == 0)) {
        # NOTE: this is very restrictive and I'm not sure how it works in different locales
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Invalid filename: ",
            param_value,
            ". Filename can only contain alphanumeric, '-', and '_' characters, and should not include the file extension."
          )
        )
      }
    } else if (param_name %in% c("dataset_description",
                                 "report_title",
                                 "timepoint_col",
                                 "item_col",
                                 "value_col",
                                 "tab_col",
                                 "plot_value_type",
                                 "plot_type",
                                 "item_label",
                                 "plot_label")
               ) {
      if (!is.null(param_value) &&
          (!is.character(param_value) ||
           length(param_value) != 1)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Expected a character string but found: [ class = ",
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 500),
            "]"
          )
        )
      }
    } else if (param_name %in% c("period")) {
      if (length(param_value) != 1 ||
          !(param_value %in% c("day", "week", "month", "quarter", "year"))) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Values allowed are: day, week, month, quarter, year but found: [ class = ",
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 100),
            "]"
          )
        )
      }
    } else if (param_name == "summary_cols") {
      if (!is_summary_cols(param_value)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ': Expected a subset of c("max_value", "last_value", "last_value_nonmissing", "last_timepoint", "mean_value") but found: [ class = ',
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 500),
            "]"
          )
        )
      }
    } else if (param_name %in% c("item_order")) {
      if (!is.null(param_value) &&
          !is_true_or_character_vector(param_value)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ": Expected either TRUE or a vector of character strings but found: [ class = ",
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 500),
            "]"
          )
        )
      }
    } else if (param_name == "sort_by") {
      if (!is_sort_by(param_value)) {
        err_validation <- append(
          err_validation,
          paste0(
            param_name,
            ': Expected a subset of c(item", "alert_overall", "max_value", "last_value", "last_value_nonmissing", "last_timepoint", "mean_value") but found: [ class = ',
            class(param_value),
            "; contents = ",
            substr(toString(param_value), 1, 500),
            "]"
          )
        )
      }
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

#' Check if object is TRUE or a character vector
#'
#' Helper for `validate_params_type()`
#'
#' @param x parameter value to test
#'
#' @return logical
#' @noRd
is_true_or_character_vector <- function(x){
  (length(x) == 1 && x == TRUE) || all(is.character(x))
}

#' Check if object is valid for summary_cols param
#'
#' Helper for `validate_params_type()`
#'
#' @param x object to test
#' @return logical
#' @noRd
is_summary_cols <- function(x){
  is.null(x) ||
  length(base::setdiff(x, c("max_value",
                                      "last_value",
                                      "last_value_nonmissing",
                                      "last_timepoint",
                                      "mean_value"))) == 0

}

is_sort_by <- function(x){
  is.null(x) ||
  length(base::setdiff(x, c("item",
                                      "alert_overall",
                                      "max_value",
                                      "last_value",
                                      "last_value_nonmissing",
                                      "last_timepoint",
                                      "mean_value",
                                      "-item",
                                      "-alert_overall",
                                      "-max_value",
                                      "-last_value",
                                      "-last_value_nonmissing",
                                      "-last_timepoint",
                                      "-mean_value"))) == 0

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
                               period = "day"

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
    period = period
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

