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
    if (params_names[i] == "df") {
      if (!is.data.frame(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a data frame but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]."
          )
        )
      }
    } else if (params_names[i] == "inputspec") {
      if (!is_inputspec(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected an inputspec object but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] == "outputspec") {
      if (!is.null(params_passed[[i]]) &&
          !is_outputspec(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected an outputspec object but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] == "alert_rules") {
      if (!is.null(params_passed[[i]]) &&
          !is_alert_rules(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected an alert_rules object but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("show_progress")) {
      if (!is.logical(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected TRUE/FALSE but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("save_directory")) {
      if (!is.character(params_passed[[i]]) ||
          !dir.exists(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Directory not found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 255),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("save_filename")) {
      if (!is.null(params_passed[[i]]) &&
          (grepl("[^a-zA-Z0-9_-]", params_passed[[i]]) ||
           nchar(params_passed[[i]]) == 0)) {
        # NOTE: this is very restrictive and I'm not sure how it works in different locales
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Invalid filename: ",
            params_passed[[i]],
            ". Filename can only contain alphanumeric, '-', and '_' characters, and should not include the file extension."
          )
        )
      }
    } else if (params_names[i] %in% c("dataset_description",
                                      "report_title",
                                      "timepoint_col",
                                      "item_col",
                                      "value_col",
                                      "tab_col")) {
      if (!is.null(params_passed[[i]]) &&
          (!is.character(params_passed[[i]]) ||
           length(params_passed[[i]]) != 1)) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a character string but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 500),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("period")) {
      if (length(params_passed[[i]]) != 1 ||
          !(params_passed[[i]] %in% c("day", "week", "month", "quarter", "year"))) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Values allowed are: day, week, month, quarter, year but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
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

