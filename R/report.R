# -----------------------------------------------------------------------------
#' Create an interactive time series report from a data frame
#'
#' Accepts a data frame containing multiple time series in long format,
#' generates a collection of interactive time series plots for visual
#' inspection, and saves the report to disk.
#'
#' @param df A data frame containing multiple time series in long format. See
#'   Details.
#' @param file String specifying the desired file name (and path) to save the
#'   report to. The file name should include the extension ".html". If only a
#'   file name is supplied, the report will be saved in the current working
#'   directory. If a path is supplied, the directory should already exist. Any
#'   existing file of the same name will be overwritten unless
#'   `add_timestamp` is set to `TRUE`.
#' @param inputspec [`inputspec()`] object specifying which columns in the
#'   supplied `df` represent the "timepoint", "item", "value"  and (optionally)
#'   "tab" for the time series. If a "tab" column is specified, a separate tab
#'   will be created for each distinct value in the column.
#' @param outputspec `outputspec` object specifying the desired format of the
#'   html table(s). If not supplied, default values will be used.
#' @param alertspec [`alertspec()`] object specifying conditions to test and
#'   display.
#' @param report_title Title to appear on the report.
#' @param dataset_description Short description of the dataset being shown. This
#'   will appear on the report.
#' @param add_timestamp Append a timestamp to the end of the filename with
#'   format `_YYMMDD_HHMMSS`. This can be used to keep multiple versions of the
#'   same report. Default = `FALSE`.
#' @param show_progress Print progress to console. Default = `TRUE`.
#' @param ... Further parameters to be passed to `rmarkdown::render()`. Cannot
#'   include any of `input`, `output_dir`, `output_file`, `params`, `quiet`.
#' @return A string containing the name and full path of the saved report.
#'
#' @details The supplied data frame should contain multiple time series in long
#'   format, i.e.:
#'
#' * one "timepoint" (date/posixt) column which will be used for the x-axes.
#'   Values should follow a regular pattern, e.g. daily or monthly, but do not
#'   have to be consecutive.
#' * one or more "item" (character) columns containing categorical values
#'   identifying distinct time series.
#' * one "value" (numeric) column containing the time series values which will
#'   be used for the y-axes.
#' * Optionally, a "tab" (character) column containing categorical values which
#'   will be used to group the time series into different tabs on the report.
#'
#'   The `inputspec` parameter maps the data frame columns to the above.
#' @examples
#' \donttest{
#' # create an interactive report in the temp directory,
#' # with one tab per Location
#' filename <- mantis_report(
#'   df = example_prescription_numbers,
#'   file = file.path(tempdir(), "example_prescription_numbers_interactive.html"),
#'   inputspec = inputspec(
#'     timepoint_col = "PrescriptionDate",
#'     item_cols = c("Location", "Antibiotic", "Spectrum"),
#'     value_col = "NumberOfPrescriptions",
#'     tab_col = "Location",
#'     timepoint_unit = "day"
#'   ),
#'   outputspec = outputspec_interactive(),
#'   report_title = "Daily antibiotic prescribing",
#'   dataset_description = "Antibiotic prescriptions by site",
#'   show_progress = TRUE
#' )
#'
#' filename
#' \dontshow{unlink(filename)}
#' }
#'
#' \donttest{
#' # create an interactive report in the temp directory, with alerting rules
#' filename <- mantis_report(
#'   df = example_prescription_numbers,
#'   file = file.path(tempdir(), "example_prescription_numbers_interactive.html"),
#'   inputspec = inputspec(
#'     timepoint_col = "PrescriptionDate",
#'     item_cols = c("Location", "Antibiotic", "Spectrum"),
#'     value_col = "NumberOfPrescriptions",
#'     tab_col = "Location",
#'     timepoint_unit = "day"
#'   ),
#'   outputspec = outputspec_interactive(),
#'   alertspec = alertspec(
#'    alert_rules = alert_rules(
#'     alert_missing(extent_type = "any", extent_value = 1),
#'     alert_equals(extent_type = "all", rule_value = 0)
#'    ),
#'    show_tab_results = c("FAIL", "NA")
#'   ),
#'   report_title = "Daily antibiotic prescribing",
#'   dataset_description = "Antibiotic prescriptions by site",
#'   show_progress = TRUE
#' )
#'
#' filename
#' \dontshow{unlink(filename)}
#' }
#'
#' @seealso [inputspec()], [outputspec_interactive()],
#'   [outputspec_static_heatmap()], [outputspec_static_multipanel()],
#'   [alertspec()]
#' @export
mantis_report <- function(
  df,
  file,
  inputspec,
  outputspec = NULL,
  alertspec = NULL,
  report_title = "mantis report",
  dataset_description = "",
  add_timestamp = FALSE,
  show_progress = TRUE,
  ...
) {
  validate_params_required(match.call())
  validate_params_type(
    match.call(),
    df = df,
    file = file,
    inputspec = inputspec,
    outputspec = outputspec,
    alertspec = alertspec,
    report_title = report_title,
    dataset_description = dataset_description,
    add_timestamp = add_timestamp,
    show_progress = show_progress
  )

  validate_df_to_inputspec(df, inputspec)
  validate_alert_rules_to_inputspec(alertspec$alert_rules, inputspec)

  if (is.null(outputspec)) {
    outputspec <- outputspec_interactive()
  }

  file_fullname <- format_filename(
    file,
    append_string = ifelse(
      add_timestamp,
      format(Sys.time(), "_%Y%m%d%_%H%M%S"),
      ""
    )
  )

  # NOTE: setting intermediates_dir in rmarkdown::render() to
  # output directory or tempdir() causes duplicate chunk label errors when package
  # is run from inside an rmd/qmd
  # So need to designate a unique subfolder of tempdir()
  temp_dirname <-
    file.path(
      tempdir(),
      paste0(
        "mantis_temp_",
        # append a short random ID
        paste0(sample(c(0:9, letters), 8, replace = TRUE), collapse = "")
      )
    )

  rmarkdown::render(
    input = system.file(
      "rmd",
      "report-html.Rmd",
      package = utils::packageName(),
      mustWork = TRUE
    ),
    output_file = file_fullname,
    intermediates_dir = temp_dirname,
    knit_root_dir = temp_dirname,
    params = list(
      df = df,
      inputspec = inputspec,
      outputspec = outputspec,
      alertspec = alertspec,
      report_title = report_title,
      dataset_description = dataset_description
    ),
    quiet = !show_progress,
    ...
  )

  # remove temporary directory created earlier by rmarkdown::render()
  unlink(temp_dirname, recursive = TRUE)

  file_fullname
}



#' Format the supplied file name and path if necessary
#'
#' This assumes the file path and name has already passed validation.
#'
#' @param file string containing file name (and optionally path), with the
#'   extension .html
#' @param append_string append the string to file name, before the extension.
#'
#' @returns A string containing the formatted file name and canonical path
#' @noRd
format_filename <- function(
  file,
  append_string = ""
) {
  path <- normalizePath(dirname(file))
  filename_sans_ext <- tools::file_path_sans_ext(basename(file))
  ext <- tools::file_ext(file)

  file.path(path, paste0(filename_sans_ext, append_string, ".", ext))
}
