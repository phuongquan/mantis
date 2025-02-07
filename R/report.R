#' Create an interactive time series report from a data frame
#'
#' Accepts a data frame containing multiple time series in long format, generates a collection of
#' interactive time series plots for visual inspection, and saves the report to disk.
#'
#' @param df A data frame containing multiple time series in long format. See Details.
#' @param inputspec [`inputspec()`] object specifying which columns in the supplied `df` represent the
#'   "timepoint", "item", "value"  and (optionally) "tab" for the time series. If a "tab" column is specified, a separate tab
#'   will be created for each distinct value in the column.
#' @param outputspec `outputspec` object specifying the desired format of the html table(s). If
#'   not supplied, default values will be used.
#' @param alertspec [`alertspec()`] object specifying conditions to test and display
#' @param report_title Title to appear on the report.
#' @param dataset_description Short description of the dataset being shown.
#'   This will appear on the report.
#' @param save_directory String specifying directory in which to save the report. Default is
#'   current directory.
#' @param save_filename String specifying filename for the report, excluding any file extension. If
#'   no filename is supplied, one will be automatically generated with the format
#'   `mantis_report_YYMMDD_HHMMSS`.
#' @param show_progress Print progress to console. Default = `TRUE`.
#' @param ... Further parameters to be passed to `rmarkdown::render()`. Cannot include any of
#'   `input`, `output_dir`, `output_file`, `params`, `quiet`.
#'
#' @return A string containing the name and path of the saved report.
#' @details The supplied data frame should contain multiple time series in long format, i.e.:
#'
#' \itemize{
#'   \item one "timepoint" (date) column which will be used for the x-axes. Values should have a regular periodicity, e.g. daily, but do not have to be consecutive.
#'   \item one "item" (character) column containing categorical values identifying distinct time series.
#'   \item one "value" (numeric) column containing the time series values which will be used for the y-axes.
#'   \item Optionally, a "tab" (character) column containing categorical values which will be used to group the time series into different tabs on the report.
#' }
#'
#' The `inputspec` parameter maps the data frame columns to the above.
#' @export
mantis_report <- function(df,
                        inputspec,
                        outputspec = NULL,
                        alertspec = NULL,
                        report_title = "mantis report",
                        dataset_description = "",
                        save_directory = ".",
                        save_filename = NULL,
                        show_progress = TRUE,
                        ...) {

  validate_params_required(match.call())
  # TODO: alert_rules are optional here, but required in mantis_alerts()
  validate_params_type(match.call(),
                       df = df,
                       inputspec = inputspec,
                       outputspec = outputspec,
                       alertspec = alertspec,
                       report_title = report_title,
                       dataset_description = dataset_description,
                       save_directory = save_directory,
                       save_filename = save_filename,
                       show_progress = show_progress
  )

  validate_df_to_inputspec(df, inputspec)
  validate_alert_rules_to_inputspec(alertspec$alert_rules, inputspec)

  timestamp_string <- format(Sys.time(), "%Y%m%d%_%H%M%S")

  if (is.null(outputspec)) {
    outputspec <- outputspec_interactive()
  }

  if (is.null(save_filename)) {
    save_filename <-
      paste0("mantis_", timestamp_string)
  }

  file_and_path <- file.path(save_directory, paste0(save_filename, ".html"))

  # temporarily copy rmd file from package library into save_directory so that
  # intermediate files also get created there.
  # NOTE: explicitly setting intermediates_dir in rmarkdown::render() to
  # save_directory or tempdir() causes duplicate chunk label errors when package
  # is run from inside an rmd/qmd
  temp_dirname <-
    file.path(save_directory, paste0("mantis_temp_", timestamp_string))
  dir.create(temp_dirname)
  file.copy(
    from = system.file(
      "rmd",
      "report-html.Rmd",
      package = utils::packageName(),
      mustWork = TRUE
    ),
    to = temp_dirname,
    overwrite = TRUE
  )

  rmarkdown::render(
    input = file.path(temp_dirname,
                      "report-html.Rmd"),
    output_file = paste0(save_filename, ".html"),
    output_dir = save_directory,
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

  # remove temporary directory created earlier
  unlink(temp_dirname, recursive = TRUE)

  file_and_path
}
