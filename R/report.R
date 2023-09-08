#' Save a single table to an html file
#'
#' @param df
#' @param colspec list
#' @param page_title title
#' @param save_directory where to save the file to
#' @param save_filename name for the file
#' @param show_progress noisy or quiet
#' @param ... further params for rmarkdown::render()
#'
#' @return path to saved file
#' @export
export_table_html <- function(df,
                        colspec,
                        outputspec = NULL,
                        page_title = "tinduck single table",
                        save_directory = ".",
                        save_filename = NULL,
                        show_progress = TRUE,
                        ...) {

  timestamp_string <- format(Sys.time(), "%Y%m%d%_%H%M%S")

  if (is.null(outputspec)) {
    outputspec <- outputspec()
  }

  if (is.null(save_filename)) {
    save_filename <-
      paste0("tinduck_", timestamp_string)
  }

  file_and_path <- file.path(save_directory, paste0(save_filename, ".html"))

  # temporarily copy rmd file from package library into save_directory so that
  # intermediate files also get created there.
  # NOTE: explicitly setting intermediates_dir in rmarkdown::render() to
  # save_directory or tempdir() causes duplicate chunk label errors when package
  # is run from inside an rmd/qmd
  temp_dirname <-
    file.path(save_directory, paste0("tinduck_temp_", timestamp_string))
  dir.create(temp_dirname)
  file.copy(
    from = system.file(
      "rmd",
      "single_table.Rmd",
      package = utils::packageName(),
      mustWork = TRUE
    ),
    to = temp_dirname,
    overwrite = TRUE
  )

  rmarkdown::render(
    input = file.path(temp_dirname,
                      "single_table.Rmd"),
    output_file = paste0(save_filename, ".html"),
    output_dir = save_directory,
    params = list(
      df = df,
      colspec = colspec,
      outputspec = outputspec,
      page_title = page_title
    ),
    quiet = !show_progress,
    ...
  )

  # remove temporary directory created earlier
  unlink(temp_dirname, recursive = TRUE)

  file_and_path
}

