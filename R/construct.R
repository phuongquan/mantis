#----------------------------------------------------------------------
# EXPORTED FUNCTIONS

#' Initialise HTML widgets
#'
#' If the output is being constructed in 'asis' chunks, there must also be at least one standard chunk that
#' contains the relevant widgets, otherwise they won't render. dyGraph also needs to be rendered with appropriate plot_type
#' @param plot_type "`bar`" or "`line`", depending on what will be used in real tables.
#'
#' @return A (mostly) invisible html widget
#' @export
bespoke_rmd_initialise_widgets <- function(plot_type){
  validate_params_required(match.call())
  validate_params_type(match.call(),
                       plot_type = plot_type
  )

  initialise_widgets(plot_type = plot_type)
}

#' Dynamically generate a single tab for an rmd chunk
#'
#' Chunk options must contain `results = 'asis'`.
#' Function writes directly to the chunk using side-effects
#'
#' @param df A data frame containing multiple time series in long format. See Details.
#' @param inputspec [`inputspec()`] object specifying which columns in the supplied `df` represent the
#'   "timepoint", "item", and "value" for the time series. If a "tab" column is specified, it will be ignored.
#' @param outputspec [`outputspec()`] object specifying the desired format of the html table(s). If
#'   not supplied, default values will be used.
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param tab_name Character string to appear on parent tab
#' @param tab_level Child level for tab. Value of 1 creates a tab with rmd level "##".
#'
#' @return (invisibly) the supplied df
#' @details The supplied data frame should contain multiple time series in long format, i.e.:
#'
#' \itemize{
#'   \item one "timepoint" (date) column which will be used for the x-axes. This currently must be at a daily granularity, but values do not have to be consecutive.
#'   \item one "item" (character) column containing categorical values identifying distinct time series.
#'   \item one "value" (numeric) column containing the time series values which will be used for the y-axes.
#'   \item Optionally, a "tab" (character) column containing categorical values which will be used to group the time series into different tabs on the report.
#' }
#'
#' The `inputspec` parameter maps the data frame columns to the above.
#' @export
bespoke_rmd_tab_item <- function(df,
                                 inputspec,
                                 outputspec,
                                 alert_rules = NULL,
                                 timepoint_limits = c(NA, NA),
                                 fill_with_zero = FALSE,
                                 tab_name = NULL,
                                 tab_level = 1) {

  validate_params_required(match.call())
  # TODO: alert_rules are optional here, but required in mantis_alerts()
  validate_params_type(match.call(),
                       df = df,
                       inputspec = inputspec,
                       outputspec = outputspec,
                       alert_rules = alert_rules,
                       timepoint_limits = timepoint_limits,
                       fill_with_zero = fill_with_zero,
                       tab_name = tab_name,
                       tab_level = tab_level
  )

  # everything will appear in the one tab
  inputspec$tab_col <- NULL
  validate_df_to_inputspec(df, inputspec)

  construct_rmd_tab_item(
    df = df,
    inputspec = inputspec,
    outputspec = outputspec,
    alert_rules = alert_rules,
    timepoint_limits = timepoint_limits,
    fill_with_zero = fill_with_zero,
    tab_name = tab_name,
    tab_level = tab_level
  )
}

#' Dynamically generate a group of tabs for an rmd chunk
#'
#' Chunk options must contain `results = 'asis'`.
#' Function writes directly to the chunk using side-effects
#'
#' @param df Data frame containing time series in long format
#' @param inputspec [`inputspec()`] object specifying which columns in the supplied `df` represent the
#'   "timepoint", "item", "value" and "tab" for the time series. A separate tab
#'   will be created for each distinct value in the "tab" column.
#' @param outputspec [`outputspec()`] object specifying the desired format of the html table(s). If
#'   not supplied, default values will be used.
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param tab_order Optional vector containing values from tab_col in desired order of display
#' @param tab_group_name Character string to appear on parent tab
#' @param tab_group_level integer specifying the nesting level of the parent tab. Value of 1 equates to rmd level "##".
#'
#' @return (invisibly) the supplied df
#' @details The supplied data frame should contain multiple time series in long format, i.e.:
#'
#' \itemize{
#'   \item one "timepoint" (date) column which will be used for the x-axes. This currently must be at a daily granularity, but values do not have to be consecutive.
#'   \item one "item" (character) column containing categorical values identifying distinct time series.
#'   \item one "value" (numeric) column containing the time series values which will be used for the y-axes.
#'   \item Optionally, a "tab" (character) column containing categorical values which will be used to group the time series into different tabs on the report.
#' }
#'
#' The `inputspec` parameter maps the data frame columns to the above.
#' @export
bespoke_rmd_tab_group <- function(df,
                                  inputspec,
                                  outputspec,
                                  alert_rules = NULL,
                                  timepoint_limits = c(NA, NA),
                                  fill_with_zero = FALSE,
                                  tab_order = NULL,
                                  tab_group_name = NULL,
                                  tab_group_level = 1) {

  validate_params_required(match.call())
  # TODO: alert_rules are optional here, but required in mantis_alerts()
  validate_params_type(match.call(),
                       df = df,
                       inputspec = inputspec,
                       outputspec = outputspec,
                       alert_rules = alert_rules,
                       timepoint_limits = timepoint_limits,
                       fill_with_zero = fill_with_zero,
                       tab_order = tab_order,
                       tab_group_name = tab_group_name,
                       tab_group_level = tab_group_level
  )
  #TODO: tab_col is required here but optional everywhere else
  if (is.null(inputspec$tab_col)) {
    stop_custom(
      .subclass = "invalid_param_type",
      message = paste0(
        "Invalid argument(s) supplied.\n",
        paste("tab_col cannot be NULL in the inputspec", collapse = "\n")
      )
    )
  }

  validate_df_to_inputspec(df, inputspec)

  construct_rmd_tab_group(
    df = df,
    inputspec = inputspec,
    outputspec = outputspec,
    alert_rules = alert_rules,
    timepoint_limits = timepoint_limits,
    fill_with_zero = fill_with_zero,
    tab_order = tab_order,
    tab_group_name = tab_group_name,
    tab_group_level = tab_group_level
  )
}


#----------------------------------------------------------------------
# INTERNAL FUNCTIONS

#' Initialise HTML widgets (internal)
#'
#' If the output is being constructed in 'asis' chunks, there must also be at least one standard chunk that
#' contains the relevant widgets, otherwise they won't render. dyGraph also needs to be rendered with appropriate plot_type
#' @param plot_type "`bar`" or "`line`", depending on what will be used in real tables.
#'
#' @return A (mostly) invisible html widget
#' @noRd
initialise_widgets <- function(plot_type){
  # https://stackoverflow.com/questions/63534247/recommended-way-to-initialize-js-renderer-in-asis-r-markdown-chunk
  # Currently appears like a line break when rendered. Could try harder to make it invisible but
  # people can always put it at the end of the file if required
  dummy_df <- data.frame(a = as.Date("2023-01-01"), b = "item", c = 1)

  prepare_df(
    dummy_df,
    inputspec = inputspec(
      timepoint_col = "a",
      item_col = "b",
      value_col = "c"
    )
  ) |>
    output_table_interactive(
      plot_type = plot_type,
      summary_cols = "",
      height = 0,
      bordered = FALSE
    )
}

#' Dynamically generate a single tab for an rmd chunk (internal)
#'
#' Don't need to repeat validation done previously
#'
#' Chunk options must contain `results = 'asis'`.
#' Function writes directly to the chunk using side-effects
#'
#' @param df Data frame containing time series in long format
#' @param inputspec Specification of data in df
#' @param outputspec Specification for display of tab contents
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param tab_name Character string to appear on parent tab
#' @param tab_level Child level for tab. Value of 1 creates a tab with rmd level "##".
#'
#' @return (invisibly) the supplied df
#' @noRd
construct_rmd_tab_item <- function(df,
                                   inputspec,
                                   outputspec,
                                   alert_rules = NULL,
                                   timepoint_limits = c(NA, NA),
                                   fill_with_zero = FALSE,
                                   tab_name = NULL,
                                   tab_level = 1) {

  prepared_df <-
    prepare_df(
      df,
      inputspec = inputspec,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero,
      item_order = outputspec$item_order
    )

  if (!is.null(alert_rules)) {
    alert_results <- run_alerts(prepared_df = prepared_df,
                                alert_rules = alert_rules)
  } else {
    alert_results <- NULL
  }

  construct_tab_label(tab_name = tab_name,
                      tab_level = tab_level,
                      alert = any(alert_results$alert_result %in% c("FAIL")))

  if (is_outputspec_static_heatmap(outputspec)) {
      plot_heatmap_static(prepared_df = prepared_df,
                          fill_colour = outputspec$fill_colour,
                          y_label = outputspec$y_label) |>
      print()
  } else if (is_outputspec_static_multipanel(outputspec)) {
      plot_multipanel_static(prepared_df = prepared_df,
                             sync_axis_range = outputspec$sync_axis_range,
                             y_label = outputspec$y_label) |>
      print()
  } else if (is_outputspec_interactive(outputspec)) {
    p <-
      output_table_interactive(
        prepared_df,
        plot_value_type = outputspec$plot_value_type,
        item_label = outputspec$item_label,
        plot_label = outputspec$plot_label,
        summary_cols = outputspec$summary_cols,
        plot_type = outputspec$plot_type,
        sync_axis_range = outputspec$sync_axis_range,
        alert_results = alert_results,
        sort_by = outputspec$sort_by
      )
    # NOTE: a regular print() doesn't render the widget
    cat(knitr::knit_print(p))
  }

  cat("\n")

  invisible(df)
}

#' Dynamically generate a group of tabs for an rmd chunk (internal)
#'
#' Don't need to repeat validation done previously
#'
#' Chunk options must contain `results = 'asis'`.
#' Function writes directly to the chunk using side-effects
#'
#' @param df Data frame containing time series in long format
#' @param timepoint_col Name of column to be used for x-axes
#' @param inputspec Specification of data in df
#' @param outputspec Specification for display of tab contents
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#' @param timepoint_limits Set start and end dates for time period to include. Defaults to min/max of timepoint_col
#' @param fill_with_zero Replace any missing or NA values with 0? Useful when value_col is a record count
#' @param tab_order Optional vector containing values from tab_col in desired order of display
#' @param tab_group_name Character string to appear on parent tab
#' @param tab_group_level integer specifying the nesting level of the parent tab. Value of 1 equates to rmd level "##".
#'
#' @return (invisibly) the supplied df
#' @noRd
#' @importFrom dplyr .data
construct_rmd_tab_group <- function(df,
                                    inputspec,
                                    outputspec = NULL,
                                    alert_rules = NULL,
                                    timepoint_limits = c(NA, NA),
                                    fill_with_zero = FALSE,
                                    tab_order = NULL,
                                    tab_group_name = NULL,
                                    tab_group_level = 1) {

  tab_names <- unique(df[inputspec$tab_col] |>
                        dplyr::pull())

  if (!is.null(tab_order)) {
    tab_names <-
      tab_names[order(match(tab_names, tab_order))]
  }

  construct_tab_label(tab_name = tab_group_name,
                      tab_level = tab_group_level,
                      has_child_tabs = TRUE)

  for (i in seq_along(tab_names)) {
    dftab <-
      df |> dplyr::filter(.data[[inputspec$tab_col]] == tab_names[i])

    construct_rmd_tab_item(
      df = dftab,
      inputspec = inputspec,
      outputspec = outputspec,
      alert_rules = alert_rules,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero,
      tab_name = tab_names[i],
      tab_level = tab_group_level + 1
    )
  }

  invisible(df)
}

#' Create markdown for tab label
#'
#' @param tab_name string label for the tab
#' @param tab_level tab_level = 1 corresponds to ###
#' @param has_child_tabs will there be more tabs underneath
#' @param alert set to TRUE to append an alert icon
#'
#' @return markdown-formatted string
#' @noRd
construct_tab_label <- function(tab_name, tab_level, has_child_tabs = FALSE, alert = FALSE){
  if (!is.null(tab_name)) {
    cat("\n",
        paste0(rep("#", tab_level + 1), collapse = ""),
        " ", tab_name,
        ifelse(alert," \u2757\ufe0e", ""),
        ifelse(has_child_tabs," {.tabset}", ""),
        "\n",
        sep = "")
  }
}

#' Calculate appropriate fig.height for the chunks
#'
#' Static plots need this setting explicitly otherwise plots with lots of items look squished.
#' Interactive plots flex automatically.
#'
#' If NULL is returned, the rmd should use the default fig.height.
#'
#' @param df data frame
#' @param inputspec `inputspec()` object
#' @param outputspec `outputspec()` object
#'
#' @return height in inches or NULL
#' @noRd
rmd_fig_height <- function(df, inputspec, outputspec){

  rows <- NULL

  fig_height <- NULL
  if (is_outputspec_static(outputspec) && nrow(df) > 0){
    maxrows <-
      df |>
      dplyr::select(
        dplyr::all_of(inputspec$item_col),
        dplyr::any_of(inputspec$tab_col)
      ) |>
      dplyr::group_by(dplyr::pick(dplyr::any_of(
        inputspec$tab_col
      ))) |>
      dplyr::distinct() |>
      dplyr::summarise(rows = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::summarise(maxrows = max(rows)) |>
      dplyr::pull()

    fig_height <- maxrows*dplyr::case_when(is_outputspec_static_heatmap(outputspec) ~ 0.6,
                                           is_outputspec_static_multipanel(outputspec) ~ 0.8,
                                           TRUE ~ 0.8)
  }

  fig_height
}
