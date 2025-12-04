# =============================================================================
# EXPORTED FUNCTIONS

# -----------------------------------------------------------------------------
#' Initialise HTML widgets
#'
#' Since the output is being constructed in `results='asis'` chunks, there must
#' also be at least one standard chunk that contains the relevant widgets,
#' otherwise they will fail to render. The `dygraph` also needs to be
#' initialised with the appropriate `plot_type`. This is only needed when
#' creating interactive reports. Make sure you read the vignette:
#' `vignette("bespoke-reports", package = "mantis")` as it contains further
#' important information. Note: The chunk currently appears like a line break
#' when rendered. See <https://github.com/rstudio/rmarkdown/issues/1877> for
#' more info.
#'
#' @param plot_type "`bar`" or "`line`", depending on what will be used in real
#'   tables. Or "none" if just want a reactable widget without dygraphs e.g. for
#'   alerts
#' @return A (mostly) invisible html widget
#'
#' @examples
#' \dontrun{
#' # put this inside its own chunk in the rmd file
#' # it ensures that the dygraphs render when built using `cat()`
#' # set the plot_type to the same plot_type as the real output
#' mantis::bespoke_rmd_initialise_widgets(plot_type = "bar")
#' }
#'
#' @export
bespoke_rmd_initialise_widgets <- function(
  plot_type
) {
  validate_params_required(match.call())
  validate_params_type(
    match.call(),
    plot_type = plot_type
  )

  initialise_widgets(plot_type = plot_type)
}


# -----------------------------------------------------------------------------
#' Dynamically generate mantis output for an rmd chunk
#'
#' Add `mantis` tabs and visualisations to an existing `rmarkdown` report. The
#' function writes directly to the chunk using side-effects, so chunk options
#' must contain `results = 'asis'`. Make sure you read the vignette:
#' `vignette("bespoke-reports", package = "mantis")` as it contains further
#' important information.
#'
#' @param df A data frame containing multiple time series in long format. See
#'   Details.
#' @param inputspec [`inputspec()`] object specifying which columns in the
#'   supplied `df` represent the "timepoint", "item", and "value" for the time
#'   series. A separate tab will be created for each distinct value in the "tab"
#'   column.
#' @param outputspec `outputspec` object specifying the desired format of the
#'   html table(s)/plot(s). If not supplied, default values will be used.
#' @param alertspec [`alertspec()`] object specifying conditions to test and
#'   display
#' @param timepoint_limits Set start and end dates for time period to include.
#'   Defaults to min/max of `timepoint_col`. If the `timepoint_unit` of
#'   `inputspec()` is a "day" or longer, this must be a Date type, otherwise it
#'   should be a POSIXt type.
#' @param fill_with_zero Logical. Replace any missing or `NA` values with 0?
#'   Useful when `value_col` is a record count
#' @param tab_name Character string to appear on the tab label. If omitted or
#'   `NULL`, only the content/child tabs (and not the parent tab) will be
#'   created.
#' @param tab_level integer specifying the nesting level of the tab. If
#'   `tab_name` is specified, a value of 1 generates a tab at rmd level "##",
#'   and any `tab_col` tabs at a level down. If `tab_name` is not specified, any
#'   `tab_col` tabs will be created at rmd level "##".
#' @return (invisibly) the supplied `df`
#'
#' @details You can:
#'
#' * add a single visualisation, with or without creating the container tab, or
#' * add a set of tabs, each based on the same output specification,
#'   with or without creating the parent tab.
#'
#' The supplied data frame should contain multiple time series in long format,
#' i.e.:
#'
#' * one "timepoint" (date/posixt) column which will be used for the x-axes.
#'   Values should follow a regular pattern, e.g. daily or monthly, but do not
#'   have to be consecutive.
#' * one or more "item" (character) columns containing categorical values
#'   identifying distinct time series.
#' * one "value" (numeric) column containing the time series values which will
#'   be used for the y-axes.
#'
#' The `inputspec` parameter maps the data frame columns to the above.
#' Optionally, if there are multiple columns specified in `item_cols`, one of
#' them can be used to group the time series into different child tabs, by using
#' the `tab_col` parameter.
#'
#' @examples
#' \dontrun{
#'
#' # put this inside a chunk in the rmd file,
#' # with chunk option `results = 'asis'`
#' mantis::bespoke_rmd_output(
#'   df = mantis::example_prescription_numbers,
#'   inputspec = mantis::inputspec(
#'     timepoint_col = "PrescriptionDate",
#'     item_cols = c("Location", "Antibiotic"),
#'     value_col = "NumberOfPrescriptions",
#'     tab_col = "Location"
#'   ),
#'   outputspec = mantis::outputspec_interactive(
#'     plot_value_type = "value",
#'     plot_type = "bar",
#'     item_labels = c("Antibiotic" = "Antibiotic name"),
#'     plot_label = "Prescriptions over time",
#'     sync_axis_range = FALSE,
#'     item_order = list("Location" = c("SITE3", "SITE2", "SITE1"))
#'   ),
#'   timepoint_limits = c(NA, Sys.Date()),
#'   fill_with_zero = FALSE,
#'   tab_name = "Group of child tabs",
#'   tab_level = 1
#' )
#' }
#'
#' @seealso [bespoke_rmd_initialise_widgets()]
#' @export
bespoke_rmd_output <- function(
  df,
  inputspec,
  outputspec,
  alertspec = NULL,
  timepoint_limits = c(NA, NA),
  fill_with_zero = FALSE,
  tab_name = NULL,
  tab_level = 1
) {
  validate_params_required(match.call())
  validate_params_type(
    match.call(),
    df = df,
    inputspec = inputspec,
    outputspec = outputspec,
    alertspec = alertspec,
    timepoint_limits = timepoint_limits,
    fill_with_zero = fill_with_zero,
    tab_name = tab_name,
    tab_level = tab_level
  )

  validate_df_to_inputspec(df, inputspec)
  validate_alert_rules_to_inputspec(alertspec$alert_rules, inputspec)

  construct_rmd_tab(
    df = df,
    inputspec = inputspec,
    outputspec = outputspec,
    alertspec = alertspec,
    timepoint_limits = timepoint_limits,
    fill_with_zero = fill_with_zero,
    tab_name = tab_name,
    tab_level = tab_level
  )

  invisible(df)
}


# -----------------------------------------------------------------------------
#' Dynamically generate a table containing alert results for an rmd chunk
#'
#' Add `mantis` alert results to an existing `rmarkdown` report. The function
#' writes directly to the chunk using side-effects, so chunk options must
#' contain `results = 'asis'`.
#'
#' @param df A data frame containing multiple time series in long format. See
#'   Details.
#' @param inputspec [`inputspec()`] object specifying which columns in the
#'   supplied `df` represent the "timepoint", "item", and "value" for the time
#'   series. Any "tab" column specification will be ignored.
#' @param alert_rules [`alert_rules()`] object specifying conditions to test
#' @param filter_results only return rows where the alert result is in this
#'   vector of values. Alert results can be "PASS", "FAIL", or "NA".
#' @param timepoint_limits Set start and end dates for time period to include.
#'   Defaults to min/max of `timepoint_col`
#' @param fill_with_zero Logical. Replace any missing or `NA` values with 0?
#'   Useful when `value_col` is a record count
#' @param tab_name Character string to appear on the tab label. If omitted or
#'   `NULL`, only the content (and not the parent tab) will be created.
#' @param tab_level integer specifying the nesting level of the tab. If
#'   `tab_name` is specified, a value of 1 generates a tab at rmd level "##". If
#'   `tab_name` is not specified, this is ignored.
#' @return (invisibly) the supplied `df`
#'
#' @examples
#' \dontrun{
#'
#' # put this inside a chunk in the rmd file,
#' # with chunk option `results = 'asis'`
#' mantis::bespoke_rmd_alert_results(
#'   df = mantis::example_prescription_numbers,
#'   inputspec = mantis::inputspec(
#'     timepoint_col = "PrescriptionDate",
#'     item_cols = c("Location", "Antibiotic"),
#'     value_col = "NumberOfPrescriptions",
#'     tab_col = "Location"
#'   ),
#'   alert_rules = alert_rules(
#'     alert_missing(extent_type = "any", extent_value = 1),
#'     alert_equals(extent_type = "all", rule_value = 0)
#'   ),
#'   filter_results = c("FAIL", "NA"),
#'   fill_with_zero = FALSE,
#'   tab_name = "Failed alerts",
#'   tab_level = 1
#' )
#' }
#'
#' @export
bespoke_rmd_alert_results <- function(
  df,
  inputspec,
  alert_rules,
  filter_results = c("PASS", "FAIL", "NA"),
  timepoint_limits = c(NA, NA),
  fill_with_zero = FALSE,
  tab_name = NULL,
  tab_level = 1
) {
  validate_params_required(match.call())
  validate_params_type(
    match.call(),
    df = df,
    inputspec = inputspec,
    alert_rules = alert_rules,
    filter_results = filter_results,
    timepoint_limits = timepoint_limits,
    fill_with_zero = fill_with_zero,
    tab_name = tab_name,
    tab_level = tab_level
  )

  alert_results <-
    prepare_and_run_alerts(
      df = df,
      inputspec = inputspec,
      alert_rules = alert_rules,
      filter_results = filter_results,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero
    )

  # create parent tab if specified
  if (!is.null(tab_name)) {
    construct_tab_label(
      tab_name = tab_name,
      tab_level = tab_level
    )
  }

  construct_alert_results_content(
    alert_results = alert_results,
    inputspec = inputspec
  )

  invisible(df)
}


# =============================================================================
# INTERNAL FUNCTIONS

# -----------------------------------------------------------------------------
#' Initialise HTML widgets (internal)
#'
#' If the output is being constructed in `results='asis'` chunks, there must
#' also be at least one standard chunk that contains the relevant widgets,
#' otherwise they won't render. The `dygraph` also needs to be initialised with
#' the appropriate `plot_type`.
#' @param plot_type "`bar`" or "`line`", depending on what will be used in real
#'   tables. "none" if just want a reactable widget without dygraphs
#'
#' @return A (mostly) invisible html widget
#' @noRd
initialise_widgets <- function(
  plot_type = "none"
) {
  # https://stackoverflow.com/questions/63534247/recommended-way-to-initialize-js-renderer-in-asis-r-markdown-chunk
  # https://github.com/rstudio/rmarkdown/issues/1877
  # Currently appears like a line break when rendered. Could try harder to make
  # it invisible but people can always put it at the end of the file if required
  dummy_df <- data.frame(a = as.Date("2023-01-01"), b = "item", c = 1)

  if (plot_type == "none") {
    reactable::reactable(dummy_df, height = 0, bordered = FALSE)
  } else {
    inputspec <- inputspec(
      timepoint_col = "a",
      item_cols = "b",
      value_col = "c"
    )

    prepare_df(dummy_df, inputspec = inputspec) |>
      output_table_interactive(
        inputspec = inputspec,
        plot_type = plot_type,
        summary_cols = NULL,
        height = 0,
        bordered = FALSE
      )
  }
}


# -----------------------------------------------------------------------------
#' Dynamically generate tabs for an rmd chunk (internal)
#'
#' A single function to create both single tab items and groups Prepare the df
#' and alerts only once at the start, rather than for each tab item individually
#' Allows you to flag alerts at the parent tab level.
#'
#' @param df Data frame containing time series in long format
#' @param inputspec Specification of data in `df`
#' @param outputspec Specification for display of tab contents
#' @param alertspec Specification for alerting conditions
#' @param timepoint_limits Set start and end dates for time period to include.
#'   Defaults to min/max of `timepoint_col`
#' @param fill_with_zero Logical. Replace any missing or `NA` values with 0?
#'   Useful when `value_col` is a record count
#' @param tab_name Character string to appear on the tab label. If omitted or
#'   `NULL`, only the content/child tabs (and not the parent tab) will be
#'   created.
#' @param tab_level integer specifying the nesting level of the tab. If
#'   `tab_name` is specified, a value of 1 generates a tab at rmd level "##",
#'   and any `tab_col` tabs at a level down. If `tab_name` is not specified, any
#'   `tab_col` tabs will be created at rmd level "##".
#'
#' @return (invisibly) the supplied `df`
#' @noRd
construct_rmd_tab <- function(
  df,
  inputspec,
  outputspec = NULL,
  alertspec = NULL,
  timepoint_limits = c(NA, NA),
  fill_with_zero = FALSE,
  tab_name = NULL,
  tab_level = 1
) {
  prepared_df <-
    prepare_df(
      df,
      inputspec = inputspec,
      timepoint_limits = timepoint_limits,
      fill_with_zero = fill_with_zero,
      item_order = outputspec$item_order
    )

  alert_results <- get_alert_results_if_specified(
    alertspec = alertspec,
    prepared_df = prepared_df,
    inputspec = inputspec
  )

  # if alert tab is to be created, main contents must also be in a tab
  if (
    !is.null(alertspec$show_tab_results) &&
      is.null(tab_name) &&
      is.null(inputspec$tab_col)
  ) {
    tab_name <- "Output"
  }

  # create parent tab if specified
  if (!is.null(tab_name)) {
    construct_tab_label(
      tab_name = tab_name,
      tab_level = tab_level,
      has_child_tabs = TRUE,
      alert = any(alert_results$alert_result %in% c("FAIL"))
    )

    tab_col_level <- tab_level + 1
  } else {
    tab_col_level <- tab_level
  }

  # if no tab_col specified, print entire df contents
  if (is.null(inputspec$tab_col)) {
    construct_tab_content(
      prepared_df_subset = prepared_df,
      inputspec = inputspec,
      outputspec = outputspec,
      alert_results_subset = alert_results
    )
  } else {
    # create tab group
    tab_col_names <- unique(
      prepared_df[item_cols_prefix(inputspec$tab_col)] |>
        dplyr::pull()
    )

    for (i in seq_along(tab_col_names)) {
      prepared_df_subset <-
        prepared_df |>
        dplyr::filter(
          .data[[item_cols_prefix(inputspec$tab_col)]] == tab_col_names[i]
        )

      if (is.null(alert_results)) {
        alert_results_subset <- NULL
      } else {
        alert_results_subset <-
          alert_results |>
          dplyr::filter(
            .data[[item_cols_prefix(inputspec$tab_col)]] == tab_col_names[i]
          )
      }

      construct_tab_label(
        tab_name = tab_col_names[i],
        tab_level = tab_col_level,
        alert = any(alert_results_subset$alert_result %in% c("FAIL"))
      )

      construct_tab_content(
        prepared_df_subset = prepared_df_subset,
        inputspec = inputspec,
        outputspec = outputspec,
        alert_results_subset = alert_results_subset
      )
    }
  }

  # create alert tab
  if (!is.null(alertspec$show_tab_results)) {
    # should be at same level as any tab_col tabs
    if (!is.null(inputspec$tab_col)) {
      tab_alert_level <- tab_col_level
    } else {
      tab_alert_level <- tab_level
    }

    construct_tab_label(
      tab_name = "Alert results",
      tab_level = tab_alert_level,
      has_child_tabs = FALSE
    )

    construct_alert_results_content(
      alert_results = alert_results,
      inputspec = inputspec,
      filter_results = alertspec$show_tab_results
    )
  }

  invisible(df)
}


# -----------------------------------------------------------------------------
#' Get alert_results df if alertspec is supplied
#'
#' @param alertspec Specification for alerting conditions
#' @param prepared_df prepared_df
#' @param inputspec Specification of data in `df`
#'
#' @return dataframe
#' @noRd
get_alert_results_if_specified <- function(
  alertspec,
  prepared_df,
  inputspec
) {
  if (!is.null(alertspec)) {
    alert_results <- run_alerts(
      prepared_df = prepared_df,
      inputspec = inputspec,
      alert_rules = alertspec$alert_rules,
    )
  } else {
    alert_results <- NULL
  }

  alert_results
}


# -----------------------------------------------------------------------------
#' Generate just the contents of a tab using side-effects
#'
#' @param prepared_df_subset prepared_df filtered for the tab
#' @param inputspec Specification of data in `df`
#' @param outputspec Specification for display of tab contents
#' @param alert_results_subset alert_results filtered for the tab
#' @return (invisibly) the supplied `df`
#'
#' @noRd
construct_tab_content <- function(
  prepared_df_subset,
  inputspec,
  outputspec,
  alert_results_subset = NULL
) {
  if (is_outputspec_static_heatmap(outputspec)) {
    plot_heatmap_static(
      prepared_df = prepared_df_subset,
      inputspec = inputspec,
      fill_colour = outputspec$fill_colour,
      y_label = outputspec$y_label
    ) |>
      print()
  } else if (is_outputspec_static_multipanel(outputspec)) {
    plot_multipanel_static(
      prepared_df = prepared_df_subset,
      inputspec = inputspec,
      sync_axis_range = outputspec$sync_axis_range,
      y_label = outputspec$y_label
    ) |>
      print()
  } else if (is_outputspec_interactive(outputspec)) {
    p <-
      output_table_interactive(
        prepared_df = prepared_df_subset,
        inputspec = inputspec,
        plot_value_type = outputspec$plot_value_type,
        item_labels = outputspec$item_labels,
        plot_label = outputspec$plot_label,
        summary_cols = outputspec$summary_cols,
        plot_type = outputspec$plot_type,
        sync_axis_range = outputspec$sync_axis_range,
        alert_results = alert_results_subset,
        sort_by = outputspec$sort_by
      )
    # NOTE: a regular print() doesn't render the widget
    cat(knitr::knit_print(p))
  }

  cat("\n")

  invisible(prepared_df_subset)
}


# -----------------------------------------------------------------------------
#' Generate an interactive table containing alert results using side-effects
#'
#' @param alert_results data frame returned from `run_alerts`
#' @param inputspec Specification of data in original `df`
#' @param filter_results only return rows where the alert result is in this
#'   vector of values. Alert results can be "PASS", "FAIL", or "NA".
#' @return (invisibly) the supplied `df`
#' @noRd
construct_alert_results_content <- function(
  alert_results,
  inputspec,
  filter_results = c("PASS", "FAIL", "NA")
) {
  alert_description <- alert_result <- NULL

  p <-
    alert_results |>
    dplyr::rename_with(
      .fn = item_cols_unprefix,
      .cols = dplyr::all_of(item_cols_prefix(inputspec$item_cols))
    ) |>
    dplyr::filter(alert_result %in% filter_results) |>
    dplyr::select(
      dplyr::all_of(inputspec$item_cols),
      "Rule" = alert_description,
      "Result" = alert_result
    ) |>
    reactable::reactable(filterable = TRUE, pagination = FALSE)

  cat(knitr::knit_print(p))

  cat("\n")

  invisible(alert_results)
}


# -----------------------------------------------------------------------------
#' Create markdown for tab label
#'
#' @param tab_name string label for the tab
#' @param tab_level `tab_level` = 1 corresponds to ###
#' @param has_child_tabs will there be more tabs underneath
#' @param alert set to `TRUE` to append an alert icon
#'
#' @noRd
construct_tab_label <- function(
  tab_name,
  tab_level,
  has_child_tabs = FALSE,
  alert = FALSE
) {
  if (!is.null(tab_name)) {
    cat(
      "\n",
      paste0(rep("#", tab_level + 1), collapse = ""),
      " ",
      tab_name,
      ifelse(alert, " <b>!</b>", ""),
      ifelse(has_child_tabs, " {.tabset}", ""),
      "\n",
      sep = ""
    )
  }
}


# -----------------------------------------------------------------------------
#' Calculate appropriate fig.height for the chunks
#'
#' Static plots need this setting explicitly otherwise plots with lots of items
#' look squished. Interactive plots flex automatically.
#'
#' If NULL is returned, the rmd should use the default fig.height.
#'
#' @param df data frame
#' @param inputspec `inputspec()` object
#' @param outputspec `outputspec()` object
#'
#' @return numeric height in inches or NULL
#' @noRd
rmd_fig_height <- function(
  df,
  inputspec,
  outputspec
) {
  rows <- NULL

  fig_height <- NULL
  if (is_outputspec_static(outputspec) && nrow(df) > 0) {
    maxrows <-
      df |>
      dplyr::select(
        dplyr::all_of(inputspec$item_cols),
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

    fig_height <- maxrows *
      dplyr::case_when(
        is_outputspec_static_heatmap(outputspec) ~ 0.6,
        is_outputspec_static_multipanel(outputspec) ~ 0.8,
        TRUE ~ 0.8
      )
  }

  fig_height
}
