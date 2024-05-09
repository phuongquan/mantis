
install.packages(
  c(
    "rmarkdown",
    "knitr",
    "magrittr",
    "reactable",
    "dplyr",
    "tidyr",
    "dygraphs",
    "xts",
    "ggplot2",
    "scales",
    "testthat",
    "roxygen2"
  )
)


devtools::load_all(".")

data("example_data")

df <- example_data
timepoint_col <- "timepoint"
item_col <- "item"
value_col <- "value"
item_label <- "Item"


prepared_df <- prepare_df(
    df,
    timepoint_col,
    item_col,
    value_col,
#    item_order = c("sparse_1")
    item_order = NULL
)
output_table_interactive(
  prepared_df,
  item_label = "item",
  plot_type = "bar",
  summary_cols = c("last_value", "mean"),
  sync_axis_range = FALSE
)

plot_heatmap_static(
  prepared_df,
  outputspec = outputspec_static_heatmap(
    fill_colour = "red",
    y_label = "Value")
)

plot_heatmap_static(
  prepared_df
)

plot_multipanel_static(
  prepared_df
)

# set timepoint limits
prepare_df(
  df,
  timepoint_col,
  item_col,
  value_col,
  timepoint_limits = c(as.Date("2022-06-01"), NA)
) %>%
output_table_interactive(
  item_label = "item",
  plot_type = "bar",
  summary_cols = c("last_value"),
  sync_axis_range = FALSE
)

prepare_df(
  df %>% dplyr::filter(startsWith(item, "sparse")),
  timepoint_col,
  item_col,
  value_col,
  timepoint_limits = c(min(df$timepoint), max(df$timepoint))
) %>%
  output_table_interactive(
    item_label = "item",
    plot_type = "bar",
    summary_cols = c("last_value"),
    sync_axis_range = FALSE
  )


mantis::mantis_report(df,
               colspec = list(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value")
               )

mantis::mantis_report(df,
                                   colspec = mantis::colspec(timepoint_col = "timepoint",
                                                    item_col = "item",
                                                    value_col = "value",
                                                    tab_col = "tab"),
                                   outputspec = outputspec_interactive(plot_type = "line",
                                                           summary_cols = "max_value")
                                   )


data("example_prescription_numbers")
mantis::mantis_report(df = example_prescription_numbers,
                        colspec = mantis::colspec(timepoint_col = "PrescriptionDate",
                                                   item_col = "Antibiotic",
                                                   value_col = "NumberOfPrescriptions",
                                                   tab_col = "Location"),
                        outputspec = outputspec_interactive(plot_type = "bar",
                                                summary_cols = c("max_value",
                                                                 "mean_value",
                                                                 "last_timepoint",
                                                                 "last_value",
                                                                 "last_value_nonmissing")
                        ))

mantis_report(df = example_prescription_numbers,
               colspec = colspec(timepoint_col = "PrescriptionDate",
                                 item_col = "Antibiotic",
                                 value_col = "NumberOfPrescriptions")
               )

mantis_report(df = example_prescription_numbers,
               colspec = colspec(timepoint_col = "PrescriptionDate",
                                 item_col = "Antibiotic",
                                 value_col = "NumberOfPrescriptions",
                                 tab_col = "Location")
)

data("example_data")
mantis_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value")
)

mantis_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value",
                              tab_col = "tab")
               )

mantis_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value",
                                 tab_col = "tab"),
               outputspec = outputspec_interactive(plot_type = "bar",
                                       summary_cols = c("max_value",
                                                        "mean_value",
                                                        "last_timepoint",
                                                        "last_value",
                                                        "last_value_nonmissing")
               ))

mantis_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value"),
               outputspec = outputspec_static_heatmap()
)

mantis_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value",
                                 tab_col = "tab"),
              dataset_description = "examples",
              outputspec = outputspec_static_heatmap()
)

mantis_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value"),
               outputspec = outputspec_static_multipanel()
)

mantis_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value"),
               outputspec = outputspec_static_multipanel(sync_axis_range = TRUE)
)

mantis_report(df = example_data,
              colspec = colspec(timepoint_col = "timepoint",
                                item_col = "item",
                                value_col = "value"),
              outputspec = outputspec_static_multipanel(),
              report_title = "example_data",
              dataset_description = "examples"
)

monthly_data <- example_data %>%
  dplyr::mutate(month = as.Date(format(timepoint, format = "%Y-%m-01"))) %>%
  dplyr::group_by(month, item, tab) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE),
                   .groups = "drop")


mantis_report(df = monthly_data,
              colspec = colspec(timepoint_col = "month",
                                item_col = "item",
                                value_col = "value"),
              outputspec = outputspec_static_multipanel()
)

## alerting
data("example_data")

df <- example_data
timepoint_col <- "timepoint"
item_col <- "item"
value_col <- "value"
item_label <- "Item"


prepared_df <- prepare_df(
  df,
  timepoint_col,
  item_col,
  value_col,
  #    item_order = c("sparse_1")
  item_order = NULL
)

alert_rules <- alert_rules(alert_missing(missing_extent_type = "all",
                                         items = "ALL"))

run_alerts(prepared_df,
           alert_rules)
