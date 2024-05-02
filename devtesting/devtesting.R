devtools::load_all(".")

data("example_data")

df <- example_data
timepoint_col <- "timepoint"
item_col <- "item"
value_col <- "value"
item_label <- "Item"

table <-
  prepare_table(
    df,
    timepoint_col,
    item_col,
    value_col,
#    item_order = c("sparse_1")
    item_order = NULL
  )

table %>%
  output_table_interactive(
    item_label = "item",
    plot_type = "bar",
    summary_cols = c("last_value", "mean"),
    sync_axis_range = FALSE
  )

prepared_df <- prepare_df(
    df,
    timepoint_col,
    item_col,
    value_col,
#    item_order = c("sparse_1")
    item_order = NULL
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

table <-
  prepare_table(
    df,
    timepoint_col,
    item_col,
    value_col,
    fill_with_zero = TRUE
  )

table %>%
  output_table_interactive(
    item_label = "item",
    plot_type = "bar",
    summary_cols = c("last_value", "mean"),
    sync_axis_range = FALSE
  )

# set timepoint limits
prepare_table(
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

prepare_table(
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


tinduck::tinduck_report(df,
               colspec = list(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value")
               )

tinduck::tinduck_report(df,
                                   colspec = tinduck::colspec(timepoint_col = "timepoint",
                                                    item_col = "item",
                                                    value_col = "value",
                                                    group_col = "group"),
                                   outputspec = outputspec_interactive(plot_type = "line",
                                                           summary_cols = "max_value")
                                   )


data("example_prescription_numbers")
tinduck::tinduck_report(df = example_prescription_numbers,
                        colspec = tinduck::colspec(timepoint_col = "PrescriptionDate",
                                                   item_col = "Antibiotic",
                                                   value_col = "NumberOfPrescriptions",
                                                   group_col = "Location"),
                        outputspec = outputspec_interactive(plot_type = "bar",
                                                summary_cols = c("max_value",
                                                                 "mean_value",
                                                                 "last_timepoint",
                                                                 "last_value",
                                                                 "last_value_nonmissing")
                        ))

tinduck_report(df = example_prescription_numbers,
               colspec = colspec(timepoint_col = "PrescriptionDate",
                                 item_col = "Antibiotic",
                                 value_col = "NumberOfPrescriptions")
               )

tinduck_report(df = example_prescription_numbers,
               colspec = colspec(timepoint_col = "PrescriptionDate",
                                 item_col = "Antibiotic",
                                 value_col = "NumberOfPrescriptions",
                                 group_col = "Location")
)

data("example_data")
tinduck_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value")
)

tinduck_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value",
                              group_col = "family")
               )

tinduck_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value",
                                 group_col = "family"),
               outputspec = outputspec_interactive(plot_type = "bar",
                                       summary_cols = c("max_value",
                                                        "mean_value",
                                                        "last_timepoint",
                                                        "last_value",
                                                        "last_value_nonmissing")
               ))

tinduck_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value"),
               outputspec = outputspec_static_heatmap()
)

tinduck_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value",
                                 group_col = "family"),
               outputspec = outputspec_static_heatmap()
)

tinduck_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value"),
               outputspec = outputspec_static_multipanel()
)

tinduck_report(df = example_data,
               colspec = colspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value"),
               outputspec = outputspec_static_multipanel(sync_axis_range = TRUE)
)

