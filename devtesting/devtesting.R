
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
    "roxygen2",
    "purrr"
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


prepared_df <- prepare_df(
  df,
  timepoint_col,
  item_col,
  value_col,
  #    item_order = c("sparse_1")
  item_order = NULL
)

allitems <- unique(prepared_df$item)

alert_rules <- alert_rules(alert_missing(extent_type = "all",
                                         items = "ALL"),
                           alert_missing(extent_type = "last",
                                         extent_value = 5,
                                         items = allitems[grep("norm", allitems)])
                           )

run_alerts(prepared_df,
           alert_rules)

library(dplyr)
library(mantis)

pop_monitor <- readRDS("./devtesting/pop_monitor.Rds")

pop_monitor %>% distinct(stat_name)

prepared_df <-
  pop_monitor %>%
  filter(stat_name == "Max SpecimenDate per Lab",
         monitor_point_subname == "sgss_cdr") %>%
  mutate(run_date = as.Date(monitor_datetime)) %>%
  # if multiple results in same day then keep the latest
  group_by(run_date, monitor_point_name, monitor_point_subname, stat_name) %>%
  filter(monitor_datetime == max(monitor_datetime)) %>%
  ungroup() %>%
  mutate(days_since_monitor_date = as.integer(
    run_date - as.Date(lubridate::parse_date_time(value, orders = c("%Y%m%d", "%Y-%m-%d"))))) %>%
  # filter(groupby_value == "BRIGHTON MICROBIOLOGY LABORATORY") %>%
  # arrange(monitor_datetime) %>%
  prepare_df(
    timepoint_col = "run_date",
    item_col = "groupby_value",
    value_col = "days_since_monitor_date",
  )

alert_rules <- alert_rules(alert_missing(extent_type = "all",
                                         items = "ALL"),
                           alert_missing(extent_type = "last",
                                         extent_value = 14)
)

alert_rules <- alert_rules(alert_missing(extent_type = "last",
                                         extent_value = 14),
                           alert_gt(extent_type = "last",
                                    extent_value = 14,
                                    rule_value = 10)
)

alert_results <-
  run_alerts(prepared_df,
             alert_rules)

alert_results %>%
  filter(if_any(.cols = everything(), .fns = ~ . == TRUE))

df <-
  pop_monitor %>%
  filter(stat_name == "STREPTOCOCCUS PNEUMONIAE  per lab",
         monitor_point_subname == "FACT_OPIE") %>%
  mutate(run_date = as.Date(monitor_datetime)) %>%
  # if multiple results in same day then keep the latest
  group_by(run_date, monitor_point_name, monitor_point_subname, stat_name) %>%
  filter(monitor_datetime == max(monitor_datetime)) %>%
  ungroup() %>%
  mutate(value = as.numeric(value)) %>%
  filter(run_date < as.Date("2023-08-09"))

prepared_df <-
  prepare_df(
    df,
    timepoint_col = colspec$timepoint_col,
    item_col = colspec$item_col,
    value_col = colspec$value_col)

alert_results <-
  mantis_alerts(
  df,
  colspec = colspec("run_date", "groupby_value", "value"),
  alert_rules = alert_rules(
    alert_difference_above_perc(
      current_period = 5,
      previous_period = 30,
      rule_value = 5
    )
  )
)

alert_results %>%
  filter(if_any(.cols = everything(), .fns = ~ . == TRUE))

df <-
  pop_monitor %>%
  filter(stat_name == "STREPTOCOCCUS PNEUMONIAE  per lab") %>%
  mutate(run_date = as.Date(monitor_datetime)) %>%
  # if multiple results in same day then keep the latest
  group_by(run_date, monitor_point_name, monitor_point_subname, stat_name) %>%
  filter(monitor_datetime == max(monitor_datetime)) %>%
  ungroup() %>%
  mutate(value = as.numeric(value)) %>%
  filter(run_date < as.Date("2023-08-09"))

alert_results <-
  mantis_alerts(
    df,
    colspec = colspec("run_date", "groupby_value", "value", "monitor_point_subname"),
    alert_rules = alert_rules(
      alert_difference_above_perc(
        current_period = 5,
        previous_period = 30,
        rule_value = 5
      ),
      alert_missing(extent_type = "last",
                    extent_value = 14)
    ),
    filter_results = TRUE
  )

