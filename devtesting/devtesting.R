
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
               inputspec = list(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value")
               )

mantis::mantis_report(df,
                                   inputspec = mantis::inputspec(timepoint_col = "timepoint",
                                                    item_col = "item",
                                                    value_col = "value",
                                                    tab_col = "tab"),
                                   outputspec = outputspec_interactive(plot_type = "line",
                                                           summary_cols = "max_value")
                                   )


data("example_prescription_numbers")
mantis::mantis_report(df = example_prescription_numbers,
                        inputspec = mantis::inputspec(timepoint_col = "PrescriptionDate",
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
               inputspec = inputspec(timepoint_col = "PrescriptionDate",
                                 item_col = "Antibiotic",
                                 value_col = "NumberOfPrescriptions")
               )

mantis_report(df = example_prescription_numbers,
               inputspec = inputspec(timepoint_col = "PrescriptionDate",
                                 item_col = "Antibiotic",
                                 value_col = "NumberOfPrescriptions",
                                 tab_col = "Location")
)

data("example_data")
mantis_report(df = example_data,
               inputspec = inputspec(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value")
)

mantis_report(df = example_data,
               inputspec = inputspec(timepoint_col = "timepoint",
                              item_col = "item",
                              value_col = "value",
                              tab_col = "tab")
               )

mantis_report(df = example_data,
               inputspec = inputspec(timepoint_col = "timepoint",
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
               inputspec = inputspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value"),
               outputspec = outputspec_static_heatmap()
)

mantis_report(df = example_data,
               inputspec = inputspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value",
                                 tab_col = "tab"),
              dataset_description = "examples",
              outputspec = outputspec_static_heatmap()
)

mantis_report(df = example_data,
               inputspec = inputspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value"),
               outputspec = outputspec_static_multipanel()
)

mantis_report(df = example_data,
               inputspec = inputspec(timepoint_col = "timepoint",
                                 item_col = "item",
                                 value_col = "value"),
               outputspec = outputspec_static_multipanel(sync_axis_range = TRUE)
)

mantis_report(df = example_data,
              inputspec = inputspec(timepoint_col = "timepoint",
                                item_col = "item",
                                value_col = "value"),
              outputspec = outputspec_static_multipanel(item_order = "sparse_1"),
              report_title = "example_data",
              dataset_description = "examples"
)

library(dplyr)
monthly_data <- example_data %>%
  dplyr::mutate(timepoint = as.Date(format(timepoint, format = "%Y-%m-01"))) %>%
  dplyr::group_by(timepoint, item, tab) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE),
                   .groups = "drop")


mantis_report(df = monthly_data,
              inputspec = inputspec(timepoint_col = "timepoint",
                                item_col = "item",
                                value_col = "value",
                                period = "month"),
              outputspec = outputspec_interactive(),
              alert_rules <- alert_rules(alert_missing(extent_type = "all",
                                                       items = "ALL"),
                                         alert_equals(extent_type = "last",
                                                       extent_value = 5,
                                                      rule_value = 0,
                                                       items = c("norm", "norm_na"))
              ),
              save_filename = "monthly"
)

prepared_df <- prepare_df(
  df = monthly_data,
  inputspec = inputspec(timepoint_col = "timepoint",
                        item_col = "item",
                        value_col = "value",
                        period = "month")
)

value_for_history <- prepared_df %>%
  filter(item == "norm_na") %>%
  pull(value)

timepoint <- prepared_df %>%
  filter(item == "norm_na") %>%
  pull(timepoint)

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

alert_results <-
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
                           alert_above(extent_type = "last",
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
    timepoint_col = inputspec$timepoint_col,
    item_col = inputspec$item_col,
    value_col = inputspec$value_col)

alert_results <-
  mantis_alerts(
  df,
  inputspec = inputspec("run_date", "groupby_value", "value"),
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
    inputspec = inputspec("run_date", "groupby_value", "value", "monitor_point_subname"),
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

library(mantis)

alert_results <-
  mantis_alerts(
    example_data,
  inputspec = inputspec("timepoint", "item", "value"),
  alert_rules = alert_rules(
    alert_missing(
      extent_type = "last",
      extent_value = 5,
      items = unique(example_data$item)[grep("norm", unique(example_data$item))]
    )
  )
)

prepared_df <- prepare_df(
  df,
  inputspec,
  item_order = NULL
)
output_table_interactive(
  prepared_df,
  item_label = "item",
  plot_type = "bar",
  summary_cols = c("last_value"),
  sync_axis_range = FALSE,
  alert_results = alert_results
)


mantis_report(df = example_data,
              inputspec = inputspec(timepoint_col = "timepoint",
                                item_col = "item",
                                value_col = "value",
                                tab_col = "tab"),
              alert_rules = alert_rules(alert_missing(extent_type = "all",
                                                      items = "ALL"),
                                        alert_missing(extent_type = "last",
                                                      extent_value = 14)
              ),
              save_filename = "alert_test"
)

mantis_report(
  df = example_data,
  inputspec = inputspec(
    timepoint_col = "timepoint",
    item_col = "item",
    value_col = "value"
  ),
  alert_rules = alert_rules(
    alert_missing(
      extent_type = "last",
      extent_value = 5,
      items = unique(example_data$item)[grep("norm", example_data$item)]
    )
  ),
  save_filename = "alert_norm_only"
)


mantis_report(df = example_data,
              inputspec = inputspec(timepoint_col = "timepoint",
                                item_col = "item",
                                value_col = "value"),
              save_filename = "interactive_notabs"
)

mantis_report(df = example_data,
              inputspec = inputspec(timepoint_col = "timepoint",
                                item_col = "item",
                                value_col = "value",
                                tab_col = "tab"),
              outputspec = outputspec_interactive(sort_by = "alert_overall"),
              alert_rules = alert_rules,
              save_filename = "interactive_tabs"
)


ars <- list(alert_above(extent_type = "any",
                        extent_value = 1,
                        rule_value = 10),
            alert_missing(extent_type = "any",
                          extent_value = 1))


alert_rule <- alert_custom(
   short_name = "my_rule",
   description = "Over 3 missing values when max value is > 10",
   function_call = quote(sum(is.na(value)) > 3 && max(value, na.rm = TRUE) > 10)
 )

alert_results <-
  mantis_alerts(
    example_data,
    inputspec = inputspec("timepoint", "item", "value"),
    alert_rules = alert_rules(
      alert_custom(
        short_name = "my_rule_doubled",
        description = "Last value is over double the first value",
        function_call = quote(rev(value)[1] > 2*value[1])
      )
    )
  )

mantis_report(df = example_data,
              inputspec = inputspec(timepoint_col = "timepoint",
                                    item_col = "item",
                                    value_col = "value"),
              alert_rules = alert_rules(
                alert_custom(
                  short_name = "my_rule_doubled",
                  description = "Last value is over double the first value",
                  function_call = quote(rev(value)[1] > 2*value[1])
                )
                ,
                alert_difference_above_perc(current_period = 2, previous_period = 4, rule_value = 50)
              )
)


