
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
library(dplyr)
library(mantis)

data("example_data")

df <- example_data
timepoint_col <- "timepoint"
item_col <- "item"
value_col <- "value"
item_label <- "Item"


monthly_data <- example_data |>
  dplyr::mutate(timepoint = as.Date(format(timepoint, format = "%Y-%m-01"))) |>
  dplyr::group_by(timepoint, item, tab) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE),
                   .groups = "drop")


mantis_report(df = monthly_data,
              inputspec = inputspec(timepoint_col = "timepoint",
                                item_col = "item",
                                value_col = "value",
                                period = "month"),
              outputspec = outputspec_interactive(),
              alert_rules <- alert_rules(alert_missing(extent_type = "all",
                                                       items = "[ALL]"),
                                         alert_equals(extent_type = "last",
                                                       extent_value = 5,
                                                      rule_value = 0,
                                                       items = c("norm", "norm_na"))
              ),
              save_filename = "monthly"
)


## alerting
data("example_data")

df <- example_data
timepoint_col <- "timepoint"
item_col <- "item"
value_col <- "value"

alert_results <-
  mantis_alerts(
    example_data,
  inputspec = inputspec("timepoint", "item", "value"),
  alert_rules = NULL
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


# multi item_cols
data("example_prescription_numbers")

df <- example_prescription_numbers

mantis_report(df = example_prescription_numbers,
              inputspec = inputspec(
                timepoint_col = "PrescriptionDate",
                item_col = "Antibiotic",
                value_col = "NumberOfPrescriptions",
                tab_col = "Location"
              ),
              outputspec = outputspec_interactive(
                item_label = "Antibiotic"
              ),
              report_title = "mantis report",
              dataset_description = "Antibiotic prescriptions by site"
)

mantis_report(df = example_prescription_numbers,
              inputspec = inputspec(
                timepoint_col = "PrescriptionDate",
                item_col = c("Antibiotic", "Location"),
                value_col = "NumberOfPrescriptions"
              ),
              outputspec = outputspec_interactive(
              ),
              alert_rules = alert_rules(
                alert_custom(
                  short_name = "my_rule_doubled",
                  description = "Last value is over double the first value",
                  function_call = quote(rev(value)[1] > 2*value[1])
                )
              ),
              report_title = "mantis report",
              dataset_description = "Antibiotic prescriptions by site"
)

mantis_report(df = example_prescription_numbers,
              inputspec = inputspec(
                timepoint_col = "PrescriptionDate",
                item_col = c("Antibiotic", "Location"),
                value_col = "NumberOfPrescriptions"
              ),
              outputspec = outputspec_static_multipanel(
              ),
              report_title = "mantis report",
              dataset_description = "Antibiotic prescriptions by site"
)

mantis_alerts(
  example_prescription_numbers,
  inputspec = inputspec(
    timepoint_col = "PrescriptionDate",
    item_col = c("Antibiotic", "Location"),
    value_col = "NumberOfPrescriptions"
  ),
  alert_rules = alert_rules(
    alert_custom(
      short_name = "my_rule_doubled",
      description = "Last value is over double the first value",
      function_call = quote(rev(value)[1] > 2*value[1])
    )
  )
)
