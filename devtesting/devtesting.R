
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
item_cols <- "item"
value_col <- "value"
item_labels <- "Item"


monthly_data <- example_data |>
  dplyr::mutate(timepoint = as.Date(format(timepoint, format = "%Y-%m-01"))) |>
  dplyr::group_by(timepoint, item, tab) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE),
                   .groups = "drop")


mantis_report(df = monthly_data,
              file = "monthly.html",
              inputspec = inputspec(timepoint_col = "timepoint",
                                item_cols = "item",
                                value_col = "value",
                                period = "month"),
              outputspec = outputspec_interactive(),
              alert_rules <- alert_rules(alert_missing(extent_type = "all",
                                                       items = NULL),
                                         alert_equals(extent_type = "last",
                                                       extent_value = 5,
                                                      rule_value = 0,
                                                       items = c("norm", "norm_na"))
              )
)


## alerting
data("example_data")

df <- example_data
timepoint_col <- "timepoint"
item_cols <- "item"
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
        expression = quote(rev(value)[1] > 2*value[1])
      )
    )
  )

mantis_report(df = example_data,
              file = "alert_rules.html",
              inputspec = inputspec(timepoint_col = "timepoint",
                                    item_cols = "item",
                                    value_col = "value"),
              alert_rules = alert_rules(
                alert_custom(
                  short_name = "my_rule_doubled",
                  description = "Last value is over double the first value",
                  expression = quote(rev(value)[1] > 2*value[1])
                )
                ,
                alert_difference_above_perc(current_period = 2, previous_period = 4, rule_value = 50)
              )
)


# multi item_cols
data("example_prescription_numbers")

df <- example_prescription_numbers

item_order = list("Location" = c("SITE2"), "Antibiotic" = TRUE)
item_order = list("Location" = TRUE, "Antibiotic" = TRUE, "Other" = TRUE)
item_order = list("Antibiotic" = TRUE)
item_order = list("Other")
item_order = list("Location" = c("SITE2"), "Antibiotic" = c("Vancomycin", "Linezolid"))
item_order = list("Antibiotic" = c("Vancomycin", "Linezolid"), "Location" = c("SITE2"))
item_order = list("Spectrum" = TRUE, "Antibiotic" = c("Vancomycin", "Coamoxiclav"))

prepared_df<-
prepare_df(
  df,
  inputspec = inputspec(
    timepoint_col = "PrescriptionDate",
    item_cols = c("Antibiotic", "Spectrum", "Location"),
    value_col = "NumberOfPrescriptions"
  ),
  item_order = item_order
)


mantis_report(df = example_prescription_numbers,
              file = "item_order_interactive.html",
              inputspec = inputspec(
                timepoint_col = "PrescriptionDate",
                item_cols = c("Antibiotic", "Location"),
                value_col = "NumberOfPrescriptions"
              ),
              outputspec = outputspec_interactive(
                item_order = item_order),
              # alert_rules = alert_rules(
              #   alert_custom(
              #     short_name = "my_rule_doubled",
              #     description = "Last value is over double the first value",
              #     expression = quote(rev(value)[1] > 2*value[1])
              #   )
              # ),
              report_title = "mantis report",
              dataset_description = "Antibiotic prescriptions by site"
)

filename <- mantis_report(df = example_prescription_numbers,
              file = "multipanel.html",
              inputspec = inputspec(
                timepoint_col = "PrescriptionDate",
                item_cols = c("Antibiotic", "Location"),
                value_col = "NumberOfPrescriptions"
              ),
              outputspec = outputspec_static_multipanel(
              ),
              report_title = "mantis report",
              dataset_description = "Antibiotic prescriptions by site",
              add_timestamp = TRUE
)

mantis_alerts(
  example_prescription_numbers,
  inputspec = inputspec(
    timepoint_col = "PrescriptionDate",
    item_cols = c("Antibiotic", "Location"),
    value_col = "NumberOfPrescriptions"
  ),
  alert_rules = alert_rules(
    alert_custom(
      short_name = "my_rule_doubled",
      description = "Last value is over double the first value",
      expression = quote(rev(value)[1] > 2*value[1]),
      items = list("Antibiotic" = "Amikacin")
    )
  )
)



mantis_report(df = data.frame(timepoint = c(seq(as.POSIXlt("2022-01-01 12:00:00"), as.POSIXlt("2022-01-03 12:00:00"), by = "hours"),
                                                         seq(as.POSIXlt("2022-01-04 12:00:00"), as.POSIXlt("2022-01-05 12:00:00"), by = "hours")),
                                           item = rep(1, 74),
                                           value = rep(3, 74),
                                           stringsAsFactors = FALSE)
              ,
              file = "hourly.html",
              inputspec = inputspec(
                timepoint_col = "timepoint",
                item_cols = c("item"),
                value_col = "value",
                tab_col = NULL,
                period = "hour"
              ),
              outputspec = outputspec_interactive(),
              report_title = "mantis report",
              dataset_description = "Antibiotic prescriptions by site",
              alert_rules = alert_rules(
                alert_custom(
                  short_name = "my_rule_doubled",
                  description = "Last value is over double the first value",
                  expression = quote(rev(value)[1] > 2*value[1]),
                )
              )
)

