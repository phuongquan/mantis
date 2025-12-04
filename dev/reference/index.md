# Package index

## Basic usage

- [`mantis_report()`](https://phuongquan.github.io/mantis/dev/reference/mantis_report.md)
  : Create an interactive time series report from a data frame
- [`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md)
  : Specify relevant columns in the source data frame
- [`example_prescription_numbers`](https://phuongquan.github.io/mantis/dev/reference/example_prescription_numbers.md)
  : Example data frame containing numbers of antibiotic prescriptions in
  long format

## Specifying the output format

- [`outputspec_interactive()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_interactive.md)
  : Specify output options for an interactive report
- [`outputspec_static_heatmap()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_static_heatmap.md)
  : Specify output options for a static report containing heatmaps
- [`outputspec_static_multipanel()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_static_multipanel.md)
  : Specify output options for a static report containing a panel of
  plots.

## Specifying alerting rules

- [`alertspec()`](https://phuongquan.github.io/mantis/dev/reference/alertspec.md)
  : Specify alerting rules to be run on the data and displayed in the
  report
- [`alert_rules()`](https://phuongquan.github.io/mantis/dev/reference/alert_rules.md)
  : Create set of alert rules
- [`alert_missing()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
  [`alert_equals()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
  [`alert_above()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
  [`alert_below()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
  [`alert_difference_above_perc()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
  [`alert_difference_below_perc()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
  [`alert_custom()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
  : Built-in alert rules
- [`mantis_alerts()`](https://phuongquan.github.io/mantis/dev/reference/mantis_alerts.md)
  : Generate a data frame containing alert results

## Creating a bespoke report

- [`bespoke_rmd_initialise_widgets()`](https://phuongquan.github.io/mantis/dev/reference/bespoke_rmd_initialise_widgets.md)
  : Initialise HTML widgets
- [`bespoke_rmd_output()`](https://phuongquan.github.io/mantis/dev/reference/bespoke_rmd_output.md)
  : Dynamically generate mantis output for an rmd chunk
- [`bespoke_rmd_alert_results()`](https://phuongquan.github.io/mantis/dev/reference/bespoke_rmd_alert_results.md)
  : Dynamically generate a table containing alert results for an rmd
  chunk
