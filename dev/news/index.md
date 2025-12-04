# Changelog

## mantis 1.0.0 (2025-10-27)

CRAN release: 2025-10-27

Incorporates changes requested for acceptance into
<https://ropensci.org/>.

### Breaking changes

- `expression` parameter of
  [`alert_custom()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
  replaces `function_call`

### Bug fixes and minor improvements

- `item_order` parameter of `outputspec()` can now handle an arbitrary
  number of items
- `timepoint_limits` parameter for alerts and bespoke reports now
  automatically adjusts user-supplied values to align with the data and
  `timepoint_unit` where necessary, instead of inserting inappropriate
  additional timepoints in plots
- Date-only timepoints cast as POSIXt using a daylight-savings-varying
  timezone no longer result in inappropriate additional timepoints
  appearing in plots
  ([\#30](https://github.com/phuongquan/mantis/issues/30))
- Stricter validation of POSIXt timepoints

## mantis 0.4.3 (2025-07-31)

CRAN release: 2025-08-01

Include bespoke reports functionality.

## mantis 0.4.2 (2025-07-25)

CRAN release: 2025-07-28

Lightweight initial CRAN submission. Only includes standard
(non-bespoke) reports.

### Breaking changes

- New `file` parameter in
  [`mantis_report()`](https://phuongquan.github.io/mantis/dev/reference/mantis_report.md)
  replaces `save_directory` and `save_filename`
  ([\#24](https://github.com/phuongquan/mantis/issues/24))
- New `add_timestamp` parameter in
  [`mantis_report()`](https://phuongquan.github.io/mantis/dev/reference/mantis_report.md)
  adds a timestamp to the supplied file name
  ([\#24](https://github.com/phuongquan/mantis/issues/24))

### Bug fixes and minor improvements

- Timestamps added to filenames are now formatted correctly on linux
- Intermediate files that are generated while creating a report are now
  created in [`tempdir()`](https://rdrr.io/r/base/tempfile.html) instead
  of the working directory where possible
  ([\#24](https://github.com/phuongquan/mantis/issues/24))

## mantis 0.3.0 (2025-06-10)

### Breaking changes

- `timepoint_unit` parameter of
  [`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md)
  replaces `period`

## mantis 0.2.0 (2025-02-07)

### Breaking changes

- [`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md)
  object now takes a `item_cols` parameter, allowing more than one
  column in the df to define individual time series, and must include
  the column being used for tabs (if any).
- New
  [`alertspec()`](https://phuongquan.github.io/mantis/dev/reference/alertspec.md)
  object adds additional output options and wraps
  [`alert_rules()`](https://phuongquan.github.io/mantis/dev/reference/alert_rules.md)
  where relevant
- Alert results now appear in an additional tab for all types of
  outputspec
- `items` parameter of alert rules now takes a named list of items
- `item_labels` parameter of
  [`outputspec_interactive()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_interactive.md)
  replaces `item_label` and now takes a named vector of labels
- `item_order` parameter of `outputspec()` now takes a named list of
  items, and allows ordering of tabs
- [`bespoke_rmd_output()`](https://phuongquan.github.io/mantis/dev/reference/bespoke_rmd_output.md)
  replaces `bespoke_rmd_tab_group()` and `bespoke_rmd_tab_item()`
- New
  [`bespoke_rmd_alert_results()`](https://phuongquan.github.io/mantis/dev/reference/bespoke_rmd_alert_results.md)
  function creates a table containing the alert results for bespoke
  reports

### Bug fixes and minor improvements

- example_prescription_numbers dataset now has additional data

## mantis 0.1.2 (2025-01-15)

Pre-release. Expect breaking changes to functions in the future.

Complete list of functions exported:

- [`alert_above()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
- [`alert_below()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
- [`alert_custom()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
- [`alert_difference_above_perc()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
- [`alert_difference_below_perc()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
- [`alert_equals()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
- [`alert_missing()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)
- [`alert_rules()`](https://phuongquan.github.io/mantis/dev/reference/alert_rules.md)
- [`bespoke_rmd_initialise_widgets()`](https://phuongquan.github.io/mantis/dev/reference/bespoke_rmd_initialise_widgets.md)
- `bespoke_rmd_tab_group()`
- `bespoke_rmd_tab_item()`
- [`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md)
- [`mantis_alerts()`](https://phuongquan.github.io/mantis/dev/reference/mantis_alerts.md)
- [`mantis_report()`](https://phuongquan.github.io/mantis/dev/reference/mantis_report.md)
- [`outputspec_interactive()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_interactive.md)
- [`outputspec_static_heatmap()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_static_heatmap.md)
- [`outputspec_static_multipanel()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_static_multipanel.md)
