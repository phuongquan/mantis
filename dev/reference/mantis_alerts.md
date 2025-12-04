# Generate a data frame containing alert results

Test the time series for a set of conditions without generating an html
report. This can be useful for incorporation into a pipeline.

## Usage

``` r
mantis_alerts(
  df,
  inputspec,
  alert_rules,
  filter_results = c("PASS", "FAIL", "NA"),
  timepoint_limits = c(NA, NA),
  fill_with_zero = FALSE
)
```

## Arguments

- df:

  A data frame containing multiple time series in long format. See
  Details.

- inputspec:

  [`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md)
  object specifying which columns in the supplied `df` represent the
  "timepoint", "item", and "value" for the time series.

- alert_rules:

  [`alert_rules()`](https://phuongquan.github.io/mantis/dev/reference/alert_rules.md)
  object specifying conditions to test

- filter_results:

  Only return rows where the alert result is in this vector of values.
  Alert results can be "PASS", "FAIL", or "NA".

- timepoint_limits:

  Set start and end dates for time period to include. Defaults to
  min/max of `timepoint_col`. Can be either Date values or NAs.

- fill_with_zero:

  Logical. Replace any missing or NA values with 0? Useful when
  value_col is a record count.

## Value

tibble

## Details

The supplied data frame should contain multiple time series in long
format, i.e.:

- one "timepoint" (date/posixt) column which will be used for the
  x-axes. Values should follow a regular pattern, e.g. daily or monthly,
  but do not have to be consecutive.

- one or more "item" (character) columns containing categorical values
  identifying distinct time series.

- one "value" (numeric) column containing the time series values which
  will be used for the y-axes.

The `inputspec` parameter maps the data frame columns to the above.

## See also

[`alert_rules()`](https://phuongquan.github.io/mantis/dev/reference/alert_rules.md),
[`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md),
[`alert_rule_types()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)

## Examples

``` r
alert_results <- mantis_alerts(
  example_prescription_numbers,
  inputspec = inputspec(
    timepoint_col = "PrescriptionDate",
    item_cols = c("Antibiotic", "Location"),
    value_col = "NumberOfPrescriptions"
  ),
  alert_rules = alert_rules(
    alert_missing(extent_type = "any", extent_value = 1),
    alert_equals(extent_type = "all", rule_value = 0)
  )
)
```
