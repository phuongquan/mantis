# Dynamically generate a table containing alert results for an rmd chunk

Add `mantis` alert results to an existing `rmarkdown` report. The
function writes directly to the chunk using side-effects, so chunk
options must contain `results = 'asis'`.

## Usage

``` r
bespoke_rmd_alert_results(
  df,
  inputspec,
  alert_rules,
  filter_results = c("PASS", "FAIL", "NA"),
  timepoint_limits = c(NA, NA),
  fill_with_zero = FALSE,
  tab_name = NULL,
  tab_level = 1
)
```

## Arguments

- df:

  A data frame containing multiple time series in long format. See
  Details.

- inputspec:

  [`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md)
  object specifying which columns in the supplied `df` represent the
  "timepoint", "item", and "value" for the time series. Any "tab" column
  specification will be ignored.

- alert_rules:

  [`alert_rules()`](https://phuongquan.github.io/mantis/dev/reference/alert_rules.md)
  object specifying conditions to test

- filter_results:

  only return rows where the alert result is in this vector of values.
  Alert results can be "PASS", "FAIL", or "NA".

- timepoint_limits:

  Set start and end dates for time period to include. Defaults to
  min/max of `timepoint_col`

- fill_with_zero:

  Logical. Replace any missing or `NA` values with 0? Useful when
  `value_col` is a record count

- tab_name:

  Character string to appear on the tab label. If omitted or `NULL`,
  only the content (and not the parent tab) will be created.

- tab_level:

  integer specifying the nesting level of the tab. If `tab_name` is
  specified, a value of 1 generates a tab at rmd level "##". If
  `tab_name` is not specified, this is ignored.

## Value

(invisibly) the supplied `df`

## Examples

``` r
if (FALSE) { # \dontrun{

# put this inside a chunk in the rmd file,
# with chunk option `results = 'asis'`
mantis::bespoke_rmd_alert_results(
  df = mantis::example_prescription_numbers,
  inputspec = mantis::inputspec(
    timepoint_col = "PrescriptionDate",
    item_cols = c("Location", "Antibiotic"),
    value_col = "NumberOfPrescriptions",
    tab_col = "Location"
  ),
  alert_rules = alert_rules(
    alert_missing(extent_type = "any", extent_value = 1),
    alert_equals(extent_type = "all", rule_value = 0)
  ),
  filter_results = c("FAIL", "NA"),
  fill_with_zero = FALSE,
  tab_name = "Failed alerts",
  tab_level = 1
)
} # }
```
