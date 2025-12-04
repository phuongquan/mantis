# Dynamically generate mantis output for an rmd chunk

Add `mantis` tabs and visualisations to an existing `rmarkdown` report.
The function writes directly to the chunk using side-effects, so chunk
options must contain `results = 'asis'`. Make sure you read the
vignette:
[`vignette("bespoke-reports", package = "mantis")`](https://phuongquan.github.io/mantis/dev/articles/bespoke-reports.md)
as it contains further important information.

## Usage

``` r
bespoke_rmd_output(
  df,
  inputspec,
  outputspec,
  alertspec = NULL,
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
  "timepoint", "item", and "value" for the time series. A separate tab
  will be created for each distinct value in the "tab" column.

- outputspec:

  `outputspec` object specifying the desired format of the html
  table(s)/plot(s). If not supplied, default values will be used.

- alertspec:

  [`alertspec()`](https://phuongquan.github.io/mantis/dev/reference/alertspec.md)
  object specifying conditions to test and display

- timepoint_limits:

  Set start and end dates for time period to include. Defaults to
  min/max of `timepoint_col`. If the `timepoint_unit` of
  [`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md)
  is a "day" or longer, this must be a Date type, otherwise it should be
  a POSIXt type.

- fill_with_zero:

  Logical. Replace any missing or `NA` values with 0? Useful when
  `value_col` is a record count

- tab_name:

  Character string to appear on the tab label. If omitted or `NULL`,
  only the content/child tabs (and not the parent tab) will be created.

- tab_level:

  integer specifying the nesting level of the tab. If `tab_name` is
  specified, a value of 1 generates a tab at rmd level "##", and any
  `tab_col` tabs at a level down. If `tab_name` is not specified, any
  `tab_col` tabs will be created at rmd level "##".

## Value

(invisibly) the supplied `df`

## Details

You can:

- add a single visualisation, with or without creating the container
  tab, or

- add a set of tabs, each based on the same output specification, with
  or without creating the parent tab.

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
Optionally, if there are multiple columns specified in `item_cols`, one
of them can be used to group the time series into different child tabs,
by using the `tab_col` parameter.

## See also

[`bespoke_rmd_initialise_widgets()`](https://phuongquan.github.io/mantis/dev/reference/bespoke_rmd_initialise_widgets.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# put this inside a chunk in the rmd file,
# with chunk option `results = 'asis'`
mantis::bespoke_rmd_output(
  df = mantis::example_prescription_numbers,
  inputspec = mantis::inputspec(
    timepoint_col = "PrescriptionDate",
    item_cols = c("Location", "Antibiotic"),
    value_col = "NumberOfPrescriptions",
    tab_col = "Location"
  ),
  outputspec = mantis::outputspec_interactive(
    plot_value_type = "value",
    plot_type = "bar",
    item_labels = c("Antibiotic" = "Antibiotic name"),
    plot_label = "Prescriptions over time",
    sync_axis_range = FALSE,
    item_order = list("Location" = c("SITE3", "SITE2", "SITE1"))
  ),
  timepoint_limits = c(NA, Sys.Date()),
  fill_with_zero = FALSE,
  tab_name = "Group of child tabs",
  tab_level = 1
)
} # }
```
