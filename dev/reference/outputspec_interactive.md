# Specify output options for an interactive report

Each tab contains a single table with one row per time series, and
sortable/filterable columns based on the `item_cols` parameter of the
[`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md).
The time series plots have tooltips and can be zoomed in by selecting an
area of the plot.

## Usage

``` r
outputspec_interactive(
  plot_value_type = "value",
  plot_type = "bar",
  item_labels = NULL,
  plot_label = NULL,
  summary_cols = c("max_value"),
  sync_axis_range = FALSE,
  item_order = NULL,
  sort_by = NULL
)
```

## Arguments

- plot_value_type:

  Display the raw "`value`" for the time series or display the
  calculated "`delta`" between consecutive values.

- plot_type:

  Display the time series as a "`bar`" or "`line`" chart.

- item_labels:

  Named vector containing string label(s) to use for the "item"
  column(s) in the report. The names should correspond to the
  `item_cols`, and the values should contain the desired labels. If
  `NULL`, the original columns name(s) will be used.

- plot_label:

  String label to use for the time series column in the report. If NULL,
  the original `value_col` name will be used.

- summary_cols:

  Summary data to include as columns in the report. Options are
  `c("max_value", "last_value", "last_value_nonmissing", "last_timepoint", "mean_value")`.

- sync_axis_range:

  Set the y-axis to be the same range for all time series in a table.
  X-axes are always synced. Logical.

- item_order:

  named list corresponding to `item_cols` columns for ordering the items
  in the output. List values are either `TRUE` for ascending order, or a
  character vector of values contained in the named column for explicit
  ordering. If `item_order = NULL`, the original order will be kept. See
  Details.

- sort_by:

  column in output table to sort by. Can be one of `alert_overall`, or
  one of the summary columns. Append a minus sign to sort in descending
  order e.g. `-max_value`. Secondary ordering will be based on
  `item_order`.

## Value

An `outputspec()` object

## Details

For `item_order`, the names of the list members should correspond to the
column names in the `df`. Any names that don't match will be ignored.
When multiple columns are specified, they are sorted together, in the
same priority order as the list. If a list item is `TRUE` then that
column is sorted in ascending order. If a list item is a character
vector then that column is sorted in the order of the vector first, with
any remaining values included alphabetically at the end. If you want to
order the tabs, it is recommended to put the `tab_col` as the first item
in the list.

## See also

[`outputspec_static_heatmap()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_static_heatmap.md),
[`outputspec_static_multipanel()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_static_multipanel.md)

## Examples

``` r
# Set explicit labels for the column headings
outspec <- outputspec_interactive(
  item_labels = c("Antibiotic" = "ABX", "Location" = "Which site?"),
  plot_label = "Daily records"
)

## Change the sort order that the items appear in the table

# Sort alphabetically by Antibiotic
outspec <- outputspec_interactive(
  item_order = list("Antibiotic" = TRUE)
)

# Sort alphabetically by Location first,
# then put "Vancomycin" and "Linezolid" before other antibiotics
outspec <- outputspec_interactive(
  item_order = list("Location" = TRUE,
                    "Antibiotic" = c("Vancomycin", "Linezolid"))
)

# Put the time series with the largest values first
outspec <- outputspec_interactive(
  sort_by = "-max_value"
)

# Put the time series with failed alerts first
outspec <- outputspec_interactive(
  sort_by = "alert_overall"
)

# Put the time series with failed alerts first,
# then sort alphabetically by Antibiotic
outspec <- outputspec_interactive(
  item_order = list("Antibiotic" = TRUE),
  sort_by = "alert_overall"
)
```
