# Specify output options for a static report containing a panel of plots.

Each tab contains a single column of scatter plots with one row per time
series.

## Usage

``` r
outputspec_static_multipanel(
  sync_axis_range = FALSE,
  y_label = NULL,
  item_order = NULL
)
```

## Arguments

- sync_axis_range:

  Set the y-axis to be the same range for all the plots. X-axes are
  always synced.

- y_label:

  string for y-axis label. Optional. If `NULL`, the label will be
  constructed from the
  [`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md)

- item_order:

  named list corresponding to `item_cols` columns for ordering the items
  in the output. List values are either `TRUE` for ascending order, or a
  character vector of values contained in the named column for explicit
  ordering. If `item_order = NULL`, the original order will be kept. See
  Details.

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

[`outputspec_interactive()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_interactive.md),
[`outputspec_static_heatmap()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_static_heatmap.md)

## Examples

``` r
# Plot all panels to same scale
outspec <- outputspec_static_multipanel(
  sync_axis_range = TRUE,
  y_label = "Daily records"
)

# Sort panels alphabetically by Antibiotic
outspec <- outputspec_static_multipanel(
  item_order = list("Antibiotic" = TRUE)
)

# Sort alphabetically by Location first,
# then put "Vancomycin" and "Linezolid" before other antibiotics
outspec <- outputspec_static_multipanel(
  item_order = list("Location" = TRUE,
                    "Antibiotic" = c("Vancomycin", "Linezolid"))
)
```
