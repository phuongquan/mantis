# Built-in alert rules

A range of built-in rules can be run on the time series to test for
particular conditions.

## Usage

``` r
alert_missing(extent_type = "all", extent_value = 1, items = NULL)

alert_equals(extent_type = "all", extent_value = 1, rule_value, items = NULL)

alert_above(extent_type = "all", extent_value = 1, rule_value, items = NULL)

alert_below(extent_type = "all", extent_value = 1, rule_value, items = NULL)

alert_difference_above_perc(
  current_period,
  previous_period,
  rule_value,
  items = NULL
)

alert_difference_below_perc(
  current_period,
  previous_period,
  rule_value,
  items = NULL
)

alert_custom(short_name, description, expression, items = NULL)
```

## Arguments

- extent_type:

  Type of subset of the time series values that must satisfy the
  condition for the rule to return "FAIL". One of "all", "any", "last",
  "consecutive". See Details.

- extent_value:

  Numeric lower limit of the extent type. See Details.

- items:

  Named list with names corresponding to members of `item_cols`. List
  members are character vectors of values contained in the named column
  that the rule should be applied to. If `items = NULL` the rule will be
  applied to all items. See Details.

- rule_value:

  Numeric value to test against. See Details.

- current_period:

  Numeric vector containing positions from end of time series to use for
  comparison

- previous_period:

  Numeric vector containing positions from end of time series to use for
  comparison. Can overlap with `current_period` if desired.

- short_name:

  Short name to uniquely identify the rule. Only include alphanumeric,
  '-', and '\_' characters.

- description:

  Short description of what the rule checks for

- expression:

  Quoted expression containing the call to be evaluated per item, that
  returns either `TRUE` or `FALSE`. Return value of `TRUE` means alert
  result is "FAIL". See Details.

## Value

An `alert_rule` object

## Details

Tolerance can be adjusted using the `extent_type` and `extent_value`
parameters, e.g. `extent_type="all"` means alert if all values satisfy
the condition, `extent_type="any"` in combination with `extent_value=5`
means alert if there are 5 or more values that satisfy the condition, in
any position. Also see Examples.

Use `items` to restrict the rule to be applied only to specified items.
`items` can either be NULL or a named list of character vectors. If
`NULL`, the rule will be applied to all items. If a named list, the
names must match members of the `item_cols` parameter in the
`inputspec`, (as well as column names in the `df`), though can be a
subset. If an `item_col` is not named in the list, the rule will apply
to all its members. If an `item_col` is named in the list, the rule will
only be applied when the `item_col`'s value is contained in the
corresponding character vector. When multiple `item_col`s are specified,
the rule will be applied only to items that satisfy all the conditions.
See Examples in
[`alert_rules()`](https://phuongquan.github.io/mantis/dev/reference/alert_rules.md)

`alert_missing()` - Test for the presence of NA values.

`alert_equals()` - Test for the presence of values equal to
`rule_value`.

`alert_above()` - Test for the presence of values strictly greater than
`rule_value`.

`alert_below()` - Test for the presence of values strictly less than
`rule_value`.

`alert_difference_above_perc()` - Test if latest values are greater than
in a previous period, increasing strictly more than the percentage
stipulated in `rule_value`. Based on the mean of values in the two
periods. Ranges should be contiguous, and denote positions from the end
of the time series.

`alert_difference_below_perc()` - Test if latest values are lower than
in a previous period, dropping strictly more than the percentage
stipulated in `rule_value`. Based on the mean of values in the two
periods. Ranges should be contiguous, and denote positions from the end
of the time series.

`alert_custom()` - Specify a custom rule. The supplied `expression` is
passed to [`eval()`](https://rdrr.io/r/base/eval.html) within a
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
after grouping by the `item_cols` and ordering by the `timepoint_col`.
Column names that can be used explicitly in the expression are `value`
and `timepoint`, and which refer to the values in the `value_col` and
`timepoint_col` columns of the data respectively. See Examples.

## See also

[`alert_rules()`](https://phuongquan.github.io/mantis/dev/reference/alert_rules.md),
[`alertspec()`](https://phuongquan.github.io/mantis/dev/reference/alertspec.md)

## Examples

``` r
# alert if all values are NA
ars <- alert_rules(alert_missing(extent_type = "all"))

# alert if there are 10 or more missing values in total
# or if the last 3 or more values are missing
# or if 5 or more values in a row are missing
ars <- alert_rules(
  alert_missing(extent_type = "any", extent_value = 10),
  alert_missing(extent_type = "last", extent_value = 3),
  alert_missing(extent_type = "consecutive", extent_value = 5)
)

# alert if any values are zero
ars <- alert_rules(alert_equals(extent_type = "any", rule_value = 0))

# alert if all values are greater than 50
ars <- alert_rules(alert_above(extent_type = "all", rule_value = 50))

# alert if all values are less than 2
ars <- alert_rules(alert_below(extent_type = "all", rule_value = 2))

# alert if mean of last 3 values is over 20% greater
# than mean of the previous 12 values
ars <- alert_rules(
  alert_difference_above_perc(
    current_period = 1:3,
    previous_period = 4:15,
    rule_value = 20)
  )

# alert if mean of last 3 values is over 20% lower than mean of
# the previous 12 values
ars <- alert_rules(
  alert_difference_below_perc(
    current_period = 1:3,
    previous_period = 4:15,
    rule_value = 20)
  )

# Create two custom rules
ars <- alert_rules(
  alert_custom(
    short_name = "my_rule_combo",
    description = "Over 3 missing values and max value is > 10",
    expression = quote(
      sum(is.na(value)) > 3 && max(value, na.rm = TRUE) > 10
    )
  ),
  alert_custom(
    short_name = "my_rule_doubled",
    description = "Last value is over double the first value",
    expression = quote(rev(value)[1] > 2 * value[1])
  )
)
```
