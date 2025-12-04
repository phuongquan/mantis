# Create set of alert rules

Specify which alert rules should be run on the time series

## Usage

``` r
alert_rules(...)
```

## Arguments

- ...:

  alerts to apply to the time series

## Value

An `alert_rules` object

## See also

[`alert_rule_types()`](https://phuongquan.github.io/mantis/dev/reference/alert_rule_types.md)

## Examples

``` r
# alert if any values are NA
# or if all values are zero
ars <- alert_rules(
  alert_missing(extent_type = "any", extent_value = 1),
  alert_equals(extent_type = "all", rule_value = 0)
)

# alert if any values are over 100, but only for certain antibiotics
ars <- alert_rules(
  alert_above(
    extent_type = "any", extent_value = 1, rule_value = 100,
    items = list("Antibiotic" = c("Coamoxiclav", "Gentamicin"))
  )
)

# alert if any values are over 100, but only for SITE1,
# and only for certain antibiotics
ars <- alert_rules(
  alert_above(
    extent_type = "any", extent_value = 1, rule_value = 100,
    items = list(
      "Location" = "SITE1",
      "Antibiotic" = c("Coamoxiclav", "Gentamicin")
    )
  )
)
```
