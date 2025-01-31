# mantis (development version)

Breaking changes

* `inputspec()` now takes a `item_cols` parameter, allowing more than one column in the df to define individual time series, and must include the column being used for tabs (if any).
* `items` parameter of alert rules now takes a list of items
* `item_order` parameter of `outputspec()` now takes a list of items, and allows ordering of tabs
* `bespoke_rmd_tab()` replaces `bespoke_rmd_tab_group()` and `bespoke_rmd_tab_item()`

Other changes

* example_prescription_numbers dataset now has additional data

# mantis 0.1.2 (2025-01-15)

Pre-release. Expect breaking changes to functions in the future.

Complete list of functions exported:

* `alert_above()`
* `alert_below()`
* `alert_custom()`
* `alert_difference_above_perc()`
* `alert_difference_below_perc()`
* `alert_equals()`
* `alert_missing()`
* `alert_rules()`
* `bespoke_rmd_initialise_widgets()`
* `bespoke_rmd_tab_group()`
* `bespoke_rmd_tab_item()`
* `inputspec()`
* `mantis_alerts()`
* `mantis_report()`
* `outputspec_interactive()`
* `outputspec_static_heatmap()`
* `outputspec_static_multipanel()`
