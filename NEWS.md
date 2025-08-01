# mantis (development version)

Re-align with CRAN release

# mantis 0.4.3 (2025-07-31)

Include bespoke reports functionality.

# mantis 0.4.2 (2025-07-25)

Lightweight initial CRAN submission. Only includes standard (non-bespoke) reports.

## Breaking changes

* New `file` parameter in `mantis_report()` replaces `save_directory` and `save_filename` (#24)
* New `add_timestamp` parameter in `mantis_report()` adds a timestamp to the supplied file name (#24)

## Bug fixes and minor improvements

* Timestamps added to filenames are now formatted correctly on linux
* Intermediate files that are generated while creating a report are now created in `tempdir()` instead of the working directory where possible (#24)

# mantis 0.3.0 (2025-06-10)

## Breaking changes

* `timepoint_unit` parameter of `inputspec()` replaces `period` 

# mantis 0.2.0 (2025-02-07)

## Breaking changes

* `inputspec()` object now takes a `item_cols` parameter, allowing more than one column in the df to define individual time series, and must include the column being used for tabs (if any).
* New `alertspec()` object adds additional output options and wraps `alert_rules()` where relevant
* Alert results now appear in an additional tab for all types of outputspec
* `items` parameter of alert rules now takes a named list of items
* `item_labels` parameter of `outputspec_interactive()` replaces `item_label` and now takes a named vector of labels
* `item_order` parameter of `outputspec()` now takes a named list of items, and allows ordering of tabs
* `bespoke_rmd_output()` replaces `bespoke_rmd_tab_group()` and `bespoke_rmd_tab_item()`
* New `bespoke_rmd_alert_results()` function creates a table containing the alert results for bespoke reports 

## Bug fixes and minor improvements

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
