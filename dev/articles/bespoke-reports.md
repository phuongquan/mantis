# Creating bespoke reports

This vignette introduces the essentials of how to add visualisations
from the `mantis` package into your own bespoke html reports. It will be
assumed that you already know how to generate html documents from a Rmd
file, including creating tabsets. If you don’t, take a look at the
official documentation and/or the many other online resources available
before proceeding.

Note: Currently, `mantis` uses `rmarkdown` to generate its content and
reports, and so the visualisations may not render as expected if added
to a Quarto report. In future there will be more support for
generating/adding to Quarto reports.

## Overview

The standard
[`mantis_report()`](https://phuongquan.github.io/mantis/dev/reference/mantis_report.md)
is designed for quick and simple review of time series that can be
stored in a single data frame. If you like the `mantis` visualisations
but want to build a more complex report, e.g. to display data from
multiple data frames, display `mantis` content alongside other
(non-`mantis`) content, or to add your own branding, you can create your
own html report using existing `rmarkdown` functionality, and then
insert `mantis` content and/or tabs into your report as desired.

The easiest way to get started with this is to clone the
[mantis-templates](https://github.com/phuongquan/mantis-templates)
project to see it in action, then choose a template Rmd file that you
can copy from/adapt.

## Adding `mantis` content

`mantis` includes one main function
[`bespoke_rmd_output()`](https://phuongquan.github.io/mantis/dev/reference/bespoke_rmd_output.md)
for creating content for bespoke reports. This function use side-effects
to generate the desired markdown, and must be placed inside a
`{r, results='asis'}` chunk.

The function can:

- add a single visualisation, with or without creating the container
  tab, or

- add a set of tabs, each based on the same output specification, with
  or without creating the parent tab.

The function also includes a few additional parameters not available in
the standard
[`mantis_report()`](https://phuongquan.github.io/mantis/dev/reference/mantis_report.md)
to allow some extra control over the display.

**Example:**

```` default
```{r, results='asis'}
# create a parent tab with a set of child tabs
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
```
````

## Extra requirements for interactive reports

Due to complexities in the way `dygraphs` widgets are rendered, there
are two additional chunks that must be included in the Rmd file,
otherwise the graphs can disappear unexpectedly.

Firstly, add the following chunk to the Rmd file to ensure the
`dygraphs` plots are initialised. Remember to set the same `plot_type`
as the one you are using for the rest of the report. This is best placed
at the bottom of the file as it can lead to an extra line space. Note:
unlike when creating `mantis` content, this chunk should *not* include
`results='asis'`.

```` default
```{r}
# this chunk is necessary when using mantis::outputspec_interactive().
# it ensures that the dygraphs render when built using `cat()`
# set the plot_type to the same plot_type as the real output
mantis::bespoke_rmd_initialise_widgets(plot_type = "bar")
```
````

Secondly, when the browser window is resized, all `dygraphs` plots on
inactive tabs disappear. A workaround for this is to redraw them all by
reloading the page. To do this, add the following chunk at the top of
the Rmd file.

```` default
```{js}
// this chunk is necessary when using mantis::outputspec_interactive(). 
// when the browser window is resized, all dygraphs on inactive tabs disappear. Reload page to 
// redraw them all.
window.onresize = function(){ location.reload(); }
```
````
