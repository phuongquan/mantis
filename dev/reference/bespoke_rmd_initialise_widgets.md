# Initialise HTML widgets

Since the output is being constructed in `results='asis'` chunks, there
must also be at least one standard chunk that contains the relevant
widgets, otherwise they will fail to render. The `dygraph` also needs to
be initialised with the appropriate `plot_type`. This is only needed
when creating interactive reports. Make sure you read the vignette:
[`vignette("bespoke-reports", package = "mantis")`](https://phuongquan.github.io/mantis/dev/articles/bespoke-reports.md)
as it contains further important information. Note: The chunk currently
appears like a line break when rendered. See
<https://github.com/rstudio/rmarkdown/issues/1877> for more info.

## Usage

``` r
bespoke_rmd_initialise_widgets(plot_type)
```

## Arguments

- plot_type:

  "`bar`" or "`line`", depending on what will be used in real tables. Or
  "none" if just want a reactable widget without dygraphs e.g. for
  alerts

## Value

A (mostly) invisible html widget

## Examples

``` r
if (FALSE) { # \dontrun{
# put this inside its own chunk in the rmd file
# it ensures that the dygraphs render when built using `cat()`
# set the plot_type to the same plot_type as the real output
mantis::bespoke_rmd_initialise_widgets(plot_type = "bar")
} # }
```
