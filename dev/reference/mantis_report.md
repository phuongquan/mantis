# Create an interactive time series report from a data frame

Accepts a data frame containing multiple time series in long format,
generates a collection of interactive time series plots for visual
inspection, and saves the report to disk.

## Usage

``` r
mantis_report(
  df,
  file,
  inputspec,
  outputspec = NULL,
  alertspec = NULL,
  report_title = "mantis report",
  dataset_description = "",
  add_timestamp = FALSE,
  show_progress = TRUE,
  ...
)
```

## Arguments

- df:

  A data frame containing multiple time series in long format. See
  Details.

- file:

  String specifying the desired file name (and path) to save the report
  to. The file name should include the extension ".html". If only a file
  name is supplied, the report will be saved in the current working
  directory. If a path is supplied, the directory should already exist.
  Any existing file of the same name will be overwritten unless
  `add_timestamp` is set to `TRUE`.

- inputspec:

  [`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md)
  object specifying which columns in the supplied `df` represent the
  "timepoint", "item", "value" and (optionally) "tab" for the time
  series. If a "tab" column is specified, a separate tab will be created
  for each distinct value in the column.

- outputspec:

  `outputspec` object specifying the desired format of the html
  table(s). If not supplied, default values will be used.

- alertspec:

  [`alertspec()`](https://phuongquan.github.io/mantis/dev/reference/alertspec.md)
  object specifying conditions to test and display.

- report_title:

  Title to appear on the report.

- dataset_description:

  Short description of the dataset being shown. This will appear on the
  report.

- add_timestamp:

  Append a timestamp to the end of the filename with format
  `_YYMMDD_HHMMSS`. This can be used to keep multiple versions of the
  same report. Default = `FALSE`.

- show_progress:

  Print progress to console. Default = `TRUE`.

- ...:

  Further parameters to be passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
  Cannot include any of `input`, `output_dir`, `output_file`, `params`,
  `quiet`.

## Value

A string containing the name and full path of the saved report.

## Details

The supplied data frame should contain multiple time series in long
format, i.e.:

- one "timepoint" (date/posixt) column which will be used for the
  x-axes. Values should follow a regular pattern, e.g. daily or monthly,
  but do not have to be consecutive.

- one or more "item" (character) columns containing categorical values
  identifying distinct time series.

- one "value" (numeric) column containing the time series values which
  will be used for the y-axes.

- Optionally, a "tab" (character) column containing categorical values
  which will be used to group the time series into different tabs on the
  report.

  The `inputspec` parameter maps the data frame columns to the above.

## See also

[`inputspec()`](https://phuongquan.github.io/mantis/dev/reference/inputspec.md),
[`outputspec_interactive()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_interactive.md),
[`outputspec_static_heatmap()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_static_heatmap.md),
[`outputspec_static_multipanel()`](https://phuongquan.github.io/mantis/dev/reference/outputspec_static_multipanel.md),
[`alertspec()`](https://phuongquan.github.io/mantis/dev/reference/alertspec.md)

## Examples

``` r
# \donttest{
# create an interactive report in the temp directory,
# with one tab per Location
filename <- mantis_report(
  df = example_prescription_numbers,
  file = file.path(tempdir(), "example_prescription_numbers_interactive.html"),
  inputspec = inputspec(
    timepoint_col = "PrescriptionDate",
    item_cols = c("Location", "Antibiotic", "Spectrum"),
    value_col = "NumberOfPrescriptions",
    tab_col = "Location",
    timepoint_unit = "day"
  ),
  outputspec = outputspec_interactive(),
  report_title = "Daily antibiotic prescribing",
  dataset_description = "Antibiotic prescriptions by site",
  show_progress = TRUE
)
#> 
#> 
#> processing file: report-html.Rmd
#> 1/12                      
#> 2/12 [mantis-setup]       
#> 3/12                      
#> 4/12 [mantis-styles]      
#> 5/12                      
#> 6/12 [mantis-initialise]  
#> 7/12                      
#> 8/12 [mantis-js]          
#> 9/12                      
#> 10/12 [mantis-content]     
#> 11/12                      
#> 12/12 [mantis-init-widgets]
#> output file: /tmp/RtmpMxpd9p/mantis_temp_14kx53xy/report-html.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS /tmp/RtmpMxpd9p/mantis_temp_14kx53xy/report-html.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output /tmp/RtmpMxpd9p/example_prescription_numbers_interactive.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpMxpd9p/rmarkdown-str1a0463dd7f9c.html 
#> 
#> Output created: /tmp/RtmpMxpd9p/example_prescription_numbers_interactive.html

filename
#> [1] "/tmp/RtmpMxpd9p/example_prescription_numbers_interactive.html"
# }

# \donttest{
# create an interactive report in the temp directory, with alerting rules
filename <- mantis_report(
  df = example_prescription_numbers,
  file = file.path(tempdir(), "example_prescription_numbers_interactive.html"),
  inputspec = inputspec(
    timepoint_col = "PrescriptionDate",
    item_cols = c("Location", "Antibiotic", "Spectrum"),
    value_col = "NumberOfPrescriptions",
    tab_col = "Location",
    timepoint_unit = "day"
  ),
  outputspec = outputspec_interactive(),
  alertspec = alertspec(
   alert_rules = alert_rules(
    alert_missing(extent_type = "any", extent_value = 1),
    alert_equals(extent_type = "all", rule_value = 0)
   ),
   show_tab_results = c("FAIL", "NA")
  ),
  report_title = "Daily antibiotic prescribing",
  dataset_description = "Antibiotic prescriptions by site",
  show_progress = TRUE
)
#> 
#> 
#> processing file: report-html.Rmd
#> 1/12                      
#> 2/12 [mantis-setup]       
#> 3/12                      
#> 4/12 [mantis-styles]      
#> 5/12                      
#> 6/12 [mantis-initialise]  
#> 7/12                      
#> 8/12 [mantis-js]          
#> 9/12                      
#> 10/12 [mantis-content]     
#> 11/12                      
#> 12/12 [mantis-init-widgets]
#> output file: /tmp/RtmpMxpd9p/mantis_temp_ol6v58l9/report-html.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS /tmp/RtmpMxpd9p/mantis_temp_ol6v58l9/report-html.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output /tmp/RtmpMxpd9p/example_prescription_numbers_interactive.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/RtmpMxpd9p/rmarkdown-str1a0468ff26f0.html 
#> 
#> Output created: /tmp/RtmpMxpd9p/example_prescription_numbers_interactive.html

filename
#> [1] "/tmp/RtmpMxpd9p/example_prescription_numbers_interactive.html"
# }
```
