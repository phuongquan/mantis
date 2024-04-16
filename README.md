
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tinduck

The tinduck package generates interactive html reports that enable quick
visual review of multiple related time series. This can help with
identification of temporal artefacts and unexpected trends. Each group
of time series is displayed together, with adjustable axes and tooltips
showing the individual dates and values.

The data frame containing the time series should be in long format,
i.e.:

- one “timepoint” (date) column which will be used for the x-axes. This
  currently must be at a daily granularity, but values do not have to be
  consecutive.
- one “item” (character) column containing categorical values
  identifying distinct time series.
- one “value” (numeric) column containing the time series values which
  will be used for the y-axes.
- Optionally, a “group” (character) column containing categorical values
  which will be used to group the time series into different tabs on the
  report.

The `colspec` parameter maps the data frame columns to the above.

The reports are shareable and can contribute to forming a transparent
record of the entire analysis process. It is designed with electronic
health records in mind, but can be used for any types of time series.

## Installation

``` r
# install direct from source
# install.packages("remotes")
remotes::install_github("phuongquan/tinduck")
```

## Usage

``` r
library(tinduck)

# this example data frame contains numbers of antibiotic prescriptions in long format, 
#   plus a column for grouping the output:
data("example_prescription_numbers")

head(example_prescription_numbers)
```

    ## # A tibble: 6 × 4
    ##   PrescriptionDate Antibiotic    NumberOfPrescriptions Location
    ##   <date>           <chr>                         <dbl> <chr>   
    ## 1 2022-01-01       Coamoxiclav                      45 SITE1   
    ## 2 2022-01-01       Gentamicin                       44 SITE1   
    ## 3 2022-01-01       Ceftriaxone                      26 SITE1   
    ## 4 2022-01-01       Metronidazole                    22 SITE1   
    ## 5 2022-01-01       Meropenem                         5 SITE1   
    ## 6 2022-01-01       Vancomycin                        0 SITE1

``` r
# create a report in the current directory
tinduck_report(
  df = example_prescription_numbers,
  colspec = colspec(
    timepoint_col = "PrescriptionDate",
    item_col = "Antibiotic",
    value_col = "NumberOfPrescriptions",
    group_col = "Location"
  )
)
```

## Acknowledgements

This work was supported by the National Institute for Health Research
Health Protection Research Unit (NIHR HPRU) in Healthcare Associated
Infections and Antimicrobial Resistance at the University of Oxford in
partnership with Public Health England (PHE) (NIHR200915), and by the
NIHR Oxford Biomedical Research Centre.

## Contributing to this package

Please report any bugs or suggestions by opening a [github
issue](https://github.com/phuongquan/tinduck/issues).
