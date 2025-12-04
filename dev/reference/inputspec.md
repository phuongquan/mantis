# Specify relevant columns in the source data frame

Specify relevant columns in the source data frame

## Usage

``` r
inputspec(
  timepoint_col,
  item_cols,
  value_col,
  tab_col = NULL,
  timepoint_unit = "day"
)
```

## Arguments

- timepoint_col:

  String denoting the (date/posixt) column which will be used for the
  x-axes.

- item_cols:

  String denoting the (character) column containing categorical values
  identifying distinct time series. Multiple columns that together
  identify a time series can be provided as a vector

- value_col:

  String denoting the (numeric) column containing the time series values
  which will be used for the y-axes.

- tab_col:

  Optional. String denoting the (character) column containing
  categorical values which will be used to group the time series into
  different tabs on the report.

- timepoint_unit:

  expected pattern of the timepoint_col values.
  "sec"/"min"/"hour"/"day"/"month"/"quarter"/year". This will be used to
  fill in any gaps in the time series.

## Value

A `inputspec()` object

## Examples

``` r
# create a flat report, and include the "Location" and "Antibiotic" fields
# in the content
inspec_flat <- inputspec(
  timepoint_col = "PrescriptionDate",
  item_cols = c("Location", "Antibiotic"),
  value_col = "NumberOfPrescriptions",
  timepoint_unit = "day"
)

# create a flat report, and include the "Location", "Spectrum",
# and "Antibiotic" fields in the content
inspec_flat2 <- inputspec(
  timepoint_col = "PrescriptionDate",
  item_cols = c("Location", "Spectrum", "Antibiotic"),
  value_col = "NumberOfPrescriptions",
  timepoint_unit = "day"
)

# create a tabbed report, with a separate tab for each unique value of
# "Location", and include just the "Antibiotic" field in the content of
# each tab
inspec_tabbed <- inputspec(
  timepoint_col = "PrescriptionDate",
  item_cols = c("Antibiotic", "Location"),
  value_col = "NumberOfPrescriptions",
  tab_col = "Location",
  timepoint_unit = "day"
)

# create a tabbed report, with a separate tab for each unique value of
# "Location", and include the "Antibiotic" and "Spectrum" fields in the
# content of each tab
inspec_tabbed2 <- inputspec(
  timepoint_col = "PrescriptionDate",
  item_cols = c("Antibiotic", "Spectrum", "Location"),
  value_col = "NumberOfPrescriptions",
  tab_col = "Location",
  timepoint_unit = "day"
)

# create a tabbed report, with a separate tab for each unique value of
# "Antibiotic", and include just the "Location" field in the content of
# each tab
inspec_tabbed3 <- inputspec(
  timepoint_col = "PrescriptionDate",
  item_cols = c("Antibiotic", "Location"),
  value_col = "NumberOfPrescriptions",
  tab_col = "Antibiotic",
  timepoint_unit = "day"
)
```
