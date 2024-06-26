% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct.R
\name{bespoke_rmd_tab_item}
\alias{bespoke_rmd_tab_item}
\title{Dynamically generate a single tab for an rmd chunk}
\usage{
bespoke_rmd_tab_item(
  df,
  inputspec,
  outputspec,
  alert_rules = NULL,
  timepoint_limits = c(NA, NA),
  fill_with_zero = FALSE,
  tab_name = NULL,
  tab_level = 1
)
}
\arguments{
\item{df}{A data frame containing multiple time series in long format. See Details.}

\item{inputspec}{[`inputspec()`] object specifying which columns in the supplied `df` represent the
"timepoint", "item", and "value" for the time series. If a "tab" column is specified, it will be ignored.}

\item{outputspec}{[`outputspec()`] object specifying the desired format of the html table(s). If
not supplied, default values will be used.}

\item{alert_rules}{[`alert_rules()`] object specifying conditions to test}

\item{timepoint_limits}{Set start and end dates for time period to include. Defaults to min/max of timepoint_col}

\item{fill_with_zero}{Replace any missing or NA values with 0? Useful when value_col is a record count}

\item{tab_name}{Character string to appear on parent tab}

\item{tab_level}{Child level for tab. Value of 1 creates a tab with rmd level "##".}
}
\value{
(invisibly) the supplied df
}
\description{
Chunk options must contain `results = 'asis'`.
Function writes directly to the chunk using side-effects
}
\details{
The supplied data frame should contain multiple time series in long format, i.e.:
* one "timepoint" (datetime) column which will be used for the x-axes. This currently must be at a daily granularity, but values do not have to be consecutive.
* one "item" (character) column containing categorical values identifying distinct time series.
* one "value" (numeric) column containing the time series values which will be used for the y-axes.
The `inputspec` parameter maps the data frame columns to the above.
}
