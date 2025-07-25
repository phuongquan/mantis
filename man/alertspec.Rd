% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alert.R
\name{alertspec}
\alias{alertspec}
\title{Specify alerting rules to be run on the data and displayed in the report}
\usage{
alertspec(alert_rules, show_tab_results = c("PASS", "FAIL", "NA"))
}
\arguments{
\item{alert_rules}{\code{\link[=alert_rules]{alert_rules()}} object specifying conditions to test}

\item{show_tab_results}{only show rows where the alert result is in this
vector of values. Alert results can be "PASS", "FAIL", or "NA". If NULL, no
separate tab will be created.}
}
\value{
An \code{alertspec()} object
}
\description{
The alert results are displayed in different ways depending on the chosen
outputspec. Tabs containing time series which failed at least one alert are
highlighted, and a separate tab containing the alert results is created by
default.
}
\examples{
# define some alerting rules
ars <- alert_rules(
  alert_missing(extent_type = "any", extent_value = 1),
  alert_equals(extent_type = "all", rule_value = 0)
)

# specify that all results should be included in the Alerts tab (the default)
alsp <- alertspec(
  alert_rules = ars
)

# specify that only results which fail or are incalculable should be included
# in the Alerts tab
alsp <- alertspec(
  alert_rules = ars,
  show_tab_results = c("FAIL", "NA")
)

}
\seealso{
\code{\link[=alert_rules]{alert_rules()}}, \code{\link[=alert_rule_types]{alert_rule_types()}}
}
