#' Example data frame containing multiple time series in long format
#'
#' Simulated data to cover a range of different behaviours of time series
#'
#' @format ## `example_data`
#' A data frame with 3,903 rows and 4 columns:
#' * timepoint - Dates for the time series
#' * item - Labels to identify the different time series
#' * value - Values for the time series
#' * tab - Labels to group related time series into tabs
"example_data"

#' Example data frame containing numbers of antibiotic prescriptions in long format
#'
#' Simulated data to demonstrate package usage
#'
#' @format ## `example_prescription_numbers`
#' A data frame with 6,570 rows and 4 columns:
#' * PrescriptionDate - The date the prescriptions were written
#' * Antibiotic - The name of the antibiotic prescribed
#' * Spectrum - The spectrum of activity of the antibiotic. This value is always the same for a particular antibiotic
#' * NumberOfPrescriptions - The number of prescriptions written for this antibiotic on this day
#' * Location - The hospital site where the prescription was written
"example_prescription_numbers"
