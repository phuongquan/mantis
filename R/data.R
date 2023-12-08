#' Example data frame containing multiple time series in long format
#'
#' Simulated data to cover a range of different behaviours of time series
#'
#' @format ## `example_data`
#' A data frame with 3,903 rows and 4 columns:
#' \describe{
#'   \item{timepoint}{Dates for the time series}
#'   \item{item}{Labels to identify the different time series}
#'   \item{value}{Vaues for the time series}
#'   \item{group}{Labels to group related time series}
#'   ...
#' }
"example_data"

#' Example data frame containing numbers of antibiotic prescriptions in long format
#'
#' Simulated data to demonstrate package usage
#'
#' @format ## `example_prescription_numbers`
#' A data frame with 6,570 rows and 4 columns:
#' \describe{
#'   \item{PrescriptionDate}{Date that the prescriptions were given}
#'   \item{Antibiotic}{The type of antibiotic prescribed}
#'   \item{NumberOfPrescriptions}{Number of prescriptions given}
#'   \item{Location}{Location of the patient}
#'   ...
#' }
"example_prescription_numbers"
