#' Sample simulated clinical trial dataset
#'
#' A simulated dataset containing variables for testing out ALPFunctions
#' functions
#'
#' @format A data frame with 120 rows and 6 variables:
#' \describe{
#'   \item{trt_group}{A character type variable used to specify the treatment
#'   conditions of the subject}
#'   \item{sex}{A character type variable used to specify the sex of the
#'   subject}
#'   \item{age}{A numeric type variable for specifying the age of the subject}
#'   \item{race}{A character type variable used to specify the race of the
#'   subject}
#'   \item{pre_treatment_bp}{A numeric type variable for specifying the
#'   systolic blood pressure of the patient before study enrollment}
#'   \item{post_treatment_bp}{A numeric type variable for specifying the
#'   systolic blood pressure of the patient after study enrollment}
#' }
#'
#' @source {Created through simulation to serve as an example}
#'
#' @examples
#' data(ct_data)
"ct_data"


#' Sample simulated clinical trial dataset with paired data
#'
#' A simulated paired dataset containing variables for testing out ALPFunctions
#' functions
#'
#' @format A data frame with 120 rows and 6 variables:
#' \describe{
#'   \item{participant_id}{A numeric variable used to uniquely identify each
#'   subject}
#'   \item{treat_time}{A character type variable used to specify the treatment
#'   time of the observation for a subject}
#'   \item{sex}{A character type variable used to specify the sex of the
#'   subject}
#'   \item{age}{A numeric type variable for specifying the age of the subject}
#'   \item{race}{A character type variable used to specify the race of the
#'   subject}
#'   \item{bp}{A numeric type variable for specifying the
#'   systolic blood pressure of the patient}
#' }
#'
#' @source {Created through simulation to serve as an example}
#'
#' @examples
#' data(paired_ct_data)
"paired_ct_data"
