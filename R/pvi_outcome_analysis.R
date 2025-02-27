#' PVI Outcome Analysis
#'
#' A convenience wrapper function that retrieves patient data and performs
#' statistical analysis on PVI outcomes in one step.
#'
#' @param conn A DBI connection object to the database
#' @param schema Character string specifying the schema name
#' @param early_cohort_id The cohort definition ID for early PVI patients (required, no default)
#' @param non_early_cohort_id The cohort definition ID for non-early PVI patients (required, no default)
#' @param file_path Optional path to save results as CSV (default: NULL - no file saved)
#' @param outcome_window Days after cohort start to begin tracking outcomes (default: 183)
#' @param end_date End date for follow-up period if death not recorded (default: "2022-12-31")
#'
#' @return A data frame containing the analysis results
#' @export
pvi_outcome_analysis <- function(conn,
                                 schema,
                                 early_cohort_id,
                                 non_early_cohort_id,
                                 file_path = NULL,
                                 outcome_window = 183,
                                 end_date = "2022-12-31") {

  # Get patient data
  patient_data <- get_patient_outcomes(
    conn = conn,
    schema = schema,
    early_cohort_id = early_cohort_id,
    non_early_cohort_id = non_early_cohort_id,
    outcome_window = outcome_window,
    end_date = end_date
  )

  # Analyze the patient data
  results <- analyze_pvi_outcomes(
    patient_data = patient_data,
    file_path = file_path
  )

  return(results)
}
