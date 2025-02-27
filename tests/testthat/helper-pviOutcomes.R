# Helper functions for pviOutcomes tests

#' Create mock database connection for testing
#'
#' @return A mock DBI connection object
create_mock_connection <- function() {
  structure(list(), class = "DBIConnection")
}

#' Create mock patient data for tests
#'
#' @param n_early Number of Early PVI patients
#' @param n_non_early Number of Non-Early PVI patients
#' @param seed Random seed for reproducibility
#'
#' @return A data frame with mock patient data
create_mock_patient_data <- function(n_early = 5, n_non_early = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Base data
  data.frame(
    person_id = 1:(n_early + n_non_early),
    cohort_group = c(rep("Early PVI", n_early), rep("Non-Early PVI", n_non_early)),
    index_date = as.Date("2022-01-01") + 1:(n_early + n_non_early),
    outcome_start_date = as.Date("2022-06-01") + 1:(n_early + n_non_early),
    end_date = as.Date("2022-12-31"),
    follow_up_days = sample(100:300, n_early + n_non_early, replace = TRUE),
    had_clti = sample(0:1, n_early + n_non_early, replace = TRUE, prob = c(0.7, 0.3)),
    days_to_clti = sample(50:200, n_early + n_non_early, replace = TRUE),
    had_pvi = sample(0:1, n_early + n_non_early, replace = TRUE, prob = c(0.6, 0.4)),
    days_to_pvi = sample(50:200, n_early + n_non_early, replace = TRUE),
    had_amputation = sample(0:1, n_early + n_non_early, replace = TRUE, prob = c(0.8, 0.2)),
    days_to_amputation = sample(50:200, n_early + n_non_early, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

#' Skip tests if package is not installed or function doesn't exist
#'
#' @param func_name Name of the function to check
#'
#' @return Invisible NULL, but will skip the test if conditions aren't met
skip_if_function_unavailable <- function(func_name) {
  skip_if_not_installed("pviOutcomes")
  skip_if(!exists(func_name, where = asNamespace("pviOutcomes")))
  invisible(NULL)
}
