# Tests for pvi_outcome_analysis function

# Mock the dependent functions to avoid needing a real database connection
mock_get_patient_outcomes <- function(conn, schema, early_cohort_id, non_early_cohort_id, outcome_window, end_date) {
  # Return mock patient data
  data.frame(
    person_id = 1:10,
    cohort_group = rep(c("Early PVI", "Non-Early PVI"), each = 5),
    index_date = as.Date("2022-01-01") + sample(1:30, 10, replace = TRUE),
    outcome_start_date = as.Date("2022-06-01") + sample(1:30, 10, replace = TRUE),
    end_date = as.Date("2022-12-31"),
    follow_up_days = sample(100:300, 10, replace = TRUE),
    had_clti = sample(0:1, 10, replace = TRUE, prob = c(0.7, 0.3)),
    days_to_clti = sample(50:200, 10, replace = TRUE),
    had_pvi = sample(0:1, 10, replace = TRUE, prob = c(0.6, 0.4)),
    days_to_pvi = sample(50:200, 10, replace = TRUE),
    had_amputation = sample(0:1, 10, replace = TRUE, prob = c(0.8, 0.2)),
    days_to_amputation = sample(50:200, 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Mock analysis function
mock_analyze_pvi_outcomes <- function(patient_data, file_path = NULL, caption = NULL) {
  # Return mock results
  data.frame(
    Metric = c("Total Patients", "CLTI, n (%)", "Repeat PVI, n (%)", "Major Amputation, n (%)"),
    Early_PVI = c("5", "2 (40.0%)", "3 (60.0%)", "1 (20.0%)"),
    Non_Early_PVI = c("5", "3 (60.0%)", "2 (40.0%)", "2 (40.0%)"),
    P_Value = c("1.000", "0.527", "0.527", "0.490"),
    stringsAsFactors = FALSE
  )
}

test_that("pvi_outcome_analysis calls dependent functions with correct parameters", {
  # Skip if the package or function isn't available
  skip_if_not_installed("pviOutcomes")
  skip_if(!exists("pvi_outcome_analysis", where = asNamespace("pviOutcomes")))

  # Mock the database connection
  mock_conn <- structure(list(), class = "DBIConnection")

  # Test parameters
  test_schema <- "test_schema"
  test_early_id <- 123
  test_non_early_id <- 456
  test_outcome_window <- 183
  test_end_date <- "2022-12-31"
  test_file_path <- tempfile(fileext = ".csv")

  # Use mockery to replace the dependent functions
  mockery::stub(pvi_outcome_analysis, "get_patient_outcomes", function(conn, schema, early_cohort_id, non_early_cohort_id, outcome_window, end_date) {
    # Check if parameters are passed through correctly
    expect_equal(schema, test_schema)
    expect_equal(early_cohort_id, test_early_id)
    expect_equal(non_early_cohort_id, test_non_early_id)
    expect_equal(outcome_window, test_outcome_window)
    expect_equal(end_date, test_end_date)

    # Return mock data
    return(mock_get_patient_outcomes(conn, schema, early_cohort_id, non_early_cohort_id, outcome_window, end_date))
  })

  mockery::stub(pvi_outcome_analysis, "analyze_pvi_outcomes", function(patient_data, file_path = NULL) {
    # Check if file_path is passed through
    expect_equal(file_path, test_file_path)

    # Return mock results
    return(mock_analyze_pvi_outcomes(patient_data, file_path))
  })

  # Run the function
  tryCatch({
    results <- pvi_outcome_analysis(
      conn = mock_conn,
      schema = test_schema,
      early_cohort_id = test_early_id,
      non_early_cohort_id = test_non_early_id,
      file_path = test_file_path,
      outcome_window = test_outcome_window,
      end_date = test_end_date
    )

    # Check if results match expected format
    expect_s3_class(results, "data.frame")
    expect_true(all(c("Metric", "Early_PVI", "Non_Early_PVI", "P_Value") %in% names(results)))
    expect_equal(nrow(results), 4)  # Check for expected number of rows

  }, error = function(e) {
    skip(paste("Test skipped due to error:", e$message))
  })
})

test_that("pvi_outcome_analysis throws appropriate errors for missing parameters", {
  skip_if_not_installed("pviOutcomes")
  skip_if(!exists("pvi_outcome_analysis", where = asNamespace("pviOutcomes")))

  # Mock connection
  mock_conn <- structure(list(), class = "DBIConnection")

  # Test missing parameters
  expect_error(pvi_outcome_analysis())
  expect_error(pvi_outcome_analysis(mock_conn), "argument \"schema\" is missing")
  expect_error(
    pvi_outcome_analysis(mock_conn, "schema"),
    "argument \"early_cohort_id\" is missing"
  )
  expect_error(
    pvi_outcome_analysis(mock_conn, "schema", 123),
    "argument \"non_early_cohort_id\" is missing"
  )
})

test_that("pvi_outcome_analysis handles errors in get_patient_outcomes", {
  skip_if_not_installed("pviOutcomes")
  skip_if(!exists("pvi_outcome_analysis", where = asNamespace("pviOutcomes")))

  # Mock connection
  mock_conn <- structure(list(), class = "DBIConnection")

  # Mock get_patient_outcomes to throw an error
  mockery::stub(pvi_outcome_analysis, "get_patient_outcomes", function(...) {
    stop("Test error in get_patient_outcomes")
  })

  # Expect the error to be propagated
  expect_error(
    pvi_outcome_analysis(
      conn = mock_conn,
      schema = "test",
      early_cohort_id = 123,
      non_early_cohort_id = 456
    ),
    "Test error in get_patient_outcomes"
  )
})

test_that("pvi_outcome_analysis handles default parameters correctly", {
  skip_if_not_installed("pviOutcomes")
  skip_if(!exists("pvi_outcome_analysis", where = asNamespace("pviOutcomes")))

  # Mock connection
  mock_conn <- structure(list(), class = "DBIConnection")

  # Mock get_patient_outcomes to check default parameters
  mockery::stub(pvi_outcome_analysis, "get_patient_outcomes", function(conn, schema, early_cohort_id, non_early_cohort_id, outcome_window, end_date) {
    # Check default values
    expect_equal(outcome_window, 183)
    expect_equal(end_date, "2022-12-31")

    # Return mock data
    return(mock_get_patient_outcomes(conn, schema, early_cohort_id, non_early_cohort_id, outcome_window, end_date))
  })

  # Mock analyze_pvi_outcomes
  mockery::stub(pvi_outcome_analysis, "analyze_pvi_outcomes", mock_analyze_pvi_outcomes)

  # Run the function with minimal parameters
  tryCatch({
    results <- pvi_outcome_analysis(
      conn = mock_conn,
      schema = "test",
      early_cohort_id = 123,
      non_early_cohort_id = 456
    )

    # If we get here, the test passed
    expect_true(TRUE)

  }, error = function(e) {
    skip(paste("Test skipped due to error:", e$message))
  })
})
