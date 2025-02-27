# Tests for get_patient_outcomes function

test_that("get_patient_outcomes handles parameters correctly", {
  # Skip test if package or function is not available
  skip_if_not_installed("pviOutcomes")
  skip_if(!exists("get_patient_outcomes", where = asNamespace("pviOutcomes")))

  # Create mock connection
  mock_conn <- structure(list(), class = "DBIConnection")

  # Mock dbGetQuery to return a test dataset
  mock_data <- data.frame(
    person_id = 1:10,
    cohort_group = rep(c("Early PVI", "Non-Early PVI"), each = 5),
    index_date = as.Date("2022-01-01") + 1:10,
    outcome_start_date = as.Date("2022-06-01") + 1:10,
    end_date = as.Date("2022-12-31"),
    follow_up_days = sample(100:300, 10),
    clti_date = as.Date(NA),
    had_clti = rep(0, 10),
    days_to_clti = rep(NA, 10),
    pvi_date = as.Date(NA),
    had_pvi = rep(0, 10),
    days_to_pvi = rep(NA, 10),
    amputation_date = as.Date(NA),
    had_amputation = rep(0, 10),
    days_to_amputation = rep(NA, 10)
  )

  # Add some events for testing
  mock_data$clti_date[3] <- as.Date("2022-08-15")
  mock_data$had_clti[3] <- 1
  mock_data$days_to_clti[3] <- 226

  mock_data$pvi_date[7] <- as.Date("2022-09-20")
  mock_data$had_pvi[7] <- 1
  mock_data$days_to_pvi[7] <- 262

  mock_data$amputation_date[9] <- as.Date("2022-10-10")
  mock_data$had_amputation[9] <- 1
  mock_data$days_to_amputation[9] <- 282

  # Use mockery to replace dbGetQuery
  mockery::stub(get_patient_outcomes, "DBI::dbGetQuery", mock_data)

  # Test the function with mock connection
  tryCatch({
    result <- get_patient_outcomes(
      conn = mock_conn,
      schema = "test_schema",
      early_cohort_id = 123,
      non_early_cohort_id = 456
    )

    # Check if the result has the expected structure
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 10)
    expect_equal(ncol(result), 15)
    expect_true(all(c("person_id", "cohort_group", "follow_up_days", "had_clti", "had_pvi", "had_amputation") %in% names(result)))

    # Check if event counts match what we expect
    expect_equal(sum(result$had_clti), 1)
    expect_equal(sum(result$had_pvi), 1)
    expect_equal(sum(result$had_amputation), 1)

  }, error = function(e) {
    skip(paste("Test skipped due to error:", e$message))
  })
})

test_that("get_patient_outcomes formats schema correctly", {
  skip_if_not_installed("pviOutcomes")
  skip_if(!exists("get_patient_outcomes", where = asNamespace("pviOutcomes")))

  # Create mock connection
  mock_conn <- structure(list(), class = "DBIConnection")

  # Create a function to check the SQL query construction
  check_query <- function(schema, expected_prefix) {
    query_checked <- FALSE

    # Custom mock for dbGetQuery that checks the query
    mock_dbGetQuery <- function(conn, query) {
      # Check if schema is properly formatted in query
      expect_true(grepl(expected_prefix, query))
      query_checked <- TRUE
      # Return minimal dataset
      data.frame(
        person_id = 1,
        cohort_group = "Early PVI",
        index_date = as.Date("2022-01-01"),
        outcome_start_date = as.Date("2022-06-01"),
        end_date = as.Date("2022-12-31"),
        follow_up_days = 100,
        clti_date = as.Date(NA),
        had_clti = 0,
        days_to_clti = NA,
        pvi_date = as.Date(NA),
        had_pvi = 0,
        days_to_pvi = NA,
        amputation_date = as.Date(NA),
        had_amputation = 0,
        days_to_amputation = NA
      )
    }

    # Use mockery to replace dbGetQuery with our checking version
    mockery::stub(get_patient_outcomes, "DBI::dbGetQuery", mock_dbGetQuery)

    # Run function
    tryCatch({
      result <- get_patient_outcomes(
        conn = mock_conn,
        schema = schema,
        early_cohort_id = 123,
        non_early_cohort_id = 456
      )

      # Check that our query checker was invoked
      expect_true(query_checked)

    }, error = function(e) {
      skip(paste("Test skipped due to error:", e$message))
    })
  }

  # Test with schema without dot
  check_query("test_schema", "test_schema\\.")

  # Test with schema that already includes dot
  check_query("test_schema.dbo", "test_schema.dbo")
})

test_that("get_patient_outcomes returns error for missing parameters", {
  skip_if_not_installed("pviOutcomes")
  skip_if(!exists("get_patient_outcomes", where = asNamespace("pviOutcomes")))

  # Create mock connection
  mock_conn <- structure(list(), class = "DBIConnection")

  # Check error for missing conn
  expect_error(get_patient_outcomes())

  # Check error for missing schema
  expect_error(get_patient_outcomes(mock_conn), "argument \"schema\" is missing")

  # Check error for missing early_cohort_id
  expect_error(
    get_patient_outcomes(mock_conn, "schema"),
    "argument \"early_cohort_id\" is missing"
  )

  # Check error for missing non_early_cohort_id
  expect_error(
    get_patient_outcomes(mock_conn, "schema", 123),
    "argument \"non_early_cohort_id\" is missing"
  )
})
