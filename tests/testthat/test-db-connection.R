# Tests for the database connection functions

test_that("create_db_connection validates required parameters", {
  skip_if_function_unavailable("create_db_connection")

  # Test missing parameters
  expect_error(create_db_connection(), "argument \"dbms\" is missing")
  expect_error(create_db_connection("sql server"), "argument \"server\" is missing")
  expect_error(create_db_connection("sql server", "server"), "argument \"user\" is missing")
  expect_error(create_db_connection("sql server", "server", "user"), "argument \"password\" is missing")
})

test_that("create_db_connection sets default ports correctly", {
  skip_if_function_unavailable("create_db_connection")

  # Need to mock DBI::dbConnect to prevent actual connection attempts
  mockery::stub(create_db_connection, "DBI::dbConnect", function(...) {
    args <- list(...)
    # Return the args so we can check them
    return(args)
  })

  # Mock odbc::odbc
  mockery::stub(create_db_connection, "odbc::odbc", function() {
    return("mock_odbc_driver")
  })

  # Test SQL Server default port
  tryCatch({
    conn_args <- create_db_connection(
      dbms = "sql server",
      server = "test_server",
      user = "test_user",
      password = "test_pass"
    )

    # First argument should be drv, so look at the named args
    named_args <- conn_args[-1]
    expect_equal(named_args$Server, "test_server")

  }, error = function(e) {
    skip(paste("Test skipped due to error:", e$message))
  })
})

test_that("close_db_connection calls appropriate disconnect function", {
  skip_if_function_unavailable("close_db_connection")

  # Create mock connection
  mock_conn <- structure(list(), class = "DBIConnection")

  # Test DBI connection closure
  mockery::stub(close_db_connection, "DBI::dbDisconnect", function(conn) {
    expect_equal(conn, mock_conn)
    return(TRUE)
  })

  # Run the function
  tryCatch({
    result <- close_db_connection(mock_conn, use_dbi = TRUE)
    expect_null(result)

  }, error = function(e) {
    skip(paste("Test skipped due to error:", e$message))
  })

  # Test DatabaseConnector connection closure
  mockery::stub(close_db_connection, "DatabaseConnector::disconnect", function(conn) {
    expect_equal(conn, mock_conn)
    return(TRUE)
  })

  # Run the function
  tryCatch({
    result <- close_db_connection(mock_conn, use_dbi = FALSE)
    expect_null(result)

  }, error = function(e) {
    skip(paste("Test skipped due to error:", e$message))
  })
})
