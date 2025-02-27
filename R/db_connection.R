#' Create Database Connection
#'
#' Creates a connection to the database using either DBI or DatabaseConnector.
#' This function is a wrapper around DatabaseConnector::createConnectionDetails when use_dbi=FALSE.
#'
#' @param dbms Character string specifying the database management system (e.g., "sql server", "postgresql")
#' @param server Server name
#' @param user Username for database connection
#' @param password Password for database connection
#' @param port Port number (default depends on dbms)
#' @param database Database name (optional)
#' @param use_dbi Whether to use DBI (TRUE) or DatabaseConnector (FALSE) (default: TRUE)
#' @param driver Driver name for ODBC/DBI connections (default: auto-detect based on dbms)
#' @param pathToDriver Path to JDBC driver files for DatabaseConnector (required if use_dbi=FALSE)
#' @param extraSettings Additional configuration settings for the connection (optional)
#' @param oracleDriver Oracle driver type for Oracle connections (optional)
#'
#' @return A database connection object
#' @export
#' @importFrom DBI dbConnect
create_db_connection <- function(dbms,
                                 server,
                                 user,
                                 password,
                                 port = NULL,
                                 database = NULL,
                                 use_dbi = TRUE,
                                 driver = NULL,
                                 pathToDriver = NULL,
                                 extraSettings = NULL,
                                 oracleDriver = NULL) {

  # Set default ports based on dbms if not provided
  if (is.null(port)) {
    port <- switch(tolower(dbms),
                   "sql server" = 1433,
                   "postgresql" = 5432,
                   "oracle" = 1521,
                   NULL)
  }

  # Set default driver based on dbms if not provided
  if (is.null(driver) && use_dbi) {
    driver <- switch(tolower(dbms),
                     "sql server" = "SQL Server",
                     "postgresql" = "PostgreSQL ODBC Driver",
                     "oracle" = "Oracle",
                     NULL)
  }

  if (use_dbi) {
    # Using DBI
    if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("odbc", quietly = TRUE)) {
      stop("The DBI and odbc packages are required for DBI connections. Please install them with install.packages(c('DBI', 'odbc'))")
    }

    conn_args <- list(
      drv = odbc::odbc(),
      Driver = driver,
      Server = server,
      UID = user,
      PWD = password
    )

    # Add port if provided
    if (!is.null(port)) {
      conn_args$Port <- port
    }

    # Add database if provided
    if (!is.null(database)) {
      conn_args$Database <- database
    }

    # Create the connection
    conn <- do.call(DBI::dbConnect, conn_args)

  } else {
    # Using DatabaseConnector
    if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
      stop("The DatabaseConnector package is required for DatabaseConnector connections. Please install it with install.packages('DatabaseConnector')")
    }

    if (is.null(pathToDriver)) {
      stop("pathToDriver is required for DatabaseConnector connections")
    }

    # Set environment variable for driver location
    Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = pathToDriver)

    # Use createConnectionDetails directly from the DatabaseConnector package
    connectionDetails <- DatabaseConnector::createConnectionDetails(
      dbms = dbms,
      server = server,
      user = user,
      password = password,
      port = port,
      pathToDriver = pathToDriver,
      extraSettings = extraSettings,
      oracleDriver = oracleDriver
    )

    # Connect to the database
    conn <- DatabaseConnector::connect(connectionDetails)
  }

  return(conn)
}

#' Close Database Connection
#'
#' Closes a database connection.
#'
#' @param conn A database connection object
#' @param use_dbi Whether the connection was created with DBI (TRUE) or DatabaseConnector (FALSE)
#'
#' @return NULL
#' @export
#' @importFrom DBI dbDisconnect
close_db_connection <- function(conn, use_dbi = TRUE) {
  if (use_dbi) {
    # Using DBI
    if (!requireNamespace("DBI", quietly = TRUE)) {
      stop("The DBI package is required for DBI connections. Please install it with install.packages('DBI')")
    }

    DBI::dbDisconnect(conn)
  } else {
    # Using DatabaseConnector
    if (!requireNamespace("DatabaseConnector", quietly = TRUE)) {
      stop("The DatabaseConnector package is required for DatabaseConnector connections. Please install it with install.packages('DatabaseConnector')")
    }

    DatabaseConnector::disconnect(conn)
  }
}
