# pviOutcomes
An R package for analyzing outcomes in patients with early versus non-early Peripheral Vascular Intervention (PVI) treatments.

# Overview
The pviOutcomes package provides a comprehensive toolset for analyzing patient outcomes following peripheral vascular interventions. It connects to OMOP CDM databases, extracts patient cohorts, and performs statistical analysis to compare early versus non-early intervention strategies. Key outcomes analyzed include:

* Chronic Limb-Threatening Ischemia (CLTI)
* Repeat PVI rates
* Major amputation rates
* Time-to-event metrics

# Installation
You can install the development version of the package from GitHub:

```{r}
# Install devtools if not already installed
install.packages("devtools")

# Install from GitHub
devtools::install_github("hlee110123/pviOutcomes")
```
# How to run

# 1. Create a Database Connection
First, establish a connection to your OMOP CDM database:

```{r}
library(pviOutcomes)
library(DBI)
library(odbc)

# Connect using DBI/odbc

conn <- create_db_connection(
  dbms = "your dbms",
  server = "your_server",
  user = "your_username",
  password = "your_password",
  database = "your_database"
)

# Or connect using DatabaseConnector
# conn <- create_db_connection(
#   dbms = "your dbms",
#   server = "your_server",
#   user = "your_username",
#   password = "your_password",
#   use_dbi = FALSE,
#   pathToDriver = "/path/to/jdbc/drivers"
# )
```

# 2. Run the Entire Analysis in One Step

```{r}
# Run the complete analysis
results <- pvi_outcome_analysis(
  conn = conn, 
  schema = "your database schema", 
  early_cohort_id = #,      #  early PVI cohort ID
  non_early_cohort_id = #,  # non-early PVI cohort ID
  file_path = "pvi_outcome_results.csv"  # Optional: save results to CSV
)

# Print the results table
print(results)

```

# 3. Step-by-Step Analysis
you can run the analysis step by step

```{r}
# 1. Retrieve patient data
patient_data <- get_patient_outcomes(
  conn = conn,
  schema = "your database schema", 
  early_cohort_id = #,      #  early PVI cohort ID
  non_early_cohort_id = #,  # non-early PVI cohort ID
  outcome_window = 183,       # Days after cohort start to begin tracking outcomes
  end_date = "2022-12-31"     # End date for follow-up period
)

# 2. Analyze outcomes
results <- analyze_pvi_outcomes(
  patient_data = patient_data,
  file_path = "pvi_outcome_results.csv",  # Optional: save results to CSV
  caption = "LVC Table2 Caption"         # Optional: custom table caption
)
```
