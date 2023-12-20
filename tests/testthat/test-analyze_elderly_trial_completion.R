test_that("analyze_elderly_trial_completion returns correct analysis", {
  # Dynamically set the path for the sliced DuckDB file within the package
  db_path <- system.file("extdata", "sliced_db.duckdb", package = "bis620.2023")

  # Connect to the database using the dynamically set path
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown=TRUE), add = TRUE)

  studies_data <- DBI::dbGetQuery(con, "SELECT * FROM Studies;")
  eligibilities_data <- DBI::dbGetQuery(con, "SELECT * FROM Eligibilities WHERE older_adult = TRUE;")

  result <- bis620.2023::analyze_elderly_trial_completion(studies_data, eligibilities_data)

  expect_true("data.frame" %in% class(result))
  expect_true("phase" %in% names(result))
  expect_true("completed_proportion" %in% names(result))
})
