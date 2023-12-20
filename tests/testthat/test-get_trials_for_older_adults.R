test_that("get_trials_for_older_adults returns a data frame with expected columns", {
  # Dynamically set the path for the sliced DuckDB file within the package
  db_path <- system.file("extdata", "sliced_db.duckdb", package = "bis620.2023")

  # Connect to the database using the dynamically set path
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown=TRUE), add = TRUE)

  result <- bis620.2023::get_trials_for_older_adults(con)

  expect_true("data.frame" %in% class(result))
  expect_true(all(c("nct_id", "study_type", "brief_title") %in% names(result)))
})
