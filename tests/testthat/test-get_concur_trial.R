test_trials <- data.frame(
  start_date = as.Date(c("2020-01-01", "2020-06-01")),
  completion_date = as.Date(c("2021-01-01", "2021-06-01")),
  stringsAsFactors = FALSE
)

test_that("get_concurrent_trials calculates concurrent trials correctly", {
  result <- get_concurrent_trials(test_trials)
  expect_true(all(!is.na(result))) # Assuming the function handles NA values
})
