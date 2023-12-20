test_that("analyze_completion_logistic_regression returns valid model summary", {
  # Create mock data
  studies_data <- data.frame(
    nct_id = paste("NCT", 1001:1010, sep = ""),
    overall_status = sample(c("Completed", "Not Completed"), 10, replace = TRUE),
    phase = factor(sample(c("Phase 1", "Phase 2", "Phase 3"), 10, replace = TRUE)),
    enrollment = sample(1:100, 10)
  )

  eligibilities_data <- data.frame(
    nct_id = paste("NCT", 1001:1010, sep = ""),
    older_adult = sample(c(TRUE, FALSE), 10, replace = TRUE)
  )

  # Run the function with mock data
  model <- bis620.2023::analyze_completion_logistic_regression(studies_data, eligibilities_data)
  model_summary <- summary(model)  # Summarize the model

  # Assertions to check the validity of the model
  expect_true("glm" %in% class(model))  # Check the class of the model
  expect_true("coefficients" %in% names(model_summary))
  expect_true(any(grepl("phase_factor", rownames(model_summary$coefficients))))
  expect_true(any(grepl("enrollment_log", rownames(model_summary$coefficients))))
  expect_true(any(grepl("is_elderly", rownames(model_summary$coefficients))))
})
