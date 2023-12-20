test_that("plot_elderly_trial_completion_by_phase returns a ggplot", {
  # Creating mock data similar to Studies and Eligibilities tables
  studies_data <- data.frame(
    nct_id = c("NCT001", "NCT002", "NCT003", "NCT004", "NCT005"),
    overall_status = c("Completed", "Completed", "Terminated", "Completed", "Withdrawn"),
    phase = c("Phase 1", "Phase 2", "Phase 3", "Phase 1", "Phase 2")
  )

  eligibilities_data <- data.frame(
    nct_id = c("NCT001", "NCT002", "NCT003", "NCT004", "NCT005"),
    older_adult = c(TRUE, TRUE, TRUE, TRUE, TRUE)
  )

  plot <- bis620.2023::plot_elderly_trial_completion_by_phase(studies_data, eligibilities_data)

  expect_true("ggplot" %in% class(plot))
})
