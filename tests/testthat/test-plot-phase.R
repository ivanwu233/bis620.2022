library(ggplot2)

test_studies <- data.frame(
  phase = c("Phase 1", "Phase 2", "Phase 3"),
  count = c(10, 20, 30),
  stringsAsFactors = FALSE
)

test_that("plot_phase_histogram returns a ggplot object", {
  result_plot <- plot_phase_histogram(test_studies)
  expect_true(is.ggplot(result_plot))
})
