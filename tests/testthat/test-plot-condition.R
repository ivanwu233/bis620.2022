test_conditions <- data.frame(
  name = c("Condition X", "Condition Y", "Condition Z"),
  count = c(5, 15, 10),
  stringsAsFactors = FALSE
)

test_that("plot_conditions_histogram returns a ggplot object", {
  result_plot <- plot_conditions_histogram(test_conditions)
  expect_true(inherits(result_plot, "ggplot"))
})
