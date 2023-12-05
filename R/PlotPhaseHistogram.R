#' Create a Histogram of the Phases with a Uniform X-axis
#'
#' This function generates a histogram of the study phases from the clinical trials database.
#' It ensures that the x-axis of the histogram always displays a consistent set of phase values,
#' regardless of the data returned by the query. This uniformity allows for easier comparison
#' across different queries.
#'
#' @param x A tibble containing the study phases to be plotted.
#' @return A ggplot object representing the histogram with a fixed set of phase values on the x-axis.
#' @importFrom ggplot2 ggplot aes geom_col scale_x_discrete theme_bw xlab ylab
#' @importFrom dplyr mutate count
#' @importFrom tidyr as_tibble complete replace_na
#' @export

plot_phase_histogram <- function(x) {
  x <- as_tibble(x)
  x <- x %>% mutate(phase = replace_na(phase, "NA"))
  phase_levels <- c("Not Applicable", "Early Phase 1", "Phase 1", "Phase 1/Phase 2",
                    "Phase 2", "Phase 2/Phase 3", "Phase 3", "Phase 4", "NA")
  phase_counts <- x %>%
    count(phase = factor(phase, levels = phase_levels)) %>%
    complete(phase = phase_levels, fill = list(n = 0))
  ggplot(phase_counts, aes(x = phase, y = n)) +
    geom_col() +
    scale_x_discrete(limits = phase_levels) +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}
