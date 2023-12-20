#' Plot Completion Status by Phase for Elderly Trials
#'
#' This function plots the completion status of clinical trials involving elderly participants, grouped by trial phase.
#'
#' @param studies Data frame from the Studies table.
#' @param eligibilities Data frame from the Eligibilities table.
#' @importFrom ggplot2 ggplot aes geom_bar stat_identity position_dodge labs theme_minimal
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_longer
#' @export

plot_elderly_trial_completion_by_phase <- function(studies, eligibilities) {
  # Merging Studies with Eligibilities
  combined_data <- merge(studies, eligibilities, by = "nct_id")

  # Filter to include only trials with elderly participants
  elderly_trials <- combined_data[combined_data$older_adult == TRUE, ]

  # Data preparation for plot
  plot_data <- elderly_trials %>%
    group_by(phase) %>%
    summarise(Completed = sum(overall_status == "Completed"),
              Not_Completed = sum(overall_status != "Completed")) %>%
    pivot_longer(cols = c("Completed", "Not_Completed"), names_to = "Status", values_to = "Count")

  # Generating the bar plot
  ggplot(plot_data, aes(x = phase, y = Count, fill = Status)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "Completion Status by Phase for Elderly Trials",
         x = "Trial Phase",
         y = "Number of Trials",
         fill = "Completion Status") +
    theme_minimal()
}
