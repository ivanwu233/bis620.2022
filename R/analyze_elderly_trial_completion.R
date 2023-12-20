#' Analyze Factors Influencing Completion Rates in Elderly Trials
#'
#' This function combines data from Studies and Eligibilities tables to analyze how
#' different factors like phase, enrollment, and intervention type affect
#' the completion rates of clinical trials involving elderly participants.
#'
#' @param studies_data Data frame containing data from the Studies table.
#' @param eligibilities_data Data frame containing data from the Eligibilities table.
#' @return A data frame summarizing the impact of different factors on completion rates.
#' @export
analyze_elderly_trial_completion <- function(studies_data, eligibilities_data) {
  # Join studies data with eligibility data on nct_id
  combined_data <- merge(studies_data, eligibilities_data, by = "nct_id")

  completion_analysis <- combined_data %>%
    group_by(phase) %>%
    dplyr::summarize(completed_proportion = mean(overall_status == "Completed", na.rm = TRUE))

  return(completion_analysis)
}
