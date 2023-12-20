#' Logistic Regression Analysis on Trial Completion for Elderly Trials
#'
#' Performs logistic regression to analyze factors influencing completion rates, focusing on elderly participation.
#'
#' @param studies Data frame from the Studies table.
#' @param eligibilities Data frame from the Eligibilities table.
#' @return A summary of the logistic regression model.
#' @importFrom dplyr %>%
#' @importFrom stats glm binomial
#' @export
analyze_completion_logistic_regression <- function(studies, eligibilities) {
  combined_data <- merge(studies, eligibilities, by = "nct_id")

  model_data <- combined_data %>%
    mutate(completion_status = as.numeric(overall_status == "Completed"),
           is_elderly = as.numeric(older_adult == TRUE),
           phase_factor = as.factor(phase),
           enrollment_log = log(enrollment + 1))

  model <- glm(completion_status ~ phase_factor + enrollment_log + is_elderly,
               data = model_data, family = binomial())

  return(model)
}
