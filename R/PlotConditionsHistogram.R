#' Plot Conditions Histogram
#'
#' This function takes a data frame that includes clinical trial conditions and
#' generates a histogram displaying the frequency of each condition. The conditions
#' are ordered by the count, ensuring that the most frequent conditions are listed first.
#'
#' @param x A data frame containing the clinical trial data with a 'name' column
#'          that holds the condition names. This data frame is typically the result
#'          of a join operation between the 'studies' table and the 'conditions' table
#'          in the clinical trials database.
#' @return A ggplot object representing the histogram of conditions.
#'         The histogram will display conditions on the y-axis and the count of trials
#'         associated with each condition on the x-axis.
#' @export
#' @importFrom ggplot2 ggplot aes geom_col theme_bw xlab ylab coord_flip
#' @importFrom stats reorder

plot_conditions_histogram <- function(x) {
  if (!("name" %in% names(x)) || !("count" %in% names(x))) {
    stop("The 'name' and/or 'count' column was not found in the provided data.")
  }

  if (length(x$name) != length(x$count)) {
    stop("Mismatch in lengths of 'name' and 'count' columns.")
  }

  ggplot(x, aes(x = reorder(name, count), y = count)) +
    geom_col() +
    theme_bw() +
    xlab("Condition") +
    ylab("Number of Trials") +
    coord_flip()
}
