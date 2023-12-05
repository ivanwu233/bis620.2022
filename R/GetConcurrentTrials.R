#' Get the Number of Concurrent Trials for Each Date in a Set of Studies
#'
#' This function computes the number of concurrent clinical trials for each date within a given set of studies.
#' It is particularly useful for understanding the distribution and overlap of clinical trials over time.
#'
#' @param d The studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#'         concurrent trials at that date.
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select distinct arrange rename
#' @importFrom purrr map_dbl
#' @export

get_concurrent_trials <- function(d) {
  # Get all of the unique dates.
  all_dates = d |>
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |>
    arrange(value) |>
    na.omit() |>
    rename(date = value)

  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }

  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count =
    map_dbl(
      all_dates$date,
      ~ .x |>
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}
