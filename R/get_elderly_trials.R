#' Extract Trials Involving Elderly Participants
#'
#' Retrieves clinical trials from the `ctgov.duckdb` database that are explicitly marked
#' as involving older adults.
#'
#' @param con A database connection object.
#' @return A data frame of trials involving older adults.
#' @export
get_trials_for_older_adults <- function(con) {
  query <- "SELECT * FROM studies WHERE nct_id IN (SELECT nct_id FROM eligibilities WHERE older_adult = TRUE)"
  trials <- DBI::dbGetQuery(con, query)
  return(trials)
}
