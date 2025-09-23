#' Retrieve GBIF occurrence data for a taxon name with error handling
#'
#' This function queries the GBIF occurrence API using a full-text 'q=' search,
#' similar to the GBIF web portal. It retrieves all available occurrence records
#' up to a specified maximum number. Results are paginated and combined into a
#' single data frame. If the API fails repeatedly or returns a partial transfer,
#' the taxon is skipped and NULL is returned.
#'
#' @param name A taxon name (genus, species, or other free-text string) to search for.
#' @param max_records Maximum number of occurrence records to retrieve. Default is 1000.
#' @param max_retries Maximum number of retry attempts in case of connection failure. Default is 3.
#' @param wait_seconds Number of seconds to wait between retries. Default is 2.
#'
#' @return A data frame containing occurrence records from GBIF, or NULL if no data is found or if the query fails.
#'
#' @examples
#' \dontrun{
#'   data <- get_gbif_data("Lens", max_records = 1000)
#'   head(data)
#' }

get_gbif_data <- function(name, max_records = 1000, max_retries = 3, wait_seconds = 2) {
  if (is.na(name) || name == "") return(NULL)

  all_results <- list()
  offset <- 0
  limit <- 300  # GBIF API max per page

  while (offset < max_records) {
    attempt <- 1
    success <- FALSE

    repeat {
      tryCatch({
        res <- httr::GET("https://api.gbif.org/v1/occurrence/search",
                         query = list(q = name, limit = limit, offset = offset),
                         httr::timeout(60))

        if (httr::status_code(res) == 200) {
          raw_content <- httr::content(res, as = "raw")
          text_content <- rawToChar(raw_content)
          parsed <- jsonlite::fromJSON(text_content, flatten = TRUE)

          all_results <- append(all_results, list(parsed$results))
          success <- TRUE
        } else {
          warning(sprintf("Attempt %d failed: HTTP %d", attempt, httr::status_code(res)))
        }
      }, error = function(e) {
        message(sprintf("API query failed for '%s' on attempt %d: %s", name, attempt, e$message))
      })

      if (success || attempt >= max_retries) break
      Sys.sleep(wait_seconds + runif(1, 0, 2))  # Random delay to reduce load
      attempt <- attempt + 1
    }

    if (!success) {
      message(sprintf("Skipping '%s' due to repeated failures.", name))
      return(NULL)
    }

    offset <- offset + limit
    if (length(parsed$results) < limit) break  # No more pages
  }

  if (length(all_results) == 0) return(NULL)
  df <- dplyr::bind_rows(all_results)
  df$lookup_name <- name  # Add original query name
  return(df)
}
