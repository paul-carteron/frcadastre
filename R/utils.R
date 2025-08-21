#' Check if a URL is accessible
#'
#' This helper function tests whether a given URL can be opened.
#' It attempts to open a connection and returns TRUE if successful,
#' or FALSE if the connection fails (error or warning).
#'
#' @param url Character string, the URL to test.
#'
#' @return Logical scalar: TRUE if the URL is accessible, FALSE otherwise.
#'
#' @examples
#' .url_exists("https://www.r-project.org")  # should return TRUE
#' .url_exists("https://nonexistent.example.com")  # should return FALSE
#'
#' @noRd
#'
.url_exists <- function(url) {
  ok <- tryCatch({
    # Build request and send HEAD
    resp <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_timeout(5) |>
      httr2::req_perform()

    # Check HTTP status code
    httr2::resp_status(resp) < 400
  },
  error = function(e) FALSE,
  warning = function(w) FALSE
  )
  ok
}
