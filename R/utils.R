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

#' Read a GeoJSON file (optionally gzipped) and extract a clean name
#'
#' Reads a GeoJSON file and returns a list with a simplified name and the spatial data.
#' Automatically decompresses gzipped files if needed.
#'
#' @param f `character`. Path to the GeoJSON or gzipped GeoJSON file.
#' @return A list with components:
#'   \item{name}{Simplified name extracted from the file name.}
#'   \item{data}{`sf` object with the spatial data.}
#'
#' @noRd
#'
.read_geojson_file <- function(f) {
  # Raw name without extensions
  name <- basename(f)
  name <- sub("\\.gz$", "", name, ignore.case = TRUE)
  name <- sub("\\.(geojson|json)$", "", name, ignore.case = TRUE)

  # Keep only what comes after the last "-"
  name <- sub(".*-", "", name)

  # If gzipped -> decompress to a temporary file
  if (grepl("\\.gz$", f, ignore.case = TRUE)) {
    tmp <- tempfile(fileext = ".geojson")
    R.utils::gunzip(f, destname = tmp, remove = FALSE, overwrite = TRUE)
    f <- tmp
  }

  list(name = name, data = sf::st_read(f, quiet = TRUE))
}
