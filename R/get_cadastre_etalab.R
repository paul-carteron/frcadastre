' Download and read cadastre data from Etalab for a given INSEE code
#'
#' This function retrieves URLs for cadastre datasets from Etalab based on a
#' given INSEE code and optional dataset filters. It downloads and extracts
#' the data into a specified directory, then reads all extracted GeoJSON files
#' as `sf` objects. When multiple files correspond to the same dataset,
#' their contents are aggregated into a single `sf` object with harmonized
#' columns. The result is returned as a named list of `sf` objects.
#'
#' @param insee_code `character`
#' The INSEE code for the commune or administrative unit to download cadastre data for.
#'
#' @param data `character` (vector) or `NULL`
#' Specific datasets to download (e.g., `"parcelle"`, `"numvoie"`).
#' If `NULL`, all datasets available for the given INSEE code are retrieved.
#'
#' @param extract_dir `character` or `NULL`
#' Path to the directory where downloaded files should be extracted.
#' If `NULL`, a unique temporary directory is created automatically.
#'
#' @param overwrite `logical` (default: `TRUE`)
#' Whether to overwrite existing files in `extract_dir`.
#'
#' @param verbose `logical` (default: `FALSE`)
#' If `TRUE`, warnings about missing URLs, or failed downloads are displayed.
#' If `FALSE`, the function fails silently and returns `NULL` when issues occur.
#'
#' @param ... Additional arguments passed to [etalab_get_data_urls()].
#'
#' @return A named list of `sf` objects containing the downloaded and read cadastre datasets.
#' Each list element corresponds to a dataset (e.g., `"parcelle"`, `"numvoie"`).
#' If multiple files correspond to the same dataset, they are combined and returned
#' as a single `sf` object with unified columns.
#'
#' @details
#' The function follows these steps:
#' 1. Retrieve the list of download URLs from Etalab using [etalab_get_data_urls()].
#' 2. Download and extract the files into `extract_dir` using [cdg_download_archives()].
#' 3. Read all extracted GeoJSON files with [read_geojson()], which aggregates them
#'    by dataset name and ensures consistent column structure across files.
#'
#' If `extract_dir` is not provided, a unique temporary directory is created
#' (`tempfile(pattern = "cadastre_extract_")`). This prevents conflicts
#' between multiple calls and keeps downloaded data isolated.
#'
#' @examples
#' \dontrun{
#' # Download and read all cadastre datasets for INSEE code 72188
#' cadastre_data <- get_cadastre_etalab("72188")
#'
#' # Download only parcelle and numvoie datasets, with a fixed extraction directory
#' cadastre_data <- get_cadastre_etalab(
#'   "72188",
#'   data = c("parcelle", "numvoie"),
#'   extract_dir = "data/cadastre"
#' )
#' }
#'
#' @export
#'
get_cadastre_etalab <- function(insee_code,
                                data = NULL,
                                extract_dir = NULL,
                                overwrite = TRUE,
                                verbose = TRUE,
                                ...) {

  .war <- function(...) if (isTRUE(verbose)) warning(...)

  # 1. Build URL table
  urls_df <- etalab_get_data_urls(insee_code, data = data, ...) |>
    etalab_filter_existing_urls(verbose = verbose)

  if (nrow(urls_df) == 0) {
    return(NULL)
  }

  # Create temporary directory if not specified
  if (is.null(extract_dir)) {
    extract_dir <- tempfile(pattern = "cadastre_extract_")
    dir.create(extract_dir, recursive = TRUE)
  }

  # Build destination file names
  urls_df$destfile <- file.path(
    extract_dir,
    paste0("pci-", urls_df$insee_code, "-", urls_df$data, ".json.gz")
  )

  # 2. Download and extract (with httr2 inside cdg_download_archives)
  download_results <- tryCatch(
    cdg_download_archives(
      urls = urls_df$url,
      destfiles = urls_df$destfile,
      extract_dir = extract_dir,
      overwrite = overwrite,
      use_subdirs = FALSE,
      verbose = verbose
    ),
    error = function(e) {
      .war("Download step failed: ", conditionMessage(e))
      return(NULL)
    }
  )

  # Remove failed downloads
  if (is.null(download_results)) {
    return(NULL)
  }

  # 3. Read only valid GeoJSON files
  sf_data <- tryCatch(
    read_geojson(extract_dir),
    error = function(e) {
      .war("Failed to read GeoJSON: ", conditionMessage(e))
      return(NULL)
    }
  )

  return(sf_data)
}
