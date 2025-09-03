#' Download and read cadastre data from Etalab for one or several INSEE codes
#'
#' This function retrieves URLs for cadastre datasets from Etalab based on
#' one or several INSEE codes and optional dataset filters. It downloads and
#' extracts the data into a specified directory, then reads all extracted
#' GeoJSON files as `sf` objects. When multiple files correspond to the same
#' dataset, their contents are aggregated into a single `sf` object with
#' harmonized columns. The result is returned as a named list of `sf` objects.
#'
#' @param insee_code `character` or `numeric`
#' One or more INSEE codes for the communes or administrative units to download cadastre data for.
#'
#' @param data `character`, `list` of `character`, or `NULL`
#' Specific datasets to download.
#' - If `NULL` (default), all available datasets are retrieved.
#' - If a character vector, the same datasets are retrieved for all `insee_code`s.
#' - If a list of character vectors, it must have the same length as `insee_code`,
#'   with each element specifying datasets for the corresponding code.
#'
#' Valid dataset names are taken from [cfg_get_data()], including both
#' `raw_data` (e.g. `"parcelle"`, `"numvoie"`) and `proc_data`
#' (e.g. `"parcelles"`, `"sections"`). An error is raised if an invalid name is provided.
#'
#' @param extract_dir `character` or `NULL`
#' Path to the directory where downloaded files should be extracted.
#' If `NULL`, a unique temporary directory is created automatically.
#'
#' @param overwrite `logical` (default: `TRUE`)
#' Whether to overwrite existing files in `extract_dir`.
#'
#' @param verbose `logical` (default: `TRUE`)
#' If `TRUE`, warnings about missing URLs or failed downloads are displayed.
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
#' 1. Validate and normalize the `data` argument (see **Details** above).
#' 2. Retrieve the list of download URLs from Etalab using [etalab_get_data_urls()].
#' 3. Download and extract the files into `extract_dir` using [cdg_download_archives()].
#' 4. Read all extracted GeoJSON files with [read_geojson()], which aggregates them
#'    by dataset name and ensures consistent column structure across files.
#'
#' If `extract_dir` is not provided, a unique temporary directory is created
#' (`tempfile(pattern = "cadastre_extract_")`). This prevents conflicts
#' between multiple calls and keeps downloaded data isolated.
#'
#' @examples
#' \dontrun{
#' # Download and read all datasets for one INSEE code
#' cadastre_data <- get_etalab_data("72188")
#'
#' # Download only parcelle and numvoie datasets, with a fixed extraction directory
#' cadastre_data <- get_etalab_data(
#'   "72188",
#'   data = c("parcelle", "numvoie"),
#'   extract_dir = "data/cadastre"
#' )
#'
#' # Different datasets per INSEE code
#' cadastre_data <- get_etalab_data(
#'   c("72187", "72188"),
#'   data = list(c("parcelle"), c("parcelle", "batiment"))
#' )
#' }
#'
#' @export
#'
get_etalab_data <- function(insee_code,
                            data = NULL,
                            extract_dir = NULL,
                            overwrite = TRUE,
                            verbose = TRUE,
                            ...) {

  .war <- function(...) if (isTRUE(verbose)) warning(...)

  # 1. Normalize `data` argument
  insee_code <- unique(as.character(insee_code))
  n <- length(insee_code)

  if (!is.null(data)) {
    if (!is.list(data)) {
      # If it's a single vector, replicate for all INSEE codes
      data <- rep(list(data), n)
    } else if (length(data) != n) {
      stop("If `data` is a list, it must have the same length as `insee_code`.")
    }

    valid_data <- unlist(cfg_get_data(), use.names = FALSE)
    all_values <- unique(unlist(data))
    bad_values <- setdiff(all_values, valid_data)

    if (length(bad_values) > 0) {
      stop(sprintf(
        "Invalid dataset(s) requested: %s\nValid options are: %s",
        paste(bad_values, collapse = ", "),
        paste(valid_data, collapse = ", ")
      ))
    }
  }

  # 2. Build URL table
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

  # 3. Download and extract (with httr2 inside cdg_download_archives)
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

  # 4. Read only valid GeoJSON files
  sf_data <- tryCatch(
    read_geojson(extract_dir),
    error = function(e) {
      .war("Failed to read GeoJSON: ", conditionMessage(e))
      return(NULL)
    }
  )

  return(sf_data)
}
