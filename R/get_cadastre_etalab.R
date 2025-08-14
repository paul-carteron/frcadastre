#' Download and read cadastre data from Etalab for a given INSEE code
#'
#' This function retrieves URLs for cadastre datasets from Etalab based on the
#' provided INSEE code and optional data types, downloads and extracts the files
#' into a specified directory, then reads all extracted GeoJSON files as `sf`
#' objects. The data are aggregated and returned as a named list of `sf` objects.
#'
#' @param insee_code `character`
#' the INSEE code for the commune or area to download data for.
#' @param data `character` (vector or `NULL`)
#' specific datasets to download (e.g., `"parcelle"`, `"numvoie"`).
#' If `NULL`, all available datasets for the INSEE code are retrieved.
#' @param extract_dir `character` or `NULL`
#' directory path where downloaded files are extracted.
#' If `NULL`, a temporary unique directory is created automatically.
#' @param overwrite `logical`.
#' Whether to overwrite existing downloaded files. Default is `TRUE`.
#' @param ... Additional arguments passed to `etalab_get_data_urls()`.
#' @param warn `logical`. Default is `TRUE`.
#' If `TRUE`, warnings about missing URLs, failed downloads,
#' missing GeoJSON files, or failed reads will be displayed. If FALSE, these warnings are suppressed
#' and the function will return NULL silently when issues occur.
#'
#' @return A named list of `sf` objects containing the downloaded and read spatial data.
#'   Each list element corresponds to a dataset (e.g., `"parcelle"`, `"numvoie"`), aggregated
#'   from one or more GeoJSON files.
#'
#' @details
#' The function performs the following steps:
#' 1. Retrieve download URLs from Etalab using `etalab_get_data_urls()`.
#' 2. Download and extract all requested files into `extract_dir`.
#' 3. Read all extracted GeoJSON files using `read_geojson()`, which aggregates data by dataset name.
#'
#' The `extract_dir` is created as a unique temporary directory if not provided,
#' ensuring that multiple calls do not interfere with each other's files.
#'
#' @examples
#' \dontrun{
#' # Download and read all cadastre datasets for INSEE code 72188
#' cadastre_data <- get_cadastre_etalab("72188")
#'
#' # Download only parcelle and numvoie data, specifying an extraction directory
#' cadastre_data <- get_cadastre_etalab("72188", data = c("parcelle", "numvoie"), extract_dir = "data/cadastre")
#' }
#'
#' @export
#'
get_cadastre_etalab <- function(insee_code,
                                data = NULL,
                                extract_dir = NULL,
                                overwrite = TRUE,
                                verbose = FALSE,
                                warn = TRUE,
                                ...) {

  .war <- function(...) if (isTRUE(verbose)) warning(...)

  # 1. Build URL table
  urls_df <- etalab_get_data_urls(insee_code, data = data, ...)

  if (nrow(urls_df) == 0) {
    stop("No URL found for provided parameters.")
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
      extract = TRUE,
      extract_dir = extract_dir,
      overwrite = overwrite,
      use_subdirs = FALSE,
      verbose = verbose,
      warn = warn
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

#' Download and return a specific cadastre layer from Etalab
#'
#' Retrieves cadastral data for one or more INSEE codes from Etalab,
#' optionally specifying the type of data layer (e.g., "parcelles" or "lieudit").
#'
#' @param insee_code `character` or `numeric`.
#' Vector of INSEE codes for the communes to download.
#' @param data `character`.
#' Name of the data layer to retrieve. Default is `"parcelles"`.
#' @param extract_dir `character` or `NULL`.
#' Directory to extract downloaded files. If `NULL`, a temporary directory is used.
#' @param overwrite `logical`.
#' Should existing files be overwritten? Default is `TRUE`.
#' @param ... Additional arguments passed to `get_cadastre_etalab()`.
#' @param warn `logical`. Default is `TRUE`.
#' If `TRUE`, warnings about missing layers in the downloaded data
#' will be displayed. If FALSE, these warnings are suppressed and the function returns `NULL` silently.
#'
#' @return `sf` object. The requested cadastral layer with unique features.
#'
#' @importFrom sf st_drop_geometry
#'
#' @seealso \code{\link{get_cadastre_etalab}}
#'
#' @export
#'
get_quick_etalab <- function(insee_code, data = "parcelles", extract_dir = NULL,
                             overwrite = TRUE, warn = TRUE, ...) {

  unique_insee <- unique(as.character(insee_code))
  data_list <- rep(list(data), length(unique_insee))

  sf_data <- get_cadastre_etalab(
    insee_code = unique_insee,
    data = data_list,
    extract_dir = extract_dir,
    overwrite = overwrite,
    warn = FALSE,
    ...
  )

  if (is.null(sf_data) || !data %in% names(sf_data)) {
    if (warn) warning(sprintf("Data '%s' not found in 'etalab' query.", data))
    return(NULL)
  }

  return(unique(sf_data[[data]]))
}
