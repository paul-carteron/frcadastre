#' Download and read cadastre data from Etalab for a given INSEE code
#'
#' This function retrieves URLs for cadastre datasets from Etalab based on the
#' provided INSEE code and optional data types, downloads and extracts the files
#' into a specified directory, then reads all extracted GeoJSON files as `sf`
#' objects. The data are aggregated and returned as a named list of `sf` objects.
#'
#' @param insee_code Character. The INSEE code for the commune or area to download data for.
#' @param data Character vector or NULL. Specific datasets to download (e.g., `"parcelle"`, `"numvoie"`).
#'   If `NULL`, all available datasets for the INSEE code are retrieved.
#' @param extract_dir Character or NULL. Directory path where downloaded files are extracted.
#'   If `NULL`, a temporary unique directory is created automatically.
#' @param overwrite Logical. Whether to overwrite existing downloaded files. Default is `TRUE`.
#' @param ... Additional arguments passed to `etalab_get_data_urls()`.
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
                                ...) {

  # 1. Récupérer les URLs
  urls_df <- etalab_get_data_urls(insee_code, data = data, ...)

  if (nrow(urls_df) == 0) {
    stop("Aucune URL trouvée pour les paramètres fournis.")
  }

  # Crée un répertoire temporaire unique si non fourni
  if (is.null(extract_dir)) {
    extract_dir <- tempfile(pattern = "cadastre_extract_")
    dir.create(extract_dir, recursive = TRUE)
  }

  # Définir les fichiers de destination dans ce répertoire
  urls_df$destfile <- file.path(
    extract_dir,
    paste0("pci-", urls_df$insee_code, "-", urls_df$data, ".json.gz")
  )

  # 2. Télécharger et extraire TOUS les fichiers dans extract_dir
  download_results <- cdg_download_archives(
    urls = urls_df$url,
    destfiles = urls_df$destfile,
    extract = TRUE,
    extract_dir = extract_dir,
    overwrite = overwrite,
    use_subdirs = FALSE
  )

  # 3. Lire tous les fichiers extraits dans extract_dir
  sf_data <- read_geojson(extract_dir)

  return(sf_data)
}
