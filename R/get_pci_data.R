#' Download and Extract PCI Cadastre Data for Given Communes and Sheets
#'
#' This function downloads PCI cadastre archives for specified communes and optionally sheets,
#' extracts all files into a single directory, and reads the spatial data into an `sf` object or list of `sf`.
#'
#' @param insee_code insee_code `character` or `numeric`
#' One or more INSEE codes for the communes or administrative units to download cadastre data for.
#' @param feuilles `character`
#' vector of sheet identifiers to filter (optional).
#' @param format `character`
#' string specifying the format to download and read. Either ``"edigeo"`` or ``"dxf"``.
#' @param extract_dir `character`
#' path to the directory where archives will be extracted. Defaults to temporary directory.
#' @param overwrite `logical`
#' indicating whether to overwrite existing downloaded or extracted files. Default is ``FALSE``.
#' @param verbose `logical` (default: `TRUE`)
#' If `TRUE`, warnings about missing URLs or failed downloads are displayed.
#' If `FALSE`, the function fails silently and returns `NULL` when issues occur.
#' @param ...
#' additional arguments passed to the URL retrieval function.
#'
#' @return A list of `sf` objects or a single `sf` object
#' containing all the spatial layers read from the extracted files,
#' depending on the format and data structure.
#'
#' @details
#' The function first retrieves download URLs for the requested communes and sheets,
#' then downloads and extracts all archive files into the specified extraction directory.
#' Finally, it reads all spatial layers from the extracted data and returns them as `sf` objects.
#'
#' @importFrom sf st_read
#' @importFrom utils download.file
#'
#' @seealso [pci_get_feuille_urls(), cdg_download_archives(), read_edigeo(), read_dxf()]
#'
#' @examples
#' \dontrun{
#' # Download and read Edigeo format for communes "12345" and "67890"
#' data_sf <- get_pci_data(communes = c("12345", "67890"), format = "edigeo")
#' }
#' @export
#'
get_pci_data <- function(insee_code,
                         feuilles = NULL,
                         format = c("edigeo", "dxf"),
                         extract_dir = NULL,
                         overwrite = TRUE,
                         verbose = TRUE,
                         ...) {

  format <- match.arg(format)

  # 1. Get all URLs
  urls_df <- pci_get_feuille_urls(insee_code, feuilles = feuilles, format = format, ...)

  if (nrow(urls_df) == 0) {
    stop(sprintf("No urls found with arguments: %s, %s, %s", insee_code, feuilles, format))
  }

  urls <- urls_df$url

  # 2. Download and extract all files in extract_dir
  download_results <- cdg_download_archives(
    urls = urls,
    destfiles = NULL,
    extract_dir = extract_dir,
    overwrite = overwrite,
    use_subdirs = FALSE,
    verbose = verbose
  )
  extraction_path <- download_results[[1]]

  # 3. Read all extrated files in extract_dir
  sf_data <- switch(format,
                    dxf = read_dxf(extraction_path),
                    edigeo = read_edigeo(extraction_path))

  return(sf_data)
}
