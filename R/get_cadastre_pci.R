#' Download and Extract PCI Cadastre Data for Given Communes and Sheets
#'
#' This function downloads PCI cadastre archives for specified communes and optionally sheets,
#' extracts all files into a single directory, and reads the spatial data into an `sf` object or list of `sf`.
#'
#' @param communes `character`
#' vector of commune codes to download data for.
#' @param feuilles `character`
#' vector of sheet identifiers to filter (optional).
#' @param format `character`
#' string specifying the format to download and read. Either ``"edigeo"`` or ``"dxf"``.
#' @param extract_dir `character`
#' path to the directory where archives will be extracted. Defaults to temporary directory.
#' @param overwrite `logical`
#' indicating whether to overwrite existing downloaded or extracted files. Default is ``FALSE``.
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
#' data_sf <- get_cadastre_pci(communes = c("12345", "67890"), format = "edigeo")
#' }
#' @export
#'
get_cadastre_pci <- function(communes,
                             feuilles = NULL,
                             format = c("edigeo", "dxf"),
                             extract_dir = NULL,
                             overwrite = TRUE,
                             ...) {

  format <- match.arg(format)

  # 1. Récupérer les URLs
  urls_df <- pci_get_feuille_urls(communes, feuilles = feuilles, format = format, ...)

  if (nrow(urls_df) == 0) {
    stop("Aucune URL trouvée pour les paramètres fournis.")
  }

  urls <- urls_df$url

  # 2. Télécharger et extraire TOUS les fichiers dans extract_dir
  download_results <- cdg_download_archives(
    urls = urls,
    extract = TRUE,
    extract_dir = extract_dir,
    overwrite = overwrite,
    use_subdirs = FALSE
  )
  extraction_path <- download_results[[1]]

  # 3. Lire tous les fichiers extraits dans extract_dir
  sf_data <- switch(format,
                    dxf = read_dxf(extraction_path),
                    edigeo = read_edigeo(extraction_path))

  return(sf_data)
}
