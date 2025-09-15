### URL section ----
#' Get the Base Data URL for cadastre.data.gouv
#'
#' This function returns the base URL for a given cadastre.data.gouv site.
#'
#' @param site `character`. The cadastre site to use. Must be one of `"pci"` or `"etalab"`.
#'
#' @return A single character string representing the base URL for the requested site.
#'
#' @details
#' The function maps the site names "pci" and "etalab" to their corresponding
#' dataset directories on the cadastre.data.gouv portal:
#'
#' - "pci" -> "dgfip-pci-vecteur"
#' - "etalab" -> "etalab-cadastre"
#'
#' It then returns the complete URL starting with "https://cadastre.data.gouv.fr/data/".
#'
#' @examples
#' \dontrun{
#' .get_base_data_url("pci")
#' # Returns: "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur"
#'
#' .get_base_data_url("etalab")
#' # Returns: "https://cadastre.data.gouv.fr/data/etalab-cadastre"
#' }
#'
#' @keywords internal
#'
.get_base_data_url <- function(site) {
  site <- match.arg(site, c("pci", "etalab"))

  mapping <- c(
    pci    = "dgfip-pci-vecteur",
    etalab = "etalab-cadastre"
  )

  sprintf("https://cadastre.data.gouv.fr/data/%s", mapping[site])
}

#' Construct the Full Data URL for cadastre.data.gouv
#'
#' This function builds the complete URL to access cadastre data for a given cadastre.data.gouv site,
#' commune, and millesime. The function handles default formats and scales for each site.
#'
#' @param site `character`. The cadastre site to use. Must be one of `"pci"` or `"etalab"`.
#' @param commune `character` vector. The INSEE code(s) of the commune(s).
#' @param millesime `character`. The version or millesime of the dataset.
#' Must be on of `get_data_millesimes("pci")`. Default is `"latest"`.
#' @param format `character`. Optional. The format of the data.
#' For "pci", must be `"edigeo"` or `"dxf"`.
#' For "etalab", the default is "geojson".
#'
#' @return A character vector of full URLs for the requested site, commune(s), and millesime.
#'
#' @details
#' The function validates the site and commune codes.
#' Default scales and formats are:
#'
#' - "pci": scale = "feuilles", format = "edigeo" or "dxf"
#' - "etalab": scale = "communes", format = "geojson"
#'
#' The returned URLs are constructed as:
#' \code{base_url / millesime / format / scale / commune}
#'
#' @seealso [get_data_millesimes()]
#'
#' @examples
#' \dontrun{
#' # PCI data for commune "72187"
#' .construct_data_url("pci", 72187)
#' # Returns: "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/latest/edigeo/feuilles/72/72187"
#'
#' # Etalab data for commune "72187"
#' .construct_data_url("etalab", "72187")
#' # Returns: "https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes/72/72187"
#' }
#'
#' @keywords internal
#'
.construct_data_url <- function(site,
                                commune,
                                millesime = "latest",
                                format = NULL) {

  # Validate site
  site <- match.arg(site, c("pci", "etalab"))

  # Validate commune codes
  commune <- as.character(commune)
  if (!all(commune %in% commune_2025$COM)) {
    stop("Some commune codes are invalid.")
  }

  # Determine millesime
  millesime <- match.arg(millesime, get_data_millesimes("pci"))

  # Default formats and scales
  if (site == "pci") {
    scale  <- "feuilles"
    format <- match.arg(format, c("edigeo", "dxf"))
  } else if (site == "etalab") {
    scale  <- "communes"
    format <- "geojson"
  }

  # Base URL
  base <- .get_base_data_url(site)

  # Construct commune path
  commune <- .construct_com_url(commune)

  # Construct URLs (vectorized)
  file.path(base, millesime, format, scale, commune)
}

#' Construct a commune path string
#'
#' Constructs a path string for a given commune code by prepending
#' the department code (first two characters) and joining them with a slash.
#'
#' @param commune `character` vector. The INSEE code(s) of the commune(s).
#'
#' @return A `character` string combining department and commune codes separated by a slash.
#'
#' @examples
#' \dontrun{
#' .construct_com_url("72187")
#' # Returns: "72/72187"
#' }
#'
#' @keywords internal
#'
.construct_com_url <- function(commune) {
  stopifnot(is.character(commune), nchar(commune) == 5)

  dep <- substr(commune, 1, ifelse(substr(commune, 1, 2) == "97", 3, 2))
  file.path(dep, commune)
}

### Milesime section ----
#' Detect and Extract URLs from a Web Page
#'
#' This function scans one or more web pages for hyperlinks (`<a href=...>`)
#' and extracts their URLs, either as absolute or relative paths.
#'
#' @param url `character`. One or more base URLs to scan.
#' @param absolute `logical`. Default is `"TRUE"`.
#' If `TRUE` (default), returned links are converted to absolute URLs.
#' If `FALSE`, relative paths are preserved.
#'
#' @return A character vector of detected URLs.
#'
#' @details
#' All `<a href>` attributes found in the HTML content of the provided `urls`
#' are returned, excluding parent directory links (`"../"`) and empty values.
#' If `absolute = TRUE`, URLs are normalized relative to the input base.
#'
#' @seealso [cdg_aggr_url(), pci_get_feuille_urls()]
#'
#' @examples
#' \dontrun{
#' links <- .detect_urls(.construct_data_url("etalab", "72187"))
#' print(links)
#' }
#'
#' @importFrom httr2 request req_perform resp_body_html
#' @importFrom xml2 xml_find_all xml_attr url_absolute
#'
#' @keywords internal
#'
.detect_urls <- function(urls, absolute = TRUE) {
  detect_one <- function(url) {
    page <- request(url) |>
      httr2::req_perform() |>
      httr2::resp_body_html()

    links <- xml2::xml_find_all(page, ".//a[@href]") |> xml2::xml_attr("href")
    links <- links[!is.na(links) & links != "../" & nzchar(links)]

    if (absolute) {
      # Ensure the base ends with the commune folder
      base <- paste0(url, "/")  # add trailing slash
      links <- xml2::url_absolute(links, base = base)
    } else {
      links <- sub("/$", "", links)
    }

    unique(links)
  }

  unlist(lapply(urls, detect_one), use.names = FALSE)
}

#' Detect available years (millesimes)
#'
#' Retrieves and returns the list of available "millesimes" (year directories) from a specified data site.
#'
#' @param site `character`. The cadastre site to use. Must be one of `"pci"` or `"etalab"`.
#'
#' @return A `character` vector of unique year identifiers (millesimes) found on the site.
#'
#' @details
#' The function queries the base data URL for the specified site and extracts
#' the list of available millésime directories.
#'
#' @examples
#' \dontrun{
#' years <- list(pci = get_data_millesimes("pci"), etalab = get_data_millesimes("etalab"))
#' print(years)
#' }
#'
#' @export
#'
get_data_millesimes <- function(site) {
  site <- match.arg(site, c("pci", "etalab"))
  .detect_urls(.get_base_data_url(site), FALSE)
}

### INSEE code section ----
#' Detect and validate INSEE code (city or department)
#'
#' Checks if the provided INSEE code(s) are valid among communes or departments.
#' Optionally returns the administrative scale ("communes" or "departements") for each code.
#'
#' @param x `character` or `numeric`. Vector of INSEE codes to validate.
#' @param scale_as_return `logical`. If `TRUE`, returns a character vector indicating
#' the detected scale ("communes" or "departements") for each code.
#' @param verbose `logical`. If `TRUE`, prints informative messages for each code.
#'
#' @return If `scale_as_return = TRUE`, a character vector of length `length(x)` with
#' values `"communes"` or `"departements"`. If `scale_as_return = FALSE`, returns
#' invisibly `NULL`.
#'
#' @details
#' The function accepts either 5-character INSEE codes for communes or 2-3 character
#' codes for departments. Invalid codes trigger an error.
#'
#' @examples
#' \dontrun{
#' # Validate a single commune
#' insee_check(72187)
#' # Returns: Commune '72187' = 'Marigné-Laillé' selected
#' insee_check(72187, TRUE)
#' # Returns: "communes"
#'
#' # Validate multiple codes and return scale
#' insee_check(c(72, 72187, 72187))
#' # Returns:
#' # Department '72' = 'Sarthe' selected
#' # Commune '72187' = 'Marigné-Laillé' selected
#' # Commune '72187' = 'Marigné-Laillé' selected
#' }
#'
#' @export
#'
insee_check <- function(x, scale_as_return = FALSE, verbose = TRUE) {
  x <- as.character(x)
  communes    <- rcadastre::commune_2025
  departements <- rcadastre::departement_2025

  scales_detected <- character(length(x))

  for (i in seq_along(x)) {
    code <- x[i]

    if (nchar(code) == 5) {
      # Commune
      idx <- which(communes$COM == code)
      if (length(idx) == 0) stop(sprintf("Commune '%s' not found. Run rcadastre::commune_2025", code))
      .log_msg(verbose, sprintf("Commune '%s' = '%s' selected", code, communes$NCCENR[idx]))
      if (scale_as_return) scales_detected[i] <- "communes"

    } else if (nchar(code) %in% c(2,3)) {
      # Département
      idx <- which(departements$DEP == code)
      if (length(idx) == 0) stop(sprintf("Department '%s' not found. Run rcadastre::departement_2025", code))
      .log_msg(verbose, sprintf("Department '%s' = '%s' selected", code, departements$LIBELLE[idx]))
      if (scale_as_return) scales_detected[i] <- "departements"

    } else {
      stop(sprintf("Invalid code '%s'. Must be a 5-char commune or 2-3 char department.", code))
    }
  }

  if (scale_as_return) return(scales_detected)
  invisible(NULL)
}
