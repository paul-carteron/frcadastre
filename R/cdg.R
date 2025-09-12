### URL section ----
#' Aggregate URL Components into a Single URL
#'
#' This function concatenates a base URL with one or more relative URL components
#' into a single complete URL string.
#'
#' @param urls `character`. Vector of relative URL components to append.
#' @param base_url `character`. String representing the base URL.
#'
#' @return A single `character` string containing the complete aggregated URL.
#'
#' @details
#' The components are concatenated with ``"/"`` as separator, starting with the ``base_url``.
#'
#' @seealso [pci_get_feuille_urls()]
#'
#' @examples
#' cdg_aggr_url(c("folder", "file.zip"), "https://example.com")
#' # Returns: "https://example.com/folder/file.zip"
#'
#' @export
#'
cdg_aggr_url <- function(urls, base_url){
  urls_clean <- urls[!sapply(urls, is.null)]
  paste(c(base_url, urls), collapse = "/")
}

#' Construct a Data Download URL
#'
#' Builds a URL for downloading data from a specified site, format, scale,
#' and optionally a specific "millesime" (data version).
#'
#' @param site `character`. Site identifier. Must be one of `allowed_sites`.
#' @param format `character`. Data format. Must be one of `allowed_formats`.
#' @param scale `character`. Data scale. Must be one of `allowed_scales`.
#' @param millesime `character` or `NULL`. The desired data version.
#'   If `NULL`, the most recent millesime is used.
#' @param allowed_sites `character`. Vector of allowed site identifiers.
#'   Defaults to `c("pci", "etalab")`.
#' @param allowed_formats `character`. Vector of allowed formats.
#' @param allowed_scales `character`. Vector of allowed scales.
#' @param ... Additional arguments passed to \code{\link{cdg_detect_millesimes}}.
#'
#' @return A `character` string representing the full download URL.
#'
#' @details
#' - The format `"shp"` is not available for the `"communes"` scale.
#' - The `millesime` parameter can be set explicitly, or defaults to the
#'   latest available.
#'
#' @seealso [cdg_detect_millesimes(), cdg_aggr_url(), cdg_get_path()]
#'
#' @examples
#' \dontrun{
#' cdg_construct_url(
#'   site = "pci",
#'   format = "gpkg",
#'   scale = "departements",
#'   allowed_formats = c("gpkg", "shp"),
#'   allowed_scales = c("communes", "departements")
#' )
#' }
#'
#' @export
#'
cdg_construct_url <- function(site,
                              format,
                              scale,
                              millesime = NULL,
                              allowed_sites = c("pci", "etalab"),
                              allowed_formats,
                              allowed_scales) {

  # Validate inputs
  site <- match.arg(site, allowed_sites)
  format <- match.arg(format, allowed_formats)
  scale <- match.arg(scale, allowed_scales)

  # Specific rule: 'shp' not available for 'communes'
  if (format == "shp" && scale == "communes") {
    stop("The format 'shp' is not available for the 'communes' scale.")
  }

  # Default millesime = latest available
  if (is.null(millesime)) {
    millesimes <- cdg_detect_millesimes(site, allowed_sites)
    millesime <- tail(millesimes, 1)
  }

  urls <- c(millesime, format, scale)
  cdg_aggr_url(urls, base_url = cdg_get_path(site))
}

#' Construct a Commune Path String
#'
#' Constructs a path string for a given commune code by prepending
#' the department code (first two characters) and joining them with a slash.
#'
#' @param commune `character`.
#' A commune code, typically a string where the first two characters represent the department code.
#'
#' @return A `character` string combining department and commune codes separated by a slash.
#'
#' @examples
#' cdg_construct_commune("75056")
#' # Returns "75/75056"
#'
#' @export
#'
cdg_construct_commune <- function(commune){
  dep <- substr(commune, 1, 2)
  file.path(dep, commune)
}

### Milesime section ----
#' Detect and Extract Links from a Webpage
#'
#' Reads the content of a webpage from a given URL and extracts all unique hyperlinks (href attributes).
#'
#' @param url `character`.
#' String specifying the URL of the webpage to scan.
#'
#' @return A `character` vector of unique URLs found in the href attributes on the page,
#'         excluding relative parent directory links ("../") and empty links.
#'
#' @details
#' This function reads the raw HTML content of the specified URL and uses regular expressions
#' to extract all href attribute values. It filters out parent directory references and empty strings.
#'
#' @seealso [cdg_aggr_url(), pci_get_feuille_urls()]
#'
#' @examples
#' \dontrun{
#' links <- cdg_detect_links("https://example.com")
#' print(links)
#' }
#'
#' @export
#'
cdg_detect_links <- function(url) {
  # Try reading the page, handle errors
  page <- tryCatch({
    readLines(url, warn = FALSE)
  }, error = function(e) {
    stop("Error while retrieving the page: ", e$message)
  })

  # Extract all href attributes
  links <- regmatches(page, gregexpr('href="[^"]+"', page))
  links <- unlist(links)

  # Clean to keep only the URL inside href
  links <- sub('href="([^"]+)"', '\\1', links)

  # Remove "../" and empty links
  links <- links[links != "../" & nzchar(links)]

  # Return unique links
  unique(links)
}

#' Detect available years (millesimes)
#'
#' Retrieves and returns the list of available "millesimes" (year directories) from a specified data site.
#'
#' @param site `character`.
#' String indicating the data site to query. Must be one of `"pci"` or `"etalab"`.
#' @param allowed_sites `character`.
#' Vector of allowed site names. Defaults to `c("pci", "etalab")`.
#'
#' @return A `character` vector of unique year identifiers (millesimes) found on the site.
#'
#' @details
#' This function uses `cdg_get_path` to get the base URL for the site,
#' then calls `cdg_detect_links` to list all hyperlinks on the site,
#' filters those that represent directories (ending with a slash),
#' and returns the unique directory names without the trailing slash.
#'
#' @seealso [cdg_detect_links(), cdg_get_path()]
#'
#' @examples
#' \dontrun{
#' years <- cdg_detect_millesimes("pci")
#' print(years)
#' }
#'
#' @export
#'
cdg_detect_millesimes <- function(site,
                                  allowed_sites = c("pci", "etalab")) {

  site <- match.arg(site, allowed_sites)
  url <- cdg_get_path(site)

  # Retrieve all links with cdg_detect_links()
  links <- cdg_detect_links(url)

  # Keep only those ending with a slash (directories)
  millesimes <- links[grepl("/$", links)]

  # Remove trailing slash
  millesimes <- sub("/$", "", millesimes)

  # Return unique values
  unique(millesimes)
}

#' Choose available years (millesimes)
#'
#' Allows the user to select a specific "millesime" (data year version) for a given site.
#' If no millesime is specified, the function defaults to `"latest"`.
#'
#' @param site `character`.
#' String specifying the data site. Passed to \code{\link{cdg_detect_millesimes}}.
#' @param millesime `character`.
#' Specifying the desired millesime:
#'   - `NULL` returns `"latest"`.
#'   - `"?"` opens an interactive menu to choose from available millesimes.
#'   - otherwise, checks if the provided millesime exists.
#' @param ... Additional arguments passed to \code{\link{cdg_detect_millesimes}}.
#'
#' @return A `character` string representing the chosen millesime, or `"latest"` if not found or canceled.
#'
#' @details
#' - If `millesime` is `"?"`, the user is prompted with an interactive selection menu.
#' - If the chosen millesime is not available, a warning is issued and `"latest"` is returned.
#'
#' @seealso
#' \code{\link{cdg_detect_millesimes}}
#'
#' @examples
#' \dontrun{
#' cdg_choose_millesime("pci")
#' cdg_choose_millesime("pci", "?")
#' cdg_choose_millesime("pci", "2023")
#' }
#'
#' @export
cdg_choose_millesime <- function(site,
                                 millesime = NULL, ...) {

  # Retrieve available millesimes
  millesimes <- cdg_detect_millesimes(site, ...)

  # Default value
  default <- "latest"

  # If no millesime provided, return default
  if (is.null(millesime)) {
    return(default)
  }

  # Interactive choice menu
  if (identical(millesime, "?")) {
    cat("Available millesimes:\n")
    choice <- utils::menu(millesimes, title = "Choose a millesime")
    if (choice == 0) {
      return(default)  # cancellation returns "latest"
    } else {
      return(millesimes[choice])
    }
  }

  # Check if millesime exists
  if (!millesime %in% millesimes) {
    warning(sprintf("Millesime '%s' not found, using '%s'", millesime, default))
    return(default)
  }

  # Return requested millesime
  return(millesime)
}

### Download section ----
#' Download and optionally extract an archive from a URL
#'
#' Downloads a file from the specified `url` to a destination file.
#' The function first checks that the URL is reachable. If the download is successful,
#' archives are automatically extracted based on their file extension (`.gz` or other formats
#' supported by `archive::archive_extract`).
#' Messages and warnings can be controlled via the `verbose` parameter.
#'
#' @param url `character`. URL of the archive to download.
#' @param destfile `character`. Optional path to save the downloaded file.
#'   If `NULL`, a temporary file with the same name as the archive is created.
#' @param extract_dir `character`. Directory where the archive will be extracted.
#'   If `NULL`, a temporary directory is created.
#' @param overwrite `logical`. Default is `FALSE`. Whether to overwrite the destination file if it already exists.
#' @param verbose `logical`. Default is `TRUE`. Whether to display messages during download and extraction.
#'
#' @return If the download and extraction succeed, returns the path to the extraction directory.
#'   Otherwise, returns `NULL`.
#'
#' @seealso [archive::archive_extract()], [archive::archive()]
#'
#' @importFrom httr2 request req_perform req_method req_timeout
#' @importFrom archive archive_extract
#'
#' @export
#'
cdg_download_archive <- function(url,
                                 destfile = NULL,
                                 extract_dir = NULL,
                                 overwrite = FALSE,
                                 verbose = TRUE) {

  .msg <- function(...) if (isTRUE(verbose)) message(...)
  .war <- function(...) if (isTRUE(verbose)) warning(..., call. = FALSE)

  # Test existing url
  if (!.url_exists(url)) {
    .war(sprintf("URL not reachable: %s", url))
    return(NULL)
  }

  if (is.null(destfile)) destfile <- tempfile(basename(url))
  if (is.null(extract_dir)) extract_dir <- tempdir()

  # Download url
  ok <- tryCatch({
    if (!file.exists(destfile) || overwrite) {
      httr2::request(url) |> httr2::req_perform(path = destfile)
    }
    TRUE
  }, error = function(e) {
    .war(sprintf("Download failed: %s", conditionMessage(e)))
    FALSE
  })
  if (!ok) return(NULL)

  # Extraction according extent
  filename <- basename(destfile)
  ext <- tolower(sub(".*\\.([^.]+)$", "\\1", filename))
  sub_extract_dir <- extract_dir

  if (ext == "gz") {
    out_file <- file.path(sub_extract_dir, sub("\\.gz$", "", filename))
    ok <- tryCatch({
      con_in <- gzfile(destfile, "rb")
      con_out <- file(out_file, "wb")
      while (length(buf <- readBin(con_in, what = raw(), n = 65536)) > 0) {
        writeBin(buf, con_out)
      }
      close(con_in); close(con_out)
      TRUE
    }, error = function(e) {
      .war(sprintf("Decompression failed: %s", conditionMessage(e)))
      FALSE
    })
    if (ok) .msg(sprintf("File '%s' downloaded and decompressed", filename))

  } else {
    ok <- tryCatch({
      archive::archive_extract(destfile, dir = sub_extract_dir)
      TRUE
    }, error = function(e) {
      .war(sprintf("Extraction failed: %s", conditionMessage(e)))
      FALSE
    })
    if (ok) .msg(sprintf("File '%s' downloaded and extracted", filename))
  }

  if (ok) return(sub_extract_dir)
  else return(NULL)
}


#' Download multiple archives and optionally extract them
#'
#' Downloads multiple archives from the given `urls`.
#'
#' @param urls `character`. Vector of URLs to download.
#' @param destfiles `character`. Optional vector of file paths to save the downloaded files.
#'   If `NULL`, temporary files are created. Must have the same length as `urls`.
#' @param extract_dir `character`. Directory where archives will be extracted.
#'   If `NULL`, a temporary directory is created.
#' @param use_subdirs `logical`. Default `FALSE`. If `TRUE`, each archive is extracted into
#'   its own subdirectory within `extract_dir`.
#' @param ... Additional arguments passed to `cdg_download_archive()`.
#'
#' @return A list of extraction directories used for each archive.
#'
#' @seealso [cdg_download_archive()]
#'
#' @export
#'
cdg_download_archives <- function(urls,
                                  destfiles = NULL,
                                  extract_dir = NULL,
                                  use_subdirs = FALSE,
                                  ...) {
  n <- length(urls)
  if (is.null(destfiles)) destfiles <- rep(NA_character_, n)
  if (is.null(extract_dir)) extract_dir <- tempdir()

  results <- vector("list", n)
  for (i in seq_along(urls)) {
    sub_dir <- if (use_subdirs) file.path(extract_dir, paste0("archive_", i)) else extract_dir
    if (use_subdirs && !dir.exists(sub_dir)) dir.create(sub_dir, recursive = TRUE)

    results[[i]] <- cdg_download_archive(
      url = urls[i],
      destfile = if (!is.na(destfiles[i])) destfiles[i] else NULL,
      extract_dir = sub_dir,
      ...
    )
  }
  results
}

### INSEE code section ----
#' Detect and validate INSEE code (city or department)
#'
#' Checks if the provided INSEE code is valid among communes or departements,
#' optionally returning the scale ("communes" or "departements").
#'
#' @param insee_code `character` or `numeric`.
#' INSEE code to validate.
#' @param scale `logical`.
#' If `TRUE`, return the detected scale ("communes" or "departements").
#' @param verbose `logical`.
#' If `TRUE`, print informative messages.
#'
#' @return If `scale = TRUE`, returns the detected scale as a character string.
#' Otherwise, returns nothing but throws an error if invalid.
#'
#' @export
#'
cdg_detect_insee_code <- function(insee_code, scale = FALSE, verbose = TRUE) {
  insee_code <- as.character(insee_code)
  communes <- rcadastre::commune_2025
  departements <- rcadastre::departement_2025

  if (nchar(insee_code) == 5) {
    if (!(insee_code %in% communes$COM)) {
      stop(sprintf("Erreur : City '%s' not find. Please run rcadastre::commune_2025",
                   insee_code))
    } else {
      if (verbose){message(sprintf("City '%s' = '%s' selected",
                                   insee_code,
                                   communes[communes$COM == insee_code, "NCCENR"]))}
      if (scale){scale_detected <- "communes"}
    }
  } else if (nchar(insee_code) == 2 | nchar(insee_code) == 3) {
    if (!(insee_code %in% departements$DEP)) {
      stop(sprintf("Erreur : department '%s' not find. Please run Rsequoia2::departement_2025",
                   insee_code))
    } else {
      if (verbose){message(sprintf("Department '%s' = '%s' selected",
                                   insee_code,
                                   departements[departements$DEP == insee_code, "LIBELLE"]))}
      if (scale){scale_detected <- "departements"}
    }
  }
  if (scale){return(scale_detected)}
}


