#' Aggregate URL Components into a Single URL
#'
#' This function concatenates a base URL with one or more relative URL components
#' into a single complete URL string.
#'
#' @param urls `character` vector of relative URL components to append.
#' @param base_url `character` string representing the base URL.
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

#' Detect and Extract Links from a Webpage
#'
#' Reads the content of a webpage from a given URL and extracts all unique hyperlinks (href attributes).
#'
#' @param url `character`
#' string specifying the URL of the webpage to scan.
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
#' @param site `character`
#' string indicating the data site to query. Must be one of `"pci"` or `"etalab"`.
#' @param allowed_sites `character`
#' vector of allowed site names. Defaults to `c("pci", "etalab")`.
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
#' @param site `character` string specifying the data site. Passed to \code{\link{cdg_detect_millesimes}}.
#' @param millesime `character` specifying the desired millesime:
#'   - `NULL` → returns `"latest"`.
#'   - `"?"` → opens an interactive menu to choose from available millesimes.
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

#' Construct a Data Download URL
#'
#' Builds a URL for downloading data from a specified site, format, scale,
#' and optionally a specific "millesime" (data version).
#'
#' @param site `character` Site identifier. Must be one of `allowed_sites`.
#' @param format `character` Data format. Must be one of `allowed_formats`.
#' @param scale `character` Data scale. Must be one of `allowed_scales`.
#' @param millesime `character` or `NULL` The desired data version.
#'   If `NULL`, the most recent millesime is used.
#' @param allowed_sites `character` Vector of allowed site identifiers.
#'   Defaults to `c("pci", "etalab")`.
#' @param allowed_formats `character` Vector of allowed formats.
#' @param allowed_scales `character` Vector of allowed scales.
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
#' @param commune `character` A commune code, typically a string where
#'   the first two characters represent the department code.
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
  paste(c(dep, commune), collapse = "/")
}


#' Download and optionally extract an archive from a URL
#'
#' Downloads a file from the specified `url` to a destination file. Optionally,
#' the downloaded archive can be extracted to a given directory.
#'
#' @param url `character` URL of the archive to download.
#' @param destfile `character` Optional path to save the downloaded file.
#'   If `NULL`, a temporary file with an appropriate extension is created.
#' @param extract `logical` Whether to extract the archive after download.
#'   Defaults to `FALSE`.
#' @param extract_dir `character` Directory where the archive will be extracted
#'   if `extract` is `TRUE`. If `NULL`, a temporary directory is created.
#' @param overwrite `logical` Whether to overwrite the destination file if it
#'   already exists. Defaults to `FALSE`.
#'
#' @return If `extract` is `TRUE`, returns the path to the extraction directory.
#'   Otherwise, returns a `list` with components:
#'   \item{file}{Path to the downloaded file.}
#'   \item{contents}{Metadata about the archive contents (as returned by `archive::archive`).}
#'
#' @importFrom utils download.file
#' @importFrom tools file_ext
#' @seealso [archive::archive_extract()], [archive::archive()]
#'
#' @export
cdg_download_archive <- function(url,
                                 destfile = NULL,
                                 extract = FALSE,
                                 extract_dir = NULL,
                                 overwrite = FALSE) {

  ext <- tools::file_ext(url)
  if (ext == "") {
    warning("Unable to detect file extension; defaulting to '.zip'")
    ext <- "zip"
  }

  if (is.null(destfile)) {
    destfile <- tempfile(fileext = paste0(".", ext))
  }

  if (file.exists(destfile) && !overwrite) {
    message("File already exists: ", destfile)
  } else {
    message("Downloading from: ", url)
    utils::download.file(url, destfile, mode = "wb", quiet = FALSE)
    message("File downloaded: ", destfile)
  }

  if (extract) {
    if (is.null(extract_dir)) {
      extract_dir <- tempfile("extract_")
      dir.create(extract_dir)
    }

    # Si fichier .gz (gzip simple), on décompresse "manuellement"
    if (ext == "gz") {
      # nom du fichier décompressé (sans .gz)
      out_file <- file.path(extract_dir, sub("\\.gz$", "", basename(destfile)))

      message("Decompressing gzip file to: ", out_file)

      con_in <- gzfile(destfile, "rb")
      con_out <- file(out_file, "wb")
      while(length(buf <- readBin(con_in, what = raw(), n = 65536)) > 0) {
        writeBin(buf, con_out)
      }
      close(con_in)
      close(con_out)

      message("File decompressed to: ", out_file)
      return(extract_dir)

    } else {
      # Sinon extraction standard avec archive
      archive::archive_extract(destfile, dir = extract_dir)
      message("Files extracted to: ", extract_dir)
      return(extract_dir)
    }

  } else {
    info <- archive::archive(destfile)
    return(list(file = destfile, contents = info))
  }
}


#' Download multiple archives and optionally extract them
#'
#' Downloads multiple archives from the given `urls`. Each archive can be
#' extracted to either a unique subdirectory or a common extraction directory.
#'
#' @param urls `character` Vector of URLs to download.
#' @param destfiles `character` Optional vector of file paths to save the downloaded
#'   files. If `NULL`, temporary files are created. Must have the same length as `urls`.
#' @param extract `logical` Whether to extract the archives after downloading.
#'   Defaults to `FALSE`.
#' @param extract_dir `character` Directory where archives will be extracted.
#'   If `NULL`, the system temporary directory is used.
#' @param overwrite `logical` Whether to overwrite existing downloaded files.
#'   Defaults to `FALSE`.
#' @param use_subdirs `logical` If `TRUE`, each archive is extracted into its own
#'   subdirectory within `extract_dir`. Otherwise, all archives are extracted into
#'   `extract_dir` directly. Defaults to `FALSE`.
#'
#' @return A list of extraction directories used for each archive. Length equals
#'   the number of `urls`.
#'
#' @importFrom utils menu
#' @seealso [cdg_download_archive()]
#'
#' @export
cdg_download_archives <- function(urls,
                                  destfiles = NULL,
                                  extract = FALSE,
                                  extract_dir = NULL,
                                  overwrite = FALSE,
                                  use_subdirs = FALSE) {
  n <- length(urls)

  # Crée un dossier temporaire unique pour CET appel
  if (is.null(extract_dir)) {
    extract_dir <- tempfile(pattern = "cdg_extract_")
    dir.create(extract_dir, recursive = TRUE)
  }

  # Définit les noms de fichiers de destination
  if (is.null(destfiles)) {
    exts <- tools::file_ext(urls)
    destfiles <- file.path(extract_dir, paste0("file_", seq_len(n), ".", exts))
  }
  if (length(destfiles) != n) {
    stop("`destfiles` must have the same length as `urls`")
  }

  results <- vector("list", n)

  for (i in seq_along(urls)) {
    message(sprintf("\n--- [%d/%d] Downloading: %s", i, n, urls[i]))

    if (use_subdirs) {
      sub_extract_dir <- file.path(extract_dir, paste0("extract_", i))
      if (!dir.exists(sub_extract_dir)) dir.create(sub_extract_dir, recursive = TRUE)
    } else {
      sub_extract_dir <- extract_dir
    }

    results[[i]] <- cdg_download_archive(
      url = urls[i],
      destfile = destfiles[i],
      extract = extract,
      extract_dir = sub_extract_dir,
      overwrite = overwrite
    )
  }

  if (use_subdirs) {
    return(lapply(seq_len(n), function(i) file.path(extract_dir, paste0("extract_", i))))
  } else {
    return(rep(list(extract_dir), n))
  }
}

#' Detect and validate INSEE code (city or department)
#'
#' Checks if the provided INSEE code is valid among communes or départements,
#' optionally returning the scale ("communes" or "departements").
#'
#' @param insee_code `character` or numeric INSEE code to validate.
#' @param scale `logical` If TRUE, return the detected scale ("communes" or "departements").
#' @param verbose `logical` If TRUE, print informative messages.
#'
#' @return If `scale = TRUE`, returns the detected scale as a character string.
#' Otherwise, returns nothing but throws an error if invalid.
#'
#' @export
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
      if (verbose){message(sprintf("Départment '%s' = '%s' selected",
                                   insee_code,
                                   departements[departements$DEP == insee_code, "LIBELLE"]))}
      if (scale){scale_detected <- "departements"}
    }
  }
  if (scale){return(scale_detected)}
}


