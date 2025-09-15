### Data section ----
#' List ETALAB cadastre layers
#'
#' Returns the names of ETALAB cadastre layers for the requested type(s).
#'
#' @param type `character`. ETALAB data type. Must be one or more of `"raw"` or `"proc"`.
#' `"raw"` corresponds to raw ETALAB layers.
#' `"proc"` to processed layers.
#' Default is `c("raw", "proc")`.
#'
#' @return A named list of character vectors containing layer names for each requested type.
#'
#' @details
#' - `"raw"`: raw ETALAB layers such as `batiment`, `commune`, `parcelle`, etc.
#' - `"proc"`: processed ETALAB layers such as `batiments`, `communes`, `parcelles`, etc.
#' - Invalid `types` values will throw an error.
#'
#' @examples
#' \dontrun{
#' # Get all raw layers
#' get_etalab_layernames("raw")
#'
#' # Get all processed layers
#' get_etalab_layernames("proc")
#'
#' # Get both types at once
#' get_etalab_layernames(c("raw", "proc"))
#' }
#'
#' @export
#'
get_etalab_layernames <- function(type = c("raw", "proc")) {
  mapping <- list(
    raw  = c("batiment", "borne", "commune", "label", "lieudit", "numvoie",
             "parcelle", "ptcanv", "section", "subdfisc", "subdsect",
             "symblim", "tline", "tpoint", "tronfluv", "tronroute", "tsurf",
             "zoncommuni"),
    proc = c("batiments", "communes", "feuilles", "lieux_dits",
             "parcelles", "prefixes_sections", "sections", "subdivisions_fiscales")
  )
  if (!all(type %in% names(mapping))) stop("Invalid type(s).")
  mapping[type]
}

#' Validate ETALAB cadastre layers
#'
#' Checks whether the requested ETALAB cadastre layers exist among the known raw and processed layers.
#'
#' @param data `character`. Vector of layer ETALAB names to validate.
#'
#' @return `TRUE` if all layers are valid.
#'   Throws an error if any layer in `data` is invalid.
#'
#' @details
#' - Uses `.etalab_data()` internally to obtain the list of valid layers.
#' - Invalid layer names will trigger an error message.
#'
#' @examples
#' \dontrun{
#' # Valid layers
#' .check_etalab_data(c("batiment", "parcelle"))
#'
#' # Invalid layers will throw an error
#' .check_etalab_data(c("batiment", "invalid_layer"))
#' }
#'
#' @keywords internal
#'
.check_etalab_data <- function(data, type = c("raw", "proc")) {
  # Match argument
  type <- match.arg(type, several.ok = TRUE)

  # Get all valid layers for the requested type(s)
  all_layers <- unlist(get_etalab_layernames(type), use.names = FALSE)

  # Detect invalid layers
  invalid <- setdiff(data, all_layers)

  if (length(invalid)) {
    stop(
      sprintf(
        "Invalid layer(s) for type(s) %s: %s\nValid layer names are: %s",
        paste(type, collapse = ", "),
        paste(invalid, collapse = ", "),
        paste(all_layers, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  TRUE
}

### Arg section ----
#' Generate Commune-Layer Pairs for Etalab Data
#'
#' Generates a data.frame of commune-layer pairs to facilitate Etalab data queries.
#'
#' @param commune `character` vector. The INSEE code(s) of the commune(s).
#' @param data `character` vector or `list`.
#' If `character`, a vector of layers will be paired with all communes (Cartesian product).
#' If `list`, each element corresponds to a vector of layers for the matching commune.
#'
#' @return A `data.frame` with columns:
#' - `commune`: commune code
#' - `layer`: corresponding layer
#'
#' @details
#' - If `data` is a character vector, all layers are combined with all communes.
#' - If `data` is a list, each element is paired with the corresponding commune.
#' - The function ensures that a list of layers matches the length of `commune`.
#'
#' @examples
#' \dontrun{
#' # Cartesian product example
#' .get_etalab_arg_pairs(c("72187", "72188"), c("parcelles", "lieux_dits"))
#'
#' # Pairwise example
#' .get_etalab_arg_pairs(c("72187", "72188"), list(
#'   c("parcelles", "lieux_dits"),
#'   c("parcelles")
#' ))
#' }
#'
#' @keywords internal
#'
.get_etalab_arg_pairs <- function(commune, data) {
  commune <- as.character(commune)
  if (is.list(data)) {
    if (length(data) != length(commune)) stop("List of data must match commune length.")
    do.call(rbind, lapply(seq_along(commune), \(i) {
      data.frame(commune = commune[i], layer = data[[i]], stringsAsFactors = FALSE)
    }))
  } else {
    expand.grid(commune = commune, layer = data, stringsAsFactors = FALSE)
  }
}

### URL section ----
#' Generate URLs for Etalab Cadastre Data
#'
#' This internal function constructs URLs for Etalab cadastre data
#' given a set of commune codes and requested layers.
#'
#' @param commune `character` or `numeric` vector. The INSEE code(s) of the commune(s).
#' @param data `character` vector or `list`.
#' If `character`, a vector of layers will be paired with all communes (Cartesian product).
#' If `list`, each element corresponds to a vector of layers for the matching commune.
#' @param millesime `character`. The version or millesime of the dataset.
#' Must be on of `get_data_millesimes("etalab")`. Default is `"latest"`.
#'
#' @return A `character` vector containing unique URLs to the requested Etalab data layers.
#'
#' @details
#' - Validates that the requested layers exist.
#' - Generates commune-layer pairs (cartesian or pairwise).
#' - Determines layer type (`raw` or `proc`) and constructs the corresponding URLs.
#' - Only URLs matching the expected pattern are returned.
#'
#' @seealso [get_etalab_layernames()], [get_data_millesimes()]
#'
#' @examples
#' \dontrun{
#' # Single commune, multiple layers
#' .get_etalab_urls("72187", c("parcelles", "lieux_dits"))
#'
#' # Multiple communes, pairwise layers
#' .get_etalab_urls(c("72187", "72181"), list(c("parcelles", "lieux_dits"),c("commune")))
#' }
#'
#' @keywords internal
#'
.get_etalab_urls <- function(commune, data, millesime = "latest") {
  commune <- as.character(commune)
  insee_check(commune)
  message("")
  data_flat <- if (is.list(data)) unlist(data, use.names = FALSE) else data
  .check_etalab_data(data_flat, type = c("raw", "proc"))

  pairs <- .get_etalab_arg_pairs(commune, data)
  layer_type <- sapply(pairs$layer, \(d) {
    if (d %in% get_etalab_layernames("proc")$proc) return("proc")
    if (d %in% get_etalab_layernames("raw")$raw) return("raw")
    NA_character_
  })

  get_url_one <- function(commune, layer, dt) {
    base <- .construct_data_url("etalab", commune, millesime)
    if (dt == "raw") base <- file.path(base, "raw")
    all_links <- .detect_urls(base, absolute = TRUE)
    pattern <- paste0("^.*/", ifelse(dt=="proc","cadastre","pci"), "-[0-9]+-", layer, "\\.json\\.gz$")
    all_links[grepl(pattern, all_links)]
  }

  unique(unlist(mapply(get_url_one, pairs$commune, pairs$layer, layer_type, SIMPLIFY = FALSE)))
}

#' Download and Read Etalab Cadastre Data
#'
#' This function downloads Etalab cadastre data for given communes
#' and layers, extracts the archives, and reads the GeoJSON files into `sf` objects.
#'
#' @param commune `character` or `numeric` vector. The INSEE code(s) of the commune(s).
#' @param data `character` vector or `list`.
#' If `character`, a vector of layers will be paired with all communes (Cartesian product).
#' If `list`, each element corresponds to a vector of layers for the matching commune.
#' Must be dataset names returned by [get_etalab_layernames()].
#' @param millesime `character`. The version or millesime of the dataset.
#' Must be on of `get_data_millesimes("etalab")`. Default is `"latest"`.
#' @param extract_dir `character` or `NULL`. Directory where files will be downloaded and extracted.
#' If `NULL`, a temporary directory is used.
#' @param verbose `logical`. If `TRUE`, prints progress messages.
#'
#' @return An `sf` object if a single layer is retrieved, or a named list of `sf`
#' objects if multiple layers are retrieved.
#' If no valid data is retrieved, the function returns `NULL` with a warning.
#'
#' @seealso [get_etalab_layernames()], [get_data_millesimes()]
#'
#' @examples
#' \dontrun{
#' # Download and read parcels and lieux_dits for one commune
#' get_etalab_data("72187", c("parcelles", "lieux_dits"))
#'
#' # Multiple communes with pairwise layers
#' get_etalab_data(c("72187", "72181"), list(c("parcelles", "lieux_dits"), c("commune")))
#' }
#'
#' @export
#'
get_etalab_data <- function(commune,
                            data,
                            millesime = "latest",
                            extract_dir = NULL,
                            verbose = TRUE) {

  # 1. Generate URLs
  urls <- .get_etalab_urls(commune, data, millesime)
  if (length(urls) == 0) {
    .log_warn(verbose, "No URLs found for the requested layers.")
    return(NULL)
  }
  .log_msg(verbose, length(urls), " URL(s) found.")

  # 2. Prepare extraction directory
  if (is.null(extract_dir)) extract_dir <- tempfile(pattern = "cadastre_extract_")
  if (!dir.exists(extract_dir)) dir.create(extract_dir, recursive = TRUE)

  # 3. Download and extract all URLs
  results <- .download_archives(
    urls = urls,
    destfiles = file.path(extract_dir, basename(urls)),
    extract_dir = extract_dir,
    verbose = verbose
  )

  # 4. Check for successful downloads
  if (all(sapply(results, is.null))) {
    .log_warn(verbose, "All downloads failed.")
    return(NULL)
  }

  # 5. Read GeoJSON files
  sf_data <- tryCatch(
    .read_geojson(list.files(extract_dir, full.names = TRUE), type = "file"),
    error = function(e) {
      .log_warn(verbose, "Failed to read GeoJSON: ", conditionMessage(e))
      NULL
    }
  )

  sf_data
}

#' Download cadastre processed datasets from Etalab "bundle" cadastre.data.gouv
#'
#' This function downloads one or several dataset layer (e.g., `"parcelles"`) for
#' one or several INSEE identifiers (department or commune) using the Etalab
#' "bundle" cadastre.data.gouv.
#' The results are returned as an `sf` object. If multiple files are retrieved
#' (e.g. several communes or layers), they are combined into a single `sf`
#' objects list.
#'
#' @param id `character` or `numeric` vector. The INSEE code(s) of the commune(s) or department(s).
#'
#' @param data `character` vector or `list`.
#' If `character`, a vector of layers will be paired with all communes (Cartesian product).
#' If `list`, each element corresponds to a vector of layers for the matching commune.
#' Must be dataset names returned by [get_etalab_layernames("proc")].
#'
#' @return An `sf` object if a single layer is retrieved, or a named list of `sf`
#' objects if multiple layers are retrieved.
#'
#' @seealso [get_etalab_layernames()]
#'
#' @examples
#' \dontrun{
#' # Download parcel geometries for a single commune
#' parcelles <- get_etalab("72187", data = "parcelles")
#'
#' # Download several layers for one commune
#' layers <- get_etalab("72187", data = c("parcelles", "sections"))
#'
#' # Download parcel geometries for multiple communes
#' multi <- get_etalab(c("72187", "72032"), data = "parcelles")
#'
#' # Different layers for each commune
#' custom <- get_etalab(id = c("72187", "72181"), data = list(c("parcelles", "sections"), "communes"))
#' }
#'
#' @export
#'
get_etalab <- function(id, data = "parcelles", verbose = TRUE) {

  # 1. Validate requested datasets
  data_flat <- if (is.list(data)) unlist(data, use.names = FALSE) else data
  tryCatch(
    .check_etalab_data(data_flat, type = "proc"),
    error = function(e) {
      stop("Error in get_etalab: ", conditionMessage(e), call. = FALSE)
    }
  )

  # 2. Build commune x layer pairs
  arg_pairs <- .get_etalab_arg_pairs(id, data)

  # 3. Detect scale for each commune
  scale <- insee_check(arg_pairs$commune, scale_as_return = TRUE, verbose = verbose)
  format <- "geojson"

  # 4. Build URLs
  url_template <- "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/%s/%s/%s/%s"
  urls <- sprintf(url_template, scale, arg_pairs$commune, format, arg_pairs$layer)

  # 5. Download and read geometries from URLs
  sf_data <- lapply(seq_along(urls), function(i) {
    u <- urls[i]
    layer_name <- arg_pairs$layer[i]
    res <- tryCatch(.read_geojson(u, type = "url"), error = function(e) NULL)
    if (!inherits(res, "sf") || is.null(res)) return(NULL)
    # Return a named list with the layer name
    setNames(list(res), layer_name)
  })

  # 6. Remove NULLs
  sf_data <- sf_data[!vapply(sf_data, is.null, logical(1))]

  # 7. Flatten the list (each element is an sf object named by layer)
  sf_data <- unlist(sf_data, recursive = FALSE)
  sf_list <- unname(sf_data)
  names_list <- names(sf_data)

  # 8. Aggregate layers by name (multiple communes for the same layer)
  .aggregate_sf_by_layer(sf_list, names_list)
}
