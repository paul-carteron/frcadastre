# new cdg ----
cdg_get_base_data_url <- function(site) {
  site <- match.arg(site, c("pci", "etalab"))

  mapping <- c(
    pci    = "dgfip-pci-vecteur",
    etalab = "etalab-cadastre"
  )

  sprintf("https://cadastre.data.gouv.fr/data/%s", mapping[site])
}

cdg_construct_data_url <- function(site,
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
  millesime <- match.arg(millesime, cdg_get_millesimes("pci"))

  # Default formats and scales
  if (site == "pci") {
    scale  <- "feuilles"
    format <- match.arg(format, c("edigeo", "dxf"))
  } else if (site == "etalab") {
    scale  <- "communes"
    format <- "geojson"
  }

  # Base URL
  base <- cdg_get_base_data_url(site)

  # Construct commune path
  commune <- cdg_construct_commune(commune)

  # Construct URLs (vectorized)
  file.path(base, millesime, format, scale, commune)
}

cdg_detect_urls <- function(urls, absolute = TRUE) {
  detect_one <- function(url) {
    page <- request(url) |>
      req_perform() |>
      resp_body_html()

    links <- xml_find_all(page, ".//a[@href]") |> xml_attr("href")
    links <- links[!is.na(links) & links != "../" & nzchar(links)]

    if (absolute) {
      # Ensure the base ends with the commune folder
      base <- paste0(url, "/")  # add trailing slash
      links <- url_absolute(links, base = base)
    } else {
      links <- sub("/$", "", links)
    }

    unique(links)
  }

  unlist(lapply(urls, detect_one), use.names = FALSE)
}

cdg_get_millesimes <- function(site) {
  site <- match.arg(site, c("pci", "etalab"))
  cdg_detect_urls(cdg_get_base_data_url(site), F)
}

# new pci ----
npci_get_feuilles <- function(commune, absolute = TRUE, ...) {
  links <- cdg_detect_urls(cdg_construct_data_url(site = "pci",
                                              commune = commune,
                                              ...), absolute)

  if (!absolute) {
    # Remove prefix and extension for relative links
    links <- sub("^(edigeo|dxf)-(.*)\\.tar\\.bz2$", "\\2", links)
  }

  links
}

npci_get_urls <- function(x,
                          millesime = "latest",
                          format = "edigeo") {

  millesime <- match.arg(millesime, cdg_get_millesimes("pci"))
  format    <- match.arg(format, c("edigeo", "dxf"))

  base  <- cdg_get_base_data_url("pci")
  scale <- "feuilles"

  urls <- lapply(x, function(code) {
    if (nchar(code) == 5) {
      # Commune = return all sheet URLs
      cdg_detect_urls(cdg_construct_data_url(site = "pci",
                                         code,
                                         millesime = millesime,
                                         format = format), absolute = TRUE)

    } else if (nchar(code) == 12) {
      # Sheet = return single sheet URL
      commune <- substr(code, 1, 5)
      feuille <- sprintf("%s-%s.tar.bz2", format, code)
      file.path(base, millesime, format, scale,
                cdg_construct_commune(commune), feuille)

    } else {
      stop("Invalid code: ", code,
           " (must be 5-digit commune or 12-character sheet)")
    }
  })

  unique(unlist(urls, use.names = FALSE))
}

nget_pci_data <- function(x,
                          milesime = "latest",
                          format = "edigeo",
                          extract_dir = NULL,
                          overwrite = TRUE,
                          verbose = TRUE) {

  millesime <- match.arg(millesime, cdg_get_millesimes("pci"))
  format    <- match.arg(format, c("edigeo", "dxf"))

  # 1. Get all URLs
  urls <- npci_get_urls(x, milesime, format)

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

# new etalab ----
etalab_all_data <- function(types = c("raw", "proc")) {
  # Validate types
  all_types <- c("raw", "proc")
  if (!all(types %in% all_types)) {
    stop("Invalid type(s). Must be one or more of: ", paste(all_types, collapse = ", "))
  }

  mapping <- list(
    raw  = c("batiment", "borne", "commune", "label", "lieudit", "numvoie",
             "parcelle", "ptcanv", "section", "subdfisc", "subdsect",
             "symblim", "tline", "tpoint", "tronfluv", "tronroute", "tsurf",
             "zoncommuni"),
    proc = c("batiments", "communes", "feuilles", "lieux_dits",
             "parcelles", "prefixes_sections", "sections", "subdivisions_fiscales")
  )

  # Return a named list for multiple types
  lapply(types, function(t) mapping[[t]]) |> setNames(types)
}

etalab_check_data <- function(data) {
  # Flatten all available layers
  all_layers <- unlist(etalab_all_data(), use.names = FALSE)

  # Identify invalid layers
  invalid <- setdiff(data, all_layers)
  if (length(invalid) > 0) stop("Invalid layer(s): ", paste(invalid, collapse = ", "))

  # Return TRUE if all layers are valid
  TRUE
}

etalab_generate_commune_data_pairs <- function(commune, data) {
  commune <- as.character(commune)

  if (is.list(data)) {
    # Pairwise: length of list must match commune
    if (length(data) != length(commune)) {
      stop("When data is a list, its length must match the number of commune")
    }
    df <- do.call(rbind, lapply(seq_along(commune), function(i) {
      data.frame(commune = commune[i], layer = data[[i]], stringsAsFactors = FALSE)
    }))
  } else {
    # Cartesian product: all layers for all commune
    df <- expand.grid(commune = commune, layer = data, stringsAsFactors = FALSE)
  }

  df
}

etalab_get_urls <- function(commune,
                            data,
                            millesime = "latest") {

  millesime <- match.arg(millesime, cdg_get_millesimes("etalab"))

  # Ensure commune is character
  commune <- as.character(commune)

  # Flatten data if it's a list for validation
  data_flat <- if (is.list(data)) unlist(data, use.names = FALSE) else data

  # Check that all requested layers exist
  etalab_check_data(data_flat)

  # Generate commune-layer pairs (cartesian or pairwise)
  pairs <- etalab_generate_commune_data_pairs(commune, data)

  # Determine type for each layer
  layer_type <- sapply(pairs$layer, function(d) {
    if (d %in% etalab_all_data("proc")$proc) return("proc")
    if (d %in% etalab_all_data("raw")$raw)   return("raw")
    NA_character_
  })

  # Function to get URLs for one row (commune + layer)
  get_url_one <- function(commune, layer, dt) {
    base <- cdg_construct_data_url("etalab", commune, millesime = millesime)
    if (dt == "raw") base <- file.path(base, "raw")

    all_links <- cdg_detect_urls(base, absolute = TRUE)

    # Filter matching layer
    pattern <- paste0("^.*/", ifelse(dt == "proc", "cadastre", "pci"),
                      "-[0-9]+-", layer, "\\.json\\.gz$")
    all_links[grepl(pattern, all_links)]
  }

  # Apply over all pairs and flatten
  urls <- mapply(get_url_one,
                 commune = pairs$commune,
                 layer = pairs$layer,
                 dt = layer_type,
                 SIMPLIFY = FALSE)

  unique(unlist(urls, use.names = FALSE))
}

nget_etalab_data <- function(commune,
                             data,
                             millesime = "latest",
                             extract_dir = NULL,
                             overwrite = TRUE,
                             verbose = TRUE,
                             ...) {

  .war <- function(...) if (isTRUE(verbose)) warning(..., call. = FALSE)

  # 1. Generate URLs directly
  urls <- etalab_get_urls(commune = commune,
                          data = data,
                          millesime = millesime)
  if (length(urls) == 0) {
    .war("No URLs found for the requested layers.")
    return(NULL)
  }

  # 2. Prepare extraction directory
  if (is.null(extract_dir)) {
    extract_dir <- tempfile(pattern = "cadastre_extract_")
    dir.create(extract_dir, recursive = TRUE)
  }

  # 3. Build destination file paths
  destfiles <- file.path(extract_dir, basename(urls))

  # 4. Download and extract archives
  download_results <- tryCatch(
    cdg_download_archives(
      urls = urls,
      destfiles = destfiles,
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

  if (is.null(download_results)) return(NULL)

  # 5. Read GeoJSON files
  sf_data <- tryCatch(
    read_geojson(extract_dir),
    error = function(e) {
      .war("Failed to read GeoJSON: ", conditionMessage(e))
      return(NULL)
    }
  )

  sf_data
}
