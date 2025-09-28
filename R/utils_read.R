# PCI section ----
#' Read DXF file(s) as an sf object
#'
#' This function reads a single DXF file or all DXF files in a directory
#' and attempts to aggregate them into a single `sf` object.
#'
#' @param path `character` Path to a DXF file or a directory containing DXF files.
#'
#' @return An `sf` object representing the spatial features read from the DXF file(s),
#' or a list of `sf` objects if aggregation fails.
#'
#' @importFrom sf st_read st_set_crs
#'
#' @seealso \code{\link[sf]{st_read}} for reading spatial vector data.
#'
#' @keywords internal
#'
read_dxf <- function(path) {
  target_crs <- 2154

  # If path is a single DXF file, read and return it directly
  if (file.exists(path) && grepl("\\.DXF$", path, ignore.case = TRUE)) {
    return(st_set_crs(st_read(path, quiet = TRUE), target_crs))
  }

  # Otherwise, assume path is a directory and list all DXF files inside
  files <- list.files(path, pattern = "\\.dxf$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop("No DXF files found in directory ", path)
  }

  # Read all DXF files as sf objects
  layers <- lapply(files, function(f) st_set_crs(st_read(f, quiet = TRUE), target_crs))

  # Try to aggregate all layers into a single sf object
  tryCatch({
    aggregated <- do.call(rbind, layers)
    return(aggregated)
  }, error = function(e) {
    warning("Failed to aggregate DXF files: ", e$message)
    return(layers)
  })
}

#' Read EDIGEO data from a directory containing .THF files
#'
#' This function reads all layers from all .THF files in the specified directory,
#' attempts to aggregate layers with matching names if their attribute structures
#' are compatible, and returns a named list of `sf` objects.
#'
#' @param edigeo_dir `character` Path to the directory containing EDIGEO .THF files.
#'
#' @return A named list of `sf` objects representing the layers read and aggregated
#' from the EDIGEO files.
#'
#' @importFrom sf st_layers st_read st_set_geometry st_geometry
#'
#' @keywords internal
#'
read_edigeo <- function(edigeo_dir) {
  if (!dir.exists(edigeo_dir)) {
    stop("The directory does not exist: ", edigeo_dir)
  }

  thf_files <- list.files(edigeo_dir, pattern = "\\.THF$", ignore.case = TRUE, full.names = TRUE)
  if (length(thf_files) == 0) {
    stop("No .THF files found in directory ", edigeo_dir)
  }

  layers_aggregated <- list()
  final_layers <- list()

  for (thf_file in thf_files) {
    layers <- st_layers(thf_file)$name

    for (layer_name in layers) {
      message("Reading layer '", layer_name, "' from ", basename(thf_file))
      layer_sf <- st_read(thf_file, layer = layer_name, quiet = TRUE)

      if (!layer_name %in% names(layers_aggregated)) {
        layers_aggregated[[layer_name]] <- layer_sf
      } else {
        existing <- layers_aggregated[[layer_name]]

        data_exist <- st_set_geometry(existing, NULL)
        data_new <- st_set_geometry(layer_sf, NULL)

        cols_exist <- names(data_exist)
        cols_new <- names(data_new)

        common_cols <- intersect(cols_exist, cols_new)
        diff_types <- sapply(common_cols, function(col) {
          !identical(class(data_exist[[col]]), class(data_new[[col]]))
        })
        diff_types_cols <- common_cols[diff_types]

        cols_only_in_exist <- setdiff(cols_exist, cols_new)
        cols_only_in_new <- setdiff(cols_new, cols_exist)

        # If attributes match exactly, rbind; otherwise, separate layers
        if (length(cols_only_in_exist) == 0 && length(cols_only_in_new) == 0 && length(diff_types_cols) == 0) {
          geom_exist <- st_geometry(existing)
          geom_new <- st_geometry(layer_sf)
          data_bind <- rbind(data_exist, data_new)
          geom_bind <- c(geom_exist, geom_new)
          layers_aggregated[[layer_name]] <- st_set_geometry(data_bind, geom_bind)
        } else {
          # If differences, move existing to final_layers (if not already), add new separately
          if (!layer_name %in% names(final_layers)) {
            final_layers[[layer_name]] <- layers_aggregated[[layer_name]]
          } else {
            layer_name <- paste0(layer_name, "_alt_", length(final_layers) + 1)
          }
          final_layers[[layer_name]] <- layer_sf
          layers_aggregated[[layer_name]] <- NULL
        }
      }
    }
  }

  # Add any remaining aggregated layers to final_layers
  for (nm in names(layers_aggregated)) {
    final_layers[[nm]] <- layers_aggregated[[nm]]
  }

  return(final_layers)
}

# Etalab section ----
#' Read a GeoJSON file (optionally gzipped) and extract a clean name
#'
#' Reads a GeoJSON file from a local path, automatically decompressing gzipped files if needed,
#' and returns a list containing a simplified layer name and the spatial data as an `sf` object.
#'
#' @param f `character`. Path to the GeoJSON or gzipped GeoJSON file.
#'
#' @return A `list` with components:
#'   \item{name}{Simplified name extracted from the file name.}
#'   \item{data}{An `sf` object with the spatial data.}
#'
#' @importFrom sf st_read
#' @importFrom R.utils gunzip
#'
#' @keywords internal
#'
read_geojson_file <- function(f) {
  name <- basename(f)
  name <- sub("\\.gz$", "", name, ignore.case = TRUE)
  name <- sub("\\.(geojson|json)$", "", name, ignore.case = TRUE)
  name <- sub(".*-", "", name)

  if (grepl("\\.gz$", f, ignore.case = TRUE)) {
    tmp <- tempfile(fileext = ".geojson")
    R.utils::gunzip(f, destname = tmp, remove = FALSE, overwrite = TRUE)
    f <- tmp
  }

  data <- sf::st_read(f, quiet = TRUE)
  list(name = name, data = data)
}

#' Read a GeoJSON dataset from a URL and extract a clean name
#'
#' Reads a GeoJSON dataset directly from a URL and returns a list containing a simplified
#' layer name and the spatial data as an `sf` object.
#'
#' @param u `character`. URL pointing to a GeoJSON file.
#'
#' @return A `list` with components:
#'   \item{name}{Simplified name extracted from the URL.}
#'   \item{data}{An `sf` object with the spatial data.}
#'
#' @importFrom sf st_read
#'
#' @keywords internal
#'
read_geojson_url <- function(u) {
  name <- basename(u)
  name <- sub("\\.gz$", "", name, ignore.case = TRUE)
  name <- sub("\\.(geojson|json)$", "", name, ignore.case = TRUE)
  name <- sub(".*-", "", name)

  data <- sf::st_read(u, quiet = TRUE)
  list(name = name, data = data)
}

#' Aggregate multiple sf objects by layer name
#'
#' Combines multiple `sf` objects corresponding to the same layer, ensuring that all
#' objects have consistent column structure. Returns either a single `sf` object if
#' only one layer exists, or a named list of `sf` objects for multiple layers.
#'
#' @param sf_list A `list` of `sf` objects to aggregate.
#' @param names_list A `character` vector of names corresponding to each `sf` object.
#'
#' @return Either a single `sf` object (if only one unique layer) or a named `list` of `sf` objects.
#'
#' @keywords internal
#'
aggregate_sf_by_layer <- function(sf_list, names_list) {
  unique_layers <- unique(names_list)
  aggregated <- lapply(unique_layers, function(layer_name) {
    idx <- which(names_list == layer_name)
    if (length(idx) == 1) {
      sf_list[[idx]]
    } else {
      # Harmoniser colonnes
      all_cols <- unique(unlist(lapply(sf_list[idx], names)))
      sf_list_fixed <- lapply(sf_list[idx], function(x) {
        missing <- setdiff(all_cols, names(x))
        for (col in missing) x[[col]] <- NA
        x[all_cols]
      })
      do.call(rbind, sf_list_fixed)
    }
  })
  names(aggregated) <- unique_layers
  if (length(aggregated) == 1) aggregated[[1]] else aggregated
}

#' Read and aggregate GeoJSON data from files or URLs
#'
#' This function reads one or more GeoJSON or JSON sources (optionally compressed
#' with `.gz`) from either local files or URLs. Files/sources are grouped and
#' aggregated by their base name (after removing extensions and the prefix
#' before the last `-`). If multiple sources share the same base name, they are
#' merged into a single `sf` object using `rbind`.
#'
#' @param sources A character vector of file paths or URLs to GeoJSON/JSON files.
#' @param type Character. Either `"file"` or `"url"`. Determines how the sources
#'   are read. `"file"` uses `read_geojson_file()`, `"url"` uses `read_geojson_url()`.
#'
#' @return Either a single `sf` object if only one unique layer is found, or a
#'   named list of `sf` objects for multiple layers. Names correspond to the
#'   extracted base names from the sources.
#'
#' @details
#' File/URL names are processed as follows:
#' 1. Remove the `.gz` extension if present.
#' 2. Remove the `.geojson` or `.json` extension.
#' 3. Keep only the part after the last dash (`-`).
#'
#' For files, gzipped sources are decompressed into a temporary location before reading.
#'
#' @examples
#' \dontrun{
#' # Read all GeoJSON files from a directory
#' dir_files <- list.files("path/to/geojson_dir", full.names = TRUE)
#' data_list <- read_geojson(dir_files, type = "file")
#'
#' # Read GeoJSON data from URLs
#' urls <- c("https://example.com/layer1.geojson",
#'           "https://example.com/layer2.geojson")
#' data_list <- read_geojson(urls, type = "url")
#'
#' # Access a specific aggregated layer
#' my_layer <- data_list[["numvoie"]]
#' }
#'
#' @keywords internal
#'
read_geojson <- function(sources, type = c("file", "url")) {
  type <- match.arg(type)
  readers <- switch(type,
                    file = read_geojson_file,
                    url  = read_geojson_url)

  # Lecture
  sf_data <- lapply(sources, readers)
  sf_list <- lapply(sf_data, `[[`, "data")
  names_list <- sapply(sf_data, `[[`, "name")

  # Agrégation par couche
  aggregate_sf_by_layer(sf_list, names_list)
}
