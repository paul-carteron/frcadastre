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
#' @importFrom sf st_read
#'
#' @seealso \code{\link[sf]{st_read}} for reading spatial vector data.
#'
#' @export
#'
read_dxf <- function(path) {
  # If path is a single DXF file, read and return it directly
  if (file.exists(path) && grepl("\\.DXF$", path, ignore.case = TRUE)) {
    return(sf::st_read(path, quiet = TRUE))
  }

  # Otherwise, assume path is a directory and list all DXF files inside
  files <- list.files(path, pattern = "\\.dxf$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop("No DXF files found in directory ", path)
  }

  # Read all DXF files as sf objects
  layers <- lapply(files, function(f) sf::st_read(f, quiet = TRUE))

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
#' @export
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
    layers <- sf::st_layers(thf_file)$name

    for (layer_name in layers) {
      message("Reading layer '", layer_name, "' from ", basename(thf_file))
      layer_sf <- sf::st_read(thf_file, layer = layer_name, quiet = TRUE)

      if (!layer_name %in% names(layers_aggregated)) {
        layers_aggregated[[layer_name]] <- layer_sf
      } else {
        existing <- layers_aggregated[[layer_name]]

        data_exist <- sf::st_set_geometry(existing, NULL)
        data_new <- sf::st_set_geometry(layer_sf, NULL)

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
          geom_exist <- sf::st_geometry(existing)
          geom_new <- sf::st_geometry(layer_sf)
          data_bind <- rbind(data_exist, data_new)
          geom_bind <- c(geom_exist, geom_new)
          layers_aggregated[[layer_name]] <- sf::st_set_geometry(data_bind, geom_bind)
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

#' Read and aggregate GeoJSON files from a directory or file path
#'
#' This function reads one or more GeoJSON or JSON files (optionally compressed
#' with `.gz`) from a given path. Files are grouped and aggregated by their
#' base name (after removing extensions and the prefix before the last `-`).
#' If multiple files share the same base name, they are merged into a single
#' `sf` object using `rbind`.
#'
#' @param path A file path or directory path. If a directory is provided, all
#'   files matching the pattern `*.geojson`, `*.json`, `*.geojson.gz`,
#'   or `*.json.gz will be read.
#'
#' @return A named list of `sf` objects, where names correspond to the
#'   extracted base names from the files.
#'   - Each element contains the spatial features from one or more files.
#'
#' @details
#' File names are processed as follows:
#' 1. Remove the `.gz` extension if present.
#' 2. Remove the `.geojson` or `.json` extension.
#' 3. Keep only the part after the last dash (`-`).
#'
#' Gzipped files are decompressed into a temporary location before reading.
#'
#' @examples
#' \dontrun{
#' # Read all GeoJSON files from a directory
#' data_list <- read_geojson("path/to/geojson_dir")
#'
#' # Read a single GeoJSON file
#' data_list <- read_geojson("path/to/file.geojson")
#'
#' # Access an aggregated dataset
#' my_layer <- data_list[["numvoie"]]
#' }
#'
#' @importFrom sf st_read
#' @importFrom R.utils gunzip
#'
#' @export
#'
read_geojson <- function(path) {
  # List files: if 'path' is a single file, use it; otherwise, list all GeoJSON/JSON (and optional .gz) files in the directory
  if (file.exists(path) && !dir.exists(path)) {
    files <- path
  } else {
    files <- list.files(
      path,
      pattern = "\\.(geojson|json)(\\.gz)?$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  }

  # Stop if no files are found
  if (length(files) == 0) stop("No GeoJSON files found in ", path)

  # Read each file and extract name + data
  file_data <- lapply(files, .read_geojson_file)
  names_list <- sapply(file_data, `[[`, "name")  # Extract the cleaned layer names
  sf_list <- lapply(file_data, `[[`, "data")    # Extract the sf objects

  # Aggregate layers by name
  aggregated <- lapply(unique(names_list), function(n) {
    idx <- which(names_list == n)  # Index of all layers with the same name
    if (length(idx) == 1) {
      # Only one file with this name, return as is
      sf_list[[idx]]
    } else {
      # Multiple files with the same name: unify columns before binding
      all_cols <- unique(unlist(lapply(sf_list[idx], names)))  # All unique column names across layers
      sf_list_fixed <- lapply(sf_list[idx], function(x) {
        # Add missing columns filled with NA
        missing_cols <- setdiff(all_cols, names(x))
        for (col in missing_cols) x[[col]] <- NA
        x[all_cols]  # Reorder columns consistently
      })
      # Combine all sf objects into a single sf object
      do.call(rbind, sf_list_fixed)
    }
  })

  # Assign layer names to the aggregated list
  names(aggregated) <- unique(names_list)

  aggregated  # Return a named list of sf objects
}
