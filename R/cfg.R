#' Get the path to a configuration file in the package
#'
#' Returns the full path to a configuration file located in the `inst/config/`
#' directory of the `rcadastre` package. If no file name is provided, returns
#' the path to the configuration directory.
#'
#' @param cfg_name `character` or `NULL`.
#' Name of the configuration file. If `NULL`, the directory path is returned.
#' @param must_exist `logical`. Default is `TRUE`.
#' If `TRUE`, throws an error if the configuration file does not exist.
#'
#' @return `character`. Full path to the configuration file or the configuration directory.
#'
#' @export
#'
cfg_get_config_path <- function(cfg_name = NULL, must_exist = TRUE) {
  config_dir <- system.file("config", package = "rcadastre")

  if (!nzchar(config_dir) || !dir.exists(config_dir)) {
    stop("Folder 'inst/config/' not found in package 'rcadastre'.", call. = FALSE)
  }

  if (is.null(cfg_name)) {
    return(config_dir)
  }

  config_path <- file.path(config_dir, cfg_name)
  if (must_exist && !file.exists(config_path)) {
    stop(sprintf("File '%s' not found in 'inst/config/'.", cfg_name), call. = FALSE)
  }

  return(config_path)
}

#' Load a YAML configuration file from the package
#'
#' Reads a YAML configuration file located in the `inst/config/` directory of
#' the `rcadastre` package and returns it as a list.
#'
#' @param cfg_name `character`. Name of the YAML configuration file to load.
#'
#' @return `list`. Contents of the YAML configuration file.
#'
#' @seealso [cfg_get_config_path()]
#'
#' @importFrom yaml read_yaml
#'
#' @export
#'
cfg_load <- function(cfg_name) {
  cfg_path <- cfg_get_config_path(cfg_name, must_exist = TRUE)
  yaml::read_yaml(cfg_path)
}

cdg_get_path <- function(site,
                         allowed_sites = c("cdg", "pci", "etalab"),
                         config = cfg_load("cdg_structure.yaml")) {

  site <- match.arg(site, allowed_sites)

  if (site %in% names(config)) {
    return(config[[site]]$path)
  }

  if (site %in% names(config$cdg)) {
    return(file.path(config$cdg$path, config$cdg[[site]]$path))
  }

  stop(sprintf("Unknown site: '%s'", site), call. = FALSE)
}

#' Get the configuration for a site, format, scale, and optional data
#'
#' Retrieves the configuration details (prefixes, extents, etc.) for a given
#' site, format, and scale from a site configuration YAML file. If `data` is
#' provided, returns the configuration for that specific data; otherwise,
#' returns the full configuration for the scale.
#'
#' @param site `character`. Name of the site to query.
#' @param format `character`. Default is `"edigeo"`.
#' Format of the data (e.g., `"dxf"`, `"edigeo"`, `"geojson"`).
#' @param scale `character` or `numeric`.
#' Scale identifier for the site/format.
#' @param data `character`, optional.
#' Specific data within the scale configuration.
#' @param config `list`. Defaults to loading `cfg_load("cdg_structure.yaml")`.
#' Configuration list, as returned by \code{\link{cfg_load}}.
#'
#' @return `list`. Configuration for the requested site/format/scale, or a specific data element if `data` is provided.
#'
#' @seealso [cfg_load()]
#'
#' @export
#'
cfg_get_prefix_extent <- function(site,
                                  format = "edigeo",
                                  scale,
                                  data = NULL,
                                  config = cfg_load("cdg_structure.yaml")) {
  cdg_cfg <- config$cdg
  if (!site %in% names(cdg_cfg))
    stop(sprintf("Unknown site: '%s'", site), call. = FALSE)

  site_cfg <- cdg_cfg[[site]]
  if (!"formats" %in% names(site_cfg))
    stop(sprintf("No 'formats' for the site '%s'", site), call. = FALSE)

  formats_list <- site_cfg$formats
  if (!format %in% names(formats_list))
    stop(sprintf("Format '%s' not available for the site '%s'", format, site), call. = FALSE)

  format_cfg <- formats_list[[format]]
  if (!scale %in% names(format_cfg))
    stop(sprintf("Scale '%s' not available for the site/format '%s'/'%s'", scale, site, format), call. = FALSE)

  data_cfg <- format_cfg[[scale]]
  if (!is.null(data)){
    if (!data %in% names(data_cfg))
      stop(sprintf("Data '%s' not available for the site/format/scale '%s'/'%s'", data, site, format, scale), call. = FALSE)
    res_cfg <- data_cfg[[data]]
  } else {
    return(data_cfg)
  }
}

#' Get data configuration for a given scale
#'
#' Retrieves the configuration of raw and processed data for a given scale
#' (e.g., "departements" or "communes") from the site configuration YAML.
#' Returns either a `list` of raw and processed data or a `data.frame` summarizing them.
#'
#' @param scale `character`.
#' One of `"departements"` or `"communes"`. Specifies the scale of data to retrieve.
#' @param as_df `logical`. Default is `FALSE`.
#' If `TRUE`, returns a `data.frame` with type and data columns; if `FALSE`, returns a `list` with `raw_data` and `proc_data`.
#' @param config `list`. Defaults to loading `cfg_load("cdg_structure.yaml")`.
#' Configuration list, as returned by \code{\link{cfg_load}}.
#'
#' @return `list` or `data.frame`. If `as_df = FALSE`, a list with elements `raw_data` and `proc_data`. If `as_df = TRUE`, a `data.frame` with columns `type` (`"raw"` or `"proc"`) and `data`.
#'
#' @seealso [cfg_load()]
#'
#' @export
#'
cfg_get_data <- function(scale = c("departements", "communes"),
                         as_df = FALSE,
                         config = cfg_load("cdg_structure.yaml")) {
  scale <- match.arg(scale)

  geojson <- config$cdg$etalab$formats$geojson[[scale]]
  if (is.null(geojson)) stop(sprintf("Scale '%s' not found in config", scale), call. = FALSE)

  raw <- geojson$raw$data
  proc <- geojson$proc$data

  if (is.null(raw) || is.null(proc)) {
    stop(sprintf("Data 'raw' or 'proc' not found in config for scale '%s'", scale), call. = FALSE)
  }

  if (as_df) {
    df_raw <- data.frame(type = "raw", data = unlist(raw), stringsAsFactors = FALSE)
    df_proc <- data.frame(type = "proc", data = unlist(proc), stringsAsFactors = FALSE)
    df <- rbind(df_raw, df_proc)
    rownames(df) <- NULL
    return(df)
  } else {
    return(list(
      raw_data = raw,
      proc_data = proc
    ))
  }
}
