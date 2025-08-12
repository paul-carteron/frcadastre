cfg_get_config_path <- function(cfg_name = NULL, must_exist = TRUE) {
  config_dir <- system.file("config", package = "Rsequoia2")

  if (!nzchar(config_dir) || !dir.exists(config_dir)) {
    stop("Impossible de localiser le dossier 'inst/config/' du package 'Rsequoia2'.", call. = FALSE)
  }

  if (is.null(cfg_name)) {
    return(config_dir)
  }

  config_path <- file.path(config_dir, cfg_name)
  if (must_exist && !file.exists(config_path)) {
    stop(sprintf("Fichier de configuration ‘%s’ introuvable dans ‘inst/config/’.", cfg_name), call. = FALSE)
  }

  return(config_path)
}

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

  stop("Unknown site: ", site)
}

#' Récupère le préfixe et l'extension selon le site, le format et l'échelle
#'
#' @param site `character`.
#' Nom du site, par ex. "pci" ou "etalab".
#' @param format `character`.
#' Nom du format, par ex. "dxf", "edigeo", "shp", "geojson".
#' @param scale `character`.
#' Échelle ou subdivision, par ex. "departements", "feuilles", "communes".
#' @param data `character`, `NULL` par défaut.
#' Le type de donnée, par ex. "raw", "proc"
#' @param config `list`.
#' Configuration YAML déjà chargée (par défaut via cfg_load()).
#'
#' @return Une liste contenant `prefix` et `extent`, ou d'autres champs s'ils sont définis.
#' @examples
#' \dontrun{
#' cfg <- cfg_load("cdg_structure.yaml")
#' cfg_get_prefix_extent("pci", "dxf", "departements", config = cfg)
#' cfg_get_prefix_extent("etalab", "geojson", "communes", config = cfg)
#' }
#' @export
cfg_get_prefix_extent <- function(site,
                                  format = "edigeo",
                                  scale,
                                  data = NULL,
                                  config = cfg_load("cdg_structure.yaml")) {
  cdg_cfg <- config$cdg
  if (!site %in% names(cdg_cfg))
    stop("Site inconnu : ", site)

  site_cfg <- cdg_cfg[[site]]
  if (!"formats" %in% names(site_cfg))
    stop("Pas de section 'formats' pour le site ", site)

  formats_list <- site_cfg$formats
  if (!format %in% names(formats_list))
    stop("Format inconnu pour ", site, " : ", format)

  format_cfg <- formats_list[[format]]
  if (!scale %in% names(format_cfg))
    stop("Échelle inconnue pour ", site, "/", format, " : ", scale)

  data_cfg <- format_cfg[[scale]]
  if (!is.null(data)){
    if (!data %in% names(data_cfg))
      stop("Donnée inconnuées pour ", site, "/", format, "/", scale, " : ", data)
    res_cfg <- data_cfg[[data]]
  } else {
    return(data_cfg)
  }
}
