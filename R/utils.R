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

  if (site %in% names(cfg)) {
    return(cfg[[site]]$path)
  }

  if (site %in% names(cfg$cdg)) {
    return(file.path(cfg$cdg$path, cfg$cdg[[site]]$path))
  }

  stop("Unknown site: ", site)
}


cfg_get_info <- function(site, format, scale, key, cfg = cfg_load("cdg_structure.yaml")) {
  # site : "cdg", "pci" ou "etalab"
  # format : "dxf", "edigeo", "shp", "geojson"
  # scale : "departements", "feuilles", "communes"
  # field : "path", "prefix", "extent", "data"
  # cfg : liste R issue de yaml::read_yaml()

  # --- 1. Aller directement au bon bloc ---
  if (site %in% names(cfg)) {
    node <- cfg[[site]]
  } else if (site %in% c("pci", "etalab")) {
    node <- cfg$cdg[[site]]
  } else {
    stop("Unknown site: ", site)
  }

  # --- 2. Descendre dans formats ---
  if (!"formats" %in% names(node)) stop("Missing 'formats' level for site: ", site)
  node <- node$formats

  # --- 3. Aller au format demandé ---
  if (!format %in% names(node)) stop("Unknown format: ", format)
  node <- node[[format]]

  # --- 4. Aller à l’échelle demandée ---
  if (!scale %in% names(node)) stop("Unknown scale: ", scale)
  node <- node[[scale]]

  # --- 5. Retourner le champ demandé ---
  if (!key %in% names(node)) stop("Unknown key: ", key)
  node <- node[[key]]

  return(node)
}
