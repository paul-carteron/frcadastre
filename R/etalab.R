etalab_base_url <- "etalab-cadastre"

etalab_construct_url <- function(format = "geojson",
                                 scale = "communes",
                                 ...) {
  
  if (format == "shp" & scale == "communes") {
    stop(sprintf("Erreur : le format '%s' n'est pas disponible à l'échelle '%s'.", 
         format, scale))
  }
  
  cdg_construct_url(
    source_base_url = cdg_get_path("etalab"),
    allowed_formats = c("geojson", "shp"),
    allowed_scales = c("departements", "communes"),
    format = format,
    scale = scale,
    ...
  )
}

etalab_get_archive <- function(format = "geojson",
                               scale = "communes", 
                               type = "raw",
                               allowed_formats = c("geojson", "shp"),
                               allowed_scales = c("departements", "communes"),
                               allowed_types = c("raw", "proc")) {
  
  # Validation des choix
  format <- match.arg(format, allowed_formats)
  scale <- match.arg(scale, allowed_scales)
  type <- match.arg(type, allowed_types)
  
  # Scale / type
  if (format == "shp") {
    if (scale == "communes" | type == "raw") {
      stop(sprintf("Erreur : le type '%s' n'est pas disponible à l'échelle '%s'.", 
                   type, scale))
    } else {
      archive_prefix <- "cadastre"
      archive_extent <- ".zip"
    }
  } else {
    if (type == "raw") {
      archive_prefix <- "pci"
    } else {
      archive_prefix <- "cadastre"
    }
    archive_extent <- ".json.gz"
  }
  
  etalab_archive <- list(archive_prefix = archive_prefix,
                         archive_extent = archive_extent)
  
  return(etalab_archive)
}

etalab_get_datatype <- function(type = "raw"){
  
}

etalab_get_rawdata_url <- function(insee_code,
                                   format = "geojson",
                                   data = "parcelle",
                                   allowed_data = c("batiment", "borne", "commune", "label", "lieudit", "numvoie", 
                                                    "parcelle", "ptcanv", "section", "subdfisc", "subdsect", "symblim",
                                                    "tline", "tpoint", "tronfluv", "tronroute", "tsurf", "zoncommuni"),
                                   ...){
  # Check insee_code
  scale_detected <- cdg_detect_insee_code(insee_code, scale = T, F)
  
  # Validation des choix
  data <- match.arg(data, allowed_data)
  
  archive_type <- etalab_get_archive(format = format, scale = scale_detected, type = "raw")
  etalab_url <- etalab_construct_url(format = format, scale = scale_detected, ...)
  
  if (scale_detected == "communes"){
    com <- cdg_construct_commune(insee_code)
    gz <- paste0(archive_type$archive_prefix,"-", insee_code, "-", data, archive_type$archive_extent)
    url <- cdg_aggr_url(c(etalab_url, com, "raw", gz))
  } else {
    gz <- paste0(archive_type$archive_prefix,"-", insee_code, "-", data, archive_type$archive_extent)
    url <- cdg_aggr_url(c(etalab_url, insee_code, "raw", gz))
  }
  
  return(url)
  
}

etalab_get_data <- function(data = c("batiments", "lieux_dits", "parcelles",
                                     "prefixes_sections", "sections", "subdivisions_fiscales"),
                            gz_prefix = "cadastre",
                            gz_extent = ".json.gz"){
  
}