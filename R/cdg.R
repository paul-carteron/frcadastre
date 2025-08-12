#' Construit une URL complète à partir d'une base et de segments
#'
#' Concatène une URL de base avec un ou plusieurs segments, en insérant des slashs (`/`)
#' entre chaque partie.
#'
#' @param urls `character`.
#' Un ou plusieurs segments d'URL à ajouter à la base.
#' @param base_url `character`.
#' L'URL de base. Par défaut `cdg_base_url`.
#'
#' @return `character`. Une chaîne représentant l'URL complète construite.
#'
#' @examples
#' cdg_aggr_url(c("dgfip-pci-vecteur", "2022", "geojson", "departements"))
#' # "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur/2022/geojson/departements"
#'
#' cdg_aggr_url("etalab-cadastre", base_url = "https://cadastre.data.gouv.fr/data")
#' # "https://cadastre.data.gouv.fr/data/etalab-cadastre"
#'
#' @export
#'
cdg_aggr_url <- function(urls, base_url){
  paste(c(base_url, urls), collapse = "/")
}

#' Extraire les liens URL d'une page web
#'
#' Télécharge une page web et extrait tous les liens contenus dans les attributs `href`
#' des balises `<a>`. Supprime les liens relatifs `"../"` et les liens vides.
#'
#' @param url `character`
#' L'URL de la page web à analyser.
#'
#' @return `character`
#' Un vecteur de chaînes de caractères correspondant aux URL uniques extraites.
#'
#' @examples
#' \dontrun{
#' cdg_detect_links("https://example.com")
#' }
#'
#' @export
#'
cdg_detect_links <- function(url) {
  # Essayer de lire la page, gérer les erreurs
  page <- tryCatch({
    readLines(url, warn = FALSE)
  }, error = function(e) {
    stop("Erreur lors de la récupération de la page : ", e$message)
  })

  # Extraire tous les href
  links <- regmatches(page, gregexpr('href="[^"]+"', page))
  links <- unlist(links)

  # Nettoyer pour ne garder que l'URL
  links <- sub('href="([^"]+)"', '\\1', links)

  # Supprimer les liens "../" et vides
  links <- links[links != "../" & nzchar(links)]

  # Retourner liens uniques
  unique(links)
}

#' Détecte les millésimes disponibles sur une source de données
#'
#' Cette fonction récupère la liste des millésimes (répertoires terminant par un slash)
#' disponibles sur une page web selon la source sélectionnée.
#'
#' @param site `character`.
#' Nom du site dont on souhaite obtenir le chemin.
#' Doit être l'une des valeurs de `allowed_sites`.
#' @param allowed_sites `character`, `c("cdg", "pci", "etalab")` par défaut.
#' Liste des noms de sites autorisés.
#'
#' @seealso [cdg_aggr_url()]
#'
#' @examples
#' \dontrun{
#' cdg_detect_millesimes("pci")
#' cdg_detect_millesimes("etalab")
#' }
#'
#' @export
#'
cdg_detect_millesimes <- function(site,
                                  allowed_sites = c("pci", "etalab")) {

  site <- match.arg(site, allowed_sites)
  url <- cdg_get_path(site)

  # Récupérer tous les liens avec cdg_detect_links()
  links <- cdg_detect_links(url)

  # Garder uniquement ceux qui se terminent par un slash (répertoire)
  millesimes <- links[grepl("/$", links)]

  # Enlever le slash final
  millesimes <- sub("/$", "", millesimes)

  # Retourner uniques
  unique(millesimes)
}

#' Choisit un millésime parmi une source de données
#'
#' Cette fonction permet de sélectionner un millésime existant parmi ceux détectés,
#' avec possibilité d'un choix interactif ou d'une valeur par défaut.
#'
#' @param site `character`.
#' Nom du site dont on souhaite obtenir le chemin. Par ex. "pci", "etalab".
#' @param millesime `character` ou `NULL`.
#' Millésime à sélectionner. Si `NULL`, retourne `"latest"`. Si `"?"`, propose un menu interactif.
#' @param ...
#' Arguments supplémentaires transmis à `cdg_detect_millesimes()`.
#'
#' @return `character`. Millésime sélectionné ou valeur par défaut `"latest"`.
#'
#' @seealso [cdg_detect_millesimes()]
#'
#' @examples
#' \dontrun{
#' cdg_choose_millesime("pci")
#' cdg_choose_millesime("etalab", millesime = "?")
#' cdg_choose_millesime("pci", millesime = "2022")
#' }
#'
#' @export
#'
cdg_choose_millesime <- function(site,
                                 millesime = NULL, ...) {

  # Récupérer la liste des millésimes disponibles
  millesimes <- cdg_detect_millesimes(site, ...)

  # Valeur par défaut
  default <- "latest"

  # Si aucun millésime fourni → valeur par défaut
  if (is.null(millesime)) {
    return(default)
  }

  # Si l'utilisateur veut choisir → menu interactif
  if (identical(millesime, "?")) {
    cat("Millésimes disponibles :\n")
    choice <- utils::menu(millesimes, title = "Choisissez un millésime")
    if (choice == 0) {
      return(default)  # annulation → latest
    } else {
      return(millesimes[choice])
    }
  }

  # Vérification si mil existe dans la liste
  if (!mil %in% millesimes) {
    warning(sprintf("Millésime '%s' introuvable, utilisation de '%s'", mil, default))
    return(default)
  }

  # Sinon, retourner celui demandé
  return(mil)
}

#' Construit une URL pour accéder à des données SIG
#'
#' Cette fonction construit une URL complète en fonction de la source,
#' du format, de l'échelle et du millésime demandés.
#'
#' @param site `character`.
#' URL de base de la source de données.
#' @param allowed_formats `character`.
#' Vecteur des formats autorisés, ex: `c("shp", "gpkg")`.
#' @param allowed_scales `character`.
#' Vecteur des échelles autorisées, ex: `c("communes", "feuilles")`.
#' @param format `character`.
#' Format demandé, doit appartenir à `allowed_formats`.
#' @param scale `character`.
#' Échelle demandée, doit appartenir à `allowed_scales`.
#' @param millesime `character` ou `NULL`.
#' Millésime demandé, ou `NULL` pour utiliser le plus récent.
#' @param ...
#' Arguments supplémentaires transmis à `cdg_detect_millesimes()`.
#'
#' @return `character`. URL construite.
#'
#' @seealso [cdg_detect_millesimes()]
#'
#' @examples
#' \dontrun{
#' cdg_construct_url(
#'   site = "https://example.com",
#'   allowed_formats = c("shp", "gpkg"),
#'   allowed_scales = c("communes", "feuilles"),
#'   format = "gpkg",
#'   scale = "feuilles",
#'   millesime = NULL
#' )
#' }
#'
#' @export
#'
cdg_construct_url <- function(site,
                              format,
                              scale,
                              millesime = NULL,
                              allowed_sites,
                              allowed_formats,
                              allowed_scales,
                              ...) {

  # Validation des choix
  site <- match.arg(format, allowed_sites)
  format <- match.arg(format, allowed_formats)
  scale <- match.arg(scale, allowed_scales)

  # Règle spécifique : pour "shp", pas de "communes"
  if (format == "shp" && scale == "communes") {
    stop("Le format 'shp' n'est pas disponible pour l'échelle 'communes'.")
  }

  # Millésime par défaut = dernier disponible
  if (is.null(millesime)) {
    millesimes <- cdg_detect_millesimes(site, ...)
    millesime <- tail(millesimes, 1)
  }

  urls <- c(site, millesime, format, scale)
  cdg_aggr_url(urls, base_url = cdg_get_path(site))
}

#' Construit un chemin relatif pour une commune
#'
#' Cette fonction vérifie que le code commune est valide
#' puis construit un chemin au format `"<dep>/<commune>"`.
#'
#' @param commune `character`.
#' Code INSEE de la commune, doit être présent dans `Rsequoia2::communes_2024$COM`.
#'
#' @return `character`.
#' Chaîne représentant le chemin relatif sous la forme `"<dep>/<commune>"`.
#'
#' @examples
#' \dontrun{
#' cdg_construct_commune("02120")
#' }
#'
#' @export
#'
cdg_construct_commune <- function(commune){
  dep <- substr(commune, 1, 2)
  paste(c(dep, commune), collapse = "/")
}


#' Télécharger une archive et optionnellement l'extraire
#'
#' Cette fonction télécharge une archive depuis une URL,
#' déduit l'extension du fichier à partir de l'URL,
#' gère l'écrasement du fichier local, et peut extraire l'archive.
#'
#' @param url `character`.
#' URL du fichier à télécharger.
#' @param destfile `character(1)` ou `NULL`.
#' Chemin local du fichier de destination. Si `NULL`, un fichier temporaire est créé.
#' @param extract `logical`.
#' Si `TRUE`, extrait le contenu de l'archive dans un dossier.
#' @param extract_dir `character` ou `NULL`.
#' Répertoire d'extraction. Si `NULL` et `extract = TRUE`, un répertoire temporaire est créé.
#' @param overwrite `logical`.
#' Si `FALSE`, le téléchargement est ignoré si le fichier existe déjà.
#'
#' @return
#' Si `extract = FALSE`, une `list` avec :
#' \item{file}{Chemin du fichier téléchargé}
#' \item{contents}{Informations sur l'archive (objet `archive`)}

#' Si `extract = TRUE`, retourne le chemin du répertoire d'extraction (`character`).
#'
#' @examples
#' \dontrun{
#' cdg_download_archive("https://example.com/data.zip", extract = TRUE)
#' }
#'
#' @importFrom utils download.file
#' @importFrom tools file_ext
#' @importFrom archive archive_extract archive
#'
#' @export
#'
cdg_download_archive <- function(url,
                                 destfile = NULL,
                                 extract = FALSE,
                                 extract_dir = NULL,
                                 overwrite = FALSE) {

  # Déduire l'extension depuis l'URL
  ext <- tools::file_ext(url)
  if (ext == "") {
    warning("Impossible de détecter l'extension, utilisation de '.zip' par défaut")
    ext <- "zip"
  }

  # Définir un fichier temporaire si non fourni
  if (is.null(destfile)) {
    destfile <- tempfile(fileext = paste0(".", ext))
  }

  # Vérifier si le fichier existe déjà
  if (file.exists(destfile) && !overwrite) {
    message("Fichier déjà téléchargé : ", destfile)
  } else {
    message("Téléchargement depuis : ", url)
    utils::download.file(url, destfile, mode = "wb", quiet = FALSE)
    message("Fichier téléchargé : ", destfile)
  }

  # Extraction ou lecture de l'archive
  if (extract) {
    if (is.null(extract_dir)) {
      extract_dir <- tempfile("extract_")
      dir.create(extract_dir)
    }
    archive::archive_extract(destfile, dir = extract_dir)
    message("Fichiers extraits dans : ", extract_dir)
    return(extract_dir)
  } else {
    info <- archive::archive(destfile)
    return(list(file = destfile, contents = info))
  }
}

#' Télécharger plusieurs archives et optionnellement les extraire
#'
#' Cette fonction télécharge plusieurs archives à partir d'une liste d'URLs,
#' en utilisant `cdg_download_archive` pour chaque fichier,
#' et peut extraire leur contenu.
#'
#' @param urls `character`.
#' Vecteur d'URLs des fichiers à télécharger.
#' @param destfiles `character` ou `NULL`.
#' Vecteur des chemins locaux de destination pour chaque fichier.
#' Si `NULL`, des fichiers temporaires sont créés dans `tempdir()`.
#' @param extract `logical`.
#' Si `TRUE`, extrait le contenu de chaque archive.
#' @param extract_dir `character` ou `NULL`.
#' Répertoire d'extraction commun pour toutes les archives.
#' Si `NULL` et `extract = TRUE`, un dossier temporaire est créé pour chaque extraction.
#' @param overwrite `logical`.
#' Si `FALSE`, le téléchargement est ignoré si le fichier existe déjà.
#'
#' @seealso [cdg_download_archive()]
#'
#' @return
#' Une `list` contenant pour chaque URL soit :
#' - le chemin et les infos de l'archive si `extract = FALSE`
#' - le chemin du dossier d'extraction si `extract = TRUE`.
#'
#' @examples
#' \dontrun{
#' cdg_download_archives(c("https://example.com/a.zip", "https://example.com/b.zip"),
#'                       extract = TRUE)
#' }
#'
#' @seealso [cdg_download_archive()]
#'
#' @export
#'
cdg_download_archives <- function(urls, destfiles = NULL, extract = FALSE, extract_dir = NULL, overwrite = FALSE) {

  n <- length(urls)
  if (is.null(destfiles)) {
    destfiles <- file.path(tempdir(), paste0("file_", seq_len(n), ".zip"))
  }

  if (length(destfiles) != n) {
    stop("'destfiles' doit avoir la même longueur que 'urls'")
  }

  results <- vector("list", n)

  for (i in seq_along(urls)) {
    message(sprintf("\n--- [%d/%d] Téléchargement de : %s", i, n, urls[i]))

    # Appel de ta fonction existante
    results[[i]] <- cdg_download_archive(
      url = urls[i],
      destfile = destfiles[i],
      extract = extract,
      extract_dir = extract_dir,
      overwrite = overwrite
    )
  }

  return(results)
}

cdg_detect_insee_code <- function(insee_code, scale = FALSE, verbose = TRUE) {
  insee_code <- as.character(insee_code)
  communes <- rcadastre::commune_2025
  departements <- rcadastre::departement_2025

  if (nchar(insee_code) == 5) {
    if (!(insee_code %in% communes$COM)) {
      stop(sprintf("Erreur : la commune '%s' n'est pas valide. Please run rcadastre::commune_2025",
                   insee_code))
    } else {
      if (verbose){message(sprintf("Commune '%s' = '%s' sélectionnée",
                                   insee_code,
                                   communes[communes$COM == insee_code, "NCCENR"]))}
      if (scale){scale_detected <- "communes"}
    }
  } else if (nchar(insee_code) == 2 | nchar(insee_code) == 3) {
    if (!(insee_code %in% departements$DEP)) {
      stop(sprintf("Erreur : le département '%s' n'est pas valide. Please run Rsequoia2::departement_2025",
                   insee_code))
    } else {
      if (verbose){message(sprintf("Département '%s' = '%s' sélectionné",
                                   insee_code,
                                   departements[departements$DEP == insee_code, "LIBELLE"]))}
      if (scale){scale_detected <- "departements"}
    }
  }
  if (scale){return(scale_detected)}
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
#' cdg_get_prefix_extent("pci", "dxf", "departements", config = cfg)
#' cdg_get_prefix_extent("etalab", "geojson", "communes", config = cfg)
#' }
#' @export
cdg_get_prefix_extent <- function(site,
                                  format,
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
