### URL section ----
#' Construct a PCI data download URL
#'
#' Constructs a URL for downloading PCI (Plan Cadastral Informatisé) data
#' from the specified site, with options to select the data format and scale.
#'
#' @param format `character`. Default is `"edigeo"`.
#' The desired data format. Allowed values are `"dxf"` and `"edigeo"`.
#' @param scale `character`. Default is `"feuilles"`.
#' The scale level of the data. Allowed values are `"departements"` and `"feuilles"`.
#' @param ... Additional arguments passed to \code{\link{cdg_construct_url}}.
#'
#' @return A `character` string representing the constructed download URL.
#'
#' @seealso [cdg_construct_url()]
#'
#' @examples
#' \dontrun{
#' # Construct URL for EDIGEO format at feuille scale
#' url <- pci_construct_url(format = "edigeo", scale = "feuilles")
#'
#' # Construct URL for DXF format at departements scale
#' url <- pci_construct_url(format = "dxf", scale = "departements")
#' }
#'
#' @export
#'
pci_construct_url <- function(format = "edigeo",
                              scale = "feuilles",
                              ...) {
  cdg_construct_url(
    site = "pci",
    format = format,
    scale = scale,
    allowed_formats = c("dxf", "edigeo"),
    allowed_scales = c("departements", "feuilles"),
    ...
  )
}

### Department section ----
#' Get PCI data URL for a specific department
#'
#' Constructs the download URL for PCI (Plan Cadastral Informatisé) data
#' at the department scale for a given department code.
#'
#' @param departement `character`.
#' The department code (e.g., `"72"`).
#' Must be a valid department listed in `departement_2025$DEP`.
#' @param ... Additional arguments passed to \code{\link{cdg_construct_url}}.
#'
#' @return A `character` string representing the constructed download URL
#'   for the specified department.
#'
#' @details
#' Validates the department code against the known list of departments.
#' Uses internal helper functions to build the correct archive prefix and URL.
#'
#' @seealso [cdg_construct_url()]
#'
#' @examples
#' \dontrun{
#' # Get URL for department 72
#' url <- pci_get_dep_url("72")
#' }
#'
#' @export
#'
pci_get_dep_url <- function(departement,
                            ...){

  # base url
  pci_url <- pci_construct_url(scale ="departements", ...)

  # dep
  scale <- cdg_detect_insee_code(departement, T, F)
  if (!departement %in% departement_2025$DEP) {
    stop(sprintf("Department '%s' not found. Please run Rsequoia2::departements_2024$DEP",
                 departement))
  } else {
    dep <- departement
  }

  # archive
  archive <- cfg_get_prefix_extent(site = "pci", scale = scale)
  zip <- paste0(archive$prefix, dep, archive$extent)

  # result
  cdg_aggr_url(zip, pci_url)
}

#' Get PCI data URLs for multiple departments
#'
#' Constructs download URLs for PCI (Plan Cadastral Informatisé) data
#' at the department scale for a vector of department codes.
#'
#' @param departements `character` vector.
#' Department codes (e.g., `c("72", "73")`).
#' Each must be a valid department listed in `departement_2025$DEP`.
#' @param ... Additional arguments passed to \code{\link{pci_get_dep_url}}.
#'
#' @return A named `character` vector of URLs corresponding to each department code.
#'
#' @details
#' Internally calls `pci_get_dep_url()` for each department code and
#' returns the resulting URLs in a vector.
#'
#' @seealso [pci_get_dep_url()]
#'
#' @examples
#' \dontrun{
#' # Get URLs for multiple departments
#' urls <- pci_get_dep_urls(c("72", "73"))
#' }
#'
#' @export
#'
pci_get_dep_urls <- function(departements, ...) {
  vapply(
    departements,
    FUN = function(dep) {
      pci_get_dep_url(departement = dep, ...)
    },
    FUN.VALUE = character(1)  # garantit un vecteur de chaînes
  )
}

### Feuilles section ----
#' Detect available PCI sheets for a municipality
#'
#' This function returns the list of available PCI sheets for a given municipality.
#'
#' @param commune `character`.
#' INSEE code of the municipality (e.g., "02120").
#' @param format `character`.
#' PCI format (default: `"edigeo"`).
#' @param skip `logical`.
#' If `TRUE`, removes the `edigeo-` or `dxf-` prefix and the `.tar.bz2` suffix from the filenames.
#' @param ... Additional arguments passed to \code{\link{pci_construct_url}}.
#'
#' @return `character`.
#' A vector of the PCI sheet names available.
#'
#' @details
#' The function builds the URL for the municipality and uses `cdg_detect_links()`
#' to retrieve the list of available sheets.
#'
#' @seealso [pci_construct_url()], [cdg_construct_commune()], [cdg_detect_links()]
#'
#' @export
#'
pci_detect_feuilles <- function(commune,
                                format = "edigeo",
                                skip = FALSE,
                                ...) {

  pci_url <- pci_construct_url(format = format, scale = "feuilles", ...)
  com <- cdg_construct_commune(commune)
  url <- cdg_aggr_url(com, pci_url)

  links <- cdg_detect_links(url)
  if (length(links) == 0) {
    stop(sprintf("No PCI sheets found for commune '%s' in format '%s'.",
                 commune, format), call. = FALSE)
  }

  if (skip) {
    links <- sub("^(edigeo|dxf)-(.*)\\.tar\\.bz2$", "\\2", links)
  }

  return(links)
}

#' Detect PCI feuille archives for a commune
#'
#' Retrieve the list of available PCI (Plan Cadastral Informatisé) feuille (sheet) archive names
#' for a given commune and format.
#'
#' @param commune `character`.
#' Commune code (e.g., INSEE code).
#' @param format `character`.
#' Data format, either `"edigeo"` or `"dxf"`. Default is `"edigeo"`.
#' @param skip `logical`. Default is `FALSE`.
#' If `TRUE`, strips the prefix (`edigeo-` or `dxf-`) and suffix (`.tar.bz2`) from the archive names.
#' @param ... Additional arguments passed to \code{\link{pci_construct_url}}.
#'
#' @return A `character` vector of archive names (with or without prefix/suffix depending on `skip`).
#'
#' @details
#' This function constructs the URL for PCI feuille scale data for the specified commune and format,
#' then detects and returns the available archive links.
#'
#' @seealso [pci_construct_url()]
#'
#' @examples
#' \dontrun{
#' # List available Edigeo feuille archives for a commune
#' archives <- pci_detect_feuilles("72187")
#'
#' # Get archive names without prefix and suffix
#' archives_clean <- pci_detect_feuilles("72187", skip = TRUE)
#' }
#'
#' @export
#'
pci_choose_feuilles <- function(commune,
                                format = "edigeo",
                                feuille = NULL,
                                skip = TRUE,
                                ...) {

  # Récupérer la liste des feuilles disponibles
  feuilles <- pci_detect_feuilles(commune, format = format, skip = skip, ...)

  # Valeur par défaut : toutes les feuilles
  default <- feuilles

  # Si aucun code feuille fourni → toutes les feuilles
  if (is.null(feuille)) {
    return(default)
  }

  # Si l'utilisateur veut choisir → menu interactif
  if (identical(feuille, "?")) {
    cat("Feuilles disponibles :\n")
    choice <- utils::menu(feuilles, title = sprintf("Choisissez une feuille pour la commune %s", commune))
    if (choice == 0) {
      return(default)  # annulation → toutes les feuilles
    } else {
      return(feuilles[choice])
    }
  }

  # Vérification si la feuille existe dans la liste
  if (!feuille %in% feuilles) {
    warning(sprintf("Feuille '%s' introuvable pour la commune %s, utilisation de toutes les feuilles",
                    feuille, commune))
    return(default)
  }

  # Sinon, retourner celle demandée
  return(feuille)
}

#' Get PCI feuille archive URLs for a commune
#'
#' Retrieve URLs for one or multiple PCI (Plan Cadastral Informatisé) feuille (sheet) archives
#' for a given commune and format.
#'
#' @param commune `character`.
#' Commune code (e.g., INSEE code).
#' @param feuille `character` or `NULL`.
#' Specific feuille(s) to get URLs for. If set to `"?"`, an interactive
#' menu is displayed allowing the user to select one or more feuilles.
#' @param format `character`. Default is `"edigeo"`.
#' Data format, either `"edigeo"` or `"dxf"`.
#' @param skip `logical`. Default is `TRUE`.
#' Passed to `pci_choose_feuilles()`.
#' If `TRUE`, skips prefix/suffix in feuille names.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return Character vector of URLs for the requested feuille archives.
#'
#' @details
#' If `feuille` is `"?"`, the user is prompted interactively to select one or multiple feuilles.
#' The function verifies availability of the requested feuilles before generating their URLs.
#'
#' @examples
#' \dontrun{
#' # Get URLs for specific feuilles
#' urls <- pci_get_feuille_url("72187", feuille = c("numvoie1", "numvoie2"))
#'
#' # Interactively select feuilles
#' urls <- pci_get_feuille_url("72187", feuille = "?")
#' }
#'
#' @export
#'
pci_get_feuille_url <- function(commune,
                                feuille = NULL,
                                format = "edigeo",
                                skip = TRUE,
                                ...) {

  # Si feuille = "?", on propose un menu pour choisir plusieurs feuilles
  if (identical(feuille, "?")) {
    # récupérer toutes les feuilles disponibles (sans filtre skip)
    all_feuilles <- pci_choose_feuilles(commune, format = format, feuille = NULL, skip = skip, ...)
    cat(sprintf("Feuilles disponibles pour la commune %s :\n", commune))

    # menu interactif : on utilise utils::select.list pour plusieurs sélections possibles
    choix <- utils::select.list(all_feuilles, multiple = TRUE, title = "Choisissez une ou plusieurs feuilles")

    if (length(choix) == 0) {
      stop("Aucune feuille sélectionnée. Abandon.")
    }

    feuilles <- choix
  } else {
    # sinon on utilise pci_choose_feuilles normalement
    feuilles <- pci_choose_feuilles(commune, format = format, feuille = feuille, skip = skip, ...)
  }

  # Base URL
  pci_url <- pci_construct_url(format = format, scale = "feuilles", ...)
  com <- cdg_construct_commune(commune)

  # Préfixe + extension des archives
  archive <- cfg_get_prefix_extent("pci", format, "feuilles")

  # Générer les noms des archives
  bz2 <- paste0(archive$prefix, "-", feuilles, archive$extent)

  # Vérification de disponibilité
  dispo <- pci_detect_feuilles(commune, skip = FALSE, format = format, ...)
  if (!all(bz2 %in% dispo)) {
    stop(sprintf(
      "Erreur : certaines feuilles demandées ne sont pas disponibles pour la commune %s.\nManquantes : %s",
      commune,
      paste(setdiff(bz2, dispo), collapse = ", ")
    ))
  }

  # Retourner toutes les URLs correspondantes
  urls <- vapply(bz2, function(f) cdg_aggr_url(c(com, f), pci_url), FUN.VALUE = character(1))
  return(urls)
}

#' Get PCI feuille archive URLs for multiple communes
#'
#' Retrieve URLs for feuille (sheet) archives for multiple communes in the PCI dataset,
#' optionally filtered by specific feuilles per commune.
#'
#' @param communes Character vector.
#' Vector of allowed commune codes (e.g., INSEE codes).
#' @param feuilles `list` or `NULL`. Default is `NULL`.
#' If `NULL`, retrieves all feuilles for all communes.
#' Otherwise, a list specifying feuilles per commune. The list can be named (names are commune codes)
#' or unnamed (must have the same length as `communes`, each element is feuilles for the corresponding commune).
#' @param format `character`. Default is `"edigeo"`.
#' Data format, either `"edigeo"` or `"dxf"`.
#' @param skip `logical`. Default is `TRUE`.
#' Passed to `pci_choose_feuilles()`.
#' If `TRUE`, skips prefix/suffix in feuille names.
#'
#' @param ... Additional arguments passed to \code{\link{pci_construct_url}}.
#'
#' @return A data.frame with columns:
#'   - `commune`: commune code
#'   - `url`: URL of the feuille archive
#'
#' @details
#' If `feuilles` is NULL, all feuilles for all communes are returned.
#' If `feuilles` is a named list, only the specified feuilles per commune are returned.
#' If unnamed, the length of `feuilles` must match `communes`, and each element corresponds to feuilles for that commune.
#'
#' @examples
#' \dontrun{
#' # Get all feuilles URLs for communes
#' df_all <- pci_get_feuille_urls(c("72187", "75056"))
#'
#' # Get specific feuilles for communes (named list)
#' df_some <- pci_get_feuille_urls(
#'   communes = c("72187", "75056"),
#'   feuilles = list("72187" = c("feuille1", "feuille2"), "75056" = "feuilleA")
#' )
#'
#' # Get feuilles with unnamed list (matching communes order)
#' df_unnamed <- pci_get_feuille_urls(
#'   communes = c("72187", "75056"),
#'   feuilles = list(c("feuille1", "feuille2"), NULL)
#' )
#' }
#'
#' @export
#'
pci_get_feuille_urls <- function(communes,       # vector of allowed communes (for validation)
                                 feuilles = NULL, # named OR unnamed list: sheets per commune, or NULL = all
                                 format = "edigeo",
                                 skip = TRUE,
                                 ...) {

  # If feuilles is NULL → all sheets for all communes
  if (is.null(feuilles)) {
    res_list <- lapply(communes, function(com) {
      urls <- pci_get_feuille_url(com, feuille = NULL, format = format, skip = skip, ...)
      data.frame(
        commune = com,
        url = urls,
        stringsAsFactors = FALSE
      )
    })

  } else {
    # feuilles must be a list
    if (!is.list(feuilles)) {
      stop("'feuilles' must be a named or unnamed list.", call. = FALSE)
    }

    if (is.null(names(feuilles))) {
      # Unnamed feuilles → length(feuilles) must match length(communes)
      if (length(feuilles) != length(communes)) {
        stop("If 'feuilles' is unnamed, its length must match the length of 'communes'.", call. = FALSE)
      }

      res_list <- mapply(function(com, com_feuilles) {
        if (is.null(com_feuilles)) {
          urls <- pci_get_feuille_url(com, feuille = NULL, format = format, skip = skip, ...)
        } else {
          urls <- vapply(com_feuilles, function(f) {
            pci_get_feuille_url(com, feuille = f, format = format, skip = skip, ...)
          }, character(1))
        }

        data.frame(
          commune = com,
          url = urls,
          stringsAsFactors = FALSE
        )
      }, communes, feuilles, SIMPLIFY = FALSE)

    } else {
      # Named feuilles → check keys
      feuilles_communes <- names(feuilles)
      if (!all(feuilles_communes %in% communes)) {
        stop("Some communes in 'feuilles' are not in the allowed 'communes' vector.", call. = FALSE)
      }

      res_list <- lapply(communes, function(com) {
        com_feuilles <- feuilles[[as.character(com)]]

        if (is.null(com_feuilles)) {
          urls <- pci_get_feuille_url(com, feuille = NULL, format = format, skip = skip, ...)
        } else {
          urls <- vapply(com_feuilles, function(f) {
            pci_get_feuille_url(com, feuille = f, format = format, skip = skip, ...)
          }, character(1))
        }

        data.frame(
          commune = com,
          url = urls,
          stringsAsFactors = FALSE
        )
      })
    }
  }

  do.call(rbind, res_list)
}
