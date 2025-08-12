#' Construire une URL pour les données PCI
#'
#' Cette fonction construit une URL complète pour accéder aux données PCI
#' en fonction du format et de l'échelle choisis.
#'
#' @param format `character`.
#' Format des données, parmi ``"dxf"`` ou ``"edigeo"``.
#' Par défaut ``"edigeo"``.
#'
#' @param scale `character`.
#' Échelle des données, parmi ``"departements"`` ou ``"feuilles"``.
#' Par défaut ``"feuilles"``.
#' @param ...
#' Arguments supplémentaires passés à `cdg_construct_url()`.
#'
#' @return  `character`.
#' Une URL construite sous forme de chaîne de caractères.
#'
#' @seealso [cdg_construct_url()]
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

#' Construire l'URL d'un département PCI
#'
#' Cette fonction génère l'URL d'accès aux données PCI pour un département donné.
#'
#' @param departement `character`.
#' Code du département à utiliser. Doit appartenir à la liste ``Rsequoia2::departements_2024$DEP``.
#' @param zip_prefix `character`.
#' Préfixe du fichier zip. Par défaut ``"dep"``.
#' @param zip_ext `character`.
#' Extension du fichier zip. Par défaut ``".zip"``.
#' @param ...
#' Arguments supplémentaires passés à \code{\link{pci_construct_url}}.
#'
#' @return `character`.
#' Une URL construite sous forme de chaîne de caractères.
#'
#' @details
#' La fonction vérifie que le département est valide avant de construire l'URL.
#'
#' @seealso  [pci_construct_url(), cdg_aggr_url()]
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
    stop(sprintf("Erreur : le département '%s' n'est pas valide. Please run Rsequoia2::departements_2024$DEP",
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

pci_get_dep_urls <- function(departements, ...) {
  vapply(
    departements,
    FUN = function(dep) {
      pci_get_dep_url(departement = dep, ...)
    },
    FUN.VALUE = character(1)  # garantit un vecteur de chaînes
  )
}

#' Détecter les feuilles PCI disponibles pour une commune
#'
#' Cette fonction retourne la liste des feuilles disponibles pour une commune donnée dans le PCI.
#'
#' @param commune `character`.
#' Code INSEE de la commune (ex: "02120").
#' @param ...
#' Arguments supplémentaires passés à \code{\link{pci_construct_url}}.
#'
#' @return  `character`.
#' Un vecteur des noms des feuilles PCI disponibles.
#'
#' @details
#' La fonction construit l'URL correspondant à la commune
#' et utilise `cdg_detect_links()` pour récupérer les feuilles disponibles.
#'
#' @seealso [pci_construct_url(), cdg_construct_commune(), cdg_detect_links()]
#'
#' @export
#'
pci_detect_feuilles <- function(commune,
                                format = "edigeo",
                                skip = FALSE,
                                ...){

  pci_url <- pci_construct_url(format = format, scale ="feuilles", ...)
  com <- cdg_construct_commune(commune)
  url <- cdg_aggr_url(com, pci_url)
  links <- cdg_detect_links(url)
  if (skip) {
    links <- sub("^(edigeo|dxf)-(.*)\\.tar\\.bz2$", "\\2", links)
  }
  return(links)
}

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

#' Obtenir l'URL d'une feuille PCI pour une commune donnée
#'
#' Cette fonction construit l'URL d'accès à une feuille PCI spécifique pour une commune.
#'
#' @param commune `character`.
#' Code INSEE de la commune (ex: "02120").
#' @param feuille `character`.
#' Nom de la feuille PCI (sans préfixe ni extension).
#' @param bz2_prefix `character`.
#' Préfixe du fichier archive (par défaut `"edigeo-"`).
#' @param bz2_ext `character`.
#' Extension du fichier archive (par défaut `".tar.bz2"`).
#' @param ...
#' Arguments supplémentaires passés à `pci_construct_url()`.
#'
#' @return `character`.
#' Une URL complète vers la feuille PCI.
#'
#' @details
#' La fonction vérifie que la feuille demandée existe parmi celles détectées pour la commune,
#' sinon elle génère une erreur.
#'
#' @seealso [pci_construct_url(), pci_detect_feuilles(), cdg_construct_commune(), cdg_aggr_url()]
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

pci_get_feuille_urls <- function(communes,   # vecteur de communes autorisées (pour contrôle)
                                 feuilles = NULL,  # liste nommée OU non nommée : feuilles par commune, ou NULL = toutes
                                 format = "edigeo",
                                 skip = TRUE,
                                 ...) {

  # Si feuilles est NULL → toutes les feuilles pour toutes les communes
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
    # feuilles doit être une liste
    if (!is.list(feuilles)) {
      stop("'feuilles' doit être une liste nommée ou non nommée.")
    }

    if (is.null(names(feuilles))) {
      # feuilles non nommée → on suppose que longueur(feuilles) == length(communes)
      if (length(feuilles) != length(communes)) {
        stop("Si 'feuilles' n'a pas de noms, sa longueur doit être égale à celle de 'communes'.")
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
      # feuilles nommée → vérification clés
      feuilles_communes <- names(feuilles)
      if (!all(feuilles_communes %in% communes)) {
        stop("Certaines communes dans 'feuilles' ne sont pas dans le vecteur 'communes' autorisées.")
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
