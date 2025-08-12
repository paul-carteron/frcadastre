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
                            zip_prefix = "dep",
                            zip_ext = ".zip",
                            ...){

  pci_url <- pci_construct_url(scale ="departements", ...)

  if (!departement %in% Rsequoia2::departements_2024$DEP) {
    stop(sprintf("Erreur : le département '%s' n'est pas valide. Please run Rsequoia2::departements_2024$DEP",
                 departement))
  } else {
    dep <- departement
  }

  zip <- paste0(zip_prefix, dep, zip_ext)
  cdg_aggr_url(c(pci_url, zip))
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
                                ...){

  pci_url <- pci_construct_url(scale ="feuilles", ...)
  com <- cdg_construct_commune(commune)
  url <- cdg_aggr_url(c(pci_url, com))
  cdg_detect_links(url)
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
                                feuille,
                                bz2_prefix = "edigeo-",
                                bz2_ext = ".tar.bz2",
                                ...){

  pci_url <- pci_construct_url(scale ="feuilles", ...)
  com <- cdg_construct_commune(commune)
  bz2 <- paste0(bz2_prefix, feuille, bz2_ext)

  if (!bz2 %in% pci_detect_feuilles(commune)) {
    stop(sprintf("Erreur : la feuille '%s' n'est pas disponible pour la commune %s.", bz2, commune))
  }

  cdg_aggr_url(c(pci_url, bz2))
}
