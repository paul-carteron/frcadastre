#' Download and return a specific cadastre layer from Etalab
#'
#' Retrieves cadastral data for one or more INSEE codes from Etalab,
#' optionally specifying the type of data layer (e.g., "parcelles" or "lieudit").
#'
#' @param insee_code `character` or `numeric`.
#' Vector of INSEE codes for the communes to download.
#' @param data `character`.
#' Name of the data layer to retrieve. Default is `"parcelles"`.
#' See available data with \code{\link{cfg_get_data}}
#' @param ... Additional arguments passed to `get_cadastre_etalab()`.
#'
#' @return `sf` object. The requested cadastral layer with unique features.
#'
#' @importFrom sf st_drop_geometry
#'
#' @seealso [get_cadastre_etalab(), cfg_get_data()]
#'
#' @export
#'
get_quick_etalab <- function(insee_code,
                             data = "parcelles",
                             ...) {

  unique_insee <- unique(as.character(insee_code))
  data_list <- rep(list(data), length(unique_insee))

  sf_data <- get_cadastre_etalab(
    insee_code = unique_insee,
    data = data_list,
    ...
  )

  if (is.null(sf_data) || !data %in% names(sf_data)) {
    warning(sprintf("Data '%s' not found in 'etalab' query.", data))
    return(NULL)
  }

  return(unique(sf_data[[data]]))
}
