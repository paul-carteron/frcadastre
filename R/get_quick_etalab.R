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
#' @param verbose `logical` (default: `FALSE`)
#' If `TRUE`, warnings about missing URLs, or failed downloads are displayed.
#' If `FALSE`, the function fails silently and returns `NULL` when issues occur.
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
                             verbose = TRUE,
                             ...) {

  unique_insee <- unique(as.character(insee_code))
  data_list <- rep(list(data), length(unique_insee))

  sf_data <- get_cadastre_etalab(
    insee_code = unique_insee,
    data = data_list,
    verbose = verbose,
    ...
  )

  if (is.null(sf_data)) {
    return(NULL)
  }

  return(sf_data)
}
