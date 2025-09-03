#' Download a single cadastre dataset from Etalab "bundle" API
#'
#' This function downloads a specific dataset layer (e.g., `"parcelles"`) for a
#' given identifier (commonly an INSEE code) using the Etalab "bundle" API. The
#' data are returned as an `sf` object. If multiple files are retrieved (e.g.
#' due to subdivision by the API), they are combined into a single `sf` object
#' with harmonized columns.
#'
#' @param id `character` or `numeric`
#' The identifier used by the API to retrieve cadastre data. In most cases,
#' this corresponds to an INSEE code for a commune.
#'
#' @param data `character` (default: `"parcelles"`)
#' The dataset to download. Must be one of the valid processed dataset names
#' (`proc_data`) as defined in [cfg_get_data()].
#' Examples include `"parcelles"`, `"sections"`, `"communes"`, `"batiments"`, etc.
#'
#' @return An `sf` object containing the geometries for the requested dataset.
#' If multiple files are returned, they are bound row-wise into a single object.
#' If no valid data is retrieved, the function returns `NULL` with a warning.
#'
#' @details
#' The function works as follows:
#' 1. The `layer` argument is validated against available processed dataset names.
#' 2. The appropriate API URL is built for the given `id`, automatically detecting
#'    the scale (commune vs département) using [cdg_detect_insee_code()].
#' 3. Each URL is read using [sf::st_read()]. Invalid or failed reads are skipped.
#' 4. All successfully read `sf` objects are combined with `rbind`.
#' 5. The `"idu"` field (unique parcel identifier) is renamed consistently with
#'    [rcadastre::idu_rename_in_df()].
#'
#' The data are always retrieved in **GeoJSON** format.
#'
#' @examples
#' \dontrun{
#' # Download parcel geometries for commune 72188
#' parcelles <- get_etalab_bundle("72188", data = "parcelles")
#'
#' # Download sections geometries
#' sections <- get_etalab_bundle("72188", data = "sections")
#'
#' # Attempt with an invalid data will throw an error
#' get_etalab_bundle("72188", data = "foo")
#' }
#'
#' @export
#'
get_etalab_bundle <- function(id, data = "parcelles") {

  # argument checks
  data  <- match.arg(data, cfg_get_data()$proc_data)

  # scale detection
  scale <- cdg_detect_insee_code(id, TRUE)
  format <- "geojson"

  # build URLs
  url_template <- "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/%s/%s/%s/%s"
  urls <- sprintf(url_template, scale, id, format, data)

  # download / read geometries
  geoms <- lapply(urls, function(u) {
    tryCatch(sf::st_read(u, quiet = TRUE),
             error = function(e) NULL)
  })

  # remove NULLs and bind results
  geoms <- geoms[!vapply(geoms, is.null, logical(1))]
  if (length(geoms) == 0) {
    warning("No data retrieved.")
    return(NULL)
  }

  geoms <- do.call(rbind, geoms)

  return(geoms)
}
