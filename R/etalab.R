#' Construct Etalab data URL
#'
#' Constructs a URL to access Etalab data for a specified scale.
#'
#' @param scale Character. Scale of data to retrieve. Must be one of `"departements"` or `"communes"`.
#'   Default is `"communes"`.
#' @param ... Additional arguments passed to `cdg_construct_url()`.
#'
#' @return Character string with the constructed URL.
#'
#' @examples
#' \dontrun{
#' etalab_construct_url(scale = "departements")
#' etalab_construct_url(scale = "communes")
#' }
#'
#' @export
#'
etalab_construct_url <- function(scale = "communes",
                                 ...) {
  cdg_construct_url(
    site = "etalab",
    format = "geojson",
    scale = scale,
    allowed_formats = c("geojson"),
    allowed_scales = c("departements", "communes"),
    ...
  )
}

#' Get Etalab data URL for a given INSEE code and data type
#'
#' Constructs the download URL for Etalab data based on the INSEE code and requested data type.
#'
#' @param insee_code Character. INSEE code of the geographic unit (commune or department).
#' @param data Character. Type of data to retrieve. Must be one of the allowed data types for the given scale.
#' @param ... Additional arguments passed to `etalab_construct_url()` and other internal functions.
#'
#' @return Character string with the constructed URL.
#'
#' @details
#' The function automatically detects the geographic scale (commune or department) from the INSEE code,
#' builds the corresponding URL, and verifies the data type is allowed. It constructs the full URL
#' including the archive prefix and extension.
#'
#' @examples
#' \dontrun{
#' etalab_get_data_url("75056", "batiments")
#' etalab_get_data_url("69", "voie")
#' }
#'
#' @export
#'
etalab_get_data_url <- function(insee_code, data, ...){

  # scale
  scale <- cdg_detect_insee_code(insee_code, T, F)
  if (scale == "communes"){
    loc <- cdg_construct_commune(insee_code)
  } else {
    loc <- insee_code
  }

  # base url
  etalab_url <- etalab_construct_url(scale = scale, ...)

  # data
  allowed_data <- cfg_get_data(scale, T)
  data <- match.arg(data, allowed_data$data)
  type <- allowed_data[allowed_data$data == data, "type"]
  if (type == "raw"){
    raw <- type
  } else {
    raw <- NULL
  }

  # archive
  archive <- cfg_get_prefix_extent(site = "etalab",
                                   format = "geojson",
                                   scale = scale
                                   )[[type]]

  gz <- paste0(archive$prefix, "-", insee_code, "-", data, archive$extent)

  # result
  cdg_aggr_url(c(loc, raw, gz), etalab_url)
}

#' Get Etalab data URLs for multiple INSEE codes and data types
#'
#' Constructs download URLs for Etalab data for a vector of INSEE codes and optionally specified data types.
#'
#' @param insee_code Character vector. Vector of INSEE codes (communes or departments).
#' @param data NULL or list. If NULL, retrieves URLs for all available data types for each INSEE code.
#'   If a list, it can be named or unnamed:
#'   - unnamed list: length must match length of `insee_code`, each element is a vector of data types for corresponding INSEE code,
#'   - named list: names correspond to INSEE codes, values are vectors of data types for those codes.
#' @param ... Additional arguments passed to `etalab_get_data_url()`.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{insee_code}{INSEE code (character).}
#'   \item{data}{Data type requested (character).}
#'   \item{url}{Constructed URL (character).}
#' }
#'
#' @details
#' This function supports flexible specification of data types to retrieve for each INSEE code.
#' If `data` is NULL, all data types available for each INSEE code's scale are returned.
#' The function internally validates arguments and constructs URLs using `etalab_get_data_url()`.
#'
#' @examples
#' \dontrun{
#' # All data for all given INSEE codes
#' etalab_get_data_urls(c("75056", "69123"))
#'
#' # Specific data types for each INSEE code (unnamed list)
#' etalab_get_data_urls(c("75056", "69123"), data = list(c("batiments"), c("voie", "parcelles")))
#'
#' # Named list of data types by INSEE code
#' etalab_get_data_urls(
#'   c("75056", "69123"),
#'   data = list("75056" = c("batiments"), "69123" = c("voie", "parcelles"))
#' )
#' }
#'
#' @export
etalab_get_data_urls <- function(insee_code,     # vecteur de codes INSEE
                                 data = NULL,     # liste nommée ou non nommée : données par code INSEE, ou NULL = toutes données
                                 ...) {

  # Si data est NULL → toutes les données pour tous les codes INSEE
  if (is.null(data)) {
    res_list <- lapply(insee_code, function(code) {
      scale <- cdg_detect_insee_code(code, T, F)
      all_data <- cfg_get_data(scale, TRUE)$data
      urls <- vapply(all_data, function(d) {
        etalab_get_data_url(insee_code = code, data = d, ...)
      }, character(1))
      data.frame(
        insee_code = code,
        data = all_data,
        url = urls,
        stringsAsFactors = FALSE
      )
    })

  } else {
    # data doit être une liste
    if (!is.list(data)) {
      stop("'data' doit être une liste nommée ou non nommée.")
    }

    if (is.null(names(data))) {
      # data non nommée → on suppose que longueur(data) == longueur(insee_code)
      if (length(data) != length(insee_code)) {
        stop("Si 'data' n'a pas de noms, sa longueur doit être égale à celle de 'insee_code'.")
      }

      res_list <- mapply(function(code, datas) {
        if (is.null(datas)) {
          scale <- cdg_detect_insee_code(code, scale = TRUE)
          datas <- cfg_get_data(scale, TRUE)$data
        }
        urls <- vapply(datas, function(d) {
          etalab_get_data_url(insee_code = code, data = d, ...)
        }, character(1))

        data.frame(
          insee_code = code,
          data = datas,
          url = urls,
          stringsAsFactors = FALSE
        )
      }, insee_code, data, SIMPLIFY = FALSE)

    } else {
      # data nommée → vérifier que toutes les clés sont dans insee_code
      data_names <- names(data)
      if (!all(data_names %in% insee_code)) {
        stop("Certaines clés dans 'data' ne sont pas dans le vecteur 'insee_code'.")
      }

      res_list <- lapply(insee_code, function(code) {
        datas <- data[[as.character(code)]]
        if (is.null(datas)) {
          scale <- cdg_detect_insee_code(code, scale = TRUE)
          datas <- cfg_get_data(scale, TRUE)$data
        }

        urls <- vapply(datas, function(d) {
          etalab_get_data_url(insee_code = code, data = d, ...)
        }, character(1))

        data.frame(
          insee_code = code,
          data = datas,
          url = urls,
          stringsAsFactors = FALSE
        )
      })
    }
  }

  do.call(rbind, res_list)
}
