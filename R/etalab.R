### URL section ----
#' Construct Etalab data URL
#'
#' Constructs a URL to access Etalab data for a specified scale.
#'
#' @param scale `character`. Default is `"communes"`.
#' Scale of data to retrieve. Must be one of `"departements"` or `"communes"`.
#' @param ... Additional arguments passed to \code{\link{cdg_construct_url}}.
#'
#' @return A `character` string with the constructed URL.
#'
#' @seealso [cdg_construct_url()]
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

### Data section ----
#' Get Etalab data URL for a given INSEE code and data type
#'
#' Constructs the download URL for Etalab data based on the INSEE code and requested data type.
#'
#' @param insee_code `character`.
#' INSEE code of the geographic unit (commune or department).
#' @param data `character`.
#' Type of data to retrieve. Must be one of the allowed data types for the given scale.
#' @param ... Additional arguments passed to \code{\link{etalab_construct_url}}.
#'
#' @return A `character` string with the constructed URL.
#'
#' @details
#' The function automatically detects the geographic scale (commune or department) from the INSEE code,
#' builds the corresponding URL, and verifies the data type is allowed. It constructs the full URL
#' including the archive prefix and extension.
#'
#' @seealso [etalab_construct_url()]
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
#' @param insee_code `character` vector.
#' Vector of INSEE codes (communes or departments).
#' @param data `list` or `NULL`. Defaut is `NULL`.
#' If `NULL`, retrieves URLs for all available data types for each INSEE code.
#' If a list, it can be named or unnamed:
#'   - unnamed list: length must match length of `insee_code`, each element is a vector of data types for corresponding INSEE code,
#'   - named list: names correspond to INSEE codes, values are vectors of data types for those codes.
#' @param verbose `logical`.
#' If `TRUE`, print informative messages.
#' @param ... Additional arguments passed to \code{\link{etalab_construct_url}}.
#'
#' @return A `data.frame` with columns:
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
#' @seealso [etalab_get_data_url(), etalab_construct_url()]
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
#'
etalab_get_data_urls <- function(insee_code,     # vector of INSEE codes
                                 data = NULL,    # list (named or unnamed): datasets per INSEE code, or NULL = retrieve all datasets
                                 verbose = TRUE,
                                 ...) {

  # Case 1: if 'data' is NULL → return all available datasets for all given INSEE codes
  if (is.null(data)) {
    res_list <- lapply(insee_code, function(code) {
      # Detect the type/scale of the INSEE code (commune, department, region, etc.)
      scale <- cdg_detect_insee_code(code, scale = TRUE, verbose)

      # Get the list of available datasets for this scale
      all_data <- cfg_get_data(scale, TRUE)$data

      # Build the URL for each dataset using 'etalab_get_data_url'
      urls <- vapply(all_data, function(d) {
        etalab_get_data_url(insee_code = code, data = d, ...)
      }, character(1))

      # Return a dataframe with one row per dataset
      data.frame(
        insee_code = code,
        data = all_data,
        url = urls,
        stringsAsFactors = FALSE
      )
    })

  } else {
    # Case 2: user provided 'data'

    # Ensure that 'data' is indeed a list
    if (!is.list(data)) {
      stop("'data' must be a list, named or not.")
    }

    # Case 2a: 'data' is an unnamed list
    if (is.null(names(data))) {
      # Ensure that 'data' has the same length as 'insee_code'
      if (length(data) != length(insee_code)) {
        stop("'data' must have same length that 'insee_code'")
      }

      # For each INSEE code and its corresponding data list
      res_list <- mapply(function(code, datas) {
        # If no data specified → retrieve all available datasets
        if (is.null(datas)) {
          scale <- cdg_detect_insee_code(code, scale = TRUE)
          datas <- cfg_get_data(scale, TRUE)$data
        }

        # Build URLs for each dataset
        urls <- vapply(datas, function(d) {
          etalab_get_data_url(insee_code = code, data = d, ...)
        }, character(1))

        # Return a dataframe for this code
        data.frame(
          insee_code = code,
          data = datas,
          url = urls,
          stringsAsFactors = FALSE
        )
      }, insee_code, data, SIMPLIFY = FALSE)

    } else {
      # Case 2b: 'data' is a named list (names = INSEE codes)

      data_names <- names(data)
      # Ensure that all provided names exist in 'insee_code'
      if (!all(data_names %in% insee_code)) {
        stop("Some keys in 'data' are not found for 'insee_code'.")
      }

      res_list <- lapply(insee_code, function(code) {
        # Get datasets associated with this INSEE code
        datas <- data[[as.character(code)]]

        # If not specified, retrieve all available datasets
        if (is.null(datas)) {
          scale <- cdg_detect_insee_code(code, scale = TRUE)
          datas <- cfg_get_data(scale, TRUE)$data
        }

        # Build URLs for each dataset
        urls <- vapply(datas, function(d) {
          etalab_get_data_url(insee_code = code, data = d, ...)
        }, character(1))

        # Return dataframe for this code
        data.frame(
          insee_code = code,
          data = datas,
          url = urls,
          stringsAsFactors = FALSE
        )
      })
    }
  }

  # Combine all dataframes into a single dataframe
  do.call(rbind, res_list)
}

#' Filter and validate existing URLs in a data.frame
#'
#' This function checks if the URLs in a data.frame are accessible.
#' Invalid or unreachable URLs are removed from the result.
#' Optionally, warnings and messages are displayed if `verbose = TRUE`.
#'
#' @param df A `data.frame` with columns \code{insee_code}, \code{data}, and \code{url}.
#' @param verbose Logical (default = `TRUE`).
#' If `TRUE`, display warnings for removed URLs and a message about how many were kept.
#'
#' @return A filtered `data.frame` containing only rows with valid and accessible URLs.
#'
#' @seealso [.url_exists] for the low-level URL existence check.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   insee_code = c("00001", "00002"),
#'   data = c("example1", "example2"),
#'   url = c("https://www.r-project.org", "https://nonexistent.example.com"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Filter valid URLs
#' etalab_filter_existing_urls(df, verbose = TRUE)
#'
#' @export
#'
etalab_filter_existing_urls <- function(df, verbose = TRUE) {
  # Check that input has the required structure
  if (!all(c("insee_code", "data", "url") %in% names(df))) {
    stop("The input must be a data.frame with columns 'insee_code', 'data' and 'url'.")
  }

  # Apply the URL test to all rows
  exists_vec <- vapply(df$url, .url_exists, logical(1))

  # If some URLs are invalid → show a warning only if verbose = TRUE
  if (any(!exists_vec) && verbose) {
    bad_urls <- df$url[!exists_vec]
    warning(sprintf("%d URL(s) not accessible and removed:\n- %s",
                    length(bad_urls),
                    paste(bad_urls, collapse = "\n- ")),
            call. = FALSE)
  }

  # Keep only the valid URLs
  res <- df[exists_vec, , drop = FALSE]

  # Inform the user if verbose = TRUE
  if (verbose) {
    message(sprintf("Kept %d / %d URLs.", nrow(res), nrow(df)))
  }

  return(res)
}
