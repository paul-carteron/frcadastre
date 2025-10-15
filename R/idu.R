### Manage IDU section ----
#' Build IDUs from their components
#'
#' Constructs a standardized 14-character IDU
#' from department, commune, prefix, section, and parcel number.
#'
#' @param dep `character`. Department code (mandatory).
#' Must be 2 characters (e.g., `"01"`, `"95"`, `"2A"`, `"2B"`)
#' or 3 characters for overseas departments (`"971"`–`"978"`).
#' @param com `character`. Commune code within the department.
#' Must be 3 characters for metropolitan France, or 2 characters for overseas departments.
#' @param prefix `character`. Prefix code (3 characters, zero-padded).
#' @param section `character`. Section code (2 characters, zero-padded, uppercase).
#' @param numero `character`. Parcel number (4 characters, zero-padded).
#'
#' @return A `character` vector of 14-character IDUs.
#'
#' @details
#' - All input vectors must have the same length.
#' - The function automatically zero-pads and uppercases fields where required.
#' - Both `dep` and `com` are required.
#'
#' @seealso [insee_check()], [idu_check()]
#'
#' @examples
#' \dontrun{
#' idu_build(dep = "72", com = "187", prefix = "000", section = "A", numero = "1")
#' }
#'
#' @export
#'
idu_build <- function(dep, com, prefix, section, numero) {
  # Check args
  missing_args <- c(
    dep     = missing(dep),
    com     = missing(com),
    prefix  = missing(prefix),
    section = missing(section),
    numero  = missing(numero)
  )
  if (any(missing_args)) {
    stop(
      "Missing required argument(s): ",
      paste(names(missing_args)[missing_args], collapse = ", "),
      call. = FALSE
    )
  }

  # Ensure character and format
  dep     <- as.character(dep)
  com     <- as.character(com)
  prefix  <- pad0(prefix, 3)
  section <- pad0(section, 2, upper = TRUE)
  numero  <- pad0(numero, 4)

  # Check consistent lengths
  lengths <- c(length(dep), length(com), length(prefix), length(section), length(numero))
  if (length(unique(lengths)) != 1) {
    stop("All input vectors (dep, com, prefix, section, numero) must have the same length.",
         call. = FALSE)
  }

  # Validate length of codes
  dep_len <- nchar(dep)
  com_len <- nchar(com)
  if (any(!dep_len %in% c(2, 3))) stop("`dep` must have 2 or 3 characters.", call. = FALSE)
  if (any(!com_len %in% c(2, 3))) stop("`com` must have 2 or 3 characters.", call. = FALSE)

  # Build full commune code
  commune <- ifelse(
    substr(dep, 1, 2) == "97",
    paste0(pad0(dep, 3, upper = TRUE), pad0(com, 2)),
    paste0(pad0(dep, 2, upper = TRUE), pad0(com, 3))
  )

  # Validate INSEE commune codes
  insee_check(commune, verbose = FALSE)

  # Build and validate IDUs
  idu <- paste0(commune, prefix, section, numero)
  valid <- idu_check(idu)
  if (!all(valid)) {
    stop("Invalid IDU(s) generated: ", paste(idu[!valid], collapse = ", "), call. = FALSE)
  }

  idu
}

#' Split IDUs into their components
#'
#' Splits a French cadastral parcel IDU (Identifiant de parcelle) into its
#' components: department, commune, prefix, section, and parcel number.
#'
#' @param idu `character`
#'   A vector of IDU codes (14 characters each). Non-character inputs will be coerced.
#'
#' @return A `data.frame` with one row per IDU and columns:
#' - `code_dep`  : Department code
#' - `code_com`  : Commune code
#' - `prefix`    : Prefix code
#' - `section`   : Section code
#' - `numero`    : Parcel number
#' - `insee`     : INSEE code of commune
#'
#' @details
#' The IDU structure is:
#' ```
#' [1-2]   : Department code (DEP)
#' [3-5]   : Commune code (COM)
#' [6-8]   : Prefix code
#' [9-10]  : Section code
#' [11-14] : Parcel number
#' ```
#'
#' @examples
#' \dontrun{
#' try(idu_split("0100200A0012"))
#' idu_split("29158000AK0001")
#' }
#'
#' @export
#'
idu_split <- function(idu) {
  idu_check(idu)

  # Extract first 2 and 3 characters (possible department codes)
  dep2 <- substr(idu, 1, 2)
  dep3 <- substr(idu, 1, 3)

  # Detect DOMs (department codes 971 to 978)
  is_dom <- dep2 == "97"

  # Department code: 2 or 3 characters depending on DOM
  code_dep <- ifelse(is_dom, dep3, dep2)

  # Commune code: adjust depending on DOM or mainland
  code_com <- ifelse(is_dom,
                     substr(idu, 4, 5),   # DOM = 2 digits for commune
                     substr(idu, 3, 5))   # Mainland = 3 digits

  # Reconstruct full INSEE code
  insee <- paste0(code_dep, code_com)

  # Validate INSEE code
  insee_check(insee, verbose = FALSE)

  # Return data.frame with all IDU components
  data.frame(
    idu      = idu,
    code_dep = code_dep,
    code_com = code_com,
    prefix   = substr(idu, 6, 8),
    section  = substr(idu, 9, 10),
    numero   = substr(idu, 11, 14),
    insee    = insee,
    stringsAsFactors = FALSE
  )
}

### Check IDU section ----
#' Check if a vector contains valid IDUs
#'
#' This function checks whether a character vector contains valid INSEE
#' cadastral parcel identifiers (IDUs). It can either return a logical
#' vector indicating validity or raise an error if invalid entries are found.
#'
#' @param x A character vector containing IDU codes to validate.
#' @param error Logical. If `TRUE` (default), the function stops with an error
#'   message when invalid IDUs are detected. If `FALSE`, it returns a logical
#'   vector indicating which entries are valid.
#'
#' @details
#' A valid IDU (INSEE cadastral identifier) must satisfy all of the following:
#' \itemize{
#'   \item Be a non-missing, non-empty string of length 14.
#'   \item The first two characters: department code — digits (0–9) or letters 'A'/'B'.
#'   \item The next three characters: commune code — digits (0–9).
#'   \item The next three characters: prefix — digits (0–9).
#'   \item The next two characters: section — digits (0–9) or uppercase letters (A–Z).
#'   \item The last four characters: parcel number — digits (0–9).
#'   \item Contain no lowercase letters or special characters.
#' }
#'
#' @return
#' If `error = FALSE`, a logical vector indicating which elements of `x` are valid.
#' If `error = TRUE`, the function stops when invalid codes are found and
#' invisibly returns `TRUE` otherwise.
#'
#' @examples
#' \dontrun{
#' # Valid IDU
#' idu_check("01001000AA0123")
#'
#' # Multiple values
#' idu_check(c("01001000AA0123", "AB12300000A0123"), error = FALSE)
#'
#' # Invalid example (will throw an error)
#' idu_check(c("01001000AA0123", "12345"))
#' }
#'
#' @export
#'
idu_check <- function(x, error = TRUE) {
  x <- as.character(x)
  pattern <- "^[0-9AB]{2}[0-9]{3}[0-9]{3}[0-9A-Z]{2}[0-9]{4}$"
  valid <- !is.na(x) & x != "" & nchar(x) == 14 & grepl(pattern, x)

  if (error && !all(valid)) {
    stop(
      "Invalid IDU(s) detected: ",
      paste(x[!valid], collapse = ", "),
      call. = FALSE
    )
  }

  if (error) {
    invisible(TRUE)
  } else {
    valid
  }
}

### Manage IDU field in df section ----
#' Detect the IDU column in a data frame
#'
#' This function scans each column of a data frame to identify the one
#' containing IDU values.
#'
#' @param df A `data.frame` or similar object to search.
#' @param output `character`. Defaults is `"both"`.
#' Astring indicating the type of output:
#'   `"name"` for the column name,
#'   `"position"` for the column index,
#'   or `"both"` for a list with both.
#'
#' @return
#' A named list with:
#' \describe{
#'   \item{name}{The column name containing IDU values}
#'   \item{position}{The column index in `df`}
#' }
#' Returns `NULL` if no column matches the IDU pattern.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   parcel_id = c("721870000A0001", "971020000B0002"),
#'   name = c("Oak", "Pine"),
#'   stringsAsFactors = FALSE
#' )
#' idu_detect_in_df(df)
#' idu_detect_in_df(df, output = "name")
#' idu_detect_in_df(df, output = "position")
#' }
#'
#' @export
#'
idu_detect_in_df <- function(df, output = c("both", "name", "position")) {
  output <- match.arg(output)
  if (!is.data.frame(df)) stop("'df' must be a data.frame or tibble", call. = FALSE)

  for (i in seq_along(df)) {
    col <- df[[i]]
    if (!is.character(col)) next
    if (all(idu_check(col, error = FALSE))) {
      return(switch(output,
                    name = names(df)[i],
                    position = i,
                    list(name = names(df)[i], position = i)))
    }
  }
  message("No column matches the IDU pattern.")
  NULL
}

#' Rename the IDU column in a data frame
#'
#' This function detects the column containing IDU values in a data frame
#' and renames it to the name provided by the user.
#'
#' @param df A `data.frame` or similar object to search.
#' @param new_name A `character` string specifying the new column name for the IDU column.
#'
#' @return A `data.frame` identical to `df` except the IDU column is renamed.
#' If no IDU column is detected, the original data frame is returned unchanged.
#'
#' @examples
#' df <- data.frame(
#'   parcel_id = c("12345ABCDE6789", "54321ZZZZZ0000"),
#'   name = c("Oak", "Pine"),
#'   stringsAsFactors = FALSE
#' )
#' df <- idu_rename_in_df(df, "IDU")
#' names(df)
#'
#' @export
#'
idu_rename_in_df <- function(df, new_name) {
  idu_info <- idu_detect_in_df(df, "both")
  if (is.null(idu_info)) {
    warning("No IDU column detected. Returning original data frame.", call. = FALSE)
    return(df)
  }
  names(df)[idu_info$position] <- new_name
  df
}

### Get attribut IDU section ----
#' Retrieve Etalab data for given IDUs
#'
#' This internal function downloads Etalab cadastral data corresponding to
#' one or more IDU codes. It also splits the IDUs into their components for
#' further processing.
#'
#' @param idu A character vector of IDU codes (14-character format).
#' @param layer Optional character string specifying a layer to retrieve.
#'   If NULL, all layers are returned.
#' @param verbose `logical`. If `TRUE`, prints progress messages.
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{An `sf` or `data.frame` object with the requested cadastral data.}
#'   \item{idu_parts}{A data.frame with the components of the provided IDUs.}
#' }
#'
#' @examples
#' \dontrun{
#' get_etalab_data_by_idu("72181000AB01")
#' get_etalab_data_by_idu(c("72181000AB01", "72181000AB02"), layer = "parcelles")
#' }
#'
#' @keywords internal
#'
get_etalab_data_by_idu <- function(idu,
                                   layer = NULL,
                                   verbose = TRUE) {
  idu_check(idu)
  idu_parts <- idu_split(idu)
  insee_codes <- unique(idu_parts$insee)

  data <- get_etalab(insee_codes, layer, verbose = verbose)

  list(data = data, idu_parts = idu_parts)
}

#' Retrieve cadastral sheets IDs for given IDUs
#'
#' This internal function retrieves the cadastral sheet (feuille) IDs associated with
#' one or more IDU codes. It validates the IDUs, extracts their INSEE and
#' sheet components, downloads sheet data from Etalab, and returns the
#' matching sheet IDs.
#'
#' @param idu A `character` vector of valid IDU codes.
#' @param result_as_list `logical`. If `TRUE`, returns a named list where
#'   names are INSEE codes and values are vectors of feuille IDs.
#'   If `FALSE` (default), returns a flat character vector of feuille IDs.
#'
#' @return Either a `character` vector of feuille IDs (`result_as_list = FALSE`)
#'   or a named `list` of feuille IDs grouped by INSEE code (`result_as_list = TRUE`).
#'
#' @importFrom sf st_drop_geometry
#'
#' @examples
#' \dontrun{
#' # Get a flat vector of feuille IDs
#' idu_get_feuille(idu = c("721870000A0001", "721870000A0002"))
#'
#' # Get feuille IDs grouped by INSEE code
#' idu_get_feuille(idu = c("721870000A0001", "721870000A0002"), result_as_list = TRUE)
#' }
#'
#' @export
#'
idu_get_feuille <- function(idu, result_as_list = FALSE) {
  # Retrieve Etalab data
  res <- get_etalab_data_by_idu(idu, "feuilles")

  # Extract feuille codes
  feuilles <- res$data
  feuilles$codes <- substr(feuilles$id, 1, 10)

  # Filter only the relevant feuilles
  selected <- feuilles[feuilles$codes %in% unique(substr(idu, 1, 10)), c("id", "commune")] |>
    st_drop_geometry()

  if (result_as_list) {
    # Create named list: names = insee codes, values = feuille IDs
    feuilles_list <- lapply(unique(res$idu_parts$insee), function(code) {
      selected$id[selected$commune == code]
    })
    names(feuilles_list) <- unique(res$idu_parts$insee)
    return(feuilles_list)
  } else {
    selected$id
  }
}

#' Get region/department/commune names from IDU codes
#'
#' This function takes one or more IDU codes, validates them, splits them into
#' their components, and merges them with reference datasets to retrieve the
#' corresponding region, department, and/or commune names.
#'
#' @param idu A `character` vector of valid IDU codes.
#' @param loc A `character` string specifying which location levels to include.
#' One of `"reg"`, `"dep"`, or `"com"`.
#' @param cog_field `character`. Défaut is `"NCC"`.
#' The name of the field in the reference datasets to use.
#'
#' @return A `data.frame` with the IDU split into its components and
#' the requested location names.
#'
#' @examples
#' \dontrun{
#' idu_get_cog(c("721870000A0001", "721870000A0002"))
#' }
#'
#' @keywords internal
#'
idu_get_cog <- function(idu, loc = c("reg", "dep", "com"), cog_field = "NCC") {
  # Match argument
  loc <- match.arg(loc, c("reg", "dep", "com"), several.ok = TRUE)

  # Validate IDUs
  idu_check(idu)

  # Split IDU into components
  idu_parts <- idu_split(idu)

  # Reference datasets
  regs <- frcadastre::region_2025
  deps <- frcadastre::departement_2025
  coms <- frcadastre::commune_2025

  res <- idu_parts

  # Merge region names if requested
  if ("reg" %in% loc) {
    res <- merge_with_name(res, deps, "code_dep", "DEP", "REG", "code_reg")
    res <- merge_with_name(res, regs, "code_reg", "REG", cog_field, "reg_name")
  }

  # Merge department names if requested
  if ("dep" %in% loc) {
    res <- merge_with_name(res, deps, "code_dep", "DEP", cog_field, "dep_name")
  }

  # Merge commune names if requested
  if ("com" %in% loc) {
    res <- merge_with_name(res, coms, "insee", "COM", cog_field, "com_name")
  }

  # Keep only idu and requested name columns
  keep_cols <- c("idu")
  if ("reg" %in% loc)   keep_cols <- c(keep_cols, "reg_name")
  if ("dep" %in% loc)   keep_cols <- c(keep_cols, "dep_name")
  if ("com" %in% loc)   keep_cols <- c(keep_cols, "com_name")

  res[, intersect(keep_cols, names(res)), drop = FALSE]
}

#' Retrieve parcel data for given IDUs
#'
#' This function takes one or more valid IDU codes, retrieves parcel data from
#' Etalab, and optionally enriches the parcels with their associated lieu-dit
#' names and administrative names. The function returns an `sf` object containing
#' the parcels and requested attributes.
#'
#' @param idu A `character` vector of valid IDU codes.
#' @param with_lieudit `logical` (default: `TRUE`). Whether to retrieve and merge
#'   lieu-dit names associated with the parcels.
#' @param with_cog `logical` (default: `TRUE`). Whether to retrieve and merge
#'   administrative names (region, department, commune) associated with the parcels.
#' @param ... Additional arguments passed to [idu_get_cog()].
#'
#' @return An `sf` object containing parcel geometries, the IDU code, and optionally
#'   associated lieu-dit names and administrative names.
#'
#' @details
#' - All IDU codes are validated before any data is retrieved.
#' - If `with_lieudit = TRUE`, the function performs a spatial join with the
#'   Etalab "lieux-dits" dataset and merges the names into the parcel data.
#' - If `with_cog = TRUE`, the function retrieves region, department, and commune
#'   names using [idu_get_cog()] and merges them into the parcel data.
#' - The function ensures that Etalab data are returned as `sf` objects.
#'
#' @importFrom sf st_join st_drop_geometry
#'
#' @examples
#' \dontrun{
#' # Retrieve parcels with both lieudit and names
#' idu_get_parcelle(c("721870000A0001", "721870000A0002"))
#'
#' # Retrieve parcels without lieudit
#' idu_get_parcelle("721870000A0001", with_lieudit = FALSE)
#'
#' # Retrieve parcels without administrative names
#' idu_get_parcelle("721870000A0001", with_cog = FALSE)
#' }
#'
#' @export
#'
idu_get_parcelle <- function(idu, with_lieudit = TRUE, with_cog = TRUE, ...) {
  # Validate IDUs
  idu_check(idu)

  # Split IDU and extract unique INSEE codes
  idu_parts   <- idu_split(idu)
  insee_codes <- unique(idu_parts$insee)

  # Download parcels
  parcelles <- get_etalab(insee_codes, verbose = TRUE) |>
    idu_rename_in_df("idu") |>
    subset(idu %in% idu_parts$idu)

  # Ensure parcels are sf objects
  if (!inherits(parcelles, "sf")) {
    stop("Etalab data must be 'sf' objects.", call. = FALSE)
  }

  # Retrieve lieux-dits if requested
  if (with_lieudit) {
    lieudits <- tryCatch(
      get_etalab(insee_codes, "lieux_dits", verbose = FALSE),
      error = function(e) NULL
    )

    # Only process if lieudits is an sf object
    if (inherits(lieudits, "sf")) {
      intersections <- suppressWarnings(
        st_join(parcelles, lieudits, largest = TRUE) |> st_drop_geometry()
      )

      # Warn if some lieu-dit names are missing
      if (anyNA(intersections$nom)) {
        warning("Some lieu-dit names are missing (NA) in the 'etalab' data.")
      }

      # Merge parcels with lieu-dit names
      parcelles <- merge_with_name(
        parcelles, intersections,
        ref_x   = "idu",
        ref_y   = "idu",
        ini_col = "nom",
        fin_col = "lieudit"
      )
    }
  }

  # Retrieve parcel names if requested
  if (with_cog) {
    names_df <- idu_get_cog(idu, ...)
    new_cols <- setdiff(names(names_df), "idu")

    parcelles <- merge_with_name(
      parcelles, names_df,
      ref_x   = "idu",
      ref_y   = "idu",
      ini_col = new_cols,
      fin_col = new_cols
    )
  }

  parcelles
}

#' Retrieve attributes for given IDUs
#'
#' This function extracts one or more attributes associated with cadastral parcels
#' identified by their IDU (Unique Parcel ID). It uses [idu_get_parcelle()] to
#' retrieve parcel data and then returns only the requested attributes.
#'
#' @param idu A `character` vector of valid IDU codes.
#' @param attribute `character`. One or more attributes to retrieve. Choices are:
#'   \describe{
#'     \item{`"lieudit"`}{Lieu-dit name associated with the parcel.}
#'     \item{`"contenance"`}{Parcel surface area (in square meters).}
#'     \item{`"reg_name"`}{Region name associated with the parcel.}
#'     \item{`"dep_name"`}{Department name associated with the parcel.}
#'     \item{`"com_name"`}{Commune name associated with the parcel.}
#'   }
#' @param sf_as_result `logical` (default: `FALSE`). If `TRUE`, the result is
#'   returned as an `sf` object with geometries; otherwise, a plain data.frame
#'   without geometries is returned.
#'
#' @return A `data.frame` or an `sf` object containing the `idu` column and the
#'   requested attribute(s).
#'
#' @details
#' - All IDU codes are validated before retrieving any data.
#' - By default, geometry is dropped and a `data.frame` is returned.
#' - Setting `sf_as_result = TRUE` preserves the geometry and returns an `sf` object.
#'
#' @importFrom sf st_drop_geometry
#'
#' @examples
#' \dontrun{
#' idu <- c("721870000A0001", "721870000A0002")
#'
#' # Retrieve lieu-dit names
#' idu_get_attribute(idu, attribute = "lieudit")
#'
#' # Retrieve parcel surfaces
#' idu_get_attribute(idu, attribute = "contenance")
#'
#' # Retrieve multiple attributes as a plain data.frame
#' idu_get_attribute(idu, attribute = c("lieudit", "contenance"))
#'
#' # Retrieve multiple attributes as an sf object
#' idu_get_attribute(idu, attribute = c("lieudit", "reg_name"), sf_as_result = TRUE)
#' }
#'
#' @export
#'
idu_get_attribute <- function(idu,
                              attribute = c("lieudit", "contenance",
                                            "reg_name", "dep_name", "com_name"),
                              sf_as_result = FALSE) {

  # Validate and normalize requested attributes
  attribute <- match.arg(attribute,
                         choices = c("lieudit", "contenance",
                                     "reg_name", "dep_name", "com_name"),
                         several.ok = TRUE)

  # Get parcels with minimal data needed
  res <- idu_get_parcelle(idu)[, c("idu", attribute), drop = FALSE]

  # Convert to plain data.frame unless sf output is requested
  if (isFALSE(sf_as_result)) {
    res <- st_drop_geometry(res)
  }

  res
}
