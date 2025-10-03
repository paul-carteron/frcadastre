### Manage IDU section ----
#' Format IDU components with padding and optional uppercase
#'
#' This internal utility formats a vector of values by padding them with
#' leading zeros to a specified width. Optionally, the values can be converted
#' to uppercase.
#'
#' @param x A vector of values to format.
#' @param width An integer specifying the target width for each value.
#' @param upper Logical; if TRUE, convert the values to uppercase.
#'
#' @return A character vector with each element formatted to the specified width
#' and optionally in uppercase.
#'
#' @details
#' - Leading spaces are replaced with zeros.
#' - Useful for constructing IDU codes or components consistently.
#'
#' @examples
#' \dontrun{
#' idu_fmt(c(1, 23, 456), width = 5)
#' # Returns: "00001" "00023" "00456"
#'
#' idu_fmt(c("ab", "cd"), width = 4, upper = TRUE)
#' # Returns: "00AB" "00CD"
#' }
#'
#' @keywords internal
#'
idu_fmt <- function(x, width, upper = FALSE) {
  x <- sprintf(paste0("%", width, "s"), as.character(x))
  x <- gsub(" ", "0", x)
  if (upper) x <- toupper(x)
  x
}

#' Build a unique parcel identifier (IDU)
#'
#' Constructs a standardized 14-character IDU (Identifiant parcellaire Unique)
#' from department, commune, prefix, section, and parcel number.
#'
#' @param dep `character` or `NULL`. Department code.
#' Must be 2 characters (e.g., `"01"`, `"95"`, `"2A"`, `"2B"`),
#' or 3 characters for overseas departments (`"971"`–`"978"`).
#' Optional if `com` already contains the full 5-character commune code.
#' @param com `character`. Commune code.
#' - If length 5: full INSEE commune code (department already included).
#' - If length 3: commune code within a metropolitan department (requires `dep`).
#' - If length 2: commune code within an overseas department (requires `dep`).
#' @param prefix `character`. Prefix code (3 characters, zero-padded).
#' @param section `character`. Section code (2 characters, zero-padded, uppercase).
#' @param numero `character`. Parcel number (4 characters, zero-padded).
#'
#' @return A `character` vector of 14-character IDUs.
#'
#' @details
#' - All input vectors must have the same length.
#' - The function automatically zero-pads and uppercases fields where required.
#' - Input is validated against INSEE commune and department codes.
#'
#' @seealso [insee_check()], [idu_check()]
#'
#' @examples
#' \dontrun{
#' # With separate department and commune codes
#' idu_build(dep = "72", com = "187", prefix = "000", section = "A", numero = "1")
#'
#' # With commune code including department
#' idu_build(com = "72187", prefix = "000", section = "A", numero = "1")
#'
#' # With a plots dataframe
#' parcelle <- get_etalab(72187)
#' parcelle$idu <- idu_build(com = parcelle$commune,
#'                           prefix=parcelle$prefixe,
#'                           section=parcelle$section,
#'                           numero=parcelle$numero)
#' }
#'
#' @export
#'
idu_build <- function(dep = NULL, com, prefix, section, numero) {
  # Ensure character
  dep     <- if (!is.null(dep)) as.character(dep) else NULL
  com     <- as.character(com)
  prefix  <- idu_fmt(prefix, 3)
  section <- idu_fmt(section, 2, upper = TRUE)
  numero  <- idu_fmt(numero, 4)

  # Check lengths consistency
  lengths <- c(
    if (!is.null(dep)) length(dep),
    length(com),
    length(prefix),
    length(section),
    length(numero)
  )
  if (length(unique(lengths)) != 1) {
    stop("All input vectors (dep, com, prefix, section, numero) must have the same length.",
         call. = FALSE)
  }

  # Commune length check
  com_len <- nchar(com)
  if (any(!com_len %in% c(2, 3, 5))) {
    stop("`com` must have 2, 3, or 5 characters.", call. = FALSE)
  }
  if (any(com_len < 5) && is.null(dep)) {
    stop("`dep` is required when `com` has 2 or 3 characters.", call. = FALSE)
  }

  # Construct commune codes
  commune <- ifelse(
    com_len == 5,
    com,
    paste0(
      idu_fmt(dep, ifelse(substr(dep, 1, 2) == "97", 3, 2), upper = TRUE),
      idu_fmt(com, ifelse(substr(dep, 1, 2) == "97", 2, 3))
    )
  )

  # Validate commune codes
  insee_check(commune, verbose = FALSE)

  # Assemble IDU
  idu <- paste0(commune, prefix, section, numero)

  # Validate IDUs
  valid <- idu_check(idu)
  if (!all(valid)) {
    stop("Invalid IDU(s) generated: ", paste(idu[!valid], collapse = ", "), call. = FALSE)
  }

  idu
}

#' Split IDU into Its Components
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
#' try(idu_split("0100200A0012"))
#' idu_split("29158000AK0001")
#'
#' @export
#'
idu_split <- function(idu) {
  idu_assert(idu)

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
#' Check if a vector contains valid IDU codes
#'
#' @param x A character vector to validate.
#' @return A logical vector, TRUE for valid IDU entries.
#'
#' @details
#' An IDU is defined as:
#' \itemize{
#'   \item A `character` vector of length 14 for all entries
#'   \item First 1 characters: department -> digits (0–9)
#'   \item Next 1 characters: department -> digits (0–9) or uppercase letters 'A'/'B'
#'   \item Next 3 characters: commune -> digits (0–9)
#'   \item Next 3 characters: prefixe -> digits (0–9)
#'   \item Next 2 characters: section -> digits (0–9) or uppercase letters (A–Z)
#'   \item Last 4 characters: numero -> digits (0–9)
#'   \item No missing values (NA) or empty strings
#'   \item No lowercase letters or special characters
#' }
#'
#' @export
#'
idu_check <- function(x) {
  x <- as.character(x)
  pattern <- "^[0-9AB]{2}[0-9]{3}[0-9]{3}[0-9A-Z]{2}[0-9]{4}$"
  !is.na(x) & x != "" & nchar(x) == 14 & grepl(pattern, x)
}

#' Assert that IDU codes are valid
#'
#' Throws an error if any element of `idu` is invalid.
#'
#' @param idu Character vector of IDU codes.
#'
#' @return Invisibly returns TRUE if all IDUs are valid.
#'
#' @keywords internal
#'
idu_assert <- function(idu) {
  idu <- as.character(idu)
  valid <- idu_check(idu)
  if (!all(valid)) {
    stop(
      "Invalid IDU(s) detected: ",
      paste(idu[!valid], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
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
#' df <- data.frame(
#'   parcel_id = c("12345ABCDE6789", "54321ZZZZZ0000"),
#'   name = c("Oak", "Pine"),
#'   stringsAsFactors = FALSE
#' )
#' idu_detect_in_df(df)
#' idu_detect_in_df(df, output = "name")
#' idu_detect_in_df(df, output = "position")
#'
#' @export
#'
idu_detect_in_df <- function(df, output = c("both", "name", "position")) {
  output <- match.arg(output)
  if (!is.data.frame(df)) stop("'df' must be a data.frame or tibble", call. = FALSE)

  for (i in seq_along(df)) {
    col <- df[[i]]
    if (!is.character(col)) next
    if (all(idu_check(col))) {
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
#' Merge two data frames and rename a column
#'
#' @description
#' Internal helper function to merge two data frames on specified key columns
#' and rename a target column in the joined result.
#'
#' @param x A \code{data.frame} containing the primary data.
#' @param df A \code{data.frame} containing the lookup or join data.
#' @param ref_x Name of the column in \code{x} to use as the join key.
#' @param ref_y Name of the column in \code{df} to use as the join key.
#' @param ini_col Name of the column in \code{df} to extract and rename.
#' @param fin_col New name to assign to \code{ini_col} in the output.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Checks that \code{ini_col} exists in \code{df}.
#'   \item Performs a left join of \code{x} with \code{df} using
#'         \code{merge()}, matching \code{ref_x} with \code{ref_y}.
#'   \item Selects only the join key (\code{ref_y}) and \code{ini_col}
#'         from \code{df}.
#'   \item Renames \code{ini_col} in the result to \code{fin_col}.
#' }
#'
#' @return
#' A \code{data.frame} containing all rows from \code{x} and the matched
#' values from \code{df}, with \code{ini_col} renamed to \code{fin_col}.
#'
#' @examples
#' \dontrun{
#' df1 <- data.frame(id = 1:3, value = letters[1:3])
#' df2 <- data.frame(key = 1:3, original = c("A", "B", "C"))
#'
#' merge_with_name(df1, df2, "id", "key", "original", "renamed")
#' }
#'
#' @keywords internal
#'
merge_with_name <- function(x, df, ref_x, ref_y, ini_col, fin_col) {
  # Check that all columns exist
  missing_cols <- setdiff(ini_col, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Column(s) '%s' not found in join table.",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  # Subset df to the columns we need
  subset_df <- df[, c(ref_y, ini_col), drop = FALSE]

  # Merge
  res <- merge(
    x,
    subset_df,
    by.x = ref_x,
    by.y = ref_y,
    all.x = TRUE,
    all.y = FALSE
  )

  # Rename columns
  names(res)[names(res) %in% ini_col] <- fin_col

  res
}

#' Retrieve Etalab data for given IDU(s)
#'
#' This internal function downloads Etalab cadastral data corresponding to
#' one or more IDU codes. It also splits the IDUs into their components for
#' further processing.
#'
#' @param idu A character vector of IDU codes (14-character format).
#' @param layer Optional character string specifying a layer to retrieve.
#'   If NULL, all layers are returned.
#' @param select_cols Optional character vector of column names to keep in
#'   the resulting data.
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
#' get_etalab_data_by_idu("72181000AB01", select_cols = c("idu", "cont"))
#' }
#'
#' @keywords internal
#'
get_etalab_data_by_idu <- function(idu,
                                   layer = NULL,
                                   select_cols = NULL,
                                   verbose = TRUE) {
  idu_assert(idu)
  idu_parts <- idu_split(idu)
  insee_codes <- unique(idu_parts$insee)

  data <- get_etalab(insee_codes, layer, verbose = verbose)

  if (!is.null(select_cols)) data <- data[, select_cols, drop = FALSE]

  list(data = data, idu_parts = idu_parts)
}

#' Get feuille IDs from IDU codes
#'
#' This internal function retrieves the feuille (sheet) IDs associated with
#' one or more IDU codes. It validates the IDUs, extracts their INSEE and
#' feuille components, downloads feuille data from Etalab, and returns the
#' matching feuille IDs.
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
#' One of `"both"`, `"reg"`, `"dep"`, or `"com"`.
#' @param cog_field `character`. Défaut is `"NCC"`.
#' The name of the field in the reference datasets to use.
#'
#' @return A `data.frame` with the IDU split into its components and
#' the requested location names.
#'
#' @examples
#' # idu_get_name(c("721870000A0001", "721870000A0002"), loc = "both")
#'
#' @keywords internal
#'
idu_get_name <- function(idu, loc = c("both", "reg", "dep", "com"), cog_field = "NCC") {
  # Match argument
  loc <- match.arg(loc)

  # Validate IDUs
  idu_assert(idu)

  # Split IDU into components
  idu_parts <- idu_split(idu)

  # Reference datasets
  regs <- frcadastre::region_2025
  deps <- frcadastre::departement_2025
  coms <- frcadastre::commune_2025

  res <- idu_parts

  # Merge region names if requested
  if (loc %in% c("both", "reg")) {
    res <- merge_with_name(res, deps, "code_dep", "DEP", "REG", "code_reg")
    res <- merge_with_name(res, regs, "code_reg", "REG", cog_field, "reg_name")
  }

  # Merge department names if requested
  if (loc %in% c("both", "dep")) {
    res <- merge_with_name(res, deps, "code_dep", "DEP", cog_field, "dep_name")
  }

  # Merge commune names if requested
  if (loc %in% c("both", "com")) {
    res <- merge_with_name(res, coms, "insee", "COM", cog_field, "com_name")
  }

  # Keep only idu and requested name columns
  keep_cols <- c("idu")
  if (loc %in% c("both", "reg"))   keep_cols <- c(keep_cols, "reg_name")
  if (loc %in% c("both", "dep"))   keep_cols <- c(keep_cols, "dep_name")
  if (loc %in% c("both", "com"))   keep_cols <- c(keep_cols, "com_name")

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
#' @param with_names `logical` (default: `TRUE`). Whether to retrieve and merge
#'   administrative names (region, department, commune) associated with the parcels.
#' @param ... Additional arguments passed to [idu_get_name()].
#'
#' @return An `sf` object containing parcel geometries, the IDU code, and optionally
#'   associated lieu-dit names and administrative names.
#'
#' @details
#' - All IDU codes are validated before any data is retrieved.
#' - If `with_lieudit = TRUE`, the function performs a spatial join with the
#'   Etalab "lieux-dits" dataset and merges the names into the parcel data.
#' - If `with_names = TRUE`, the function retrieves region, department, and commune
#'   names using [idu_get_name()] and merges them into the parcel data.
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
#' idu_get_parcelle("721870000A0001", with_names = FALSE)
#' }
#'
#' @export
#'
idu_get_parcelle <- function(idu, with_lieudit = TRUE, with_names = TRUE, ...) {
  # Validate IDUs
  idu_assert(idu)

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
  if (with_names) {
    names_df <- idu_get_name(idu, ...)
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
