### Build IDU section ----
#' Build a Unique ID (IDU) for a Parcel
#'
#' Constructs a standardized 14-character ID for a parcel based on department, commune, prefix, section, and number.
#'
#' @param dep `character` or `NULL` Department code (optional).
#' @param com `character` Commune code.
#' @param prefix `character` Prefix code (default `"0"`).
#' @param section `character` Section code.
#' @param numero `character` Parcel number.
#'
#' @return `character` A 14-character unique ID (IDU).
#'
#' @details
#' Some IDs do not include the department code.
#' In such cases, it needs to be specified manually,
#' which is why `dep` is by defaut set to `NULL`
#'
#' @examples
#' idu_build(dep = "01", com = "002", prefix = "0", section = "A", numero = "12")
#'
#' @export
#'
idu_build <- function(dep = NULL, com, prefix = "0", section, numero) {
  com     <- sprintf("%03s", as.character(com))
  prefix  <- sprintf("%03s", as.character(prefix))
  section <- sprintf("%02s", toupper(as.character(section)))
  numero  <- sprintf("%04s", as.character(numero))

  com     <- gsub(" ", "0", com)
  prefix  <- gsub(" ", "0", prefix)
  section <- gsub(" ", "0", section)
  numero  <- gsub(" ", "0", numero)

  if (!is.null(dep)) {
    dep <- sprintf("%02s", as.character(dep))
    dep <- gsub(" ", "0", dep)
    paste0(dep, com, prefix, section, numero)
  } else {
    paste0(com, prefix, section, numero)
  }
}

#' Build IDU Codes into a data frame
#'
#' Adds an `idu` column to a data frame using parcel components.
#'
#' @param df `data.frame` Input data frame.
#' @param col_com `character`. Name of the commune column.
#' @param col_prefix `character` Name of the prefix column.
#' @param col_section `character` Name of the section column.
#' @param col_number `character` Name of the parcel number column.
#' @param col_dep `character` or `NULL` Name of the department column (optional).
#'
#' @return data.frame. Input data frame with an additional `idu` column.
#'
#' @details
#' Some IDs do not include the department code.
#' In such cases, it needs to be specified manually,
#' which is why `col_dep` is by defaut set to `NULL`.
#'
#' @seealso [idu_build()]
#'
#' @examples
#' df <- data.frame(dep="01", com="002", prefix="0", section="A", number="12")
#' idu_build_in_df(df, "com", "prefix", "section", "number", "dep")
#'
#' @export
#'
idu_build_in_df <- function(df,
                            col_com, col_prefix, col_section, col_number, col_dep = NULL) {
  if (!is.null(col_dep) && !(col_dep %in% names(df))) {
    stop(paste0("Column '", col_dep, "' does not exist in the data frame."))
  }
  cols <- c(col_com, col_prefix, col_section, col_number)
  if (!all(cols %in% names(df))) {
    stop("Some specified columns do not exist in the data frame.")
  }

  if (!is.null(col_dep)) {
    df$idu <- mapply(
      idu_build,
      df[[col_dep]],
      df[[col_com]],
      df[[col_prefix]],
      df[[col_section]],
      df[[col_number]],
      SIMPLIFY = TRUE
    )
  } else {
    df$idu <- mapply(
      idu_build,
      MoreArgs = list(),
      NULL,
      df[[col_com]],
      df[[col_prefix]],
      df[[col_section]],
      df[[col_number]],
      SIMPLIFY = TRUE
    )
  }
  return(df)
}

### Detect IDU section ----
#' Check if a vector contains valid IDU codes
#'
#' @param x A character vector to validate.
#' @return A logical vector, TRUE for valid IDU entries.
#'
#' @details
#' An IDU is defined as:
#' \itemize{
#'   \item A `character` vector of length 14 for all entries
#'   \item First 5 characters are digits (0–9)
#'   \item Next 5 characters are uppercase letters (A–Z) or digits (0–9)
#'   \item Last 4 characters are digits (0–9)
#'   \item No missing values (NA) or empty strings
#'   \item No lowercase letters or special characters
#' }
#'
#' @export
#'
idu_check <- function(x) {
  # Coerce to character
  x <- as.character(x)

  # Define IDU pattern: 5 digits + 5 uppercase alphanumeric + 4 digits
  pattern <- "^[0-9]{5}[0-9A-Z]{5}[0-9]{4}$"

  # Check: not NA, not empty, length 14, match pattern
  valid <- !is.na(x) & x != "" & nchar(x, type = "chars") == 14 & grepl(pattern, x, perl = TRUE)

  return(valid)
}

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
  # Match argument
  output <- match.arg(output)

  # Validate input
  if (!is.data.frame(df)) {
    stop("'df' must be a data.frame or tibble", call. = FALSE)
  }

  # Check
  for (i in seq_along(df)) {
    col <- df[[i]]
    if (!is.character(col)) next

    # Use idu_check to validate the whole column
    if (all(idu_check(col))) {
      if (output == "name") return(names(df)[i])
      if (output == "position") return(i)
      return(list(name = names(df)[i], position = i))
    }
  }
  message("No column matches the IDU pattern.")

  # No match found
  return(NULL)
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
  # Input checks
  if (!inherits(df, c("data.frame", "sf"))) {
    stop("'df' must be a data.frame or sf object.", call. = FALSE)
  }
  if (!is.character(new_name) || length(new_name) != 1){
    stop("'new_name' must be a single character string", call. = FALSE)
  }

  # Detect the IDU column
  idu_info <- idu_detect_in_df(df, output = "both")

  if (is.null(idu_info)) {
    warning("No IDU column detected. Returning original data frame.", call. = FALSE)
    return(df)
  }

  # Rename the column
  names(df)[idu_info$position] <- new_name
  return(df)
}

### Split IDU section ----
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
  # Coerce to character and validate
  idu <- as.character(idu)
  if (length(idu) == 0L) stop("`idu` must not be empty.", call. = FALSE)
  if (anyNA(idu)) stop("`idu` contains NA values.", call. = FALSE)
  if (!all(nchar(idu) == 14)) stop("All IDU codes must have exactly 14 characters.", call. = FALSE)

  # Split into components
  data.frame(
    idu     = idu,
    code_dep= substr(idu, 1, 2),
    code_com= substr(idu, 3, 5),
    prefix  = substr(idu, 6, 8),
    section = substr(idu, 9, 10),
    numero  = substr(idu, 11, 14),
    insee   = substr(idu, 1, 5),
    stringsAsFactors = FALSE
  )
}
### Add IDU section ----
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
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df1 <- data.frame(id = 1:3, value = letters[1:3])
#' df2 <- data.frame(key = 1:3, original = c("A", "B", "C"))
#'
#' .merge_with_name(df1, df2, "id", "key", "original", "renamed")
#' }
.merge_with_name <- function(x, df, ref_x, ref_y, ini_col, fin_col) {
  if (!ini_col %in% names(df)) {
    stop(sprintf("Column '%s' not found in join table.", ini_col), call. = FALSE)
  }
  res <- merge(
    x,
    df[, c(ref_y, ini_col), drop = FALSE],
    by.x = ref_x,
    by.y = ref_y,
    all.x = TRUE,
    all.y = FALSE
  )
  names(res)[names(res) == ini_col] <- fin_col
  res
}

#' Get feuille IDs from IDU codes
#'
#' This function takes one or more IDU codes, validates them, extracts their
#' INSEE and feuille components, downloads feuille data from Etalab, and returns
#' the matching feuille IDs.
#'
#' @param idu A `character` vector of valid IDU codes.
#'
#' @return A `character` vector of feuille IDs corresponding to the input IDUs.
#'
#'
#' @examples
#' \dontrun{
#' # Get feuille IDs for a set of IDU codes
#' idu_get_feuille(c("12345ABCDE6789", "54321ZZZZZ1234"))
#' }
#'
#' @export
#'
idu_get_feuille <- function(idu, group_by_insee = FALSE) {
  # Validate IDUs
  valid <- idu_check(idu)
  if (!all(valid)) {
    stop(
      "Invalid IDU(s) detected: ",
      paste(idu[!valid], collapse = ", "),
      call. = FALSE
    )
  }

  # Split IDU into parts (must return: code_dep, code_reg, insee, etc.)
  idu_parts <- idu_split(idu)

  # Get unique INSEE and feuille codes
  insee_codes <- unique(idu_parts$insee)
  feuilles_codes <- unique(substr(idu, 1, 10))

  # Download feuilles from Etalab
  feuilles <- get_quick_etalab(insee_codes, "feuilles") |>
    transform(codes = substr(id, 1, 10))

  # Keep only feuilles matching the provided IDUs
  selected_feuilles <- feuilles[feuilles$codes %in% feuilles_codes, c("id", "commune")] |>
    sf::st_drop_geometry()

  if (group_by_insee) {
    # Group feuille IDs by insee
    feuilles_list <- lapply(
      insee_codes,
      function(code) selected_feuilles$id[selected_feuilles$commune == code]
    )
    return(list(insee_code = insee_codes, feuilles = feuilles_list))
  } else {
    # Return flat vector of feuille IDs
    return(selected_feuilles$id)
  }
}

' Get feuille IDs from IDU codes in a data.frame
#'
#' This function detects the column containing IDU codes in a given
#' `data.frame` or `sf` object, validates the codes, and retrieves the
#' corresponding feuille identifiers from the Etalab reference dataset.
#'
#' @param df A `data.frame` or `sf` object containing at least one column
#'   with valid IDU codes.
#'
#' @return The input `df` with an additional column `feuille_id`
#'   containing the feuille identifiers corresponding to each IDU.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   idu_code = c("12345ABCDE6789", "54321ZZZZZ1234")
#' )
#' idu_get_feuille_in_df(df)
#' }
#'
#' @seealso [idu_get_feuille()], [idu_detect_in_df()]
#'
#' @export
#'
idu_get_feuille_in_df <- function(df) {
  # Validate input type
  if (!inherits(df, c("data.frame", "sf"))) {
    stop("'df' must be a data.frame or sf object.", call. = FALSE)
  }

  # Detect IDU column
  idu_info <- idu_detect_in_df(df, "both")
  if (is.null(idu_info)) {
    stop("No valid IDU column found in data.frame.", call. = FALSE)
  }
  idu_colname <- idu_info$name

  # Get feuille IDs from IDUs
  feuille_data <- idu_get_feuille(
    idu = df[[idu_colname]]
  )

  # Bind the feuille column to the original df (preserve order)
  df$feuille_id <- feuille_data
  df
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
#' # idu_get_name(c("12345ABCDE6789", "54321ZZZZZ1234"), loc = "both")
#'
#' @export
#'
idu_get_name <- function(idu,
                         loc = c("both", "reg", "dep", "com"),
                         cog_field = "NCC") {
  # Match argument
  loc <- match.arg(loc)

  # Validate IDUs
  valid <- idu_check(idu)
  if (!all(valid)) {
    stop(
      "Invalid IDU(s) detected: ",
      paste(idu[!valid], collapse = ", "),
      call. = FALSE
    )
  }

  # Split IDU into parts (must return: code_dep, code_reg, insee, etc.)
  idu_parts <- idu_split(idu)

  # Reference datasets
  regs <- rcadastre::region_2025
  deps <- rcadastre::departement_2025
  coms <- rcadastre::commune_2025

  res <- idu_parts

  # Add names according to 'loc'
  if (loc %in% c("both", "reg")) {
    res <- .merge_with_name(res, deps, "code_dep", "DEP", "REG", "code_reg")
    res <- .merge_with_name(res, regs, "code_reg", "REG", cog_field, "reg_name")
  }
  if (loc %in% c("both", "dep")) {
    res <- .merge_with_name(res, deps, "code_dep", "DEP", cog_field, "dep_name")
  }
  if (loc %in% c("both", "com")) {
    res <- .merge_with_name(res, coms, "insee", "COM", cog_field, "com_name")
  }

  res
}

#' Append location names to a data.frame containing IDU codes
#'
#' This function detects the column containing IDU codes in a data.frame,
#' then uses \code{\link{idu_get_name}} to append the corresponding
#' region, department, and/or commune names.
#'
#' @param df A `data.frame` containing an IDU column.
#' @param loc A `character` string specifying which location levels to include.
#' One of `"both"`, `"reg"`, `"dep"`, or `"com"`.
#' @param cog_field `character`. Défaut is `"NCC"`.
#' The name of the field in the reference datasets to use.
#'
#' @return A `data.frame` with new location name columns appended.
#' @export
#'
#' @examples
#' # my_df <- data.frame(idu = c("12345ABCDE6789", "54321ZZZZZ1234"))
#' # idu_get_name_in_df(my_df, loc = "both")
#'
idu_get_name_in_df <- function(df,
                               loc = c("both", "reg", "dep", "com"),
                               cog_field = "NCC") {
  # Validate input type
  if (!inherits(df, c("data.frame", "sf"))) {
    stop("'df' must be a data.frame or sf object.", call. = FALSE)
  }

  # Detect IDU column
  idu_info <- idu_detect_in_df(df, "both")
  if (is.null(idu_info)) {
    stop("No valid IDU column found in data.frame.", call. = FALSE)
  }
  idu_colname <- idu_info$name

  # Get names from IDUs
  name_data <- idu_get_name(
    idu = df[[idu_colname]],
    loc = loc,
    cog_field = cog_field
  )

  # Merge with original data.frame (preserve row order)
  cbind(df, name_data[ , setdiff(names(name_data), names(df)), drop = FALSE])
}

#' Retrieve "lieu-dit" names for given IDUs
#'
#' This function takes one or more valid IDU codes, retrieves parcel and lieu-dit
#' data from Etalab, performs a spatial join to determine the lieu-dit for each IDU,
#' and returns a data.frame mapping IDUs to their lieu-dit names.
#'
#' @param idu A `character` vector of valid IDU codes.
#'
#' @return A `data.frame` containing the split IDU parts and an additional
#' `lieudit` column with the matched lieu-dit names.
#'
#' @examples
#' \dontrun{
#' idu_get_lieudit(c("12345ABCDE6789", "54321ZZZZZ1234"))
#' }
#'
#' @export
#'
idu_get_lieudit <- function(idu){
  # Validate IDUs
  valid <- idu_check(idu)
  if (!all(valid)) {
    stop(
      "Invalid IDU(s) detected: ",
      paste(idu[!valid], collapse = ", "),
      call. = FALSE
    )
  }

  # Split IDU into parts (must include: code_dep, code_reg, insee)
  idu_parts <- idu_split(idu)

  # Get unique INSEE codes
  insee_codes <- unique(idu_parts$insee)

  # Download data from Etalab
  parcelles <- get_quick_etalab(insee_codes) |> idu_rename_in_df("idu")
  lieudits <- tryCatch(
    get_quick_etalab(insee_codes, "lieux_dits"),
    error = function(e) {
      return(NULL)
    }
  )
  if (is.null(lieudits)) {
    return(NULL)
  }

  # Ensure returned objects are sf
  if (!inherits(parcelles, "sf") || !inherits(lieudits, "sf")) {
    stop("Etalab data must be 'sf' objects.", call. = FALSE)
  }

  # Compute centroids of parcels for spatial join
  parcelles_centroids <- suppressWarnings(sf::st_centroid(parcelles))

  # Join parcels with lieu-dit polygons
  intersections <- sf::st_join(parcelles_centroids,
                               lieudits,
                               left = TRUE,
                               join = sf::st_intersects
                               ) |> sf::st_drop_geometry()

  # Check for NA in 'nom' field of lieudits
  if (any(is.na(intersections$nom)) || is.null(intersections)) {
    warning("Some lieu-dit names are missing (NA) in the 'etalab' data.")
  }

  # Merge
  res <- .merge_with_name(idu_parts, intersections,
                          "idu", "idu",
                          "nom", "lieudit")

  return(res)
}

#' Append lieu-dit names to a data.frame containing IDUs
#'
#' This function searches for a column containing valid IDUs in the input
#' data.frame (or sf object), retrieves their lieu-dit names via
#' \code{\link{idu_get_lieudit}}, and returns the data with a new
#' `lieudit` column.
#'
#' @param df A `data.frame` or sf object containing at least one column with IDUs.
#'
#' @return The same object as `df`, with an added `lieudit` column.
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(idu = c("12345ABCDE6789", "54321ZZZZZ1234"))
#' idu_get_lieudit_in_df(my_data)
#' }
#'
#' @export
#'
idu_get_lieudit_in_df <- function(df) {
  # Validate input type
  if (!inherits(df, c("data.frame", "sf"))) {
    stop("'df' must be a data.frame or sf object.", call. = FALSE)
  }

  # Detect IDU column
  idu_info <- idu_detect_in_df(df, "both")
  if (is.null(idu_info)) {
    stop("No valid IDU column found in data.frame.", call. = FALSE)
  }
  idu_colname <- idu_info$name

  # Extract IDU vector
  idu_vec <- df[[idu_colname]]

  # Retrieve lieudit names
  lieudit_df <- idu_get_lieudit(idu_vec)[, c("idu", "lieudit"), drop = FALSE]

  # Merge results into original df
  df <- merge(
    df,
    lieudit_df,
    by.x = idu_colname,
    by.y = "idu",
    all.x = TRUE
  )

  return(df)
}

#' Retrieve contenance values for given IDUs
#'
#' This function takes one or more valid IDU codes, retrieves parcel
#' data from Etalab, and returns a data.frame linking each IDU to its `contenance`
#' (surface area in square meters).
#'
#' @param idu A `character` vector of valid IDU codes.
#'
#' @return A `data.frame` containing the split IDU parts and an additional
#' `contenance` column with the matched surfaces.
#'
#' @examples
#' \dontrun{
#' # Example IDUs
#' my_idus <- c("12345ABCDE6789", "54321ZZZZZ1234")
#'
#' # Get parcel contenance
#' contenance_df <- idu_get_contenance(my_idus)
#' print(contenance_df)
#' }
#'
#' @export
#'
idu_get_contenance <- function(idu){
  # Validate IDUs
  valid <- idu_check(idu)
  if (!all(valid)) {
    stop(
      "Invalid IDU(s) detected: ",
      paste(idu[!valid], collapse = ", "),
      call. = FALSE
    )
  }

  # Split IDU into parts (must include: code_dep, code_reg, insee)
  idu_parts <- idu_split(idu)

  # Get unique INSEE codes
  insee_codes <- unique(idu_parts$insee)

  # Download data from Etalab
  parcelles <- get_quick_etalab(insee_codes) |> idu_rename_in_df("idu")

  # Ensure returned objects are sf
  if (!inherits(parcelles, "sf")) {
    stop("Etalab data must be 'sf' objects.", call. = FALSE)
  }

  # Merge
  res <- .merge_with_name(idu_parts, parcelles,
                          "idu", "idu",
                          "contenance", "contenance")

  return(res)
}

#' Append contenance values to a data.frame containing IDUs
#'
#' This function searches for a column containing valid IDUs in the input
#' data.frame (or sf object), retrieves their contenance values via
#' \code{\link{idu_get_contenance}}, and returns the data with a new
#' `contenance` column.
#'
#' @param df A `data.frame` or sf object containing at least one column with IDUs.
#'
#' @return The same object as `df`, with an added `contenance` column.
#'
#' @examples
#' \dontrun{
#' my_data <- data.frame(idu = c("12345ABCDE6789", "54321ZZZZZ1234"))
#' idu_get_contenance_in_df(my_data)
#' }
#'
#' @export
#'
idu_get_contenance_in_df <- function(df) {
  # Validate input type
  if (!inherits(df, c("data.frame", "sf"))) {
    stop("'df' must be a data.frame or sf object.", call. = FALSE)
  }

  # Detect IDU column
  idu_info <- idu_detect_in_df(df, "both")
  if (is.null(idu_info)) {
    stop("No valid IDU column found in data.frame.", call. = FALSE)
  }
  idu_colname <- idu_info$name

  # Extract IDU vector
  idu_vec <- df[[idu_colname]]

  # Retrieve contenance values
  contenance_df <- idu_get_contenance(idu_vec)[, c("idu", "contenance"), drop = FALSE]

  # Merge results into original df
  df <- merge(
    df,
    contenance_df,
    by.x = idu_colname,
    by.y = "idu",
    all.x = TRUE
  )

  return(df)
}
