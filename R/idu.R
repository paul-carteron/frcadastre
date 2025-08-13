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

#' Fill a Data Frame with IDU Codes
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
#' idu_fill_df(df, "com", "prefix", "section", "number", "dep")
#'
#' @export
#'
idu_fill_df <- function(df,
                        col_com,
                        col_prefix,
                        col_section,
                        col_number,
                        col_dep = NULL) {
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

#' Split IDU into Its Components
#'
#' Splits a French cadastral parcel IDU (Identifiant de parcelle) into its
#' administrative and cadastral components: department, commune, prefix,
#' section, and parcel number.
#' Optionally adds region codes and names for all administrative levels.
#'
#' @param idu `character`
#' A vector of IDU codes (14 characters each). Non-character inputs will be coerced.
#'
#' @param col_dep,col_com,col_prefix,col_section,col_number `character`
#' Names to use for the department, commune, prefix, section, and number columns
#' in the returned data frame.
#'
#' @param add_reg `logical`
#' If `TRUE` (default), add a column with the region code corresponding to the department.
#'
#' @param col_name `character` or `NULL`
#' If provided, this column name from the `rcadastre` reference tables will be
#' used to add labels for region, department, and commune.
#' Common choices: `"NOM_DEP"`, `"NOM_REG"`, `"NOM_COM"`.
#'
#' @return A `data.frame` with one row per IDU and columns for the components:
#' - Department
#' - Commune
#' - Prefix
#' - Section
#' - Number
#' Optionally, region codes and labels are added.
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
#' Reference datasets are taken from `rcadastre::region_2025`,
#' `rcadastre::departement_2025`, and `rcadastre::commune_2025`.
#'
#' @examples
#' # Basic split
#' idu_split("0100200A0012")
#'
#' # Add region and labels
#' idu_split("0100200A0012", add_reg = TRUE, col_name = "NOM_DEP")
#'
#' @export
idu_split <- function(idu,
                      col_dep = "dep",
                      col_com = "commune",
                      col_prefix = "prefixe",
                      col_section = "section",
                      col_number = "numero",
                      add_reg = TRUE,
                      col_name = NULL) {

  # Coerce to character and validate length
  idu <- as.character(idu)
  if (length(idu) == 0L) {
    stop("`idu` must not be empty.", call. = FALSE)
  }
  if (anyNA(idu)) {
    stop("`idu` contains NA values. Remove or replace them before calling.", call. = FALSE)
  }
  if (!all(nchar(idu) == 14)) {
    stop("All IDU codes must have exactly 14 characters.", call. = FALSE)
  }

  # Reference datasets
  regs <- rcadastre::region_2025
  deps <- rcadastre::departement_2025
  coms <- rcadastre::commune_2025

  # Base split
  res <- data.frame(
    dep     = substr(idu, 1, 2),
    com     = substr(idu, 3, 5),
    prefix  = substr(idu, 6, 8),
    section = substr(idu, 9, 10),
    number  = substr(idu, 11, 14),
    stringsAsFactors = FALSE
  )
  names(res) <- c(col_dep, col_com, col_prefix, col_section, col_number)

  # Internal helper
  .merge_with_name <- function(res, df, refx, refy, ini_col, fincol) {
    if (!all(c(refy, ini_col) %in% names(df))) {
      stop(sprintf("Reference columns '%s' and/or '%s' not found in reference data frame.",
                   refy, ini_col), call. = FALSE)
    }
    if (!refx %in% names(res)) {
      stop(sprintf("Column '%s' not found in data to merge.", refx), call. = FALSE)
    }
    res <- merge(
      res,
      df[, c(refy, ini_col), drop = FALSE],
      by.x = refx,
      by.y = refy,
      all.x = TRUE,
      all.y = FALSE
    )
    names(res)[names(res) == ini_col] <- fincol
    res
  }

  # Optionally add region code
  if (isTRUE(add_reg)) {
    res <- .merge_with_name(res, deps, col_dep, "DEP", "REG", "reg")
  }

  # Optionally add names
  if (!is.null(col_name)) {
    if (!is.character(col_name) || length(col_name) != 1L) {
      stop("`col_name` must be a single string naming a column in the reference tables.", call. = FALSE)
    }
    if (!col_name %in% names(deps) || !col_name %in% names(coms) || !col_name %in% names(regs)) {
      warning("`col_name` not found in all reference datasets; only available names will be added.", call. = FALSE)
    }

    res$insee <- paste0(res[[col_dep]], res[[col_com]])

    if (isTRUE(add_reg) && col_name %in% names(regs)) {
      res <- .merge_with_name(res, regs, "reg", "REG", col_name, "reg_name")
    }
    if (col_name %in% names(deps)) {
      res <- .merge_with_name(res, deps, col_dep, "DEP", col_name, "dep_name")
    }
    if (col_name %in% names(coms)) {
      res <- .merge_with_name(res, coms, "insee", "COM", col_name, "commune_name")
    }
  }

  res
}

#' Associate Lieudits to Parcelles
#'
#' Computes centroid for each parcel, intersects with lieudits, and merges the lieudit name into the parcels table.
#'
#' @param parcelles `sf` object.
#' Parcel geometries with `id` column.
#' @param lieudits `sf` object.
#' Lieudit geometries with `id` and `nom` columns.
#'
#' @return sf object. Parcelles with an additional `lieudit` column.
#'
#' @importFrom sf st_centroid st_join st_intersects st_drop_geometry
#' @seealso [idu_get_parcelle()]
#'
#' @export
#'
lieudits_for_parcelles <- function(parcelles, lieudits, col_idu = "id"){
  parcelles_centroids <- sf::st_centroid(parcelles)
  intersections <- sf::st_join(parcelles_centroids, lieudits, left = TRUE, join = sf::st_intersects) |>
    transform(lieudit = nom) |>
    subset(select = c(col_idu, "lieudit")) |>
    sf::st_drop_geometry()
  result <- merge(parcelles, intersections, by = col_idu, all = TRUE)
  return(result)
}

#' Retrieve Parcelles from IDU Codes
#'
#' Downloads and filters parcel data corresponding to a vector of IDU codes.
#'
#' @param idu `character`. Vector of IDU codes.
#' @param filter `logical`. Defaut is `TRUE`.
#' If `TRUE`, only returns the parcels matching the IDU codes.
#'
#' @return A `sf` object. Filtered parcel geometries.
#'
#' @importFrom sf st_drop_geometry st_centroid st_join st_intersects
#'
#' @seealso [idu_split(), get_quick_etalab(), lieudits_for_parcelles()]
#'
#' @export
#'
idu_get_parcelle <- function(idu, filter = TRUE) {
  idu_parts <- idu_split(idu)
  insee_code <- unique(paste0(idu_parts$dep, idu_parts$com))
  parcelles <- get_quick_etalab(insee_code)
  lieudits <- get_quick_etalab(insee_code, "lieux_dits")
  parcelles <- lieudits_for_parcelles(parcelles, lieudits)
  if (filter) {
    col_candidates <- c("idu", "IDU", "id")
    col_idu <- intersect(col_candidates, names(parcelles))
    if (length(col_idu) == 0) stop("No ID column found in parcelles. Tried 'idu', 'IDU', 'id'.")
    col_idu <- col_idu[1]
    parcelles <- parcelles[parcelles[[col_idu]] %in% idu, ] |> unique()
  }
  return(parcelles)
}
