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
#' @examples
#' df <- data.frame(dep="01", com="002", prefix="0", section="A", number="12")
#' idu_fill_df(df, "com", "prefix", "section", "number", "dep")
#'
#' @seealso [idu_build()]
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
#' Splits a 14-character parcel IDU into department, commune, prefix, section, and number.
#'
#' @param idu `character`
#' A vector of IDU codes.
#' @param col_dep `character`
#' Name for the department column (default `"dep"`).
#' @param col_com `character`
#' Name for the commune column (default `"com"`).
#' @param col_prefix `character`
#' Name for the prefix column (default `"prefix"`).
#' @param col_section `character`
#' Name for the section column (default `"section"`).
#' @param col_number `character`
#' Name for the number column (default `"number"`).
#'
#' @return data.frame. Columns corresponding to the components of the IDU.
#'
#' @examples
#' idu_split("0100200A0012")
#'
#' @export
#'
idu_split <- function(idu,
                      col_dep = "dep",
                      col_com = "com",
                      col_prefix = "prefix",
                      col_section = "section",
                      col_number = "number") {
  idu <- as.character(idu)
  if (!all(nchar(idu) == 14)) {
    stop("All IDU codes must have exactly 14 characters.")
  }
  res <- data.frame(
    dep     = substr(idu, 1, 2),
    com     = substr(idu, 3, 5),
    prefix  = substr(idu, 6, 8),
    section = substr(idu, 9, 10),
    number  = substr(idu, 11, 14),
    stringsAsFactors = FALSE
  )
  names(res) <- c(col_dep, col_com, col_prefix, col_section, col_number)
  return(res)
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
lieudits_for_parcelles <- function(parcelles, lieudits){
  parcelles_centroids <- sf::st_centroid(parcelles)
  intersections <- sf::st_join(parcelles_centroids, lieudits, left = TRUE, join = sf::st_intersects) |>
    transform(lieudit = nom) |>
    subset(select = c("id", "lieudit")) |>
    sf::st_drop_geometry()
  result <- merge(parcelles, intersections, by = "id", all = TRUE)
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
