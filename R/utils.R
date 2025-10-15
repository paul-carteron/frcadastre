#' Format string components with padding and optional uppercase
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
#' pad0(c(1, 23, 456), width = 5)
#' # Returns: "00001" "00023" "00456"
#'
#' pad0(c("ab", "cd"), width = 4, upper = TRUE)
#' # Returns: "00AB" "00CD"
#' }
#'
#' @keywords internal
#'
pad0 <- function(x, width, upper = FALSE) {
  x <- sprintf(paste0("%", width, "s"), as.character(x))
  x <- gsub(" ", "0", x)
  if (upper) x <- toupper(x)
  x
}

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
