#' Extract IDUs from cadastral matrices in PDF format
#'
#' When cadastral matrices are requested from the DGFIP, they are provided
#' as PDF files.
#' This function parses one or several PDFs to extract all IDU codes.
#'
#' @param path `character`; vector of file paths to cadastral matrix PDF files.
#' @param interactive `boolean`; if `TRUE`, prompts the user to select PDF
#' files interactively.
#'
#' @importFrom pdftools pdf_text
#' @importFrom utils choose.files strcapture
#'
#' @return A character vector of all extracted IDU codes.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' idu_from_pdf(c("C://cad1.pdf", "C://cad2.pdf"))
#'
#' }
idu_from_pdf <- function(path = NULL, interactive = FALSE) {
  if (interactive && is.null(path)) {
    path <- choose.files(
      caption = "Select one or more cadastral matrix PDF files",
      filters = c("PDF files (*.pdf)", "*.pdf"),
      multi = TRUE
    )
    if (length(path) == 0) return(character(0))
  }

  if (is.null(path) || length(path) == 0){
    stop("No PDF file provided. Use `interactive = TRUE` to select files manually.")
  }

  idu <- lapply(path, function(path){

    txt <- pdf_text(path)
    dep <- sub(".*Département\\s*:\\s*([0-9 ]+).*", "\\1", txt[1])
    dep <- gsub("\\s*", "", dep) |> substr(1, 2)

    com <- sub(".*Commune\\s*:\\s*([0-9 ]+).*", "\\1", txt[1])
    com <- gsub("\\s*", "", com)

    lines <- trimws(unlist(strsplit(txt, "\n", fixed = TRUE)))
    cap <- "\\s*(\\d{2})\\s*(?:(\\d{3}))?\\s*([A-Za-z]{1,2})\\s*(\\d{1,4})"
    proto <- data.frame(
      an = character(),
      prefix  = character(),
      section = character(),
      numero = character()
    )

    capture <- strcapture(cap, lines, proto) |>
    capture <- capture[!is.na(capture$an), ]

    idu <- frcadastre::idu_build(
      dep = rep(dep, nrow(df)),
      com = rep(com, nrow(df)),
      prefix = df$prefix,
      section = df$section,
      numero =  df$numero
    )

    return(idu)
  })

  return(unlist(idu))
}
