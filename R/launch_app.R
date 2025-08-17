#' Launch Shiny rcadastre
#'
#' @import shiny
#' @import leaflet
#' @importFrom sf st_sf st_sfc st_bbox st_transform st_write
#' @importFrom openxlsx2 write_xlsx
#' @importFrom DT DTOutput renderDT
#'
#' @export
launch_app <- function() {
  app_dir <- system.file("shiny/app", package = "rcadastre")
  if (app_dir == "") stop("App not found. Please re-install `rcadastre`.", call. = FALSE)
  shiny::runApp(app_dir, display.mode = "normal")
}
