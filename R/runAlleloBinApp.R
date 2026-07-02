#' Run AlleloBin Shiny App
#'
#' Launches the Shiny application for allele binning.
#' @export
AlleloBin <- function() {
  appDir <- system.file("shiny-examples", "AlleloBinApp", package = "AlleloBin")
  shiny::runApp(appDir, display.mode = "normal")
}
