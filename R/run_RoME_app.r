#' Run the RoME Shiny application
#' @description
#' Launches the embedded Shiny application included in the package.
#' @return No return value, called for side effects
#' @export
run_RoME_app <- function() {
  app_dir <- system.file("shiny/RoMEApp", package = "RoME")

  if (app_dir == "") {
    stop("Could not find the RoME Shiny app directory. Please reinstall the package.")
  }

  shiny::runApp(app_dir, display.mode = "normal")
}


