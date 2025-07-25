#' Lancer l'application Shiny
#'
#' @export
run_app <- function() {
  appDir <- system.file("app", package = "PackageCerealesSN")
  if (appDir == "") {
    stop("Dossier app non trouvé. Réinstallez le package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
