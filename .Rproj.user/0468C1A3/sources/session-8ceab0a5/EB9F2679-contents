#' Charger et fusionner les données
#'
#' @param shp_path Chemin vers le shapefile (.shp)
#' @param csv_path Chemin vers le CSV contenant la consommation
#'
#' @return Un objet sf fusionné
#' @export
import_data <- function(shp_path, csv_path) {
  shp <- sf::st_read(shp_path, quiet = TRUE)
  csv <- readr::read_csv(csv_path)
  merged <- dplyr::left_join(shp, csv, by = "region")
  return(merged)
}
