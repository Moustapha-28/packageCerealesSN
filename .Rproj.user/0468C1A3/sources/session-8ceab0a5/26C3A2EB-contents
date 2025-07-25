#' Charger le shapefile des régions du Sénégal
load_regions <- function(shp_path) {
  sf::st_read(shp_path)
}

#' Ajouter les données de céréales consommées
add_quantite_cereales <- function(regions, data) {
  dplyr::left_join(regions, data, by = "region")
}

#' Calculer les statistiques de base
summary_quantite_cereales <- function(data) {
  data %>%
    dplyr::summarise(
      moyenne = mean(quantite, na.rm = TRUE),
      totale = sum(quantite, na.rm = TRUE),
      max = max(quantite, na.rm = TRUE),
      min = min(quantite, na.rm = TRUE)
    )
}

#' Afficher la carte
plot_quantite_cereales <- function(regions_data) {
  ggplot2::ggplot(regions_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = quantite)) +
    ggplot2::scale_fill_viridis_c(option = "C") +
    ggplot2::labs(fill = "Quantité consommée (kg)", title = "Consommation de céréales par région") +
    ggplot2::theme_minimal()
}
