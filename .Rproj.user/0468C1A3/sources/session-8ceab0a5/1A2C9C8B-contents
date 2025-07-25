library(dplyr)
library(ggplot2)
library(sf)

plot_map <- function(data_menages, regions_sf, var, region_col = "region", poids = "nb_personnes") {
  # Vérifier que les colonnes existent
  if (!(var %in% names(data_menages))) stop(paste("Colonne", var, "manquante dans les données"))
  if (!(region_col %in% names(data_menages))) stop(paste("Colonne", region_col, "manquante"))
  if (!(poids %in% names(data_menages))) stop(paste("Colonne de poids", poids, "manquante"))

  # 🔍 Nettoyage des noms de région
  data_menages[[region_col]] <- gsub("SAINT-LOUIS", "SAINT LOUIS", data_menages[[region_col]])

  # Calcul de la moyenne pondérée par région
  agg_data <- data_menages %>%
    group_by(.data[[region_col]]) %>%
    summarise(
      valeur = sum(.data[[var]] * .data[[poids]], na.rm = TRUE) / sum(.data[[poids]], na.rm = TRUE)
    )

  # Fusion avec les données spatiales
  merged <- regions_sf %>%
    left_join(agg_data, by = setNames(region_col, "NOMREG"))

  # Tracer la carte
  ggplot(merged) +
    geom_sf(aes(fill = valeur)) +
    geom_sf_text(aes(label = paste0(NOMREG, "\n", round(valeur, 1))), size = 2, color = "black") +
    scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
    theme_minimal() +
    labs(
      title = paste("Moyenne pondérée de", var, "par région"),
      fill = var
    )
}
