# Guardar SHAP individual real desde estimador_precioFINAL.qmd
# Ejecutar después de crear shap_3_modelos y seleccionar el Modelo 1 final.

library(dplyr)
library(tidyr)

# Ejemplo para el modelo escogido en el TFM:
shap_obj <- shap_3_modelos[["1_con_probabilidades_lesion"]]

shap_df <- shap_obj$shap_df |>
  mutate(row_id = row_number())

x_df <- shap_obj$x_valid_df |>
  mutate(row_id = row_number())

# Debe existir una tabla de identificación de validación/test con player_id y jugador.
# Sustituye valid_meta por tu dataframe de metadatos del conjunto usado para SHAP.
# valid_meta: row_id, player_id, player_name, team, primary_position, primary_role, market_value_mill, pred_iso_mill

shap_long <- shap_df |>
  pivot_longer(-row_id, names_to = "feature", values_to = "shap_value") |>
  left_join(
    x_df |>
      pivot_longer(-row_id, names_to = "feature", values_to = "feature_value"),
    by = c("row_id", "feature")
  ) |>
  left_join(valid_meta, by = "row_id") |>
  mutate(
    feature_label = feature,
    feature_value = suppressWarnings(as.numeric(feature_value)),
    feature_value_scaled = ave(feature_value, feature, FUN = function(z) {
      rng <- range(z, na.rm = TRUE)
      if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) return(rep(0.5, length(z)))
      (z - rng[1]) / diff(rng)
    })
  )

readr::write_csv(shap_long, "data/app/shap_dependence_long.csv")
