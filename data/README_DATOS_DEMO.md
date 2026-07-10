# Datos demo sintéticos

Esta carpeta replica la estructura de `data/app` de la aplicación Shiny del TFM, pero utiliza jugadores, equipos, lesiones, eventos y predicciones sintéticas.

- Los CSV mantienen los mismos nombres de archivo y las mismas variables/columnas que la versión real.
- Se generan 60 jugadores ficticios (`Player_001` ... `Player_060`).
- Las tablas longitudinales contienen más filas porque una misma observación de jugador puede aparecer por evento, partido, lesión o variable SHAP.
- Las tablas agregadas de métricas/importancias del modelo se conservan como outputs metodológicos para que las visualizaciones de rendimiento del modelo sigan funcionando.
- Estos datos no corresponden a futbolistas, clubes ni lesiones reales.
