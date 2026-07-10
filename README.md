# App Shiny del TFM

Aplicación Shiny para visualizar los outputs del TFM sobre estimación del valor de mercado de futbolistas de LaLiga 2023/24 integrando:

- roles de juego obtenidos mediante Weighted PCA y clustering;
- riesgo de lesión por historial clínico mediante LASSO;
- riesgo de lesión por carga física mediante XGBoost;
- valoración de mercado mediante LightGBM y calibración isotónica;
- eventos Opta sectorizados en pitch 120 x 80;
- buscador y estimador ligero de precio por jugadores similares.

## Cómo ejecutar

Abre RStudio en esta carpeta y ejecuta:

```r
source("install_packages.R")
shiny::runApp(".")
```

También puedes ejecutar desde consola:

```bash
Rscript -e "shiny::runApp('.')"
```

## Estructura

```text
app.R                       # aplicación Shiny
R/helpers.R                 # funciones auxiliares
www/styles.css              # estilos
www/bodymap2.png            # imagen auxiliar para lesiones
data/app/*.csv              # datos app-ready
data-raw/prepare_app_data.py# script reproducible para reconstruir data/app
```

## Datos que la app carga al iniciar

La app no recalcula el pipeline completo. Lee directamente los CSV de `data/app`:

- `players_master.csv`: tabla jugador-temporada con valor real, predicciones, rol, riesgos, contexto y métricas deportivas/físicas.
- `market_predictions.csv`: predicciones calibradas y originales del modelo de precio.
- `roles_pca.csv`: PC1, PC2, cluster y rol por jugador-posición.
- `event_percentiles.csv`: percentiles de eventos por posición y por rol.
- `role_pca_loadings.csv`: cargas PCA para interpretación visual.
- `events_pitch.csv`: extracto compacto de `eventos_final.json` con coordenadas 120 x 80, evento, sector, posición del evento y flags de sector principal/vecino.
- `physical_block_importance.csv`: importancia por bloque del modelo de carga.
- `physical_gain_categories.csv`: Gain por categorías fisiológicas del modelo de carga.
- `shap_importance.csv`, `metricas_calibracion_todas.csv` y `comparacion_final_3_modelos.csv`: interpretabilidad y evaluación del modelo de mercado.
- `shap_dependence_long.csv`: tabla larga jugador-variable para dependencia/distribución SHAP. En esta versión se ha generado una tabla app-ready para que el módulo funcione sin recalcular el LightGBM original; para máxima fidelidad, reemplázala por el SHAP individual exportado desde `estimador_precioFINAL.qmd`.

## Data frames que conviene guardar desde los códigos originales

Para evitar coste computacional dentro de Shiny, deben guardarse siempre los outputs finales de cada módulo:

1. `roles` final con `player_id`, `posicion_evento`, `cluster`, `rol`, `PC1`, `PC2` e `interpretacion`.
2. `percentiles` con percentiles por posición y cluster/rol.
3. `eventos_final` ya sectorizado, pero convertido a CSV ligero con solo las columnas necesarias para el pitch.
4. `p_lesionContext.csv` con `prob_lesion_lasso`, `p_basal_hist` y `exceso_riesgo_hist_pos`.
5. `p_lesion2.csv` con `prob_lesion_temporada`, `prob_lesion_temporada_basal_exposicion` y `exceso_riesgo_lesion_exposicion`.
6. `modelo_precio.csv` con las variables finales de LightGBM.
7. `predicciones_cal` y `predicciones_cal_original` con `player_id`, valor real, valor estimado, diferencia y ratio.
8. `shap_importance.csv` y el SHAP individual largo con `player_id`, `feature`, `feature_value`, `shap_value` y `feature_label`.
9. Importancias del modelo de carga agregadas por bloque y, si se desea, por variable.

## Estimador de precio

El estimador de la pestaña **Buscador** no reentrena LightGBM dentro de Shiny. Usa un método ligero por vecinos similares:

1. El usuario introduce los campos obligatorios: edad, país/grupo, equipo, minutos 23/24, contrato, posición y rol.
2. Si una estadística opcional queda vacía, se imputa con la media del rol seleccionado.
3. Se calculan jugadores similares combinando distancia numérica estandarizada y penalización por posición/rol/grupo.
4. El precio estimado es una media ponderada de los vecinos más cercanos.

Para un estimador exactamente igual al modelo del TFM, conviene guardar el modelo LightGBM final como RDS o PMML junto con su matriz de diseño y el mapa de variables categóricas.

## Informes automáticos con RAG

Esta versión incorpora una pestaña **Informes** con dos módulos:

- **Informe individual**: genera un informe de 1-2 páginas para un jugador.
- **Informe comparativo**: compara de 2 a 5 jugadores y produce un ranking según objetivo.

El sistema usa datos estructurados de `data/app` y contexto metodológico de `rag/corpus`. No usa agentes ni redacción generativa externa. El RAG solo aporta metodología y cautelas; las cifras salen de los CSV.

### Uso desde Shiny

1. Instala las dependencias R:

```r
source("install_packages.R")
```

2. Instala las dependencias mínimas de Python desde la carpeta de la app:

```bash
python -m pip install -r rag/requirements_minimal.txt
```

3. En la pestaña **Informes**, el campo **Ejecutable Python** acepta la ruta completa a tu entorno. Con Anaconda puedes usar, por ejemplo:

```text
C:/Users/aleja/anaconda3/envs/tfm-rag/python.exe
```

También puedes fijarlo desde R antes de ejecutar la app:

```r
Sys.setenv(TFM_PYTHON = "C:/Users/aleja/anaconda3/envs/tfm-rag/python.exe")
```

4. Ejecuta la app:

```r
shiny::runApp(".")
```

### Uso desde terminal

```bash
python rag/tfm_report_generator.py --report individual --player-name "Vinicius Junior" --data-dir data/app --out-dir reports/generated

python rag/tfm_report_generator.py --report comparison --player-name "Vinicius Junior" --player-name "Rodrygo" --objective Fichaje --data-dir data/app --out-dir reports/generated
```

### Chroma opcional

El recuperador funciona sin Chroma mediante búsqueda lexical. Para índice vectorial, instala las dependencias de `rag/requirements.txt` y ejecuta:

```bash
python rag/build_tfm_rag_index.py --rebuild
```
