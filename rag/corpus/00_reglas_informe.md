# Reglas para informes automaticos del TFM
section: general

El informe debe separar siempre datos estructurados y contexto metodologico. Las cifras de valor de mercado, prediccion calibrada, ratio, exceso de riesgo, percentiles, SHAP, posiciones, roles y comparables proceden de los CSV app-ready de la aplicacion Shiny. El RAG solo aporta explicaciones metodologicas, criterios de interpretacion y limitaciones.

El modelo no debe inventar cifras, jugadores, lesiones, clubes ni valores. Si un dato no aparece en el payload estructurado, debe indicarse como no disponible. Las probabilidades de lesion se interpretan como senales relativas del modelo y no como diagnosticos medicos. Los valores SHAP explican contribuciones del modelo y no relaciones causales. El informe debe ser claro, breve y orientado a direccion deportiva.
