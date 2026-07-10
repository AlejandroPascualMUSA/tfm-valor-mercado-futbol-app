# Modelo de historial medico LASSO
section: historial

El modelo de historial medico estima la propension general del jugador a sufrir una lesion durante la temporada 2023/24 a partir de informacion historica de lesiones. El conjunto de datos resume una observacion por jugador y utiliza antecedentes de temporadas previas. La variable objetivo indica si el jugador sufrio al menos una lesion relevante en la temporada objetivo.

El modelo final seleccionado fue LASSO por su equilibrio entre discriminacion, clasificacion y parsimonia. Las variables retenidas fueron incidencia y dias desde ultima lesion. Incidencia mide la frecuencia relativa de lesiones ponderada por exposicion. Dias desde ultima lesion aproxima la distancia temporal respecto al ultimo episodio registrado.

El exceso de riesgo historico se calcula como la parte positiva de P(historial) menos un riesgo basal. De este modo se aisla el riesgo adicional sobre el nivel medio de referencia y se evita interpretar como penalizacion una probabilidad que simplemente refleje el riesgo medio de la poblacion.
