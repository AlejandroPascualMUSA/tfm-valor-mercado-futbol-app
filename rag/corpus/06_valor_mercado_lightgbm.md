# Estimacion del valor de mercado con LightGBM
section: mercado

El modelo de valor de mercado usa como variable objetivo el valor publicado por Transfermarkt en la actualizacion mas cercana al cierre de la temporada 2023/24. Ese valor no es una transaccion real, pero funciona como proxy de mercado. La distribucion del valor es muy asimetrica, con mayoria de jugadores de valor bajo y pocos outliers de elite, por lo que se entrena sobre logaritmo del valor y se transforma despues a millones de euros.

El modelo integra fisiologia, edad, altura, peso, pie dominante, rendimiento Wyscout, contexto del club, posicion, rol, contrato, minutos, nacionalidad y riesgos de lesion. La calibracion isotonic ayuda a corregir parte del sesgo de regresion hacia la media, especialmente relevante en valores extremos.

El ratio estimado/observado permite interpretar el ajuste. Ratio mayor que 1 sugiere que el modelo estima un valor superior al observado. Ratio menor que 1 sugiere que el valor observado supera la estimacion del modelo. Debe interpretarse con cautela porque hay factores de mercado no observados, como reputacion, expectativas, demanda, salario o poder negociador.
