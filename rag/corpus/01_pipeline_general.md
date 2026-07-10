# Pipeline metodologico integrado
section: general

El proyecto desarrolla un marco integrado para estimar el valor de mercado de futbolistas de LaLiga 2023/24. La base combina rendimiento deportivo, contexto competitivo, roles de juego y riesgo lesional. Se integran datos de Opta, Transfermarkt, SkillCorner y Wyscout. Opta se usa para eventos y roles; Transfermarkt para valor de mercado, minutos e historial de lesiones; SkillCorner para carga fisica por partido; Wyscout para estadisticas de rendimiento y variables de feature engineering.

La metodologia tiene cuatro fases: integracion y depuracion de datos, identificacion de roles con reduccion de dimensionalidad y clustering, estimacion de dos riesgos lesionales independientes, y modelo LightGBM para valor de mercado. El resultado no es una tasacion oficial de transferencia, sino una estimacion cuantitativa que ayuda a detectar perfiles ajustados, infravalorados o sobrestimados respecto al modelo.
