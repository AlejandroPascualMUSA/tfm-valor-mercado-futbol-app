# Modelo de carga fisica XGBoost
section: carga

El modelo de carga fisica estima riesgo de lesion a partir de variables de SkillCorner registradas en partidos. A diferencia del modelo de historial, tiene estructura longitudinal: cada jugador genera multiples observaciones mediante ventanas moviles retrospectivas de partidos anteriores. Para cada partido se usa solo informacion previa, evitando data leakage.

La variable positiva se define cuando la lesion ocurre en una ventana posterior al partido de referencia. El modelo considera lesiones potencialmente asociadas a carga, como musculares, tendinosas o ligamentosas, y excluye eventos menos relacionados con sobrecarga o fatiga.

Se evaluan variables de carga externa, alta intensidad, carga neuromuscular, capacidad de rendimiento, densidad competitiva, ACWR, load spike y transformaciones relativas por posicion. La calibracion transforma los scores de XGBoost en probabilidades mas interpretables. La probabilidad de temporada se ajusta por exposicion y se convierte en exceso de riesgo por carga para no penalizar simplemente a jugadores que disputan mas partidos.
