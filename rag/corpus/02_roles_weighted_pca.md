# Roles de juego con Weighted PCA y clustering
section: roles

La posicion nominal no describe completamente el comportamiento real del jugador. Dos futbolistas en la misma posicion pueden tener funciones muy distintas. Por eso se construyen roles tacticos a partir de eventos de juego. Los eventos se normalizan por 90 minutos, se transforman para reducir asimetria y se estandarizan por posicion.

El Weighted PCA incorpora pesos de conocimiento experto para que cada evento tenga una relevancia tactica diferente segun la posicion. Acciones defensivas pesan mas en centrales y pivotes; acciones ofensivas, pases clave, tiros, conducciones y desborde pesan mas en posiciones avanzadas. Esto permite que las primeras componentes concentren mejor los patrones funcionales.

El clustering se realiza sobre el espacio latente obtenido con PCA ponderado. Los clusters se interpretan con ayuda de las cargas factoriales y del mapa PC1-PC2. La interpretacion no es puramente matematica: se combina separacion estadistica, coherencia tactica y conocimiento experto.
