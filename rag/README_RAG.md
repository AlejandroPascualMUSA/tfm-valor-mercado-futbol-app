# RAG de informes del TFM

Este modulo adapta el RAG y el generador de informes al TFM de estimacion de valor de mercado.

## Componentes

- `tfm_rag.py`: recuperador metodologico. Usa Chroma si esta disponible y, si no, busqueda lexical.
- `tfm_report_generator.py`: genera informes individuales y comparativos.
- `build_tfm_rag_index.py`: construye el indice vectorial Chroma.
- `corpus/`: textos metodologicos curados a partir del TFM.
- `requirements_minimal.txt`: dependencias minimas para informes con RAG determinista.
- `requirements.txt`: dependencias completas para Chroma + embeddings.

## Principio de seguridad metodologica

El RAG no calcula cifras ni busca valores de jugadores. Los datos numericos salen de `data/app/*.csv`. El RAG solo aporta metodologia y cautelas interpretativas.

## Informes

Individual:

```bash
python rag/tfm_report_generator.py --report individual --player-name "Rodrygo" --data-dir data/app --out-dir reports/generated
```

Comparativo:

```bash
python rag/tfm_report_generator.py --report comparison --player-name "Rodrygo" --player-name "Y. En-Nesyri" --objective Fichaje --data-dir data/app --out-dir reports/generated
```

