from __future__ import annotations

import argparse
import json
from pathlib import Path

from tfm_rag import TFMRAG


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Construye el indice vectorial Chroma del RAG TFM.")
    parser.add_argument("--corpus-dir", default=str(Path(__file__).resolve().parent / "corpus"))
    parser.add_argument("--persist-dir", default=str(Path(__file__).resolve().parent / "chroma_db"))
    parser.add_argument("--rebuild", action="store_true")
    args = parser.parse_args()

    rag = TFMRAG(corpus_dir=args.corpus_dir, persist_dir=args.persist_dir)
    result = rag.build_index(rebuild=args.rebuild)
    print(json.dumps(result, ensure_ascii=False, indent=2))
