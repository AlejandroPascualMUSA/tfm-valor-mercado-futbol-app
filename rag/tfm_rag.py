"""RAG metodologico para informes del TFM.

Este modulo adapta el RAG de ejemplo a un flujo cerrado para informes de
jugadores. La idea central es:

- Los numeros exactos salen de los CSV app-ready de Shiny.
- El RAG solo recupera contexto metodologico del TFM.
- No hay agentes ni generación externa: el RAG solo recupera contexto metodológico.

El modulo intenta usar Chroma + sentence-transformers si estan instalados. Si no,
usa un recuperador lexical simple sobre los mismos documentos del corpus.
"""

from __future__ import annotations

import json
import math
import os
import re
import sqlite3
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable


# ---------------------------------------------------------------------------
# Normalizacion y utilidades
# ---------------------------------------------------------------------------


def normalize_text(text: Any) -> str:
    value = str(text or "").lower()
    value = re.sub(r"[^a-z0-9áéíóúüñç_ %./:-]+", " ", value)
    value = re.sub(r"\s+", " ", value).strip()
    return value


def tokenize(text: Any) -> list[str]:
    value = normalize_text(text)
    return [tok for tok in re.split(r"\s+", value) if len(tok) > 2]


def chunk_text(text: str, chunk_size: int = 950, overlap: int = 160) -> list[str]:
    text = re.sub(r"\s+", " ", text).strip()
    if not text:
        return []
    chunks: list[str] = []
    start = 0
    n = len(text)
    while start < n:
        end = min(start + chunk_size, n)
        # Try to cut on sentence boundary.
        if end < n:
            window = text[start:end]
            cut = max(window.rfind(". "), window.rfind("; "), window.rfind("\n"))
            if cut > int(chunk_size * 0.55):
                end = start + cut + 1
        chunk = text[start:end].strip()
        if chunk:
            chunks.append(chunk)
        if end >= n:
            break
        start = max(0, end - overlap)
    return chunks


@dataclass
class RAGDocument:
    text: str
    title: str
    source: str
    section: str = "general"
    doc_id: str | None = None
    score: float | None = None

    def as_dict(self) -> dict[str, Any]:
        return {
            "text": self.text,
            "title": self.title,
            "source": self.source,
            "section": self.section,
            "doc_id": self.doc_id or self.source,
            "score": self.score,
        }


# ---------------------------------------------------------------------------
# Recuperador lexical fallback
# ---------------------------------------------------------------------------


class LexicalRetriever:
    def __init__(self, docs: list[RAGDocument]) -> None:
        self.docs = docs
        self.doc_tokens = [tokenize(d.text + " " + d.title + " " + d.section) for d in docs]
        self.df: dict[str, int] = {}
        for toks in self.doc_tokens:
            for tok in set(toks):
                self.df[tok] = self.df.get(tok, 0) + 1
        self.n_docs = max(1, len(docs))

    def score(self, query: str, doc_index: int) -> float:
        q_tokens = tokenize(query)
        if not q_tokens:
            return 0.0
        toks = self.doc_tokens[doc_index]
        if not toks:
            return 0.0
        counts: dict[str, int] = {}
        for tok in toks:
            counts[tok] = counts.get(tok, 0) + 1
        length_norm = math.sqrt(len(toks))
        score = 0.0
        for tok in q_tokens:
            tf = counts.get(tok, 0)
            if tf <= 0:
                continue
            idf = math.log((self.n_docs + 1) / (1 + self.df.get(tok, 0))) + 1
            score += (tf / length_norm) * idf
        return score

    def retrieve(self, query: str, k: int = 5, section_filter: list[str] | None = None) -> list[RAGDocument]:
        allowed = {normalize_text(x) for x in section_filter or [] if x}
        scored: list[tuple[float, int]] = []
        for i, doc in enumerate(self.docs):
            if allowed and normalize_text(doc.section) not in allowed:
                continue
            s = self.score(query, i)
            if s > 0:
                scored.append((s, i))
        scored.sort(reverse=True)
        result: list[RAGDocument] = []
        for s, i in scored[:k]:
            d = self.docs[i]
            result.append(RAGDocument(d.text, d.title, d.source, d.section, d.doc_id, score=round(float(s), 4)))
        return result


# ---------------------------------------------------------------------------
# RAG principal
# ---------------------------------------------------------------------------


class TFMRAG:
    def __init__(
        self,
        corpus_dir: str | Path | None = None,
        persist_dir: str | Path | None = None,
        collection_name: str = "tfm_rag_corpus",
        embedding_model: str = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2",
    ) -> None:
        root = Path(__file__).resolve().parent
        self.corpus_dir = Path(corpus_dir) if corpus_dir else root / "corpus"
        self.persist_dir = Path(persist_dir) if persist_dir else root / "chroma_db"
        self.collection_name = collection_name
        self.embedding_model = embedding_model
        self._docs = self._load_corpus()
        self._lexical = LexicalRetriever(self._docs)
        self._chroma = None
        self._embeddings = None

    def _load_corpus(self) -> list[RAGDocument]:
        docs: list[RAGDocument] = []
        if not self.corpus_dir.exists():
            return docs
        for fp in sorted(self.corpus_dir.glob("*.md")) + sorted(self.corpus_dir.glob("*.txt")):
            raw = fp.read_text(encoding="utf-8", errors="ignore")
            title = fp.stem.replace("_", " ").strip()
            section = "general"
            m = re.search(r"^section\s*:\s*(.+)$", raw, flags=re.IGNORECASE | re.MULTILINE)
            if m:
                section = m.group(1).strip()
            m2 = re.search(r"^#\s+(.+)$", raw, flags=re.MULTILINE)
            if m2:
                title = m2.group(1).strip()
            for idx, chunk in enumerate(chunk_text(raw)):
                docs.append(
                    RAGDocument(
                        text=chunk,
                        title=title,
                        source=fp.name,
                        section=section,
                        doc_id=f"{fp.stem}:{idx}",
                    )
                )
        return docs

    def _try_chroma(self):
        if self._chroma is not None:
            return self._chroma
        # No crear una base Chroma vacia durante la generacion de informes.
        # Si el indice no existe, se usa recuperacion lexical. Esto evita que
        # Shiny/RStudio reinicie la app por cambios de ficheros dentro de rag/.
        if not self.persist_dir.exists() or not any(self.persist_dir.iterdir()):
            return None
        try:
            from langchain_chroma import Chroma
            from langchain_huggingface import HuggingFaceEmbeddings
        except Exception:
            return None
        try:
            self._embeddings = HuggingFaceEmbeddings(model_name=self.embedding_model)
            self._chroma = Chroma(
                persist_directory=str(self.persist_dir),
                embedding_function=self._embeddings,
                collection_name=self.collection_name,
            )
            # Probe collection. Empty collections can throw in some versions.
            return self._chroma
        except Exception:
            self._chroma = None
            return None

    def build_index(self, rebuild: bool = False) -> dict[str, Any]:
        """Build a Chroma index when optional dependencies are available."""
        if rebuild and self.persist_dir.exists():
            import shutil
            shutil.rmtree(self.persist_dir)
        try:
            from langchain_core.documents import Document
            from langchain_chroma import Chroma
            from langchain_huggingface import HuggingFaceEmbeddings
        except Exception as exc:
            return {"ok": False, "backend": "lexical", "message": f"Chroma no disponible: {exc}", "n_docs": len(self._docs)}
        self.persist_dir.mkdir(parents=True, exist_ok=True)
        embeddings = HuggingFaceEmbeddings(model_name=self.embedding_model)
        lc_docs = [
            Document(
                page_content=d.text,
                metadata={"title": d.title, "source": d.source, "section": d.section, "doc_id": d.doc_id or d.source},
            )
            for d in self._docs
        ]
        Chroma.from_documents(
            documents=lc_docs,
            embedding=embeddings,
            persist_directory=str(self.persist_dir),
            collection_name=self.collection_name,
        )
        self._chroma = None
        return {"ok": True, "backend": "chroma", "n_docs": len(lc_docs), "persist_dir": str(self.persist_dir)}

    def retrieve(self, query: str, k: int = 5, section_filter: list[str] | None = None) -> list[dict[str, Any]]:
        chroma = self._try_chroma()
        if chroma is not None:
            try:
                filter_arg = None
                if section_filter and len(section_filter) == 1:
                    filter_arg = {"section": section_filter[0]}
                elif section_filter and len(section_filter) > 1:
                    # Chroma metadata filters vary by version. If complex filter fails,
                    # we fallback to post-filtering below.
                    filter_arg = None
                docs_scores = chroma.similarity_search_with_relevance_scores(query, k=max(k * 3, k), filter=filter_arg)
                out: list[RAGDocument] = []
                allowed = {normalize_text(x) for x in section_filter or [] if x}
                for doc, score in docs_scores:
                    section = str(doc.metadata.get("section", "general"))
                    if allowed and normalize_text(section) not in allowed:
                        continue
                    out.append(
                        RAGDocument(
                            text=doc.page_content,
                            title=str(doc.metadata.get("title", "documento")),
                            source=str(doc.metadata.get("source", "desconocido")),
                            section=section,
                            doc_id=str(doc.metadata.get("doc_id", doc.metadata.get("source", "documento"))),
                            score=round(float(score), 4) if isinstance(score, (int, float)) else None,
                        )
                    )
                    if len(out) >= k:
                        break
                if out:
                    return [d.as_dict() for d in out]
            except Exception:
                pass
        return [d.as_dict() for d in self._lexical.retrieve(query, k=k, section_filter=section_filter)]

    def context_for_topics(self, topics: Iterable[str], k_per_topic: int = 3) -> list[dict[str, Any]]:
        seen: set[str] = set()
        contexts: list[dict[str, Any]] = []
        topic_to_sections = {
            "roles": ["roles"],
            "historial": ["historial"],
            "carga": ["carga"],
            "mercado": ["mercado"],
            "shap": ["shap"],
            "limitaciones": ["limitaciones"],
            "general": ["general"],
        }
        query_templates = {
            "roles": "interpretacion roles de juego weighted PCA clustering PC1 PC2 posicion jugador",
            "historial": "modelo historial medico LASSO incidencia dias desde ultima lesion exceso riesgo",
            "carga": "modelo carga fisica XGBoost ventanas riesgo lesion exceso riesgo exposicion",
            "mercado": "estimacion valor mercado LightGBM valor observado estimado calibracion ratio sesgo",
            "shap": "interpretacion SHAP importancia variables dependence plot contribucion modelo",
            "limitaciones": "limitaciones interpretacion lesiones valor mercado riesgo no causalidad",
            "general": "pipeline integrado TFM roles lesiones carga valor mercado LaLiga",
        }
        for topic in topics:
            key = normalize_text(topic)
            sections = topic_to_sections.get(key, None)
            query = query_templates.get(key, str(topic))
            for item in self.retrieve(query, k=k_per_topic, section_filter=sections):
                doc_id = str(item.get("doc_id") or item.get("source") or item.get("text", "")[:80])
                if doc_id not in seen:
                    seen.add(doc_id)
                    contexts.append(item)
        return contexts


def format_contexts(contexts: list[dict[str, Any]], max_chars: int = 3000) -> str:
    lines: list[str] = []
    used = 0
    for idx, item in enumerate(contexts, start=1):
        text = re.sub(r"\s+", " ", str(item.get("text", "")).strip())
        title = str(item.get("title", "documento"))
        source = str(item.get("source", "desconocido"))
        chunk = f"[{idx}] {title} ({source})\n{text}"
        if used + len(chunk) > max_chars:
            remaining = max_chars - used
            if remaining > 120:
                lines.append(chunk[:remaining].rstrip() + "...")
            break
        lines.append(chunk)
        used += len(chunk)
    return "\n\n".join(lines) if lines else "No se recupero contexto RAG."



if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="RAG metodologico TFM")
    parser.add_argument("--query", default="Como interpretar valor de mercado, roles y riesgo lesional?")
    parser.add_argument("--build", action="store_true")
    parser.add_argument("--rebuild", action="store_true")
    parser.add_argument("--k", type=int, default=5)
    args = parser.parse_args()

    rag = TFMRAG()
    if args.build:
        print(json.dumps(rag.build_index(rebuild=args.rebuild), ensure_ascii=False, indent=2))
    results = rag.retrieve(args.query, k=args.k)
    print(json.dumps(results, ensure_ascii=False, indent=2))
