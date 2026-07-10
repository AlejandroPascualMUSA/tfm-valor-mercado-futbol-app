"""Generador de informes para el TFM de valor de mercado.

Adapta el generador de reportes de partido a dos casos:

1. Informe individual de jugador, de 1-2 paginas.
2. Informe comparativo de 2-5 jugadores.

El flujo no usa agentes. Combina:
- datos estructurados de data/app/*.csv;
- contexto metodologico recuperado por RAG;
- plantilla determinista.

No usa agentes ni redaccion generativa externa: el RAG se limita a recuperar
contexto metodologico y los datos numericos proceden de los CSV app-ready.
"""

from __future__ import annotations

import argparse
import html
import json
import math
import os
import re
import unicodedata
from datetime import datetime
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd

from tfm_rag import TFMRAG, format_contexts


POSITION_ORDER = ["CB", "FB", "DMF", "CMF", "AMF", "WF", "CF"]
RISK_NOTE = (
    "Las probabilidades y excesos de riesgo deben leerse como senales relativas del modelo, "
    "no como diagnostico medico ni como predicciones deterministas."
)
SHAP_NOTE = (
    "Los valores SHAP explican contribuciones del modelo y no deben interpretarse como causalidad."
)

ROLE_BY_POSITION = {
    "CB": ["Stopper", "Ball Playing Defender", "Sweeper"],
    "FB": ["Wing Back", "Inverted Wing Back", "Full Back"],
    "DMF": ["Deep Lying Playmaker", "Ball Winning Midfielder"],
    "CMF": ["Playmaker", "Holding Midfielder", "Box-to-box Midfielder"],
    "AMF": ["Advanced Playmaker", "Second Striker"],
    "WF": ["Inside Forward", "Wide Playmaker", "Winger"],
    "CF": ["Poacher", "Mobile Striker", "Target Man"],
}

PC_INTERPRETATION = {
    "CB": {"PC1_neg": "Defender", "PC1_pos": "Playmaker", "PC2_neg": "Agresivo", "PC2_pos": "Controlador"},
    "FB": {"PC1_neg": "Ofensivo", "PC1_pos": "Defensivo", "PC2_neg": "Interior", "PC2_pos": "Exterior"},
    "DMF": {"PC1_neg": "Destructor", "PC1_pos": "Organizador", "PC2_neg": "Posicional", "PC2_pos": "Presionante"},
    "CMF": {"PC1_neg": "Creador", "PC1_pos": "Llegador", "PC2_neg": "Pausado", "PC2_pos": "Vertical"},
    "AMF": {"PC1_neg": "Playmaker", "PC1_pos": "Llegador", "PC2_neg": "Finalizador", "PC2_pos": "Asistente"},
    "WF": {"PC1_neg": "De banda", "PC1_pos": "Interior", "PC2_neg": "Asociativo", "PC2_pos": "Desbordador"},
    "CF": {"PC1_neg": "Finalizador", "PC1_pos": "Asociativo", "PC2_neg": "Fisico", "PC2_pos": "Tecnico"},
}

EVENT_FAMILY_RULES = [
    ("Penetracion", ["take on", "dribble", "penetr", "foul won"]),
    ("Progresion", ["progressive", "through", "deep", "long ball", "carry"]),
    ("Participacion", ["pass", "touch", "ball recovery"]),
    ("Verticalidad", ["key pass", "assist", "shot", "wide", "cross"]),
    ("Creatividad", ["key pass", "assist", "through", "chance"]),
    ("Finalizacion", ["shot", "goal", "xg", "aerial won"]),
    ("Defensa", ["tackle", "interception", "clearance", "blocked", "defensive"]),
    ("Aereo", ["aerial"]),
]

TECH_FEATURES = [
    "xg90", "xa90", "goles90", "remates90", "shots_on_target90",
    "offensive_impact", "expected_offensive_impact", "creativity_index",
    "chance_creation", "passes_completed90", "progressive_passes90",
    "progression_index", "deep_progression", "dribbles_completed90",
    "penetration", "involvement", "centrality", "box_presence",
    "defensive_duels_won90", "aerial_duels_won90", "defensive_impact", "defense_global",
]

SIMILARITY_FEATURES = [
    "age", "minutes_2324", "contract_years", "market_value_mill", "pred_iso_mill",
    "risk_hist_pct", "risk_load_pct", "xg90", "xa90", "goles90", "creativity_index",
    "progression_index", "defense_global", "involvement",
]

OBJECTIVE_LABELS = {
    "general": "Comparativa general",
    "comparativa": "Comparativa general",
    "comparativa general": "Comparativa general",
    "fichaje": "Fichaje",
    "renovacion": "Renovación",
    "renovación": "Renovación",
    "venta": "Venta",
    "seguimiento": "Seguimiento",
}


# ---------------------------------------------------------------------------
# Utilidades basicas
# ---------------------------------------------------------------------------


def safe_float(value: Any, default: float | None = None) -> float | None:
    try:
        x = float(value)
        if math.isfinite(x):
            return x
    except Exception:
        pass
    return default


def fmt_num(value: Any, digits: int = 2, suffix: str = "") -> str:
    x = safe_float(value)
    if x is None:
        return "no disponible"
    return f"{x:.{digits}f}{suffix}"


def fmt_pct(value: Any, digits: int = 1) -> str:
    x = safe_float(value)
    if x is None:
        return "no disponible"
    return f"{x:.{digits}f}%"


def fmt_mill(value: Any, digits: int = 2) -> str:
    x = safe_float(value)
    if x is None:
        return "no disponible"
    return f"{x:.{digits}f} M EUR"


def normalize_key(text: Any) -> str:
    value = str(text or "")
    value = unicodedata.normalize("NFKD", value)
    value = "".join(ch for ch in value if not unicodedata.combining(ch))
    value = value.lower()
    value = re.sub(r"[^a-z0-9]+", " ", value)
    return re.sub(r"\s+", " ", value).strip()




def objective_label(objective: Any) -> str:
    key = normalize_key(objective)
    labels = {
        "general": "Comparativa general",
        "comparativa": "Comparativa general",
        "comparativa general": "Comparativa general",
        "fichaje": "Fichaje",
        "renovacion": "Renovación",
        "renovación": "Renovación",
        "venta": "Venta",
        "seguimiento": "Seguimiento",
    }
    return labels.get(key, str(objective or "Comparativa general"))


def display_objective(objective: Any) -> str:
    return objective_label(objective)


def market_signal_from_ratio(ratio: Any) -> str:
    r = safe_float(ratio)
    if r is None:
        return "No disponible"
    if r > 1.10:
        return "Potencialmente infravalorado por mercado"
    if r < 0.90:
        return "Potencialmente sobrevalorado por mercado"
    return "Ajustado al modelo"

def json_safe(obj: Any) -> Any:
    if isinstance(obj, dict):
        return {str(k): json_safe(v) for k, v in obj.items()}
    if isinstance(obj, list):
        return [json_safe(v) for v in obj]
    if isinstance(obj, tuple):
        return [json_safe(v) for v in obj]
    if isinstance(obj, (np.integer,)):
        return int(obj)
    if isinstance(obj, (np.floating,)):
        x = float(obj)
        return None if not math.isfinite(x) else x
    if isinstance(obj, float):
        return None if not math.isfinite(obj) else obj
    if pd.isna(obj) if not isinstance(obj, (list, dict, tuple)) else False:
        return None
    return obj


def md_table(rows: list[dict[str, Any]], columns: list[str] | None = None) -> str:
    if not rows:
        return ""
    columns = columns or list(rows[0].keys())
    header = "| " + " | ".join(columns) + " |"
    sep = "| " + " | ".join(["---"] * len(columns)) + " |"
    body = []
    for row in rows:
        values = []
        for col in columns:
            val = row.get(col, "")
            text = str(val).replace("|", "/").replace("\n", " ")
            values.append(text)
        body.append("| " + " | ".join(values) + " |")
    return "\n".join([header, sep] + body)


# ---------------------------------------------------------------------------
# Carga de datos
# ---------------------------------------------------------------------------


class TFMData:
    def __init__(self, data_dir: str | Path) -> None:
        self.data_dir = Path(data_dir)
        if not self.data_dir.exists():
            raise FileNotFoundError(f"No existe data_dir: {self.data_dir}")
        self.players = self._read("players_master.csv")
        self.roles = self._read("roles_pca.csv")
        self.role_summary = self._read("role_summary.csv")
        self.events = self._read("event_percentiles.csv")
        self.injuries = self._read("injuries_long.csv")
        self.market = self._read("market_predictions.csv")
        self.shap_imp = self._read("shap_importance.csv")
        self.shap_dep = self._read("shap_dependence_long.csv")
        self.calibration = self._read("calibration_deciles.csv")
        self.model_metrics = self._read("comparacion_final_3_modelos.csv")
        self._prepare()

    def _read(self, name: str) -> pd.DataFrame:
        path = self.data_dir / name
        if not path.exists():
            return pd.DataFrame()
        return pd.read_csv(path)

    def _prepare(self) -> None:
        for df in [self.players, self.roles, self.events, self.injuries, self.market, self.shap_dep]:
            if not df.empty and "player_name" in df.columns:
                df["player_key"] = df["player_name"].map(normalize_key)
        if not self.players.empty:
            if "risk_hist_pct" not in self.players.columns and "risk_hist_excess" in self.players.columns:
                self.players["risk_hist_pct"] = self.players["risk_hist_excess"].astype(float) * 100
            if "risk_load_pct" not in self.players.columns and "risk_load_excess" in self.players.columns:
                self.players["risk_load_pct"] = self.players["risk_load_excess"].astype(float) * 100
            if "all_positions" not in self.players.columns:
                self.players["all_positions"] = self.players.apply(self._all_positions_row, axis=1)
            if "all_roles" not in self.players.columns:
                self.players["all_roles"] = self.players.apply(self._all_roles_row, axis=1)
            if "market_value_mill" in self.players.columns:
                vals = pd.to_numeric(self.players["market_value_mill"], errors="coerce")
                try:
                    # Decil por valor observado. El rank(method="first") evita problemas
                    # cuando hay muchos empates en valores bajos.
                    self.players["market_decile"] = pd.qcut(
                        vals.rank(method="first"),
                        q=10,
                        labels=[f"D{i}" for i in range(1, 11)]
                    ).astype(str)
                except Exception:
                    self.players["market_decile"] = "D?"

    @staticmethod
    def _all_positions_row(row: pd.Series) -> str:
        values = []
        for col in ["primary_position", "secondary_position"]:
            val = row.get(col)
            if isinstance(val, str) and val and val not in values:
                values.append(val)
        return " | ".join(values)

    @staticmethod
    def _all_roles_row(row: pd.Series) -> str:
        values = []
        for col in ["primary_role", "secondary_role"]:
            val = row.get(col)
            if isinstance(val, str) and val and val not in values:
                values.append(val)
        return " | ".join(values)

    def find_player_id(self, player_id: str | int | None = None, player_name: str | None = None) -> int:
        if player_id not in [None, ""]:
            pid = int(float(player_id))
            if pid in set(self.players["player_id"].astype(int).tolist()):
                return pid
        if not player_name:
            raise ValueError("Debes indicar player_id o player_name")
        key = normalize_key(player_name)
        matches = self.players[self.players["player_key"] == key]
        if matches.empty:
            matches = self.players[self.players["player_key"].str.contains(re.escape(key), na=False)]
        if matches.empty:
            raise ValueError(f"No se encontro jugador: {player_name}")
        return int(matches.iloc[0]["player_id"])

    def player_row(self, player_id: int) -> pd.Series:
        rows = self.players[self.players["player_id"].astype(int) == int(player_id)]
        if rows.empty:
            raise ValueError(f"player_id no encontrado: {player_id}")
        return rows.iloc[0]


# ---------------------------------------------------------------------------
# Construccion de payloads
# ---------------------------------------------------------------------------


def event_family(event_name: str) -> str:
    low = normalize_key(event_name)
    for family, pats in EVENT_FAMILY_RULES:
        if any(pat in low for pat in pats):
            return family
    return "Otros"


def top_event_profile(data: TFMData, player_id: int) -> dict[str, Any]:
    if data.events.empty:
        return {"top_events": [], "weak_events": [], "families": []}
    df = data.events[data.events["player_id"].astype(int) == int(player_id)].copy()
    if df.empty:
        return {"top_events": [], "weak_events": [], "families": []}
    df["pct"] = pd.to_numeric(df.get("percentile_position"), errors="coerce").fillna(0)
    df["family"] = df["event_name"].map(event_family)
    top = df.sort_values("pct", ascending=False).head(8)
    low = df.sort_values("pct", ascending=True).head(5)
    fam = df.groupby("family", as_index=False)["pct"].mean().sort_values("pct", ascending=False)
    return {
        "top_events": [{"event": r.event_name, "percentile": round(float(r.pct), 1)} for r in top.itertuples()],
        "weak_events": [{"event": r.event_name, "percentile": round(float(r.pct), 1)} for r in low.itertuples()],
        "families": [{"family": r.family, "percentile": round(float(r.pct), 1)} for r in fam.itertuples()],
    }


def role_profile(data: TFMData, player_id: int) -> dict[str, Any]:
    if data.roles.empty:
        return {"roles": [], "main": {}}
    df = data.roles[data.roles["player_id"].astype(int) == int(player_id)].copy()
    if df.empty:
        return {"roles": [], "main": {}}
    df["position_rank"] = df["position"].map({p: i for i, p in enumerate(POSITION_ORDER)}).fillna(99)
    df = df.sort_values(["event_share", "position_rank"], ascending=[False, True])
    rows = []
    for r in df.itertuples():
        interp = PC_INTERPRETATION.get(str(r.position), {})
        pc1 = safe_float(getattr(r, "PC1", None), 0) or 0
        pc2 = safe_float(getattr(r, "PC2", None), 0) or 0
        pc_text = []
        if interp:
            pc_text.append(interp["PC1_pos"] if pc1 >= 0 else interp["PC1_neg"])
            pc_text.append(interp["PC2_pos"] if pc2 >= 0 else interp["PC2_neg"])
        rows.append({
            "position": getattr(r, "position", None),
            "role": getattr(r, "role", None),
            "cluster": getattr(r, "cluster", None),
            "PC1": round(float(pc1), 3),
            "PC2": round(float(pc2), 3),
            "interpretation": getattr(r, "interpretation", None) or ", ".join(pc_text),
            "event_share": round(float(safe_float(getattr(r, "event_share", None), 0) or 0), 3),
        })
    return {"roles": rows, "main": rows[0] if rows else {}}


def injury_profile(data: TFMData, player_id: int) -> dict[str, Any]:
    row = data.player_row(player_id)
    injuries = pd.DataFrame()
    if not data.injuries.empty:
        injuries = data.injuries[data.injuries["player_id"].astype(int) == int(player_id)].copy()
    by_group: list[dict[str, Any]] = []
    by_season: list[dict[str, Any]] = []
    recent: list[dict[str, Any]] = []
    if not injuries.empty:
        by_group_df = injuries.groupby("injury_group", dropna=False).agg(
            n=("injury", "count"), days=("duration_days", "sum"), games=("games_missed", "sum")
        ).reset_index().sort_values("n", ascending=False)
        by_group = [
            {"group": str(r.injury_group), "n": int(r.n), "days": round(float(r.days or 0), 1), "games": round(float(r.games or 0), 1)}
            for r in by_group_df.itertuples()
        ]
        by_season_df = injuries.groupby("season_injured", dropna=False).agg(
            n=("injury", "count"), days=("duration_days", "sum"), games=("games_missed", "sum")
        ).reset_index().sort_values("season_injured")
        by_season = [
            {"season": str(r.season_injured), "n": int(r.n), "days": round(float(r.days or 0), 1), "games": round(float(r.games or 0), 1)}
            for r in by_season_df.itertuples()
        ]
        recent_df = injuries.sort_values("date_from", ascending=False).head(5)
        recent = [
            {
                "season": str(getattr(r, "season_injured", "")),
                "injury": str(getattr(r, "injury", "")),
                "group": str(getattr(r, "injury_group", "")),
                "duration_days": fmt_num(getattr(r, "duration_days", None), 0),
                "games_missed": fmt_num(getattr(r, "games_missed", None), 1),
            }
            for r in recent_df.itertuples()
        ]
    return {
        "lesionado_2324_hist": bool(row.get("injured_2324_hist", row.get("lesionado", False))),
        "p_hist": safe_float(row.get("p_hist")),
        "p_basal_hist": safe_float(row.get("p_basal_hist")),
        "risk_hist_pct": safe_float(row.get("risk_hist_pct")),
        "incidencia": safe_float(row.get("incidencia")),
        "dias_desde_ultima_lesion": safe_float(row.get("dias_desde_ultima_lesion")),
        "dias_baja_acumulados": safe_float(row.get("dias_baja_acumulados")),
        "n_lesiones": int(len(injuries)) if not injuries.empty else 0,
        "by_group": by_group,
        "by_season": by_season,
        "recent": recent,
        "note": RISK_NOTE,
    }


def load_profile(data: TFMData, player_id: int) -> dict[str, Any]:
    row = data.player_row(player_id)
    return {
        "lesionado_2324_load": bool(row.get("injured_2324_load", False)),
        "p_load_temporada": safe_float(row.get("prob_lesion_temporada")),
        "p_load_media": safe_float(row.get("prob_lesion_media")),
        "risk_load_pct": safe_float(row.get("risk_load_pct")),
        "n_ventanas": safe_float(row.get("n_ventanas_modelo")),
        "fecha_ultima_ventana": row.get("fecha_ultima_ventana"),
        "prob_lesion_reciente": safe_float(row.get("prob_lesion_reciente")),
        "prob_lesion_p90": safe_float(row.get("prob_lesion_p90")),
        "note": RISK_NOTE,
    }


def shap_profile(data: TFMData, player_id: int) -> dict[str, Any]:
    if data.shap_dep.empty:
        return {"top_positive": [], "top_negative": [], "global_top": []}
    df = data.shap_dep[data.shap_dep["player_id"].astype(int) == int(player_id)].copy()
    if df.empty:
        df = data.shap_dep.copy()
    df["shap_value"] = pd.to_numeric(df["shap_value"], errors="coerce").fillna(0)
    pos = df.sort_values("shap_value", ascending=False).head(6)
    neg = df.sort_values("shap_value", ascending=True).head(6)
    global_top: list[dict[str, Any]] = []
    if not data.shap_imp.empty:
        gi = data.shap_imp.sort_values("mean_abs_shap", ascending=False).head(8)
        global_top = [
            {"feature": str(r.feature), "importance": round(float(r.mean_abs_shap), 4), "group": str(getattr(r, "group", ""))}
            for r in gi.itertuples()
        ]
    return {
        "top_positive": [
            {"feature": str(r.feature_label), "value": fmt_num(r.feature_value, 3), "shap": round(float(r.shap_value), 4)}
            for r in pos.itertuples()
        ],
        "top_negative": [
            {"feature": str(r.feature_label), "value": fmt_num(r.feature_value, 3), "shap": round(float(r.shap_value), 4)}
            for r in neg.itertuples()
        ],
        "global_top": global_top,
        "note": SHAP_NOTE,
    }


def similar_players(data: TFMData, player_id: int, k: int = 8) -> list[dict[str, Any]]:
    players = data.players.copy()
    row = data.player_row(player_id)
    players = players[players["player_id"].astype(int) != int(player_id)].copy()
    if players.empty:
        return []
    # Prefer players sharing at least one position or role, but keep fallback.
    pos = {str(row.get("primary_position", "")), str(row.get("secondary_position", ""))}
    rol = {str(row.get("primary_role", "")), str(row.get("secondary_role", ""))}
    pos = {x for x in pos if x and x != "nan"}
    rol = {x for x in rol if x and x != "nan"}
    mask_pos = players["all_positions"].fillna("").map(lambda x: any(p in str(x) for p in pos))
    mask_rol = players["all_roles"].fillna("").map(lambda x: any(r in str(x) for r in rol))
    filtered = players[mask_pos | mask_rol].copy()
    if len(filtered) < max(4, k // 2):
        filtered = players.copy()
    features = [c for c in SIMILARITY_FEATURES if c in filtered.columns and c in row.index]
    dist = np.zeros(len(filtered))
    used = 0
    for col in features:
        vals = pd.to_numeric(filtered[col], errors="coerce")
        target = safe_float(row.get(col))
        sd = float(vals.std(skipna=True)) if vals.notna().sum() > 1 else 0.0
        if target is None or not math.isfinite(sd) or sd <= 0:
            continue
        z = (vals.fillna(vals.median()) - target) / sd
        dist += np.square(z.to_numpy(dtype=float))
        used += 1
    if used > 0:
        dist = np.sqrt(dist / used)
    filtered["distance"] = dist
    filtered["similarity_pct"] = 100 * np.exp(-filtered["distance"])
    filtered = filtered.sort_values("distance").head(k)
    return [
        {
            "player": str(r.player_name),
            "team": str(r.team),
            "positions": str(getattr(r, "all_positions", "")),
            "roles": str(getattr(r, "all_roles", "")),
            "market_value_mill": round(float(safe_float(getattr(r, "market_value_mill", None), 0) or 0), 2),
            "pred_iso_mill": round(float(safe_float(getattr(r, "pred_iso_mill", None), 0) or 0), 2),
            "similarity_pct": round(float(getattr(r, "similarity_pct", 0)), 1),
        }
        for r in filtered.itertuples()
    ]


def market_profile(row: pd.Series, data: TFMData | None = None) -> dict[str, Any]:
    real = safe_float(row.get("market_value_mill"))
    pred_iso = safe_float(row.get("pred_iso_mill"))
    diff_iso = safe_float(row.get("diff_iso_mill"))
    ratio_iso = safe_float(row.get("ratio_iso"))
    pred_raw = safe_float(row.get("pred_raw_mill"))
    diff_raw = safe_float(row.get("diff_raw_mill"))
    ratio_raw = safe_float(row.get("ratio_raw"))

    if pred_iso is not None and real is not None:
        diff_iso = pred_iso - real
        ratio_iso = pred_iso / real if real > 0 else None
    if pred_raw is not None and real is not None:
        diff_raw = pred_raw - real
        ratio_raw = pred_raw / real if real > 0 else None

    signal = market_signal_from_ratio(ratio_iso)
    signal_raw = market_signal_from_ratio(ratio_raw)

    decile = str(row.get("market_decile", "")) if row.get("market_decile", "") is not None else ""
    decile_info: dict[str, Any] = {}
    if data is not None and not data.calibration.empty and decile:
        cal = data.calibration[data.calibration["decile"].astype(str) == decile]
        if not cal.empty:
            cr = cal.iloc[0]
            real_mean = safe_float(cr.get("real_mean_mill"))
            iso_mean = safe_float(cr.get("pred_iso_mean_mill"))
            raw_mean = safe_float(cr.get("pred_raw_mean_mill"))
            decile_info = {
                "decile": decile,
                "n_decile": safe_float(cr.get("n")),
                "real_mean_mill": real_mean,
                "pred_iso_mean_mill": iso_mean,
                "pred_raw_mean_mill": raw_mean,
                "ratio_iso_mean": safe_float(cr.get("ratio_iso_mean")),
                "ratio_raw_mean": safe_float(cr.get("ratio_raw_mean")),
                "bias_iso_mill": iso_mean - real_mean if iso_mean is not None and real_mean is not None else None,
                "bias_raw_mill": raw_mean - real_mean if raw_mean is not None and real_mean is not None else None,
            }
    if not decile_info:
        decile_info = {"decile": decile or "no disponible"}

    return {
        "observed_mill": real,
        "estimated_mill": pred_iso,
        "diff_mill": diff_iso,
        "ratio": ratio_iso,
        "estimated_iso_mill": pred_iso,
        "diff_iso_mill": diff_iso,
        "ratio_iso": ratio_iso,
        "estimated_raw_mill": pred_raw,
        "diff_raw_mill": diff_raw,
        "ratio_raw": ratio_raw,
        "abs_error_mill": abs(diff_iso) if diff_iso is not None else None,
        "abs_error_raw_mill": abs(diff_raw) if diff_raw is not None else None,
        "signal": signal,
        "signal_raw": signal_raw,
        "calibration_decile": decile_info,
    }

def build_player_payload(data: TFMData, player_id: int) -> dict[str, Any]:
    row = data.player_row(player_id)
    role = role_profile(data, player_id)
    events = top_event_profile(data, player_id)
    injury = injury_profile(data, player_id)
    load = load_profile(data, player_id)
    shap = shap_profile(data, player_id)
    sims = similar_players(data, player_id, k=8)
    profile = {
        "player_id": int(player_id),
        "name": str(row.get("player_name", "")),
        "team": str(row.get("team", "")),
        "team_group": str(row.get("team_group_model", row.get("team_group_table", ""))),
        "country_group": str(row.get("country_group", "")),
        "nationality": str(row.get("nationality", "")),
        "age": safe_float(row.get("age")),
        "contract_years": safe_float(row.get("contract_years")),
        "minutes_2324": safe_float(row.get("minutes_2324")),
        "positions": str(row.get("all_positions", row.get("primary_position", ""))),
        "roles": str(row.get("all_roles", row.get("primary_role", ""))),
        "primary_position": str(row.get("primary_position", "")),
        "primary_role": str(row.get("primary_role", "")),
        "value_band": str(row.get("value_band", "")),
    }
    return json_safe({
        "profile": profile,
        "market": market_profile(row, data),
        "roles": role,
        "events": events,
        "injury": injury,
        "load": load,
        "shap": shap,
        "similar_players": sims,
    })


def build_comparison_payload(data: TFMData, player_ids: list[int], objective: str = "Comparativa general") -> dict[str, Any]:
    objective = objective_label(objective)
    players_payload = [build_player_payload(data, pid) for pid in player_ids]
    # Common positions and roles.
    pos_sets = []
    role_sets = []
    for p in players_payload:
        pos_sets.append({x.strip() for x in str(p["profile"].get("positions", "")).split("|") if x.strip()})
        role_sets.append({x.strip() for x in str(p["profile"].get("roles", "")).split("|") if x.strip()})
    common_positions = sorted(set.intersection(*pos_sets)) if pos_sets else []
    common_roles = sorted(set.intersection(*role_sets)) if role_sets else []

    # Event matrix using family percentiles.
    event_rows: list[dict[str, Any]] = []
    for p in players_payload:
        fams = {item["family"]: item["percentile"] for item in p["events"].get("families", [])}
        event_rows.append({"player": p["profile"]["name"], **fams})
    # Differential families.
    all_fams = sorted({k for row in event_rows for k in row.keys() if k != "player"})
    differentials = []
    for fam in all_fams:
        vals = [safe_float(row.get(fam), 0) or 0 for row in event_rows]
        if vals:
            differentials.append({"family": fam, "range": round(max(vals) - min(vals), 1), "max": round(max(vals), 1), "min": round(min(vals), 1)})
    differentials.sort(key=lambda x: x["range"], reverse=True)

    # Simple objective score. Higher is preferred.
    ranking = []
    for p in players_payload:
        m = p["market"]
        prof = p["profile"]
        inj = p["injury"]
        load = p["load"]
        age = safe_float(prof.get("age"), 26) or 26
        # En informes comparativos, la referencia económica principal es la predicción
        # original sin calibrar. La isotónica se conserva como contexto por decil.
        ratio = safe_float(m.get("ratio_raw"), 1) or 1
        risk = (safe_float(inj.get("risk_hist_pct"), 0) or 0) + (safe_float(load.get("risk_load_pct"), 0) or 0)
        value = safe_float(m.get("observed_mill"), 0) or 0
        minutes = safe_float(prof.get("minutes_2324"), 0) or 0
        if normalize_key(objective) == "venta":
            score = (1 - ratio) * 35 + age * 0.3 + risk * 0.5 + value * 0.05
        elif normalize_key(objective) == "renovacion":
            score = ratio * 25 + minutes / 200 - risk * 0.8 - max(0, age - 30) * 1.2
        elif normalize_key(objective) == "fichaje":
            score = ratio * 30 + max(0, 28 - age) * 1.2 + minutes / 300 - risk * 0.9
        else:
            score = ratio * 20 + minutes / 300 - risk * 0.6 + max(0, 28 - age) * 0.5
        ranking.append({"player": prof["name"], "score": round(score, 2), "signal": m.get("signal_raw", m.get("signal", ""))})
    ranking.sort(key=lambda x: x["score"], reverse=True)

    return json_safe({
        "objective": objective_label(objective),
        "objective_key": normalize_key(objective),
        "players": players_payload,
        "common_positions": common_positions,
        "common_roles": common_roles,
        "event_family_rows": event_rows,
        "event_differentials": differentials[:8],
        "ranking": ranking,
    })


# ---------------------------------------------------------------------------
# Plantillas deterministas
# ---------------------------------------------------------------------------


def individual_template(payload: dict[str, Any], contexts: list[dict[str, Any]]) -> str:
    p = payload["profile"]
    m = payload["market"]
    role = payload["roles"].get("main", {})
    inj = payload["injury"]
    load = payload["load"]
    shap = payload["shap"]
    events = payload["events"]
    sims = payload.get("similar_players", [])

    rows = [
        {"Campo": "Equipo", "Valor": p.get("team")},
        {"Campo": "Edad", "Valor": fmt_num(p.get("age"), 0)},
        {"Campo": "Posiciones", "Valor": p.get("positions")},
        {"Campo": "Roles", "Valor": p.get("roles")},
        {"Campo": "Minutos 23/24", "Valor": fmt_num(p.get("minutes_2324"), 0)},
        {"Campo": "Contrato", "Valor": fmt_num(p.get("contract_years"), 1) + " anos"},
        {"Campo": "Valor observado", "Valor": fmt_mill(m.get("observed_mill"), 2)},
        {"Campo": "Estimación isotónica", "Valor": fmt_mill(m.get("estimated_iso_mill"), 2)},
        {"Campo": "Predicción sin calibrar", "Valor": fmt_mill(m.get("estimated_raw_mill"), 2)},
    ]
    top_events = ", ".join([f"{x['event']} p{x['percentile']}" for x in events.get("top_events", [])[:5]]) or "no disponible"
    weak_events = ", ".join([f"{x['event']} p{x['percentile']}" for x in events.get("weak_events", [])[:3]]) or "no disponible"
    pos_shap = ", ".join([f"{x['feature']} ({x['shap']})" for x in shap.get("top_positive", [])[:4]]) or "no disponible"
    neg_shap = ", ".join([f"{x['feature']} ({x['shap']})" for x in shap.get("top_negative", [])[:4]]) or "no disponible"

    sim_rows = [
        {
            "Jugador": x["player"],
            "Equipo": x["team"],
            "Similitud": fmt_pct(x["similarity_pct"], 1),
            "Valor": fmt_mill(x["market_value_mill"], 1),
            "Pred. iso": fmt_mill(x["pred_iso_mill"], 1),
        }
        for x in sims[:5]
    ]

    lines = [
        f"# Informe individual - {p.get('name')}",
        "",
        "## 1. Ficha rapida",
        "",
        md_table(rows, ["Campo", "Valor"]),
        "",
        "## 2. Diagnostico de valor de mercado",
        "",
        market_text(m),
        "",
        "## 3. Perfil tactico y rol",
        "",
        (
            f"El rol principal detectado es **{role.get('role', p.get('primary_role'))}** en posicion "
            f"**{role.get('position', p.get('primary_position'))}**. En el plano PCA aparece con PC1="
            f"{fmt_num(role.get('PC1'), 2)} y PC2={fmt_num(role.get('PC2'), 2)}, interpretado como "
            f"**{role.get('interpretation', 'no disponible')}**. Sus puntos fuertes por percentil de eventos son: {top_events}. "
            f"Las areas con percentiles mas bajos son: {weak_events}."
        ),
        "",
        "## 4. Riesgo lesional y disponibilidad",
        "",
        (
            f"Por historial medico, P(historial)={fmt_pct((safe_float(inj.get('p_hist'), 0) or 0) * 100, 1)} y "
            f"exceso de riesgo={fmt_pct(inj.get('risk_hist_pct'), 2)}. Las variables LASSO clave son incidencia "
            f"({fmt_num(inj.get('incidencia'), 2)}) y dias desde ultima lesion ({fmt_num(inj.get('dias_desde_ultima_lesion'), 0)}). "
            f"Por carga fisica, P(carga temporada)={fmt_pct((safe_float(load.get('p_load_temporada'), 0) or 0) * 100, 1)} y "
            f"exceso por carga={fmt_pct(load.get('risk_load_pct'), 3)}. {RISK_NOTE}"
        ),
        "",
        "## 5. Explicacion del modelo",
        "",
        (
            f"Variables que mas elevan la prediccion en el perfil disponible: {pos_shap}. "
            f"Variables que mas reducen la prediccion: {neg_shap}. {SHAP_NOTE}"
        ),
        "",
        "## 6. Comparables",
        "",
        md_table(sim_rows, ["Jugador", "Equipo", "Similitud", "Valor", "Pred. iso"]) if sim_rows else "No hay comparables disponibles.",
        "",
        "## 7. Conclusion ejecutiva",
        "",
        conclusion_individual(payload),
        "",
        "## Contexto RAG usado",
        "",
        sources_block(contexts),
    ]
    return "\n".join(lines)


def market_text(m: dict[str, Any]) -> str:
    """Bloque ordenado para diagnosticar valor de mercado.

    Se muestran siempre dos lecturas: la estimacion calibrada por isotónica y la
    prediccion original sin calibrar. La calibrada corrige sesgos globales por decil,
    mientras que la original ayuda a leer jugadores de cola alta donde la calibracion
    puede acercar demasiado la prediccion a valores medios.
    """
    dec = m.get("calibration_decile", {}) or {}
    decile = dec.get("decile", "no disponible") or "no disponible"

    pred_rows = [
        {
            "Lectura": "Valor observado",
            "Valor": fmt_mill(m.get("observed_mill"), 2),
            "Diferencia": "-",
            "Ratio": "-",
        },
        {
            "Lectura": "Estimacion isotónica",
            "Valor": fmt_mill(m.get("estimated_iso_mill"), 2),
            "Diferencia": fmt_mill(m.get("diff_iso_mill"), 2),
            "Ratio": fmt_num(m.get("ratio_iso"), 2),
        },
        {
            "Lectura": "Prediccion original sin calibrar",
            "Valor": fmt_mill(m.get("estimated_raw_mill"), 2),
            "Diferencia": fmt_mill(m.get("diff_raw_mill"), 2),
            "Ratio": fmt_num(m.get("ratio_raw"), 2),
        },
    ]

    decile_rows: list[dict[str, Any]] = []
    if decile != "no disponible":
        decile_rows.append({
            "Decil": decile,
            "N": fmt_num(dec.get("n_decile"), 0),
            "Real medio": fmt_mill(dec.get("real_mean_mill"), 2),
            "Iso medio": fmt_mill(dec.get("pred_iso_mean_mill"), 2),
            "Sesgo iso": fmt_mill(dec.get("bias_iso_mill"), 2),
            "Raw medio": fmt_mill(dec.get("pred_raw_mean_mill"), 2),
            "Sesgo raw": fmt_mill(dec.get("bias_raw_mill"), 2),
        })

    intro = (
        f"La lectura principal del modelo es **{m.get('signal')}**. "
        f"El jugador pertenece al **decil {decile}** de valor observado."
        if decile != "no disponible"
        else f"La lectura principal del modelo es **{m.get('signal')}**."
    )

    decile_note = ""
    if decile_rows:
        decile_note = (
            "La calibracion isotónica debe interpretarse dentro de su segmento de mercado: "
            f"en el decil {decile}, el valor estimado isotónico medio es "
            f"**{fmt_mill(dec.get('pred_iso_mean_mill'), 2)}** y el sesgo medio isotónico "
            f"es **{fmt_mill(dec.get('bias_iso_mill'), 2)}**. "
            "La prediccion original se conserva como contraste porque, en jugadores estrella, "
            "la calibracion puede suavizar la cola alta y acercar la estimacion a valores medios."
        )

    return "\n\n".join([
        intro,
        md_table(pred_rows, ["Lectura", "Valor", "Diferencia", "Ratio"]),
        md_table(decile_rows, ["Decil", "N", "Real medio", "Iso medio", "Sesgo iso", "Raw medio", "Sesgo raw"]) if decile_rows else "",
        decile_note.strip(),
    ]).strip()

def comparative_market_decile_rows(players: list[dict[str, Any]]) -> list[dict[str, Any]]:
    rows: list[dict[str, Any]] = []
    for p in players:
        prof = p.get("profile", {})
        m = p.get("market", {})
        dec = m.get("calibration_decile", {}) or {}
        rows.append({
            "Jugador": prof.get("name", ""),
            "Valor": fmt_mill(m.get("observed_mill"), 1),
            "Raw": fmt_mill(m.get("estimated_raw_mill"), 1),
            "Ratio raw": fmt_num(m.get("ratio_raw"), 2),
            "Raw decil": fmt_mill(dec.get("pred_raw_mean_mill"), 1),
            "Sesgo raw": fmt_mill(dec.get("bias_raw_mill"), 1),
            "Iso": fmt_mill(m.get("estimated_iso_mill"), 1),
            "Ratio iso": fmt_num(m.get("ratio_iso"), 2),
            "Iso decil": fmt_mill(dec.get("pred_iso_mean_mill"), 1),
            "Sesgo iso": fmt_mill(dec.get("bias_iso_mill"), 1),
            "Decil": dec.get("decile", ""),
            "Real decil": fmt_mill(dec.get("real_mean_mill"), 1),
        })
    return rows


def conclusion_individual(payload: dict[str, Any]) -> str:
    p = payload["profile"]
    m = payload["market"]
    inj = payload["injury"]
    load = payload["load"]
    ratio = safe_float(m.get("ratio"), 1) or 1
    risk = (safe_float(inj.get("risk_hist_pct"), 0) or 0) + (safe_float(load.get("risk_load_pct"), 0) or 0)
    age = safe_float(p.get("age"), 26) or 26
    if ratio > 1.1 and risk < 5 and age <= 28:
        return "Perfil de oportunidad: el modelo estima un valor superior al observado y no aparecen alertas extremas de riesgo relativo. Conviene revisar encaje tactico y disponibilidad reciente antes de tomar decision."
    if ratio < 0.9:
        return "Perfil exigente en precio: el modelo estima un valor inferior al observado. Puede estar incorporando factores de mercado no observados, pero requiere cautela si el objetivo es fichaje."
    if risk >= 8:
        return "Perfil con alerta de disponibilidad: el valor aparece razonablemente ajustado, pero el riesgo relativo aconseja revisar historial clinico, carga reciente y plan de gestion fisica."
    return "Perfil ajustado: el valor estimado y observado son razonablemente coherentes. La decision deberia apoyarse en el encaje de rol, edad, contrato y comparables."


def comparison_template(payload: dict[str, Any], contexts: list[dict[str, Any]]) -> str:
    players = payload["players"]
    objective = payload.get("objective", "Comparativa general")
    summary_rows = []
    for p in players:
        prof = p["profile"]
        m = p["market"]
        inj = p["injury"]
        load = p["load"]
        summary_rows.append({
            "Jugador": prof["name"],
            "Equipo": prof["team"],
            "Edad": fmt_num(prof.get("age"), 0),
            "Posiciones": prof.get("positions"),
            "Roles": prof.get("roles"),
            "Valor": fmt_mill(m.get("observed_mill"), 1),
            "Pred. raw": fmt_mill(m.get("estimated_raw_mill"), 1),
            "Ratio raw": fmt_num(m.get("ratio_raw"), 2),
            "Pred. iso": fmt_mill(m.get("estimated_iso_mill"), 1),
            "Ratio iso": fmt_num(m.get("ratio_iso"), 2),
            "Decil": (m.get("calibration_decile", {}) or {}).get("decile", ""),
            "R.hist": fmt_pct(inj.get("risk_hist_pct"), 1),
            "R.carga": fmt_pct(load.get("risk_load_pct"), 2),
        })
    rank_rows = [{"Rank": i + 1, "Jugador": r["player"], "Score": r["score"], "Lectura": r["signal"]} for i, r in enumerate(payload.get("ranking", []))]
    market_decile_rows = comparative_market_decile_rows(players)
    diff_rows = payload.get("event_differentials", [])
    diff_rows_fmt = [
        {"Familia": r["family"], "Rango pct": r["range"], "Max": r["max"], "Min": r["min"]}
        for r in diff_rows
    ]
    lines = [
        f"# Informe comparativo de jugadores - {objective}",
        "",
        "## 1. Resumen comparativo",
        "",
        md_table(summary_rows, ["Jugador", "Equipo", "Edad", "Posiciones", "Roles", "Valor", "Pred. raw", "Ratio raw", "Pred. iso", "Ratio iso", "Decil", "R.hist", "R.carga"]),
        "",
        "## 2. Posiciones y roles comunes",
        "",
        f"Posiciones comunes: **{', '.join(payload.get('common_positions') or ['sin comun'])}**. Roles comunes: **{', '.join(payload.get('common_roles') or ['sin comun'])}**.",
        "",
        "## 3. Valor de mercado y oportunidad relativa",
        "",
        comparative_market_reading(players),
        "",
        md_table(market_decile_rows, ["Jugador", "Valor", "Raw", "Ratio raw", "Raw decil", "Sesgo raw", "Iso", "Ratio iso", "Iso decil", "Sesgo iso", "Decil", "Real decil"]),
        "",
        "## 4. Diferencias tacticas y de eventos",
        "",
        md_table(diff_rows_fmt, ["Familia", "Rango pct", "Max", "Min"]) if diff_rows_fmt else "No hay eventos suficientes para calcular diferenciales.",
        "",
        "## 5. Riesgo y disponibilidad",
        "",
        comparative_risk_reading(players),
        "",
        "## 6. Ranking segun objetivo",
        "",
        md_table(rank_rows, ["Rank", "Jugador", "Score", "Lectura"]) if rank_rows else "No hay ranking disponible.",
        "",
        "## 7. Conclusion",
        "",
        conclusion_comparison(payload),
        "",
        "## Contexto RAG usado",
        "",
        sources_block(contexts),
    ]
    return "\n".join(lines)


def comparative_market_reading(players: list[dict[str, Any]]) -> str:
    # Referencia principal: prediccion original sin calibrar.
    # La isotónica se mantiene como informacion complementaria para interpretar
    # sesgo y deciles, pero no como comparacion principal de precio.
    raw_best = max(players, key=lambda p: safe_float(p["market"].get("ratio_raw"), 0) or 0)
    raw_worst = min(players, key=lambda p: safe_float(p["market"].get("ratio_raw"), 999) or 999)
    iso_best = max(players, key=lambda p: safe_float(p["market"].get("ratio_iso"), 0) or 0)
    best_dec = (raw_best["market"].get("calibration_decile", {}) or {}).get("decile", "no disponible")
    worst_dec = (raw_worst["market"].get("calibration_decile", {}) or {}).get("decile", "no disponible")
    return (
        "La comparacion economica toma como referencia principal la **prediccion original sin calibrar**, "
        "porque conserva mejor las diferencias relativas entre jugadores y evita que la calibracion isotónica suavice en exceso la cola alta del mercado. "
        f"Con esta lectura, el mayor ratio prediccion/valor observado corresponde a **{raw_best['profile']['name']}** "
        f"({fmt_num(raw_best['market'].get('ratio_raw'), 2)}, decil {best_dec}), mientras que el menor ratio corresponde a "
        f"**{raw_worst['profile']['name']}** ({fmt_num(raw_worst['market'].get('ratio_raw'), 2)}, decil {worst_dec}). "
        f"La estimacion isotónica se mantiene como contraste por segmentos de mercado: con calibracion, el mayor ratio corresponde a "
        f"**{iso_best['profile']['name']}** ({fmt_num(iso_best['market'].get('ratio_iso'), 2)}). "
        "La tabla resume para cada jugador la prediccion sin calibrar, su ratio, las medias/sesgos del decil y la lectura isotónica complementaria."
    )


def comparative_risk_reading(players: list[dict[str, Any]]) -> str:
    def total_risk(p: dict[str, Any]) -> float:
        return (safe_float(p["injury"].get("risk_hist_pct"), 0) or 0) + (safe_float(p["load"].get("risk_load_pct"), 0) or 0)
    high = max(players, key=total_risk)
    low = min(players, key=total_risk)
    return (
        f"El mayor riesgo relativo agregado aparece en **{high['profile']['name']}** "
        f"({fmt_pct(total_risk(high), 2)} sumando historial y carga). El menor aparece en "
        f"**{low['profile']['name']}** ({fmt_pct(total_risk(low), 2)}). {RISK_NOTE}"
    )


def conclusion_comparison(payload: dict[str, Any]) -> str:
    ranking = payload.get("ranking", [])
    if not ranking:
        return "La comparacion no permite establecer un ranking por falta de datos suficientes."
    top = ranking[0]
    objective = payload.get("objective", "comparativa general")
    return (
        f"Para el objetivo **{objective}**, el perfil que queda mejor situado por el score compuesto es "
        f"**{top['player']}**. Esta recomendacion es orientativa: combina valor relativo, edad, minutos, contrato y riesgo, "
        "pero debe contrastarse con scouting cualitativo, necesidades de plantilla y condiciones reales de mercado."
    )


def sources_block(contexts: list[dict[str, Any]]) -> str:
    if not contexts:
        return "No se recuperaron documentos metodologicos."
    rows = []
    seen = set()
    for c in contexts[:8]:
        key = c.get("source") or c.get("title")
        if key in seen:
            continue
        seen.add(key)
        rows.append(f"- {c.get('title', 'documento')} ({c.get('source', 'sin fuente')})")
    return "\n".join(rows)


# ---------------------------------------------------------------------------
# HTML / IO
# ---------------------------------------------------------------------------




def simple_markdown_to_html(md: str) -> str:
    lines = md.splitlines()
    out: list[str] = []
    in_ul = False
    in_table = False
    for line in lines:
        raw = line.rstrip()
        if not raw:
            if in_ul:
                out.append("</ul>")
                in_ul = False
            if in_table:
                out.append("</tbody></table>")
                in_table = False
            out.append("")
            continue
        if raw.startswith("| ") and raw.endswith(" |"):
            cells = [c.strip() for c in raw.strip("|").split("|")]
            if all(set(c) <= {"-", ":", " "} for c in cells):
                continue
            if not in_table:
                out.append('<table class="report-table"><tbody>')
                in_table = True
            tag = "th" if "<tr>" not in "".join(out[-1:]) and len(out) > 0 else "td"
            # First table row after opening is header.
            if out and out[-1].endswith("<tbody>"):
                tag = "th"
            row_html = "".join([f"<{tag}>{html.escape(c)}</{tag}>" for c in cells])
            out.append(f"<tr>{row_html}</tr>")
            continue
        if in_table:
            out.append("</tbody></table>")
            in_table = False
        if raw.startswith("# "):
            out.append(f"<h1>{html.escape(raw[2:])}</h1>")
        elif raw.startswith("## "):
            out.append(f"<h2>{html.escape(raw[3:])}</h2>")
        elif raw.startswith("### "):
            out.append(f"<h3>{html.escape(raw[4:])}</h3>")
        elif raw.startswith("- "):
            if not in_ul:
                out.append("<ul>")
                in_ul = True
            out.append(f"<li>{inline_md(html.escape(raw[2:]))}</li>")
        else:
            if in_ul:
                out.append("</ul>")
                in_ul = False
            out.append(f"<p>{inline_md(html.escape(raw))}</p>")
    if in_ul:
        out.append("</ul>")
    if in_table:
        out.append("</tbody></table>")
    return "\n".join(out)


def inline_md(text: str) -> str:
    text = re.sub(r"\*\*(.+?)\*\*", r"<strong>\1</strong>", text)
    return text


def write_markdown(text: str, path: str | Path) -> Path:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


def write_html(text: str, path: str | Path, title: str = "Informe TFM") -> Path:
    now = datetime.now().strftime("%Y-%m-%d %H:%M")
    body = simple_markdown_to_html(text)
    html_text = f"""<!doctype html>
<html lang="es">
<head>
  <meta charset="utf-8">
  <title>{html.escape(title)}</title>
  <style>
    :root {{ --blue:#1f4e79; --light:#f5f7fb; --border:#dfe6ef; --text:#162033; }}
    body {{ font-family: Arial, Helvetica, sans-serif; color: var(--text); max-width: 980px; margin: 28px auto; line-height: 1.45; }}
    h1 {{ color: var(--blue); font-size: 28px; border-bottom: 3px solid var(--blue); padding-bottom: 8px; }}
    h2 {{ color: var(--blue); font-size: 19px; margin-top: 24px; }}
    h3 {{ color: var(--blue); font-size: 16px; }}
    p {{ margin: 8px 0; }}
    .meta {{ color:#687386; font-size: 12px; margin-bottom: 16px; }}
    .report-table {{ border-collapse: collapse; width: 100%; margin: 10px 0 16px 0; font-size: 13px; }}
    .report-table th {{ background: var(--light); color: var(--blue); text-align:left; }}
    .report-table th, .report-table td {{ border: 1px solid var(--border); padding: 7px 8px; vertical-align: top; }}
    ul {{ margin-top: 6px; }}
    strong {{ color: #0f3f67; }}
    @media print {{ body {{ max-width: none; margin: 14mm; font-size: 11pt; }} h1 {{ font-size: 20pt; }} h2 {{ font-size: 14pt; page-break-after: avoid; }} }}
  </style>
</head>
<body>
  <div class="meta">Generado automaticamente - {html.escape(now)}</div>
  {body}
</body>
</html>
"""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(html_text, encoding="utf-8")
    return path


def generate_report(
    data_dir: str | Path,
    report_type: str,
    player_ids: list[int],
    out_dir: str | Path,
    objective: str = "Comparativa general",
) -> dict[str, Any]:
    data = TFMData(data_dir)
    rag = TFMRAG()
    if report_type == "individual":
        if not player_ids:
            raise ValueError("Informe individual requiere un jugador")
        payload = build_player_payload(data, player_ids[0])
        topics = ["general", "mercado", "roles", "historial", "carga", "shap", "limitaciones"]
        contexts = rag.context_for_topics(topics, k_per_topic=2)
        text = individual_template(payload, contexts)
        slug = normalize_key(payload["profile"]["name"]).replace(" ", "_") or str(player_ids[0])
        title = f"Informe individual - {payload['profile']['name']}"
    elif report_type == "comparison":
        if len(player_ids) < 2:
            raise ValueError("Informe comparativo requiere al menos dos jugadores")
        payload = build_comparison_payload(data, player_ids, objective=objective)
        topics = ["general", "mercado", "roles", "historial", "carga", "shap", "limitaciones"]
        contexts = rag.context_for_topics(topics, k_per_topic=2)
        text = comparison_template(payload, contexts)
        names = "_vs_".join([normalize_key(p["profile"]["name"]).replace(" ", "_") for p in payload["players"][:3]])
        slug = names[:120] or "comparativo"
        title = "Informe comparativo"
    else:
        raise ValueError("report_type debe ser individual o comparison")

    out_dir = Path(out_dir)
    stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    md_path = out_dir / f"{report_type}_{slug}_{stamp}.md"
    html_path = out_dir / f"{report_type}_{slug}_{stamp}.html"
    write_markdown(text, md_path)
    write_html(text, html_path, title=title)
    json_path = out_dir / f"{report_type}_{slug}_{stamp}.json"
    json_path.write_text(json.dumps(json_safe(payload), ensure_ascii=False, indent=2), encoding="utf-8")
    return {
        "ok": True,
        "report_type": report_type,
        "markdown": str(md_path.resolve()),
        "html": str(html_path.resolve()),
        "payload": str(json_path.resolve()),
        "title": title,
    }


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Genera informes TFM con datos estructurados + RAG")
    parser.add_argument("--data-dir", default=str(Path(__file__).resolve().parents[1] / "data" / "app"))
    parser.add_argument("--report", choices=["individual", "comparison"], required=True)
    parser.add_argument("--player-id", action="append", default=[])
    parser.add_argument("--player-name", action="append", default=[])
    parser.add_argument("--objective", default="general")
    parser.add_argument("--out-dir", default=str(Path(__file__).resolve().parents[1] / "reports" / "generated"))
    args, unknown = parser.parse_known_args()
    # Compatibilidad con llamadas antiguas desde Windows/R que partían "Comparativa general"
    # en dos argumentos: --objective Comparativa general.
    if unknown:
        if args.report == "comparison" and not any(str(x).startswith("-") for x in unknown):
            args.objective = " ".join([str(args.objective)] + [str(x) for x in unknown]).strip()
        else:
            parser.error("unrecognized arguments: " + " ".join(map(str, unknown)))
    args.objective = display_objective(args.objective)
    return args


if __name__ == "__main__":
    args = parse_args()
    try:
        data = TFMData(args.data_dir)
        ids: list[int] = []
        for pid in args.player_id:
            if str(pid).strip():
                ids.append(data.find_player_id(player_id=pid))
        for name in args.player_name:
            if str(name).strip():
                ids.append(data.find_player_id(player_name=name))
        # Deduplicate preserving order.
        dedup: list[int] = []
        for pid in ids:
            if pid not in dedup:
                dedup.append(pid)
        result = generate_report(args.data_dir, args.report, dedup, args.out_dir, objective=args.objective)
        print("JSON_RESULT=" + json.dumps(result, ensure_ascii=False))
    except Exception as exc:
        err = {"ok": False, "error": str(exc)}
        print("JSON_RESULT=" + json.dumps(err, ensure_ascii=False))
        raise
