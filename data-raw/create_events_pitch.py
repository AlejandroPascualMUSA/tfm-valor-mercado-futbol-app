"""Create data/app/events_pitch.csv from eventos_final.json.
Run from the app root after placing eventos_final.json in data-raw or changing INPUT_JSON.
"""
import json
from pathlib import Path
import pandas as pd

APP_DIR = Path(__file__).resolve().parents[1]
INPUT_JSON = APP_DIR / "data-raw" / "eventos_final.json"
PLAYERS_CSV = APP_DIR / "data" / "app" / "players_master.csv"
OUTPUT_CSV = APP_DIR / "data" / "app" / "events_pitch.csv"

players = pd.read_csv(PLAYERS_CSV, usecols=["player_id"])
valid_ids = set(players["player_id"].astype(int))
rows = []
with INPUT_JSON.open("r", encoding="utf-8") as f:
    data = json.load(f)
for d in data:
    try:
        pid = int(d.get("player_id"))
    except Exception:
        continue
    if pid not in valid_ids:
        continue
    loc = d.get("location") or []
    if not isinstance(loc, list) or len(loc) < 2:
        continue
    x_raw, y_raw = float(loc[0]), float(loc[1])
    game_date = d.get("game_date") or ""
    rows.append({
        "player_id": pid,
        "player_name": d.get("player_name"),
        "team": d.get("team_name"),
        "event_name": d.get("event_name"),
        "x": max(0, min(120, x_raw * 1.2)),
        "y": max(0, min(80, y_raw * 0.8)),
        "sector": d.get("sector"),
        "posicion_evento": d.get("posicion_evento"),
        "sector_en_posicion": bool(d.get("sector_en_posicion")),
        "sector_en_vecino": bool(d.get("sector_en_vecino")),
        "game_date": game_date[:10],
        "match": f"{d.get('home_team','')} v {d.get('away_team','')}",
        "outcome": d.get("outcome")
    })
pd.DataFrame(rows).to_csv(OUTPUT_CSV, index=False)
