import os, re, json, math, shutil, unicodedata
from pathlib import Path
import numpy as np
import pandas as pd
from sklearn.decomposition import PCA

# Ejecutar desde la carpeta raíz de la app o adaptar source_dir.
ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "data-raw" / "Personalizado"
OUT = ROOT / "data" / "app"
OUT.mkdir(parents=True, exist_ok=True)

# Cuando se ejecuta dentro de este entorno, los datos fuente están fuera de la app.
if not SOURCE.exists():
    alt = Path('/mnt/data/shiny_build2/src/Personalizado')
    if alt.exists():
        SOURCE = alt


def clean_id(x):
    if pd.isna(x):
        return np.nan
    try:
        return int(str(x).strip().lstrip('0') or '0')
    except Exception:
        return np.nan


def snake(s):
    s = str(s)
    s = unicodedata.normalize('NFKD', s).encode('ascii', 'ignore').decode('ascii')
    s = s.replace('%', 'pct').replace('/', '_').replace('€', 'eur')
    s = re.sub(r'[^A-Za-z0-9]+', '_', s)
    return re.sub(r'_+', '_', s).strip('_').lower()


def read_xlsx(name, sheet=0):
    return pd.read_excel(SOURCE / name, sheet_name=sheet)


def read_csv(name):
    return pd.read_csv(SOURCE / name)


def write_csv(df, name):
    df.to_csv(OUT / name, index=False)
    print(f"{name}: {df.shape}")


def parse_date(x, dayfirst=False):
    return pd.to_datetime(x, errors='coerce', dayfirst=dayfirst)

# -----------------------------------------------------------------------------
# Fuentes pequeñas y outputs de modelo
# -----------------------------------------------------------------------------
modelo = read_csv('modelo_precio.csv')
modelo = modelo.drop(columns=[c for c in modelo.columns if c.startswith('Unnamed')], errors='ignore')
modelo['player_id'] = modelo['player_id'].apply(clean_id).astype('Int64')

pred_iso = read_csv('predicciones_cal').drop(columns=[c for c in ['Unnamed: 0'] if c in read_csv('predicciones_cal').columns], errors='ignore')
pred_raw = read_csv('predicciones_cal_original').drop(columns=[c for c in ['Unnamed: 0'] if c in read_csv('predicciones_cal_original').columns], errors='ignore')
for df in (pred_iso, pred_raw):
    df['player_id'] = df['player_id'].apply(clean_id).astype('Int64')

p_hist = read_csv('p_lesionContext.csv')
p_hist['player_id'] = p_hist['player_id'].apply(clean_id).astype('Int64')
p_load = read_csv('p_lesion2.csv')
p_load['player_id'] = p_load['player_id'].apply(clean_id).astype('Int64')
pen_load = read_csv('penalizador_lesion.csv')
pen_load['player_id'] = pen_load['player_id'].apply(clean_id).astype('Int64')

roles = read_xlsx('roles5.xlsx')
roles['player_id'] = roles['player_id'].apply(clean_id).astype('Int64')
pct_events = read_xlsx('porcentaje_eventos.xlsx')
pct_events['player_id'] = pct_events['player_id'].apply(clean_id).astype('Int64')
percentiles = read_xlsx('percentiles.xlsx')
percentiles['player_id'] = percentiles['player_id'].apply(clean_id).astype('Int64')

skill_season = read_xlsx('Skillcorner_2324T.xlsx')
skill_season['player_id'] = skill_season['player_id'].apply(clean_id).astype('Int64')
skill_match = read_xlsx('Skillcorner_23_24.xlsx')
skill_match['player_id'] = skill_match['player_id'].apply(clean_id).astype('Int64')

wys = read_xlsx('Wyscout_Jugadores.xlsx')
wys['player_id'] = wys['player_id'].apply(clean_id).astype('Int64')
opta = read_xlsx('Opta_2324.xlsx')
opta['player_id'] = opta['player_id'].apply(clean_id).astype('Int64')
team_map = read_xlsx('grupos_equipos_laliga_23_24.xlsx', 'Mapeo_equipos')

# -----------------------------------------------------------------------------
# Predicciones, métricas y SHAP
# -----------------------------------------------------------------------------
market_pred = pred_iso.rename(columns={
    'Jugador': 'player_name',
    'valor_real_eur': 'market_value_eur',
    'valor_real_mill': 'market_value_mill',
    'valor_estimado_eur': 'pred_iso_eur',
    'valor_estimado_mill': 'pred_iso_mill',
    'diferencia_eur': 'diff_iso_eur',
    'diferencia_mill': 'diff_iso_mill',
    'ratio': 'ratio_iso'
})
raw2 = pred_raw.rename(columns={
    'valor_estimado_eur': 'pred_raw_eur',
    'valor_estimado_mill': 'pred_raw_mill',
    'diferencia_eur': 'diff_raw_eur',
    'diferencia_mill': 'diff_raw_mill',
    'ratio': 'ratio_raw'
})[['player_id', 'pred_raw_eur', 'pred_raw_mill', 'diff_raw_eur', 'diff_raw_mill', 'ratio_raw']]
market_pred = market_pred.merge(raw2, on='player_id', how='left')
market_pred['abs_error_iso_mill'] = market_pred['diff_iso_mill'].abs()
market_pred['abs_pct_error_iso'] = np.where(market_pred['market_value_mill'] > 0, market_pred['abs_error_iso_mill'] / market_pred['market_value_mill'], np.nan)
market_pred['market_signal'] = np.where(market_pred['diff_iso_mill'] > 0, 'Potencialmente infravalorado',
                                np.where(market_pred['diff_iso_mill'] < 0, 'Potencialmente sobrevalorado', 'Ajustado'))
write_csv(market_pred, 'market_predictions.csv')

metrics_cal = read_csv('metricas_calibracion_todas.csv').drop(columns=[c for c in ['Unnamed: 0'] if c in read_csv('metricas_calibracion_todas.csv').columns], errors='ignore')
metrics_final = read_csv('comparacion_final_3_modelos.csv').drop(columns=[c for c in ['Unnamed: 0'] if c in read_csv('comparacion_final_3_modelos.csv').columns], errors='ignore')
write_csv(metrics_cal, 'metricas_calibracion_todas.csv')
write_csv(metrics_final, 'comparacion_final_3_modelos.csv')

shap = read_csv('shap_importance.csv').drop(columns=[c for c in ['Unnamed: 0'] if c in read_csv('shap_importance.csv').columns], errors='ignore')

def shap_group(feature):
    f = str(feature).lower()
    if any(x in f for x in ['edad', 'altura', 'peso', 'pie']): return 'Fisiologia'
    if any(x in f for x in ['contrato', 'grupo_equipo', 'puntos_en_1a', 'rank', 'grupo_pais', 'nacionalidad']): return 'Contexto'
    if any(x in f for x in ['minutos', 'variacion']): return 'Participacion'
    if any(x in f for x in ['riesgo', 'lesion', 'lesionado', 'incidencia']): return 'Riesgo lesion'
    if any(x in f for x in ['rol', 'posicion', 'linea', 'polivalencia', 'versatilidad']): return 'Rol y posicion'
    if any(x in f for x in ['def', 'duel', 'tackle', 'clean', 'discipline']): return 'Defensa'
    if any(x in f for x in ['xg', 'gol', 'shot', 'remate', 'box', 'finishing', 'offensive']): return 'Ataque'
    if any(x in f for x in ['pass', 'progress', 'vertical', 'deep', 'long']): return 'Pase y progresion'
    if any(x in f for x in ['dribble', 'foul', 'involvement', 'centrality', 'creativ', 'assist', 'chance', 'cross']): return 'Creacion y participacion'
    return 'Otros'

shap['group'] = shap['feature'].apply(shap_group)
shap = shap.sort_values('mean_abs_shap', ascending=False)
write_csv(shap, 'shap_importance.csv')

# -----------------------------------------------------------------------------
# Players master, usando modelo_precio como tabla central final
# -----------------------------------------------------------------------------
players = modelo.copy()
rename = {
    'Jugador': 'player_name',
    'Equipo durante el período seleccionado': 'team',
    'valor_mercado_eur': 'market_value_eur',
    'contrato': 'contract_years',
    'edad': 'age',
    'grupo_pais': 'country_group',
    'grupo_equipo': 'team_group_model',
    'puntos_en_1a': 'team_points_1a',
    'minutos_2324': 'minutes_2324',
    'variacion_min': 'minutes_variation',
    'exceso_riesgo_lesion_exposicion': 'risk_load_excess',
    'exceso_riesgo_hist_pos': 'risk_hist_excess',
    'posicion_principal': 'primary_position',
    'pct_posicion_principal': 'primary_position_share',
    'posicion_secundaria': 'secondary_position',
    'pct_posicion_secundaria': 'secondary_position_share',
    'rol_principal': 'primary_role',
    'rol_secundario': 'secondary_role',
    'posicion_rol_secundario': 'secondary_role_position',
    'Pie': 'foot',
    'Altura': 'height',
    'Peso': 'weight'
}
players = players.rename(columns=rename)
# El modelo de precio guarda los roles como códigos tipo WF_3; para la app añadimos nombres tácticos.
role_name_map = {
    ('CB',1): 'Stopper', ('CB',2): 'Ball Playing Defender', ('CB',3): 'Sweeper',
    ('FB',1): 'Wing Back', ('FB',2): 'Inverted Wing Back', ('FB',3): 'Full Back',
    ('DMF',1): 'Deep Lying Playmaker', ('DMF',2): 'Ball Winning Midfielder',
    ('CMF',1): 'Playmaker', ('CMF',2): 'Holding Midfielder', ('CMF',3): 'Box-to-box Midfielder',
    ('AMF',1): 'Advanced Playmaker', ('AMF',2): 'Second Striker',
    ('WF',1): 'Inside Forward', ('WF',2): 'Wide Playmaker', ('WF',3): 'Winger',
    ('CF',1): 'Poacher', ('CF',2): 'Mobile Striker', ('CF',3): 'Target Man'
}
def map_role(pos, code):
    if pd.isna(pos) or pd.isna(code):
        return np.nan
    m = re.search(r'_(\d+)$', str(code))
    if not m:
        return str(code)
    return role_name_map.get((str(pos), int(m.group(1))), str(code))
players['primary_role_code'] = players.get('primary_role')
players['secondary_role_code'] = players.get('secondary_role')
players['primary_role'] = [map_role(p, c) for p, c in zip(players.get('primary_position'), players.get('primary_role_code'))]
players['secondary_role'] = [map_role(p, c) for p, c in zip(players.get('secondary_position'), players.get('secondary_role_code'))]
players['market_value_mill'] = players['market_value_eur'] / 1e6

# Datos físicos agregados de temporada
skill_cols = {
    'Minutes': 'skill_minutes',
    'Distance P90': 'distance_p90',
    'M/min P90': 'm_per_min_p90',
    'Running Distance P90': 'running_distance_p90',
    'HSR Distance P90': 'hsr_distance_p90',
    'Sprint Distance P90': 'sprint_distance_p90',
    'HI Distance P90': 'hi_distance_p90',
    'High Acceleration Count P90': 'high_acc_count_p90',
    'High Deceleration Count P90': 'high_dec_count_p90',
    'Explosive Acceleration to Sprint Count P90': 'explosive_acc_to_sprint_p90',
    'PSV-99': 'psv99'
}
sk = skill_season[['player_id'] + [c for c in skill_cols if c in skill_season.columns]].rename(columns=skill_cols)
players = players.merge(sk, on='player_id', how='left')

# Opta/Wyscout complementario para nacionalidad, birthday, position original
opta_cols = [c for c in ['player_id','team_name','birthday','nationality'] if c in opta.columns]
players = players.merge(opta[opta_cols].drop_duplicates('player_id'), on='player_id', how='left')

# Riesgo histórico y carga con detalle
hist_cols = ['player_id','lesionado','incidencia','duracion_mediana','critical_injury','gravedad','recurrence','reincidencia','muscle_share','Edad','diversity','dias_desde_ultima_lesion','dias_baja_acumulados','sd_dias_baja','media_gravedad','split','prob_lesion_lasso','percentil_riesgo_lasso','grupo_riesgo_lasso','pred_lesion_lasso','p_hist','p_basal_hist','exceso_riesgo_hist','exceso_riesgo_hist_pos']
hist_ren = p_hist[[c for c in hist_cols if c in p_hist.columns]].rename(columns={'split':'split_hist','Edad':'age_hist'})
players = players.merge(hist_ren, on='player_id', how='left', suffixes=('','_hist_src'))
load_cols = ['player_id','split_jugador','n_ventanas_modelo','fecha_ultima_ventana','lesion_observada_jugador','suma_prob_ventanas','lesiones_esperadas','prob_lesion_temporada','prob_lesion_media','prob_lesion_ultima','prob_lesion_reciente','prob_lesion_p90','prob_lesion_max','n_episodios_observados','prob_lesion_temporada_basal_exposicion','exceso_riesgo_lesion_exposicion','penalizacion_lesion_pct']
load_ren = p_load[[c for c in load_cols if c in p_load.columns]].rename(columns={'exceso_riesgo_lesion_exposicion':'risk_load_excess_src','penalizacion_lesion_pct':'penalizacion_lesion_pct_load'})
players = players.merge(load_ren, on='player_id', how='left')
players['risk_hist_excess'] = players['risk_hist_excess'].combine_first(players.get('exceso_riesgo_hist_pos'))
players['risk_load_excess'] = players['risk_load_excess'].combine_first(players.get('risk_load_excess_src'))

# Predicciones
pred_cols = ['player_id','split','pred_iso_mill','pred_raw_mill','diff_iso_mill','diff_raw_mill','ratio_iso','ratio_raw','abs_error_iso_mill','abs_pct_error_iso','market_signal']
players = players.merge(market_pred[[c for c in pred_cols if c in market_pred.columns]], on='player_id', how='left')

# Equipo/contexto
if 'equipo' in team_map.columns:
    tm = team_map.rename(columns={'equipo':'team','grupo_modelo':'team_group_table','puntos':'team_points_2324','clasificacion_uefa':'uefa_classification'})
    keep = [c for c in ['team','team_group_table','posicion_final','team_points_2324','uefa_classification','puntos_en_1a'] if c in tm.columns]
    players = players.merge(tm[keep].drop_duplicates('team'), on='team', how='left')

# Variables auxiliares
players['injured_2324_hist'] = players['lesionado'].astype(str).str.upper().isin(['X1','1','TRUE','SI','SÍ'])
players['injured_2324_load'] = pd.to_numeric(players.get('lesion_observada_jugador'), errors='coerce').fillna(0).astype(int).astype(bool)
players['risk_hist_pct'] = 100 * pd.to_numeric(players['risk_hist_excess'], errors='coerce')
players['risk_load_pct'] = 100 * pd.to_numeric(players['risk_load_excess'], errors='coerce')
players['error_label'] = np.where(players['diff_iso_mill'] > 0, 'Infravalorado por mercado',
                           np.where(players['diff_iso_mill'] < 0, 'Sobrevalorado por mercado', 'Ajustado'))
players['value_band'] = pd.cut(players['market_value_mill'], [-np.inf,1,5,15,40,np.inf], labels=['0-1 M€','1-5 M€','5-15 M€','15-40 M€','>40 M€'], right=False).astype(str)
players = players.sort_values('market_value_mill', ascending=False)
write_csv(players, 'players_master.csv')

# -----------------------------------------------------------------------------
# Roles y percentiles
# -----------------------------------------------------------------------------
roles_app = roles.copy()
roles_app = roles_app.rename(columns={'team_name':'team','posicion_evento':'position','rol':'role','interpretacion':'interpretation'})
roles_app = roles_app.merge(pct_events.rename(columns={'posicion_evento':'position','count':'event_count','porcentaje':'event_share'})[['player_id','position','event_count','event_share']], on=['player_id','position'], how='left')
roles_app = roles_app.merge(players[['player_id','market_value_mill','team_group_model']].drop_duplicates('player_id'), on='player_id', how='left')
roles_app['role_position'] = roles_app['position'].astype(str) + ' - ' + roles_app['role'].astype(str)
write_csv(roles_app, 'roles_pca.csv')

# resumen de roles con prima vs posición
role_val = roles_app.dropna(subset=['role']).copy()
pos_med = role_val.groupby('position')['market_value_mill'].median().rename('position_median_mill')
role_summary = role_val.groupby(['position','cluster','role'], dropna=False).agg(
    n_players=('player_id','nunique'),
    median_market_mill=('market_value_mill','median'),
    mean_market_mill=('market_value_mill','mean'),
    mean_event_share=('event_share','mean'),
    mean_PC1=('PC1','mean'),
    mean_PC2=('PC2','mean')
).reset_index().merge(pos_med, on='position', how='left')
role_summary['premium_vs_position_mill'] = role_summary['median_market_mill'] - role_summary['position_median_mill']
write_csv(role_summary, 'role_summary.csv')

# percentiles de eventos
perc_app = percentiles.rename(columns={
    'team_name':'team','posicion_evento':'position','count.event':'count_event',
    'count.event.neighbor':'count_event_neighbor','percentil':'percentile_position','percentil_cluster':'percentile_role'
})
perc_app = perc_app.merge(players[['player_id','primary_position','primary_role','market_value_mill']].drop_duplicates('player_id'), on='player_id', how='left')
write_csv(perc_app, 'event_percentiles.csv')

# Cargas PCA aproximadas para heatmap interpretativo. No se usa para reproducir coordenadas finales.
features = ['Aerial','Ball recovery','Clearance','Defensive reading','Foul Won','Foul commit','Key pass','Long ball','Pass','Progressive pass','Shot','Tackle','Take on','Through ball','Turnover','Wide ball']
weights = pd.DataFrame({
    'CB':[2,2,2,2,0.5,2,0.5,1,1,1,0.5,2,0.5,0.5,2,0.5],
    'FB':[1,2,1,2,0.5,1,1,1,1,2,0.5,1,1,1,1,2],
    'DMF':[1,2,1,2,1,1,1,2,2,2,0.5,2,0.5,1,1,0.5],
    'CMF':[0.5,1,0.5,1,1,1,2,1,2,2,1,1,1,2,2,0.5],
    'AMF':[0.5,0.5,0.5,0.5,1,0.5,2,1,1,2,2,0.5,2,2,1,1],
    'WF':[0.5,0.5,0.5,0.5,2,0.5,2,0.5,1,1,2,0.5,2,1,1,2],
    'CF':[2,0.5,0.5,0.5,2,1,1,0.5,1,1,2,0.5,1,1,1,0.5]
}, index=features)
load_rows=[]
pivot = percentiles[percentiles['event_name'].isin(features)].pivot_table(index=['player_id','player_name','team_name','posicion_evento'], columns='event_name', values='puntos_total', aggfunc='sum', fill_value=0).reset_index()
for f in features:
    if f not in pivot.columns:
        pivot[f] = 0.0
for pos in sorted(roles_app['position'].dropna().unique()):
    sub = pivot[pivot['posicion_evento'] == pos]
    if sub.shape[0] < 4:
        continue
    X = np.log1p(np.maximum(sub[features].astype(float).values, 0))
    sd = np.nanstd(X, axis=0)
    mu = np.nanmean(X, axis=0)
    Xs = np.where(sd > 0, (X - mu) / sd, 0)
    w = np.sqrt(weights[pos].reindex(features).fillna(1).values) if pos in weights.columns else np.ones(len(features))
    Xw = Xs * w
    ncomp = min(4, Xw.shape[0], Xw.shape[1])
    pca = PCA(n_components=ncomp, random_state=12345)
    pca.fit(Xw)
    for i, f in enumerate(features):
        for j in range(ncomp):
            load_rows.append({'position':pos, 'event_name':f, 'PC':f'PC{j+1}', 'loading':pca.components_[j, i], 'explained_variance':pca.explained_variance_ratio_[j]})
write_csv(pd.DataFrame(load_rows), 'role_pca_loadings.csv')

# -----------------------------------------------------------------------------
# Carga física partido a partido
# -----------------------------------------------------------------------------
pm_cols = {
    'Player':'player_name', 'Team':'team', 'Match':'match', 'Date':'date', 'Position':'position', 'Minutes':'minutes',
    'Distance P90':'distance_p90', 'M/min P90':'m_per_min_p90', 'Running Distance P90':'running_distance_p90',
    'HSR Distance P90':'hsr_distance_p90', 'Sprint Distance P90':'sprint_distance_p90', 'HI Distance P90':'hi_distance_p90',
    'High Acceleration Count P90':'high_acc_count_p90', 'High Deceleration Count P90':'high_dec_count_p90',
    'Explosive Acceleration to Sprint Count P90':'explosive_acc_to_sprint_p90', 'PSV-99':'psv99'
}
physical_match = skill_match[['player_id'] + [c for c in pm_cols if c in skill_match.columns]].rename(columns=pm_cols)
physical_match['date'] = parse_date(physical_match['date'])
write_csv(physical_match, 'physical_match.csv')

# -----------------------------------------------------------------------------
# Lesiones y minutos longitudinales
# -----------------------------------------------------------------------------
with open(SOURCE / 'lesiones.json', encoding='utf-8') as f:
    injury_json = json.load(f)
with open(SOURCE / 'minutos.json', encoding='utf-8') as f:
    minutes_json = json.load(f)
with open(SOURCE / 'Transfermarkt_2324.json', encoding='utf-8') as f:
    tm_json = json.load(f)
inj=[]
mins=[]
for player, injuries, minutes in zip(tm_json, injury_json, minutes_json):
    pid = clean_id(player.get('player_id'))
    pname = player.get('player_name')
    team = player.get('squad')
    if isinstance(injuries, dict):
        injuries = [injuries]
    if injuries:
        for rec in injuries:
            if isinstance(rec, dict):
                row = {'player_id':pid, 'player_name':pname, 'team':team}
                row.update(rec)
                inj.append(row)
    if isinstance(minutes, dict):
        minutes = [minutes]
    if minutes:
        for rec in minutes:
            if isinstance(rec, dict):
                row = {'player_id':pid, 'player_name':pname, 'team':team}
                row.update(rec)
                mins.append(row)

inj = pd.DataFrame(inj)
if not inj.empty:
    inj['date_from'] = parse_date(inj.get('date_from'), dayfirst=True)
    inj['date_until'] = parse_date(inj.get('date_until'), dayfirst=True)
    def duration_days(x):
        m = re.search(r'(\d+)', str(x))
        return float(m.group(1)) if m else np.nan
    inj['duration_days'] = inj.get('duration', '').apply(duration_days)
    low = inj['injury'].astype(str).str.lower()
    inj['injury_group'] = np.select([
        low.str.contains('hamstring|muscle|calf|adductor|thigh|strain|fibr|tear|groin|abductor', regex=True),
        low.str.contains('ligament|meniscus|tendon|achilles|knee|ankle|pubalgia', regex=True),
        low.str.contains('virus|corona|flu|ill|infection|fever|cold|tonsillitis', regex=True),
        low.str.contains('fracture|knock|bruise|concussion|wound|shoulder|head|facial|rib|broken', regex=True),
    ], ['Muscular','Ligamentosa/tendinosa','Viral/enfermedad','Traumatica/otras'], default='Otro')
write_csv(inj, 'injuries_long.csv')

mins = pd.DataFrame(mins)
if not mins.empty:
    mins['minutes'] = pd.to_numeric(mins.get('minutes'), errors='coerce')
write_csv(mins, 'minutes_long.csv')

# Deciles de calibración para app
mp = market_pred.dropna(subset=['market_value_mill','pred_iso_mill']).copy()
try:
    mp['decile'] = pd.qcut(mp['market_value_mill'].rank(method='first'), 10, labels=[f'D{i}' for i in range(1,11)])
except Exception:
    mp['decile'] = 'Todos'
deciles = mp.groupby('decile', observed=False).agg(
    n=('player_id','nunique'),
    real_mean_mill=('market_value_mill','mean'),
    pred_iso_mean_mill=('pred_iso_mill','mean'),
    pred_raw_mean_mill=('pred_raw_mill','mean'),
    ratio_iso_mean=('ratio_iso','mean'),
    ratio_raw_mean=('ratio_raw','mean')
).reset_index()
write_csv(deciles, 'calibration_deciles.csv')

# Feature values in modelo_precio con nombres ya legibles para export avanzado
features_final = modelo.rename(columns=rename)
write_csv(features_final, 'model_features.csv')

# Copia bodymap si existe
bodymap = SOURCE / 'bodymap2.png'
if bodymap.exists():
    shutil.copy(bodymap, ROOT / 'www' / 'bodymap2.png')

print('Preparación terminada:', OUT)
