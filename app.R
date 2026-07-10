# App Shiny | TFM valoración de mercado, roles y riesgo lesional ----------------
# Datos: LaLiga 2023/24. Outputs ligeros guardados en data/app.

required_packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly", "DT",
  "readr", "scales", "stringr", "ggrepel", "tibble", "htmltools", "png", "colourpicker", "jsonlite"
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop(
    "Faltan paquetes: ", paste(missing_packages, collapse = ", "),
    "\nInstálalos con: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))"
  )
}

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(scales)
library(stringr)
library(ggrepel)
library(tibble)
library(htmltools)
library(colourpicker)
library(jsonlite)

source(file.path("R", "helpers.R"), local = TRUE)
source(file.path("R", "reporting.R"), local = TRUE)

# -----------------------------------------------------------------------------
# Carga de datos
# -----------------------------------------------------------------------------
app_data_dir <- file.path("data", "app")
read_app_csv <- function(file) {
  readr::read_csv(
    file.path(app_data_dir, file),
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
}

players <- read_app_csv("players_master.csv") |>
  mutate(
    player_id = as.integer(player_id),
    market_value_mill = as.numeric(market_value_mill),
    pred_iso_mill = as.numeric(pred_iso_mill),
    pred_raw_mill = as.numeric(pred_raw_mill),
    diff_iso_mill = as.numeric(diff_iso_mill),
    diff_raw_mill = as.numeric(diff_raw_mill),
    ratio_iso = as.numeric(ratio_iso),
    ratio_raw = as.numeric(ratio_raw),
    age = as.numeric(age),
    contract_years = as.numeric(contract_years),
    minutes_2324 = as.numeric(minutes_2324),
    risk_hist_excess = as.numeric(risk_hist_excess),
    risk_load_excess = as.numeric(risk_load_excess),
    risk_hist_pct = 100 * risk_hist_excess,
    risk_load_pct = 100 * risk_load_excess,
    injured_2324_hist_label = as_bool_label(injured_2324_hist),
    injured_2324_load_label = as_bool_label(injured_2324_load),
    value_band = factor(value_band, levels = value_band_levels),
    split = ifelse(is.na(split), "sin split", split)
  )

roles_pca <- read_app_csv("roles_pca.csv") |>
  mutate(player_id = as.integer(player_id), cluster = as.integer(cluster))

player_role_positions <- roles_pca |>
  group_by(player_id) |>
  summarise(
    all_positions = paste(sort(unique(na.omit(position))), collapse = " | "),
    all_roles = paste(sort(unique(na.omit(role))), collapse = " | "),
    all_position_roles = paste(sort(unique(na.omit(paste(position, role, sep = " - ")))), collapse = " | "),
    .groups = "drop"
  )
players <- players |>
  left_join(player_role_positions, by = "player_id") |>
  mutate(
    all_positions = ifelse(is.na(all_positions) | !nzchar(all_positions), primary_position, all_positions),
    all_roles = ifelse(is.na(all_roles) | !nzchar(all_roles), primary_role, all_roles),
    all_position_roles = ifelse(is.na(all_position_roles) | !nzchar(all_position_roles), paste(primary_position, primary_role, sep = " - "), all_position_roles)
  )
role_summary <- read_app_csv("role_summary.csv")
event_percentiles <- read_app_csv("event_percentiles.csv") |>
  mutate(player_id = as.integer(player_id))
role_loadings <- read_app_csv("role_pca_loadings.csv")
physical_match <- read_app_csv("physical_match.csv") |>
  mutate(player_id = as.integer(player_id), date = as.Date(date))

injuries_long <- read_app_csv("injuries_long.csv") |>
  mutate(player_id = as.integer(player_id), date_from = as.Date(date_from), date_until = as.Date(date_until))
injury_dates_by_player <- injuries_long |>
  filter(!is.na(date_from), is.na(duration_days) | duration_days >= 7) |>
  group_by(player_id) |>
  summarise(injury_dates = list(date_from), .groups = "drop")

physical_match <- physical_match |>
  left_join(injury_dates_by_player, by = "player_id") |>
  rowwise() |>
  mutate(
    injured_window = {
      dates <- injury_dates
      if (length(dates) == 0 || all(is.na(dates)) || is.na(date)) FALSE else any(dates >= date & dates <= date + 30, na.rm = TRUE)
    },
    injured_window_label = ifelse(injured_window, "Sí", "No")
  ) |>
  ungroup() |>
  select(-injury_dates)
p_lesion_context <- read_app_csv("p_lesionContext.csv") |>
  mutate(
    player_id = as.integer(player_id),
    lesionado = as.character(lesionado),
    incidencia = as.numeric(incidencia),
    dias_desde_ultima_lesion = as.numeric(dias_desde_ultima_lesion),
    prob_lesion_lasso = as.numeric(prob_lesion_lasso),
    exceso_riesgo_hist_pos = as.numeric(exceso_riesgo_hist_pos),
    estado_lesion = ifelse(lesionado %in% c("X1", "1", "TRUE", "true", "Sí", "Si"), "Lesionado", "No lesionado")
  ) |>
  left_join(players |> select(player_id, player_name, team), by = "player_id")
minutes_long <- read_app_csv("minutes_long.csv") |>
  mutate(player_id = as.integer(player_id), minutes = as.numeric(minutes))
market_predictions <- read_app_csv("market_predictions.csv") |>
  mutate(player_id = as.integer(player_id))
metrics_cal <- read_app_csv("metricas_calibracion_todas.csv")
metrics_final <- read_app_csv("comparacion_final_3_modelos.csv")
shap_importance <- read_app_csv("shap_importance.csv") |> 
  mutate(group = case_when(
    group == "Participacion" ~ "Potencial",
    TRUE ~ group
  ))
calibration_deciles <- read_app_csv("calibration_deciles.csv")
model_features <- read_app_csv("model_features.csv") |>
  mutate(player_id = as.integer(player_id))
events_pitch <- read_app_csv("events_pitch.csv") |>
  mutate(
    player_id = as.integer(player_id),
    game_date = as.Date(game_date),
    x = as.numeric(x),
    y = as.numeric(y),
    sector_en_posicion = tolower(as.character(sector_en_posicion)) %in% c("true", "1", "sí", "si"),
    sector_en_vecino = tolower(as.character(sector_en_vecino)) %in% c("true", "1", "sí", "si")
  )
physical_block_importance <- read_app_csv("physical_block_importance.csv") |>
  mutate(gain = as.numeric(gain), pct = as.numeric(pct))
physical_gain_categories <- read_app_csv("physical_gain_categories.csv") |>
  mutate(Gain = as.numeric(Gain), group = as.character(group), Feature = as.character(Feature)) |>
  group_by(group) |>
  mutate(pct_within_group = Gain / sum(Gain, na.rm = TRUE)) |>
  ungroup()
shap_dependence <- read_app_csv("shap_dependence_long.csv") |>
  mutate(
    player_id = as.integer(player_id),
    feature = as.character(feature),
    feature_label = as.character(feature_label),
    feature_value = as.numeric(feature_value),
    feature_value_scaled = as.numeric(feature_value_scaled),
    shap_value = as.numeric(shap_value),
    mean_abs_shap_proxy = as.numeric(mean_abs_shap_proxy)
  )

# -----------------------------------------------------------------------------
# Opciones
# -----------------------------------------------------------------------------
position_order <- c("CB", "FB", "DMF", "CMF", "AMF", "WF", "CF")
physical_position_order <- c("CB", "FB", "DMF", "CMF", "AMF", "Forward")
position_choices <- position_order[position_order %in% unique(na.omit(as.character(players$primary_position)))]
if (length(position_choices) == 0) position_choices <- sort(unique(na.omit(as.character(players$primary_position))))
team_choices <- sort(unique(na.omit(players$team)))
team_group_choices <- sort(unique(na.omit(players$team_group_model)))
role_catalog <- tibble::tribble(
  ~position, ~role,
  "CB", "Stopper",
  "CB", "Ball Playing Defender",
  "CB", "Sweeper",
  "FB", "Wing Back",
  "FB", "Inverted Wing Back",
  "FB", "Full Back",
  "DMF", "Deep Lying Playmaker",
  "DMF", "Ball Winning Midfielder",
  "CMF", "Playmaker",
  "CMF", "Holding Midfielder",
  "CMF", "Box-to-box Midfielder",
  "AMF", "Advanced Playmaker",
  "AMF", "Second Striker",
  "WF", "Inside Forward",
  "WF", "Wide Playmaker",
  "WF", "Winger",
  "CF", "Poacher",
  "CF", "Mobile Striker",
  "CF", "Target Man"
) |>
  semi_join(roles_pca |> distinct(position, role), by = c("position", "role"))
roles_for_positions <- function(pos) {
  pos <- pos %||% character()
  if (length(pos) == 0 || any(pos == "Todas")) {
    return(unique(role_catalog$role))
  }
  role_catalog |>
    filter(position %in% pos) |>
    pull(role) |>
    unique()
}
role_choices <- roles_for_positions(position_choices)
split_choices <- c("Todas", sort(unique(na.omit(players$split))))
APP_BLUE <- "steelblue"
APP_RED <- "#de2d26"
APP_GREEN <- "#31a354"

text_clean <- function(x) {
  x <- as.character(x %||% "")
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  tolower(x)
}

safe_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

role_axis_interpretation <- tibble::tribble(
  ~position, ~pc1_negative, ~pc1_positive, ~pc2_negative, ~pc2_positive,
  "CB", "Defender", "Playmaker", "Agresivo", "Controlador",
  "FB", "Ofensivo", "Defensivo", "Interior", "Exterior",
  "DMF", "Destructor", "Organizador", "Posicional", "Presionante",
  "CMF", "Creador", "Llegador", "Pausado", "Vertical",
  "AMF", "Playmaker", "Llegador", "Finalizador", "Asistente",
  "WF", "De banda", "Interior", "Asociativo", "Desbordador",
  "CF", "Finalizador", "Asociativo", "Físico", "Técnico"
)

normaliza_temporada <- function(season) {
  season <- as.character(season)
  dplyr::case_when(
    season == "2021" ~ "20/21",
    season == "2022" ~ "21/22",
    season == "2023" ~ "22/23",
    season == "2024" ~ "23/24",
    TRUE ~ season
  )
}

injury_to_zone <- function(injury) {
  injury <- stringr::str_to_lower(as.character(injury %||% ""))
  dplyr::case_when(
    stringr::str_detect(injury, "head|facial|eyebrow|nose|eye|jaw|concussion|dental|cheekbone|toothache") ~ "head",
    stringr::str_detect(injury, "shoulder|acromioclavicular|collarbone") ~ "shoulder",
    stringr::str_detect(injury, "arm|elbow|hand|wrist|metacarpal|thumb|finger") ~ "shoulder",
    stringr::str_detect(injury, "chest|ribs") ~ "chest",
    stringr::str_detect(injury, "back|lumbago|lumbar|sciatica|vertebral") ~ "back",
    stringr::str_detect(injury, "abdominal") ~ "abdomen",
    stringr::str_detect(injury, "pelvic|hip|groin|inguinal|pubic|pubalgia|testicle") ~ "groin",
    stringr::str_detect(injury, "thigh|leg|adductor") ~ "thigh",
    stringr::str_detect(injury, "hamstring") ~ "thigh",
    stringr::str_detect(injury, "knee|meniscus|patellar|cruciate") ~ "knee",
    stringr::str_detect(injury, "tibia|shin|peroneus|fibula") ~ "calf",
    stringr::str_detect(injury, "calf|achilles") ~ "calf",
    stringr::str_detect(injury, "ankle") ~ "ankle",
    stringr::str_detect(injury, "foot|toe|arch|plantar|metatarsal|heel") ~ "foot",
    TRUE ~ "other"
  )
}

body_polygons <- dplyr::bind_rows(
  tibble(zone="head", x=c(280, 295, 320, 350, 375, 380, 330, 310, 275), y=c(120, 20, 15, 20, 120, 170, 280, 280, 170), group="head_front"),
  tibble(zone="shoulder", x=c(355, 400, 440, 480, 420, 390, 255, 220, 180, 200, 270, 295, 325), y=c(250, 340, 340, 430, 500, 400, 400, 490, 430, 350, 330, 250, 290), group="shoulder_front"),
  tibble(zone="chest", x=c(390, 420, 420, 410, 325, 245, 230, 220, 260), y=c(400, 470, 520, 835, 670, 835, 520, 480, 400), group="chest_front"),
  tibble(zone="abdomen", x=c(410, 325, 245, 290, 370), y=c(835, 670, 840, 940, 940), group="abdomen_front"),
  tibble(zone="groin", x=c(410, 370, 290, 230, 225, 420), y=c(835, 940, 940, 840, 1050, 1050), group="groin_front"),
  tibble(zone="thigh", x=c(225, 225, 255, 280, 320, 330, 340, 360, 400, 420, 420), y=c(1050, 1200, 1350, 1370, 1355, 1150, 1370, 1360, 1350, 1220, 1050), group="thigh_front"),
  tibble(zone="knee", x=c(250, 250, 280, 320, 320, 280), y=c(1340, 1520, 1470, 1520, 1355, 1370), group="knee_front_left"),
  tibble(zone="knee", x=c(340, 340, 360, 400, 400, 370), y=c(1370, 1490, 1470, 1490, 1345, 1360), group="knee_front_right"),
  tibble(zone="calf", x=c(250, 265, 300, 320, 320, 310, 280), y=c(1500, 1830, 1830, 1600, 1650, 1500, 1480), group="calf_front_left"),
  tibble(zone="calf", x=c(340, 330, 340, 340, 380, 400, 370), y=c(1490, 1600, 1650, 1830, 1830, 1490, 1470), group="calf_front_right"),
  tibble(zone="ankle", x=c(265, 300, 310, 265), y=c(1830, 1830, 1880, 1880), group="ankle_front_left"),
  tibble(zone="ankle", x=c(345, 380, 380, 340), y=c(1830, 1830, 1880, 1890), group="ankle_front_right"),
  tibble(zone="foot", x=c(310, 310, 270, 220, 240, 260), y=c(1880, 1940, 2030, 2030, 1940, 1880), group="foot_front_left"),
  tibble(zone="foot", x=c(380, 405, 440, 375, 330, 340), y=c(1880, 1925, 2030, 2030, 1940, 1880), group="foot_front_right"),
  tibble(zone="head", x=c(1010, 1020, 1050, 1080, 1100, 1110, 1070, 1010, 1000), y=c(120, 20, 15, 20, 110, 180, 250, 210, 180), group="head_back"),
  tibble(zone="back", x=c(1020, 1070, 1090, 1100, 1180, 1200, 1150, 1130, 1100, 1050, 1020, 980, 960, 970, 910, 940, 1000), y=c(230, 240, 230, 320, 360, 490, 550, 850, 840, 920, 840, 850, 560, 550, 510, 350, 320), group="back"),
  tibble(zone="groin", x=c(1050, 1020, 980, 960, 1010, 1050, 1100, 1150, 1130, 1100), y=c(920, 840, 850, 1050, 1100, 1060, 1080, 1010, 850, 850), group="groin_back"),
  tibble(zone="thigh", x=c(1010, 960, 970, 980, 1050, 1060, 1070, 1125, 1155, 1100, 1050), y=c(1100, 1060, 1300, 1500, 1500, 1150, 1500, 1490, 1020, 1080, 1050), group="thigh_back"),
  tibble(zone="calf", x=c(1050, 980, 975, 980, 1000, 1040, 1050, 1055, 1050), y=c(1500, 1500, 1550, 1620, 1890, 1890, 1700, 1670, 1500), group="calf_back"),
  tibble(zone="calf", x=c(1125, 1070, 1070, 1060, 1070, 1070, 1110, 1135), y=c(1490, 1500, 1550, 1580, 1610, 1880, 1870, 1570), group="calf_back_right"),
  tibble(zone="ankle", x=c(1040, 1000, 990, 1045), y=c(1890, 1890, 1950, 1965), group="ankle_back"),
  tibble(zone="ankle", x=c(1070, 1060, 1110, 1110), y=c(1880, 1965, 1920, 1880), group="ankle_back_right"),
  tibble(zone="foot", x=c(1000, 990, 950, 955, 1050, 1050), y=c(1950, 1920, 1910, 1980, 2040, 1950), group="foot_back"),
  tibble(zone="foot", x=c(1060, 1070, 1170, 1170, 1110), y=c(1965, 2030, 1990, 1950, 1920), group="foot_back_right")
)

zone_labels_es <- c(
  head = "Cabeza", shoulder = "Hombro / brazo", chest = "Pecho",
  back = "Espalda", abdomen = "Abdomen", groin = "Cadera / ingle", thigh = "Muslo / isquios / aductor",
  knee = "Rodilla", calf = "Tibia / gemelo / Aquiles", ankle = "Tobillo", foot = "Pie", other = "Otra"
)
body_zone_choices <- setNames(sort(unique(body_polygons$zone)), zone_labels_es[sort(unique(body_polygons$zone))])

polygon_area <- function(x, y) {
  if (length(x) < 3 || length(y) < 3) return(0)
  abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1])) / 2)
}

body_img <- tryCatch(png::readPNG(file.path("www", "bodymap2.png")), error = function(e) NULL)
body_img_w <- if (!is.null(body_img) && length(dim(body_img)) >= 2) dim(body_img)[2] else 1365
body_img_h <- if (!is.null(body_img) && length(dim(body_img)) >= 2) dim(body_img)[1] else 2048

plot_injury_fillmap_by_player_app <- function(
    data,
    player_name = "Jugador seleccionado",
    value = c("count", "duration", "games_missed"),
    mode = c("percentage", "absolute"),
    zones_keep = NULL,
    seasons_keep = NULL,
    min_duration_days = NULL,
    show_labels = TRUE
) {
  value <- match.arg(value)
  mode <- match.arg(mode)
  player_data <- data |> mutate(season_injured = normaliza_temporada(season_injured))

  if (!is.null(seasons_keep) && length(seasons_keep) > 0) {
    player_data <- player_data |> filter(season_injured %in% normaliza_temporada(seasons_keep))
  }
  if (!is.null(min_duration_days) && is.finite(min_duration_days) && min_duration_days > 0) {
    player_data <- player_data |> filter(!is.na(duration_days), duration_days >= min_duration_days)
  }
  if (nrow(player_data) > 0) {
    player_data <- player_data |>
      mutate(zone = injury_to_zone(injury)) |>
      filter(zone %in% unique(body_polygons$zone))
  }
  if (!is.null(zones_keep) && length(zones_keep) > 0 && nrow(player_data) > 0) {
    player_data <- player_data |> filter(zone %in% zones_keep)
  }

  metric_label <- dplyr::case_when(
    value == "count" ~ "Lesiones",
    value == "duration" ~ "Días baja",
    value == "games_missed" ~ "Partidos perdidos",
    TRUE ~ "Valor"
  )

  if (nrow(player_data) > 0) {
    if (value == "count") {
      player_data <- player_data |> mutate(metric_value = 1)
    } else if (value == "duration") {
      player_data <- player_data |> mutate(metric_value = suppressWarnings(as.numeric(duration_days)))
    } else {
      player_data <- player_data |> mutate(metric_value = suppressWarnings(as.numeric(games_missed)))
    }
    summary_df <- player_data |>
      group_by(zone) |>
      summarise(metric = sum(metric_value, na.rm = TRUE), n_lesiones = n(), .groups = "drop")
    total_metric <- sum(summary_df$metric, na.rm = TRUE)
    summary_df <- summary_df |>
      mutate(pct = ifelse(is.finite(total_metric) && total_metric > 0, 100 * metric / total_metric, 0))
  } else {
    summary_df <- tibble(zone = character(), metric = numeric(), n_lesiones = integer(), pct = numeric())
  }

  # Las coordenadas originales del TFM están en una imagen 1365 x 2048. La app usa
  # la misma figura reescalada, por eso se transforman los polígonos al tamaño real
  # del PNG antes de pintarlos.
  poly_df <- body_polygons |>
    left_join(summary_df, by = "zone") |>
    mutate(
      metric = tidyr::replace_na(metric, 0),
      n_lesiones = tidyr::replace_na(n_lesiones, 0),
      pct = tidyr::replace_na(pct, 0),
      x_plot = x / 1365 * body_img_w,
      y_plot = (2048 - y) / 2048 * body_img_h
    )

  fill_var <- ifelse(mode == "percentage", "pct", "metric")
  fill_title <- dplyr::case_when(
    mode == "percentage" & value == "count" ~ "% lesiones",
    mode == "percentage" & value == "duration" ~ "% días baja",
    mode == "percentage" & value == "games_missed" ~ "% partidos perdidos",
    TRUE ~ metric_label
  )

  label_df <- poly_df |>
    group_by(zone, group) |>
    summarise(
      x = mean(x_plot, na.rm = TRUE),
      y = mean(y_plot, na.rm = TRUE),
      metric = first(metric),
      pct = first(pct),
      n_lesiones = first(n_lesiones),
      area = polygon_area(x_plot, y_plot),
      .groups = "drop"
    ) |>
    filter(metric > 0) |>
    group_by(zone) |>
    slice_max(order_by = area, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(label = ifelse(mode == "percentage", paste0(round(pct, 1), "%"), as.character(round(metric, ifelse(value == "count", 0, 1)))))

  p <- ggplot()
  if (!is.null(body_img)) {
    p <- p + annotation_custom(
      grid::rasterGrob(body_img, width = grid::unit(1, "npc"), height = grid::unit(1, "npc")),
      xmin = 0, xmax = body_img_w, ymin = 0, ymax = body_img_h
    )
  }
  p <- p +
    geom_polygon(
      data = poly_df,
      aes(x = x_plot, y = y_plot, group = group, fill = .data[[fill_var]]),
      alpha = 0.68,
      color = NA
    ) +
    scale_fill_gradient(
      low = "#f7f7f7",
      high = "#8b0000",
      name = fill_title,
      labels = if (mode == "percentage") function(x) paste0(round(x, 1), "%") else waiver()
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barheight = grid::unit(45, "pt"),
        barwidth = grid::unit(8, "pt")
      )
    ) +
    coord_fixed(xlim = c(0, body_img_w), ylim = c(0, body_img_h), expand = FALSE) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      plot.margin = ggplot2::margin(t = 4, r = 12, b = 4, l = 4)
    ) +
    labs(title = NULL, subtitle = NULL)

  if (show_labels && nrow(label_df) > 0) {
    p <- p +
      geom_text(data = label_df, aes(x = x, y = y, label = label), color = "white", size = 4.1, fontface = "bold", lineheight = 0.9) +
      geom_text(data = label_df, aes(x = x, y = y, label = label), color = "black", size = 3.1, fontface = "bold", lineheight = 0.9)
  }
  if (nrow(player_data) == 0) {
    p <- p + annotate("text", x = body_img_w / 2, y = body_img_h / 2, label = "Sin lesiones registradas", color = APP_BLUE, fontface = "bold", size = 5)
  }
  p
}

# Eventos base utilizados para construir los roles en el TFM.
# Coinciden con la tabla de variables de eventos: 16 dimensiones funcionales.
role_event_vars_all <- c(
  "Pass", "Long ball", "Wide ball", "Through ball",
  "Progressive pass", "Key pass", "Take on",
  "Foul commit", "Foul Won", "Tackle",
  "Defensive reading", "Clearance", "Shot",
  "Aerial", "Ball recovery", "Turnover"
)

radar_event_groups <- list(
  "Penetración" = c("Take on", "Success take on", "Through ball"),
  "Progresión" = c("Progressive pass", "Long ball", "Wide ball"),
  "Participación" = c("Pass", "Ball recovery", "Foul Won"),
  "Verticalidad" = c("Progressive pass", "Through ball", "Shot"),
  "Creatividad" = c("Key pass", "Through ball", "Wide ball"),
  "Eficiencia de pase" = c("Pass", "Long ball"),
  "Calidad de tiro" = c("Shot"),
  "Conversión" = c("Shot", "Foul Won"),
  "Impacto ofensivo" = c("Shot", "Key pass", "Foul Won"),
  "Dominio aéreo" = c("Aerial", "Aerial won"),
  "Actividad defensiva" = c("Tackle", "Foul commit", "Defensive reading", "Ball recovery"),
  "Éxito defensivo" = c("Tackle won", "Defensive reading", "Clearance")
)
radar_group_levels <- names(radar_event_groups)

injury_zone_coordinates <- tibble::tribble(
  ~body_zone, ~x, ~y,
  "Cabeza / cara", 0.50, 0.90,
  "Cuello", 0.50, 0.79,
  "Hombro", 0.32, 0.73,
  "Brazo / mano", 0.22, 0.56,
  "Espalda", 0.50, 0.62,
  "Torso / abdomen", 0.50, 0.56,
  "Cadera / pubis / aductor", 0.50, 0.43,
  "Muslo / isquios / cuádriceps", 0.40, 0.31,
  "Rodilla", 0.40, 0.22,
  "Gemelo / sóleo / Aquiles", 0.60, 0.15,
  "Tobillo", 0.40, 0.08,
  "Pie", 0.60, 0.04,
  "Enfermedad / otro", 0.82, 0.78
)

classify_injury_zone <- function(injury, injury_group = "") {
  txt <- text_clean(paste(injury, injury_group))
  dplyr::case_when(
    stringr::str_detect(txt, "head|face|nose|eye|jaw|concussion|skull|cabeza|cara|nariz|ojo|mandib") ~ "Cabeza / cara",
    stringr::str_detect(txt, "neck|cervic|cuello") ~ "Cuello",
    stringr::str_detect(txt, "shoulder|clavicle|hombro|clavicula") ~ "Hombro",
    stringr::str_detect(txt, "arm|elbow|hand|wrist|finger|brazo|codo|mano|muneca|dedo") ~ "Brazo / mano",
    stringr::str_detect(txt, "back|lumbar|spine|espalda|lumb") ~ "Espalda",
    stringr::str_detect(txt, "abdom|rib|chest|torso|costilla|pecho") ~ "Torso / abdomen",
    stringr::str_detect(txt, "hip|groin|adductor|pubis|pelvis|cadera|ingle|aductor") ~ "Cadera / pubis / aductor",
    stringr::str_detect(txt, "hamstring|thigh|quad|quadriceps|femoral|isquio|muslo|cuadriceps|muscular") ~ "Muslo / isquios / cuádriceps",
    stringr::str_detect(txt, "knee|menisc|cruciate|patellar|rodilla|ligament|ligamento") ~ "Rodilla",
    stringr::str_detect(txt, "calf|soleus|achilles|gemelo|soleo|aquiles") ~ "Gemelo / sóleo / Aquiles",
    stringr::str_detect(txt, "ankle|tobillo") ~ "Tobillo",
    stringr::str_detect(txt, "foot|toe|metatars|heel|pie|dedo del pie|talon") ~ "Pie",
    stringr::str_detect(txt, "virus|viral|corona|covid|illness|infection|fever|flu|enfermedad|infecc") ~ "Enfermedad / otro",
    TRUE ~ "Enfermedad / otro"
  )
}

# Sectorización 120 x 80, misma lógica que Roles_v4 del TFM.
field_length <- 120
field_width <- 80
n_cols <- 4
n_rows <- 5
col_width <- field_length / n_cols
row_height <- field_width / n_rows
pitch_sectors <- expand.grid(col = 1:n_cols, row = 1:n_rows) |>
  arrange(col, row) |>
  mutate(
    xmin = (col - 1) * col_width,
    xmax = col * col_width,
    ymin = (row - 1) * row_height,
    ymax = row * row_height,
    cell = paste0("C", row_number()),
    xcenter = (xmin + xmax) / 2,
    ycenter = (ymin + ymax) / 2
  )

country_choices <- sort(unique(na.omit(players$country_group)))
event_choices <- role_event_vars_all[role_event_vars_all %in% unique(na.omit(events_pitch$event_name))]
if (length(event_choices) == 0) event_choices <- sort(unique(na.omit(events_pitch$event_name)))
event_choices <- stats::setNames(event_choices, event_choices)
role_event_vars <- role_event_vars_all[role_event_vars_all %in% unique(na.omit(event_percentiles$event_name))]
if (length(role_event_vars) == 0) role_event_vars <- sort(unique(na.omit(event_percentiles$event_name)))
shap_feature_tbl <- shap_dependence |>
  group_by(feature, feature_label) |>
  summarise(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE), .groups = "drop") |>
  arrange(desc(mean_abs_shap))
shap_feature_choices <- setNames(shap_feature_tbl$feature, shap_feature_tbl$feature_label)
shap_color_choices <- c("Sin color" = "", shap_feature_choices)

pretty_feature_label <- function(x) {
  x |>
    stringr::str_replace_all("_posicion_principal", " pos. principal") |>
    stringr::str_replace_all("_rol_principal", " rol principal") |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_replace_all("90", "/90") |>
    stringr::str_to_sentence()
}

stat_input_id <- function(prefix, feature) {
  paste0(prefix, "_", gsub("[^A-Za-z0-9_]", "_", feature))
}

stat_step <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  rng <- range(x, na.rm = TRUE)
  d <- diff(rng)
  if (!is.finite(d) || d <= 0) return(0.01)
  if (d <= 1) return(0.01)
  if (d <= 10) return(0.1)
  if (d <= 100) return(1)
  10
}

model_feature_names <- names(model_features)
wyscout_start <- match("offensive_impact", model_feature_names)
wyscout_end <- match("defensa_score_rol_principal", model_feature_names)
if (is.na(wyscout_start) || is.na(wyscout_end) || wyscout_end < wyscout_start) {
  wyscout_feature_candidates <- names(model_features)[vapply(model_features, is.numeric, logical(1))]
} else {
  wyscout_feature_candidates <- model_feature_names[wyscout_start:wyscout_end]
  wyscout_feature_candidates <- wyscout_feature_candidates[vapply(model_features[wyscout_feature_candidates], is.numeric, logical(1))]
}
wyscout_feature_candidates <- intersect(wyscout_feature_candidates, names(players))
wyscout_stat_choices <- setNames(wyscout_feature_candidates, pretty_feature_label(wyscout_feature_candidates))

estimator_numeric_features <- unique(c(
  "age", "minutes_2324", "contract_years", "team_points_1a", "minutes_variation",
  "risk_hist_excess", "risk_load_excess", "height", "weight", wyscout_feature_candidates
))
estimator_numeric_features <- intersect(estimator_numeric_features, names(players))

player_choices <- players |>
  arrange(player_name) |>
  transmute(label = player_name, value = as.character(player_id))

default_compare_palette <- c(
  "#2D6CDF", "#00A896", "#F25C54", "#7B2CBF",
  "#FFB703", "#219EBC", "#6D6875", "#2A9D8F"
)

compare_color_input_id <- function(player_id) {
  paste0("compare_color_", gsub("[^A-Za-z0-9_]", "_", as.character(player_id)))
}

metric_choices <- c(
  "Distancia total P90" = "distance_p90",
  "m/min P90" = "m_per_min_p90",
  "Running distance P90" = "running_distance_p90",
  "HSR distance P90" = "hsr_distance_p90",
  "Sprint distance P90" = "sprint_distance_p90",
  "HI distance P90" = "hi_distance_p90",
  "Aceleraciones alta intensidad P90" = "high_acc_count_p90",
  "Deceleraciones alta intensidad P90" = "high_dec_count_p90",
  "Explosive acceleration to sprint P90" = "explosive_acc_to_sprint_p90",
  "PSV-99" = "psv99"
)

compare_metrics <- c(
  "Valor observado M€" = "market_value_mill",
  "Valor estimado M€" = "pred_iso_mill",
  "Edad" = "age",
  "Minutos 23/24" = "minutes_2324",
  "Contrato" = "contract_years",
  "Riesgo historial %" = "risk_hist_pct",
  "Riesgo carga %" = "risk_load_pct",
  "xG/90" = "xg90",
  "Goles/90" = "goles90",
  "Pases progresivos/90" = "progressive_passes90",
  "Defensa global" = "defense_global"
)
compare_metrics <- compare_metrics[compare_metrics %in% names(players)]
compare_metric_levels <- names(compare_metrics)

# Métricas seleccionables para el radar principal del comparador. Se organizan
# siguiendo los bloques del modelo LightGBM descritos en el TFM.
compare_metric_groups_raw <- list(
  "Valor y predicción" = c("market_value_mill", "pred_iso_mill", "pred_raw_mill", "diff_iso_mill", "diff_raw_mill", "ratio_iso", "ratio_raw"),
  "Fisiología" = c("age", "height", "weight"),
  "Contexto y contrato" = c("rank", "team_points_1a", "puntos_en_1a", "team_points_2324", "contract_years"),
  "Potencial y participación" = c("minutes_2324", "minutes_variation", "primary_position_share", "secondary_position_share", "n_posiciones", "versatilidad_posicional", "polivalencia_global"),
  "Riesgo lesión" = c("risk_hist_excess", "risk_load_excess", "risk_hist_pct", "risk_load_pct", "p_hist", "p_basal_hist", "prob_lesion_temporada", "prob_lesion_media", "prob_lesion_reciente", "prob_lesion_p90"),
  "Ataque y finalización" = c("offensive_impact", "expected_offensive_impact", "xg90", "xa90", "npg_minus_xg90", "shots_on_target90", "shot_quality", "goal_conversion", "finishing_ratio", "box_threat", "remates90", "goles90"),
  "Creación y participación" = c("creativity_index", "assist_chain90", "chance_creation", "assist_involvement", "movement_threat", "key_passes90", "assist90", "involvement", "centrality", "box_presence", "foul_draw_rate"),
  "Pase y progresión" = c("passes_completed90", "forward_passes_completed90", "long_passes_completed90", "final_third_passes_completed90", "deep_passes_completed90", "progressive_passes_completed90", "progressive_passing_ratio", "verticality", "final_third_ratio", "progressive_passes90", "progression_index", "progression_eff", "deep_progression", "box_progression"),
  "Desequilibrio y banda" = c("dribbles_completed90", "attacking_duels_won90", "offensive_duel_proxy", "penetration", "crosses_completed90", "crossing_threat"),
  "Defensa" = c("defensive_duels_won90", "aerial_duels_won90", "def_activity", "discipline_penalty", "clean_defense", "def_activity_per_foul", "defensive_impact", "defense_global"),
  "Scores por posición" = c("ataque_score_posicion_principal", "creacion_score_posicion_principal", "pase_score_posicion_principal", "progresion_regate_score_posicion_principal", "participacion_score_posicion_principal", "defensa_score_posicion_principal"),
  "Scores por rol" = c("ataque_score_rol_principal", "creacion_score_rol_principal", "pase_score_rol_principal", "progresion_regate_score_rol_principal", "participacion_score_rol_principal", "defensa_score_rol_principal"),
  "Otros" = c("area_raw", "primary_role_code", "secondary_role_code")
)
model_numeric_features <- names(model_features)[vapply(model_features, is.numeric, logical(1))]
model_numeric_features <- setdiff(model_numeric_features, c("player_id", "market_value_eur"))
all_compare_metric_cols <- unique(c(unname(compare_metrics), model_numeric_features))
all_compare_metric_cols <- intersect(all_compare_metric_cols, names(players))
# Añadir a Otros cualquier predictor numérico del modelo que no haya quedado asignado.
assigned_compare_cols <- unique(unlist(compare_metric_groups_raw, use.names = FALSE))
compare_metric_groups_raw[["Otros"]] <- unique(c(compare_metric_groups_raw[["Otros"]], setdiff(all_compare_metric_cols, assigned_compare_cols)))
compare_metric_groups <- lapply(compare_metric_groups_raw, function(cols) {
  cols <- intersect(unique(cols), all_compare_metric_cols)
  cols[vapply(players[cols], function(x) is.numeric(x) || is.integer(x), logical(1))]
})
compare_metric_groups <- compare_metric_groups[lengths(compare_metric_groups) > 0]
# Tabla de métricas del comparador. Se mantiene una selección plana para evitar
# problemas de selectize con listas anidadas en algunas sesiones de Windows/RStudio.
# La organización por bloques queda explícita en la etiqueta de cada opción.
compare_metric_lookup <- dplyr::bind_rows(lapply(names(compare_metric_groups), function(grp) {
  tibble(metric_group = grp, metric_col = compare_metric_groups[[grp]])
})) |>
  distinct(metric_col, .keep_all = TRUE) |>
  mutate(
    metric_label = pretty_feature_label(metric_col),
    axis_label = metric_label,
    choice_label = paste0(metric_group, " · ", metric_label)
  )
compare_metric_choices_flat <- stats::setNames(compare_metric_lookup$metric_col, compare_metric_lookup$choice_label)
compare_metric_labels <- stats::setNames(compare_metric_lookup$metric_label, compare_metric_lookup$metric_col)
compare_metric_axis_labels <- stats::setNames(compare_metric_lookup$axis_label, compare_metric_lookup$metric_col)
lookup_compare_metric_label <- function(metric_col) {
  metric_col <- as.character(metric_col)[1]
  lab <- unname(compare_metric_labels[metric_col])
  if (length(lab) == 0 || is.na(lab) || !nzchar(lab)) pretty_feature_label(metric_col) else lab
}
lookup_compare_metric_axis_label <- function(metric_col) {
  metric_col <- as.character(metric_col)[1]
  lab <- unname(compare_metric_axis_labels[metric_col])
  if (length(lab) == 0 || is.na(lab) || !nzchar(lab)) pretty_feature_label(metric_col) else lab
}

# En el radar se usa una etiqueta corta para evitar ejes ilegibles.
# El bloque se conserva en el buscador de métricas.
lookup_compare_metric_radar_label <- function(metric_col) {
  lookup_compare_metric_label(metric_col)
}

compare_metric_default <- intersect(unname(compare_metrics), all_compare_metric_cols)
compare_metric_default <- unname(compare_metric_default)
if (length(compare_metric_default) < 3) compare_metric_default <- head(all_compare_metric_cols, 10)
compare_metric_group_choices <- stats::setNames(names(compare_metric_groups), names(compare_metric_groups))
compare_metric_default_groups <- intersect(
  c("Valor y predicción", "Fisiología", "Contexto y contrato", "Potencial y participación",
    "Riesgo lesión", "Ataque y finalización", "Creación y participación",
    "Pase y progresión", "Defensa"),
  names(compare_metric_groups)
)
if (length(compare_metric_default_groups) == 0) compare_metric_default_groups <- names(compare_metric_groups)
compare_metric_group_ids <- stats::setNames(
  paste0("compare_metric_block_", seq_along(compare_metric_groups)),
  names(compare_metric_groups)
)
compare_metric_group_tab_titles <- stats::setNames(names(compare_metric_groups), names(compare_metric_groups))


radar_default_palette <- c(
  "#2D6CDF", "#2CA02C", "#E74C3C", "#8E44AD",
  "#F39C12", "#17A2B8", "#E377C2", "#7F8C8D"
)

valid_hex_color <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && grepl("^#[0-9A-Fa-f]{6}$", x)
}

make_radar_plot <- function(
  df,
  category_col = "category",
  value_col = "percentile_plot",
  group_col = "player_name",
  title = NULL,
  subtitle = NULL,
  label_width = 14,
  palette = NULL,
  player_label_size = 15,
  area_alpha = 0.14,
  axis_label_size = NULL
) {
  shiny::validate(shiny::need(nrow(df) > 0, "No hay datos para el radar."))

  raw_categories <- df[[category_col]]
  categories <- if (is.factor(raw_categories)) {
    levels(droplevels(raw_categories))
  } else {
    unique(as.character(raw_categories))
  }
  categories <- categories[!is.na(categories) & nzchar(categories)]
  shiny::validate(shiny::need(length(categories) >= 3, "El radar necesita al menos tres dimensiones."))

  n_cat <- length(categories)
  if (is.null(axis_label_size)) {
    axis_label_size <- dplyr::case_when(
      n_cat <= 8 ~ 5.8,
      n_cat <= 12 ~ 5.2,
      n_cat <= 16 ~ 4.75,
      n_cat <= 22 ~ 4.15,
      TRUE ~ 3.65
    )
  }
  label_radius <- dplyr::case_when(
    n_cat <= 8 ~ 1.28,
    n_cat <= 12 ~ 1.38,
    n_cat <= 16 ~ 1.50,
    TRUE ~ 1.62
  )
  coord_limit <- max(1.48, label_radius + 0.22)
  angles <- pi / 2 - 2 * pi * (seq_len(n_cat) - 1) / n_cat
  angle_df <- tibble(
    .category = categories,
    .order = seq_len(n_cat),
    angle = angles,
    axis_x = cos(angles),
    axis_y = sin(angles),
    label_x = label_radius * cos(angles),
    label_y = label_radius * sin(angles),
    label = stringr::str_wrap(categories, width = label_width),
    hjust = dplyr::case_when(
      label_x < -0.12 ~ 1,
      label_x > 0.12 ~ 0,
      TRUE ~ 0.5
    ),
    vjust = dplyr::case_when(
      label_y < -0.12 ~ 1,
      label_y > 0.12 ~ 0,
      TRUE ~ 0.5
    )
  )

  grid_df <- tidyr::expand_grid(r = c(0.25, 0.50, 0.75, 1.00), .category = categories) |>
    left_join(angle_df, by = ".category") |>
    arrange(r, .order) |>
    mutate(x = r * axis_x, y = r * axis_y)
  grid_closed <- grid_df |>
    group_by(r) |>
    group_modify(~ dplyr::bind_rows(.x, .x |> slice(1))) |>
    ungroup()

  ring_labels <- tibble(
    r = c(0.25, 0.50, 0.75, 1.00),
    x = 0.04,
    y = c(0.25, 0.50, 0.75, 1.00),
    label = c("25", "50", "75", "100")
  )

  plot_df <- df |>
    transmute(
      .category = as.character(.data[[category_col]]),
      .group = as.character(.data[[group_col]]),
      .value = pmax(0, pmin(100, suppressWarnings(as.numeric(.data[[value_col]]))))
    ) |>
    filter(!is.na(.category), !is.na(.group), !is.na(.value)) |>
    left_join(angle_df, by = ".category") |>
    filter(!is.na(.order)) |>
    mutate(r = .value / 100, x = r * axis_x, y = r * axis_y) |>
    arrange(.group, .order)

  plot_closed <- plot_df |>
    group_by(.group) |>
    group_modify(~ dplyr::bind_rows(.x, .x |> slice(1))) |>
    ungroup()

  group_levels <- unique(plot_df$.group)
  if (is.null(palette) || length(palette) == 0) {
    palette <- setNames(rep(radar_default_palette, length.out = length(group_levels)), group_levels)
  } else {
    palette <- palette[!is.na(names(palette)) & nzchar(names(palette))]
    missing_groups <- setdiff(group_levels, names(palette))
    if (length(missing_groups) > 0) {
      palette <- c(
        palette,
        setNames(rep(radar_default_palette, length.out = length(missing_groups)), missing_groups)
      )
    }
    palette <- palette[group_levels]
  }

  ggplot() +
    geom_path(data = grid_closed, aes(x = x, y = y, group = r), color = "#d9e1e8", linewidth = 0.45) +
    geom_segment(data = angle_df, aes(x = 0, y = 0, xend = axis_x, yend = axis_y), color = "#e8eef3", linewidth = 0.45) +
    geom_text(data = ring_labels, aes(x = x, y = y, label = label), color = "#8a96a3", size = 3.8) +
    geom_polygon(data = plot_closed, aes(x = x, y = y, group = .group, fill = .group), alpha = area_alpha, color = NA) +
    geom_path(data = plot_closed, aes(x = x, y = y, group = .group, color = .group), linewidth = 1.45, alpha = 0.96) +
    geom_point(data = plot_df, aes(x = x, y = y, color = .group), size = 3.0, alpha = 0.96) +
    geom_label(
      data = angle_df,
      aes(x = label_x, y = label_y, label = label, hjust = hjust, vjust = vjust),
      inherit.aes = FALSE,
      size = axis_label_size,
      fontface = "bold",
      lineheight = 0.90,
      color = "#25313b",
      fill = scales::alpha("white", 0.92),
      label.size = 0,
      label.padding = grid::unit(0.14, "lines")
    ) +
    scale_color_manual(values = palette, drop = FALSE) +
    scale_fill_manual(values = palette, drop = FALSE) +
    coord_equal(xlim = c(-coord_limit, coord_limit), ylim = c(-coord_limit, coord_limit), expand = FALSE, clip = "off") +
    guides(fill = "none", color = guide_legend(override.aes = list(linewidth = 1.8, size = 4, alpha = 0.9))) +
    labs(title = title, subtitle = subtitle, color = "Jugador") +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 5), size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 11, margin = ggplot2::margin(b = 16), color = "#6c7a89"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = player_label_size, face = "bold"),
      legend.key.width = grid::unit(30, "pt"),
      legend.key.height = grid::unit(20, "pt"),
      legend.box = "horizontal",
      legend.margin = ggplot2::margin(t = 8),
      plot.margin = ggplot2::margin(t = 30, r = 150, b = 82, l = 150)
    )
}


hex_to_rgba <- function(hex, alpha = 0.16) {
  if (!valid_hex_color(hex)) hex <- "#2D6CDF"
  rgb <- grDevices::col2rgb(hex)[, 1]
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1], rgb[2], rgb[3], alpha)
}

make_radar_plotly <- function(
  df,
  category_col = "category",
  value_col = "percentile_plot",
  group_col = "player_name",
  title = NULL,
  subtitle = NULL,
  label_width = 14,
  palette = NULL,
  player_label_size = 16,
  area_alpha = 0.16,
  axis_label_size = NULL
) {
  shiny::validate(shiny::need(nrow(df) > 0, "No hay datos para el radar."))

  raw_categories <- df[[category_col]]
  categories <- if (is.factor(raw_categories)) {
    levels(droplevels(raw_categories))
  } else {
    unique(as.character(raw_categories))
  }
  categories <- categories[!is.na(categories) & nzchar(categories)]
  shiny::validate(shiny::need(length(categories) >= 3, "El radar necesita al menos tres dimensiones."))

  if (is.null(axis_label_size)) {
    axis_label_size <- dplyr::case_when(
      length(categories) <= 8 ~ 15,
      length(categories) <= 12 ~ 14,
      length(categories) <= 16 ~ 13,
      length(categories) <= 22 ~ 11,
      TRUE ~ 9
    )
  }

  plot_df <- df |>
    mutate(
      .category = as.character(.data[[category_col]]),
      .group = as.character(.data[[group_col]]),
      .value = pmax(0, pmin(100, suppressWarnings(as.numeric(.data[[value_col]])))),
      .text = if ("tooltip" %in% names(df)) as.character(.data[["tooltip"]]) else paste0(.group, "<br>", .category, ": ", round(.value, 1))
    ) |>
    filter(!is.na(.category), !is.na(.group), !is.na(.value), .category %in% categories) |>
    mutate(.category = factor(.category, levels = categories)) |>
    arrange(.group, .category)

  shiny::validate(shiny::need(nrow(plot_df) > 0, "No hay datos válidos para el radar."))

  group_levels <- unique(plot_df$.group)
  if (is.null(palette) || length(palette) == 0) {
    palette <- setNames(rep(radar_default_palette, length.out = length(group_levels)), group_levels)
  } else {
    palette <- palette[!is.na(names(palette)) & nzchar(names(palette))]
    missing_groups <- setdiff(group_levels, names(palette))
    if (length(missing_groups) > 0) {
      palette <- c(palette, setNames(rep(radar_default_palette, length.out = length(missing_groups)), missing_groups))
    }
    palette <- palette[group_levels]
  }

  tick_text <- stringr::str_wrap(categories, width = label_width)
  tick_text <- gsub("\\n", "<br>", tick_text)

  fig <- plotly::plot_ly(type = "scatterpolar")
  for (grp in group_levels) {
    dfg <- plot_df |> filter(.group == grp) |> arrange(.category)
    first_row <- dfg |> slice(1)
    rr <- c(dfg$.value, first_row$.value)
    tt <- c(as.character(dfg$.category), as.character(first_row$.category))
    hx <- c(dfg$.text, first_row$.text)
    grp_scalar <- as.character(grp)[1]
    col <- unname(palette[grp_scalar])
    if (length(col) == 0 || is.na(col) || !valid_hex_color(col)) col <- radar_default_palette[((match(grp_scalar, group_levels) - 1) %% length(radar_default_palette)) + 1]
    fig <- fig |>
      plotly::add_trace(
        r = rr,
        theta = tt,
        mode = "lines+markers",
        name = grp,
        text = hx,
        hoverinfo = "text",
        fill = "toself",
        fillcolor = hex_to_rgba(col, area_alpha),
        line = list(color = col, width = 3),
        marker = list(color = col, size = 8, line = list(color = "white", width = 1.2))
      )
  }

  fig |>
    plotly::layout(
      title = list(
        text = paste0("<b>", htmltools::htmlEscape(title %||% "Radar comparativo"), "</b>",
                      if (!is.null(subtitle) && nzchar(subtitle)) paste0("<br><sup>", htmltools::htmlEscape(subtitle), "</sup>") else ""),
        x = 0.5,
        xanchor = "center",
        font = list(size = 22, color = "#16213e")
      ),
      polar = list(
        bgcolor = "white",
        radialaxis = list(
          visible = TRUE,
          range = c(0, 100),
          tickmode = "array",
          tickvals = c(0, 25, 50, 75, 100),
          ticktext = c("0", "25", "50", "75", "100"),
          tickfont = list(size = 11, color = "#6c7a89"),
          gridcolor = "#dce6ef",
          gridwidth = 1,
          linecolor = "#dce6ef",
          angle = 90
        ),
        angularaxis = list(
          tickmode = "array",
          tickvals = categories,
          ticktext = tick_text,
          tickfont = list(size = axis_label_size, color = "#25313b", family = "Arial"),
          gridcolor = "#edf2f7",
          linecolor = "#dce6ef",
          direction = "clockwise",
          rotation = 90
        )
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top",
        font = list(size = player_label_size, color = "#16213e", family = "Arial")
      ),
      margin = list(l = 115, r = 115, t = 105, b = 120),
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      hoverlabel = list(bgcolor = "white", bordercolor = "#dce6ef", font = list(size = 12, color = "#16213e"))
    ) |>
    plotly::config(displayModeBar = TRUE, responsive = TRUE)
}



section_card <- function(title, subtitle = NULL, ..., class_extra = "") {
  div(
    class = paste("section-card", class_extra),
    div(
      class = "section-card-header",
      div(
        class = "section-card-title-wrap",
        tags$h3(class = "section-card-title", title),
        if (!is.null(subtitle) && nzchar(as.character(subtitle))) {
          tags$p(class = "section-card-subtitle", subtitle)
        }
      )
    ),
    div(class = "section-card-body", ...)
  )
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#2d6cdf"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      $(document).on('click', '#toggle_filters', function(e) {
        e.preventDefault();
        var panel = $('#global_filters_panel');
        var sidebar = panel.closest('div[class*=\"col-sm-\"], div[class*=\"col-md-\"], div[class*=\"col-lg-\"], div[class*=\"col-xl-\"], div[class*=\"col-\"]');
        if (!sidebar.length) { sidebar = panel.parent(); }
        var main = sidebar.next('div[class*=\"col-sm-\"], div[class*=\"col-md-\"], div[class*=\"col-lg-\"], div[class*=\"col-xl-\"], div[class*=\"col-\"]');
        if (!main.length) { main = sidebar.next(); }
        var collapsed = !sidebar.hasClass('global-filters-hidden');
        sidebar.toggleClass('global-filters-hidden', collapsed);
        main.toggleClass('global-main-expanded', collapsed);
        $('body').toggleClass('filters-collapsed', collapsed);
        $('#toggle_filters_text').text(collapsed ? 'Mostrar filtros globales' : 'Ocultar filtros globales');
        $('#toggle_filters').attr('aria-expanded', collapsed ? 'false' : 'true');
        setTimeout(function() {
          $(window).trigger('resize');
          if (window.Plotly) {
            $('.js-plotly-plot').each(function() { Plotly.Plots.resize(this); });
          }
        }, 250);
      });
    "))
  ),

  tags$h2(class = "main-title", "TFM | Valor de mercado, roles y riesgo lesional en LaLiga 2023/24"),
  tags$p(class = "subtitle", "Visualización interactiva de los outputs de roles, riesgo por historial, riesgo por carga y predicción de valor de mercado."),

  div(
    class = "global-filter-toggle-bar",
    tags$button(
      id = "toggle_filters",
      type = "button",
      class = "btn btn-outline-primary btn-sm global-filter-toggle-button",
      `aria-expanded` = "true",
      tags$span(class = "filter-toggle-icon", "[ ]"),
      tags$span(id = "toggle_filters_text", "Ocultar filtros globales")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        id = "global_filters_panel",
        class = "global-filters-panel",
        h4("Filtros globales"),
        selectInput("position_filter", "Posición principal", choices = position_choices, selected = position_choices, multiple = TRUE),
        selectInput("team_filter", "Equipo", choices = team_choices, selected = team_choices, multiple = TRUE),
        selectInput("role_filter", "Rol principal", choices = role_choices, selected = role_choices, multiple = TRUE),
        selectInput("split_filter", "Partición del modelo de precio", choices = split_choices, selected = "Todas"),
        sliderInput(
          "value_range", "Valor de mercado observado (M€)",
          min = floor(min(players$market_value_mill, na.rm = TRUE)),
          max = ceiling(max(players$market_value_mill, na.rm = TRUE)),
          value = c(floor(min(players$market_value_mill, na.rm = TRUE)), ceiling(max(players$market_value_mill, na.rm = TRUE))),
          step = 1
        ),
        checkboxInput("only_with_load", "Solo jugadores con riesgo por carga", value = FALSE),
        hr(),
        selectizeInput(
          "selected_player", "Jugador para detalle",
          choices = setNames(player_choices$value, player_choices$label),
          selected = as.character(players$player_id[1]),
          options = list(maxOptions = 3000)
        ),
        div(class = "sidebar-note", "Los filtros afectan a resumen, roles, lesión, valoración y tablas. La serie física muestra siempre el jugador seleccionado.")
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        tabPanel(
          "Resumen",
          br(),
          uiOutput("kpi_cards"),
          div(class = "section-tabset",
            tabsetPanel(
              id = "summary_tabs",
              tabPanel(
                "Mercado y riesgo",
                br(),
                section_card(
                  "Vista general del mercado y del riesgo",
                  "Distribuci\u00f3n del valor, relaci\u00f3n entre los dos excesos de riesgo y desagregaci\u00f3n por posici\u00f3n y rol.",
                  fluidRow(
                    column(6, div(class = "plot-panel", plotlyOutput("market_distribution", height = 360))),
                    column(6, div(class = "plot-panel", plotlyOutput("risk_scatter", height = 360)))
                  ),
                  fluidRow(
                    column(6, div(class = "plot-panel", plotlyOutput("value_by_position", height = 390))),
                    column(6, div(class = "plot-panel", plotlyOutput("value_by_role", height = 390)))
                  )
                )
              ),
              tabPanel(
                "Jugadores filtrados",
                br(),
                section_card(
                  "Jugadores filtrados",
                  "Tabla de consulta con los filtros globales aplicados.",
                  div(class = "plot-panel", DTOutput("players_table"))
                )
              )
            )
          )
        ),

        tabPanel(
          "Roles",
          br(),
          div(class = "section-tabset",
            tabsetPanel(
              id = "roles_tabs",
              tabPanel(
                "Clustering y PCA",
                br(),
                section_card(
                  "Clustering y PCA ponderado",
                  "Mapa PC1-PC2, prima de valor por rol y cargas factoriales usadas para interpretar los clusters.",
                  fluidRow(
                    column(4, selectInput("role_position", "Posici\u00f3n en el mapa", choices = position_choices, selected = "CF")),
                    column(4, checkboxInput("label_role_players", "Mostrar todas las etiquetas", value = FALSE)),
                    column(4, uiOutput("role_axis_ui"))
                  ),
                  fluidRow(
                    column(12, div(class = "plot-panel hero-plot", plotlyOutput("role_scatter", height = 720)))
                  ),
                  fluidRow(
                    column(5, div(class = "plot-panel", plotlyOutput("role_premium", height = 430))),
                    column(7, div(class = "plot-panel", plotlyOutput("loadings_heatmap", height = 430)))
                  )
                )
              ),
              tabPanel(
                "Sectorizaci\u00f3n del campo",
                br(),
                section_card(
                  "Sectorizaci\u00f3n del campo",
                  "Eventos del jugador seleccionado sobre la malla C1-C20, con sectores propios y vecinos.",
                  fluidRow(
                    column(4, selectizeInput(
                      "pitch_event_filter", "Eventos en el pitch (vac\u00edo = todos)",
                      choices = event_choices,
                      selected = event_choices,
                      multiple = TRUE,
                      options = list(plugins = list("remove_button"), maxOptions = 4000)
                    )),
                    column(2, checkboxInput("pitch_show_sectors", "Sectores propios", value = TRUE)),
                    column(2, checkboxInput("pitch_show_neighbors", "Vecinos", value = TRUE)),
                    column(4, sliderInput("pitch_max_events", "M\u00e1x. eventos visibles", min = 250, max = 5000, value = 1500, step = 250))
                  ),
                  div(class = "plot-panel hero-plot", plotlyOutput("pitch_events_plot", height = 650)),
                  div(class = "plot-panel", DTOutput("roles_table"))
                )
              )
            )
          )
        ),

        tabPanel(
          "Historial m\u00e9dico",
          br(),
          div(class = "section-tabset",
            tabsetPanel(
              id = "medical_tabs",
              tabPanel(
                "Variables LASSO",
                br(),
                section_card(
                  "Variables independientes del modelo LASSO",
                  "Distribuci\u00f3n de d\u00edas desde la \u00faltima lesi\u00f3n, incidencia hist\u00f3rica y exceso de riesgo por historial.",
                  fluidRow(
                    column(6, div(class = "plot-panel", plotOutput("hist_days_density", height = 360))),
                    column(6, div(class = "plot-panel", plotOutput("hist_incidence_violin", height = 360)))
                  ),
                  fluidRow(
                    column(12, div(class = "plot-panel", plotOutput("risk_hist_distribution", height = 340)))
                  )
                )
              ),
              tabPanel(
                "Detalle y bodymap",
                br(),
                section_card(
                  "Detalle m\u00e9dico del jugador seleccionado",
                  "Cards individuales, mapa anat\u00f3mico y resumen de lesiones por temporada y grupo.",
                  fluidRow(column(12, uiOutput("player_risk_card"))),
                  div(class = "section-controls",
                    fluidRow(
                      column(3, selectInput("body_value", "M\u00e9trica", choices = c("N\u00famero de lesiones" = "count", "D\u00edas de baja" = "duration", "Partidos perdidos" = "games_missed"), selected = "count")),
                      column(3, selectInput("body_mode", "Modo", choices = c("Porcentaje" = "percentage", "Absoluto" = "absolute"), selected = "percentage")),
                      column(3, numericInput("body_min_duration", "Duraci\u00f3n m\u00ednima (d\u00edas)", value = 0, min = 0, max = 365, step = 1)),
                      column(3, checkboxInput("body_show_labels", "Mostrar etiquetas", value = TRUE))
                    ),
                    fluidRow(
                      column(6, selectizeInput("body_seasons", "Temporadas", choices = sort(unique(na.omit(normaliza_temporada(injuries_long$season_injured)))), selected = sort(unique(na.omit(normaliza_temporada(injuries_long$season_injured)))), multiple = TRUE, options = list(plugins = list("remove_button")))),
                      column(6, selectizeInput("body_zones", "Zonas", choices = body_zone_choices, selected = unname(body_zone_choices), multiple = TRUE, options = list(plugins = list("remove_button"))))
                    )
                  ),
                  fluidRow(
                    column(6, div(class = "plot-panel bodymap-panel", plotOutput("body_map", height = 640))),
                    column(6,
                      div(class = "plot-panel", plotlyOutput("injuries_by_season", height = 300)),
                      div(class = "plot-panel", plotlyOutput("injuries_by_group", height = 300))
                    )
                  ),
                  fluidRow(
                    column(12, div(class = "plot-panel", DTOutput("selected_injuries_table")))
                  )
                )
              )
            )
          )
        ),

        tabPanel(
          "Carga f\u00edsica",
          br(),
          div(class = "section-tabset",
            tabsetPanel(
              id = "load_tabs",
              tabPanel(
                "Perfil f\u00edsico",
                br(),
                section_card(
                  "Perfil f\u00edsico del jugador",
                  "Selector de m\u00e9trica f\u00edsica, cards individuales y evoluci\u00f3n partido a partido.",
                  fluidRow(column(12, uiOutput("player_load_card"))),
                  fluidRow(
                    column(5, selectInput("physical_metric", "M\u00e9trica f\u00edsica", choices = metric_choices, selected = "hi_distance_p90")),
                    column(7, div(class = "small-note", "La serie temporal muestra registros con al menos 60 minutos. Los puntos se colorean seg\u00fan si esa ventana queda asociada a una lesi\u00f3n posterior."))
                  ),
                  fluidRow(
                    column(7, div(class = "plot-panel", plotlyOutput("physical_timeline", height = 460))),
                    column(5, div(class = "plot-panel", plotlyOutput("physical_position_box", height = 460)))
                  )
                )
              ),
              tabPanel(
                "Riesgo e interpretabilidad",
                br(),
                section_card(
                  "Riesgo por carga e interpretabilidad del modelo",
                  "Exceso de riesgo por ventanas, distribuci\u00f3n poblacional, importancia por bloques y Gain por categor\u00edas.",
                  fluidRow(
                    column(6, div(class = "plot-panel", plotlyOutput("risk_load_windows", height = 370))),
                    column(6, div(class = "plot-panel", plotOutput("risk_load_distribution", height = 370)))
                  ),
                  fluidRow(
                    column(12, div(class = "plot-panel", plotlyOutput("physical_block_importance", height = 360)))
                  ),
                  fluidRow(
                    column(12, div(class = "plot-panel hero-plot", plotOutput("physical_gain_categories", height = 760)))
                  )
                )
              ),
              tabPanel(
                "Datos f\u00edsicos",
                br(),
                section_card(
                  "Datos f\u00edsicos del jugador seleccionado",
                  "Registros usados para la serie f\u00edsica.",
                  div(class = "plot-panel", DTOutput("physical_table"))
                )
              )
            )
          )
        ),

        tabPanel(
          "Valor de mercado",
          br(),
          div(class = "section-tabset",
            tabsetPanel(
              id = "market_tabs",
              tabPanel(
                "Predicci\u00f3n y calibraci\u00f3n",
                br(),
                section_card(
                  "Predicci\u00f3n, calibraci\u00f3n y sesgo relativo",
                  "Comparaci\u00f3n entre valor observado y estimado, calibraci\u00f3n por deciles y an\u00e1lisis del ratio predicho/observado.",
                  fluidRow(
                    column(4, selectInput("prediction_type", "Predicci\u00f3n", choices = c("Calibraci\u00f3n isot\u00f3nica" = "iso", "Original / sin calibrar" = "raw"), selected = "iso")),
                    column(4, sliderInput("outlier_pct", "Umbral de error absoluto", min = 80, max = 99, value = 95, step = 1, post = "p")),
                    column(4, checkboxInput("label_top_errors", "Etiquetar at\u00edpicos", value = TRUE))
                  ),
                  fluidRow(
                    column(7, div(class = "plot-panel hero-plot", plotlyOutput("predicted_vs_real", height = 560))),
                    column(5, div(class = "plot-panel", plotlyOutput("calibration_decile_plot", height = 560)))
                  ),
                  fluidRow(
                    column(12, div(class = "plot-panel", plotlyOutput("ratio_by_band", height = 390)))
                  )
                )
              ),
              tabPanel(
                "Importancia SHAP",
                br(),
                section_card(
                  "Par\u00e1metros del modelo e interpretabilidad SHAP",
                  "Importancia global, importancia por bloque y distribuci\u00f3n SHAP individual. Los dependence plots tienen una pesta\u00f1a propia para ganar espacio de lectura.",
                  fluidRow(
                    column(4, sliderInput("top_shap", "Variables SHAP", min = 10, max = 40, value = 25, step = 5)),
                    column(8, div(class = "small-note", "La importancia global resume las variables dominantes del modelo; la agrupaci\u00f3n por bloque permite interpretar qu\u00e9 familias de variables aportan m\u00e1s informaci\u00f3n."))
                  ),
                  fluidRow(
                    column(6, div(class = "plot-panel", plotlyOutput("shap_global_plot", height = 460))),
                    column(6, div(class = "plot-panel", plotlyOutput("shap_group_plot", height = 460)))
                  ),
                  fluidRow(
                    column(12, div(class = "plot-panel", plotlyOutput("shap_distribution_plot", height = 430)))
                  ),
                  fluidRow(
                    column(12, div(class = "plot-panel", DTOutput("model_metrics_table")))
                  )
                )
              ),
              tabPanel(
                "SHAP dependence",
                br(),
                section_card(
                  "Dependence plots SHAP",
                  "Relaci\u00f3n entre el valor de una variable y su contribuci\u00f3n SHAP al valor estimado. El color permite estudiar interacciones con otra variable del modelo.",
                  fluidRow(
                    column(4, selectInput("shap_feature", "Variable SHAP", choices = shap_feature_choices, selected = shap_feature_tbl$feature[1])),
                    column(4, selectInput("shap_color_var", "Colorear por", choices = shap_color_choices, selected = "minutes_2324")),
                    column(4, div(class = "small-note", "Valores positivos de SHAP empujan la predicci\u00f3n al alza; valores negativos la reducen. La curva suavizada ayuda a detectar no linealidades."))
                  ),
                  fluidRow(
                    column(12, div(class = "plot-panel hero-plot", plotlyOutput("shap_dependence_plot", height = 680)))
                  )
                )
              ),
              tabPanel(
                "Tabla de valoraci\u00f3n",
                br(),
                section_card(
                  "Tabla de valoraci\u00f3n y errores",
                  "Predicciones calibradas/originales y clasificaci\u00f3n din\u00e1mica de infravalorados y sobrestimados.",
                  div(class = "plot-panel", DTOutput("market_table"))
                )
              )
            )
          )
        ),

        tabPanel(
          "Comparador",
          br(),
          section_card(
            "Selector común de comparación",
            "Selecciona jugadores, restringe por posiciones o roles compartidos y define un color estable para cada jugador. Estos controles afectan a los dos radares.",
            uiOutput("compare_players_ui"),
            fluidRow(
              column(4, checkboxInput("compare_common_positions", "Solo posiciones comunes", value = FALSE)),
              column(4, checkboxInput("compare_common_roles", "Solo roles comunes", value = FALSE)),
              column(4, uiOutput("compare_scope_note"))
            ),
            uiOutput("compare_color_ui")
          ),
          div(class = "section-tabset",
            tabsetPanel(
              id = "compare_subtabs",
              type = "tabs",
            tabPanel(
              "Métricas generales",
              br(),
              section_card(
                "Comparación de métricas generales",
                "Radar de percentiles generales y tabla de perfil de los jugadores seleccionados.",
                div(class = "metric-selector-panel",
                  div(class = "metric-selector-actions metric-selector-actions-wide",
                    actionButton("compare_select_current_block", "Seleccionar bloque actual"),
                    actionButton("compare_clear_current_block", "Limpiar bloque actual"),
                    actionButton("compare_select_default_metrics", "Resumen recomendado"),
                    actionButton("compare_select_all_metrics", "Seleccionar todas"),
                    actionButton("compare_clear_metrics", "Limpiar todo")
                  ),
                  uiOutput("compare_metric_tabs_ui"),
                  uiOutput("compare_metric_selected_note")
                ),
                fluidRow(
                  column(12, div(class = "plot-panel compare-radar-panel hero-plot", plotlyOutput("compare_bar", height = "820px")))
                ),
                fluidRow(
                  column(12, div(class = "plot-panel", DTOutput("compare_table")))
                )
              )
            ),
            tabPanel(
              "Eventos",
              br(),
              section_card(
                "Comparación de eventos",
                "Radar fijo con los 16 eventos utilizados para la construcción de roles y la sectorización del campo.",
                fluidRow(
                  column(12, div(class = "plot-panel compare-radar-panel hero-plot", plotlyOutput("compare_events", height = "820px")))
                )
              )
            )
          )
          )
        ),

        tabPanel(
          "Buscador",
          br(),
          div(class = "section-tabset",
            tabsetPanel(
              id = "search_tabs",
              tabPanel(
                "Buscar jugadores",
                br(),
                section_card(
                  "Buscar jugadores",
                  "Filtros de perfil, riesgo, valor y estad\u00edsticas Wyscout/feature engineering con umbrales din\u00e1micos.",
                  fluidRow(
                    column(3, selectInput("search_position", "Posici\u00f3n", choices = c("Todas", position_choices), selected = "Todas")),
                    column(3, selectInput("search_role", "Rol", choices = c("Todos", role_choices), selected = "Todos")),
                    column(3, selectInput("search_team_group", "Grupo equipo", choices = c("Todos", team_group_choices), selected = "Todos")),
                    column(3, selectInput("search_country", "Pa\u00eds / grupo", choices = c("Todos", country_choices), selected = "Todos"))
                  ),
                  fluidRow(
                    column(3, sliderInput("search_age", "Edad", min = floor(min(players$age, na.rm = TRUE)), max = ceiling(max(players$age, na.rm = TRUE)), value = c(floor(min(players$age, na.rm = TRUE)), ceiling(max(players$age, na.rm = TRUE))), step = 1)),
                    column(3, sliderInput("search_minutes", "Minutos 23/24", min = floor(min(players$minutes_2324, na.rm = TRUE)), max = ceiling(max(players$minutes_2324, na.rm = TRUE)), value = c(floor(min(players$minutes_2324, na.rm = TRUE)), ceiling(max(players$minutes_2324, na.rm = TRUE))), step = 50)),
                    column(3, sliderInput("search_contract", "Contrato restante", min = floor(min(players$contract_years, na.rm = TRUE)), max = ceiling(max(players$contract_years, na.rm = TRUE)), value = c(floor(min(players$contract_years, na.rm = TRUE)), ceiling(max(players$contract_years, na.rm = TRUE))), step = 0.5)),
                    column(3, sliderInput("search_value", "Valor M\u20ac", min = floor(min(players$market_value_mill, na.rm = TRUE)), max = ceiling(max(players$market_value_mill, na.rm = TRUE)), value = c(floor(min(players$market_value_mill, na.rm = TRUE)), ceiling(max(players$market_value_mill, na.rm = TRUE))), step = 1))
                  ),
                  fluidRow(
                    column(6, sliderInput("search_risk_hist", "Exceso riesgo historial %", min = 0, max = ceiling(max(players$risk_hist_pct, na.rm = TRUE)), value = c(0, ceiling(max(players$risk_hist_pct, na.rm = TRUE))), step = 0.5)),
                    column(6, sliderInput("search_risk_load", "Exceso riesgo carga %", min = 0, max = max(0.1, ceiling(max(players$risk_load_pct, na.rm = TRUE) * 10) / 10), value = c(0, max(0.1, ceiling(max(players$risk_load_pct, na.rm = TRUE) * 10) / 10)), step = 0.1))
                  ),
                  fluidRow(
                    column(12, selectizeInput("search_stats_selected", "Estad\u00edsticas", choices = wyscout_stat_choices, selected = character(0), multiple = TRUE, options = list(plugins = list("remove_button"), placeholder = "Selecciona estad\u00edsticas para filtrar...", maxOptions = 5000)))
                  ),
                  uiOutput("search_stats_filters"),
                  div(class = "plot-panel", DTOutput("search_table"))
                )
              ),
              tabPanel(
                "Estimar precio",
                br(),
                section_card(
                  "Estimar precio",
                  "Perfil obligatorio y umbrales opcionales de estad\u00edsticas Wyscout. Las variables no seleccionadas se imputan con la media del rol.",
                  fluidRow(
                    column(3, numericInput("est_age", "Edad*", value = 24, min = 16, max = 40, step = 1)),
                    column(3, selectInput("est_country", "Pa\u00eds / grupo*", choices = country_choices, selected = country_choices[1])),
                    column(3, selectInput("est_team_group", "Grupo equipo*", choices = team_group_choices, selected = team_group_choices[1])),
                    column(3, numericInput("est_minutes", "Minutos 23/24*", value = 1800, min = 0, max = 4000, step = 50))
                  ),
                  fluidRow(
                    column(3, numericInput("est_contract", "Contrato restante*", value = 3, min = 0, max = 8, step = 0.5)),
                    column(3, selectInput("est_position", "Posici\u00f3n*", choices = position_choices, selected = position_choices[1])),
                    column(3, selectInput("est_role", "Rol*", choices = roles_for_positions(position_choices[1]), selected = roles_for_positions(position_choices[1])[1])),
                    column(3, numericInput("est_k", "Vecinos similares", value = 12, min = 5, max = 30, step = 1))
                  ),
                  fluidRow(
                    column(4, sliderInput("est_risk_hist_range", "Exceso riesgo historial %", min = 0, max = ceiling(max(players$risk_hist_pct, na.rm = TRUE)), value = c(0, ceiling(max(players$risk_hist_pct, na.rm = TRUE))), step = 0.5)),
                    column(4, sliderInput("est_risk_load_range", "Exceso riesgo carga %", min = 0, max = max(0.1, ceiling(max(players$risk_load_pct, na.rm = TRUE) * 10) / 10), value = c(0, max(0.1, ceiling(max(players$risk_load_pct, na.rm = TRUE) * 10) / 10)), step = 0.1)),
                    column(4, uiOutput("estimate_price_card"))
                  ),
                  fluidRow(
                    column(12, selectizeInput("est_stats_selected", "Estad\u00edsticas", choices = wyscout_stat_choices, selected = character(0), multiple = TRUE, options = list(plugins = list("remove_button"), placeholder = "Selecciona estad\u00edsticas para acotar vecinos con umbrales...", maxOptions = 5000)))
                  ),
                  uiOutput("est_stats_inputs"),
                  fluidRow(
                    column(6, div(class = "plot-panel", plotlyOutput("estimate_neighbors_plot", height = 370))),
                    column(6, div(class = "plot-panel", DTOutput("estimate_neighbors_table")))
                  )
                )
              )
            )
          )
        ),


        tabPanel(
          "Informes",
          br(),
          section_card(
            "Configuración del generador",
            "El informe combina datos estructurados de la app, contexto metodológico recuperado por RAG y una plantilla estable. No usa agentes ni generación externa: el RAG solo recupera contexto metodológico y los números salen de los CSV.",
            fluidRow(
              column(6, textInput("report_python", "Ejecutable Python", value = Sys.getenv("TFM_PYTHON", unset = "auto"), placeholder = "C:/Users/aleja/anaconda3/envs/tfm-rag/python.exe")),
              column(6, div(class = "report-config-note", HTML("Con Anaconda, usa la ruta completa del entorno, por ejemplo <code>C:/Users/aleja/anaconda3/envs/tfm-rag/python.exe</code>. Dependencias mínimas: <code>pip install -r rag/requirements_minimal.txt</code>.")))
            )
          ),
          div(class = "section-tabset",
            tabsetPanel(
              id = "report_tabs",
              tabPanel(
                "Informe individual",
                br(),
                section_card(
                  "Informe individual de jugador",
                  "Genera un informe ejecutivo de 1-2 páginas con ficha, valoración, rol, riesgo lesional, SHAP y comparables.",
                  fluidRow(
                    column(6, selectizeInput(
                      "report_player", "Jugador",
                      choices = setNames(player_choices$value, player_choices$label),
                      selected = as.character(players$player_id[1]),
                      options = list(maxOptions = 3000)
                    )),
                    column(3, br(), actionButton("report_generate_individual", "Generar informe", class = "btn-primary")),
                    column(3, br(), downloadButton("download_report_individual_html", "Descargar HTML"))
                  ),
                  fluidRow(
                    column(3, downloadButton("download_report_individual_md", "Descargar Markdown")),
                    column(3, downloadButton("download_report_individual_json", "Descargar JSON datos")),
                    column(6, uiOutput("report_status_individual"))
                  ),
                  uiOutput("report_preview_individual")
                )
              ),
              tabPanel(
                "Informe comparativo",
                br(),
                section_card(
                  "Informe comparativo de jugadores",
                  "Compara entre 2 y 5 jugadores con valor observado/estimado, roles, métricas, riesgo y ranking según el objetivo del análisis.",
                  fluidRow(
                    column(6, selectizeInput(
                      "report_compare_players", "Jugadores a comparar",
                      choices = setNames(player_choices$value, player_choices$label),
                      selected = as.character(head(players$player_id, 2)),
                      multiple = TRUE,
                      options = list(plugins = list("remove_button"), maxItems = 5, maxOptions = 3000)
                    )),
                    column(3, selectInput("report_objective", "Objetivo", choices = c("Comparativa general" = "general", "Fichaje" = "fichaje", "Renovación" = "renovacion", "Venta" = "venta", "Seguimiento" = "seguimiento"), selected = "general")),
                    column(3, br(), actionButton("report_generate_comparison", "Generar comparativo", class = "btn-primary"))
                  ),
                  fluidRow(
                    column(3, downloadButton("download_report_comparison_html", "Descargar HTML")),
                    column(3, downloadButton("download_report_comparison_md", "Descargar Markdown")),
                    column(3, downloadButton("download_report_comparison_json", "Descargar JSON datos")),
                    column(3, uiOutput("report_status_comparison"))
                  ),
                  uiOutput("report_preview_comparison")
                )
              )
            )
          )
        ),
        tabPanel(
          "Datos y exportación",
          br(),
          div(class = "info-box", HTML("La app no recalcula matching, Weighted PCA, LASSO, XGBoost, LightGBM ni SHAP al arrancar. Lee CSV ligeros en <code>data/app</code>.")),
          fluidRow(
            column(4, downloadButton("download_filtered_players", "Descargar jugadores filtrados")),
            column(4, downloadButton("download_role_data", "Descargar roles filtrados")),
            column(4, downloadButton("download_predictions", "Descargar predicciones"))
          ),
          br(),
          h4("Archivos app-ready"),
          DTOutput("data_files_table")
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {


  # ---------------------------------------------------------------------------
  # Informes automaticos: RAG metodologico + datos estructurados
  # ---------------------------------------------------------------------------
  report_individual_state <- reactiveVal(NULL)
  report_comparison_state <- reactiveVal(NULL)
  # IMPORTANTE: los informes generados desde Shiny se guardan fuera de la carpeta
  # de la app. Escribir en www/ o reports/ dentro del proyecto puede activar el
  # autoreload de Shiny/RStudio y hacer que la sesión se reinicie al pulsar
  # "Generar informe". Para la vista previa usamos srcdoc, sin crear archivos en www/.
  report_session_id <- gsub("[^A-Za-z0-9_-]", "_", session$token %||% as.character(Sys.getpid()))
  report_out_dir <- file.path(tempdir(), paste0("tfm_shiny_reports_", report_session_id))
  dir.create(report_out_dir, recursive = TRUE, showWarnings = FALSE)

  sanitize_report_console <- function(x) {
    if (length(x) == 0) return(character())
    x <- as.character(x)
    y <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = "byte"))
    y[is.na(y)] <- ""
    status <- attr(x, "status")
    if (!is.null(status)) attr(y, "status") <- status
    y
  }

  with_python_subprocess_env <- function(expr) {
    old_encoding <- Sys.getenv("PYTHONIOENCODING", unset = NA_character_)
    old_no_bytecode <- Sys.getenv("PYTHONDONTWRITEBYTECODE", unset = NA_character_)
    on.exit({
      if (is.na(old_encoding)) Sys.unsetenv("PYTHONIOENCODING") else Sys.setenv(PYTHONIOENCODING = old_encoding)
      if (is.na(old_no_bytecode)) Sys.unsetenv("PYTHONDONTWRITEBYTECODE") else Sys.setenv(PYTHONDONTWRITEBYTECODE = old_no_bytecode)
    }, add = TRUE)
    Sys.setenv(PYTHONIOENCODING = "utf-8", PYTHONDONTWRITEBYTECODE = "1")
    force(expr)
  }

  split_python_command <- function(cmd) {
    cmd <- trimws(as.character(cmd %||% "auto"))
    if (!nzchar(cmd) || tolower(cmd) %in% c("auto", "automatico", "automático")) {
      cmd <- Sys.getenv("TFM_PYTHON", unset = "auto")
      if (!nzchar(cmd) || tolower(cmd) == "auto") {
        cmd <- if (.Platform$OS.type == "windows") "py -3" else "python3"
      }
    }

    if (file.exists(cmd)) {
      return(list(command = normalizePath(cmd, winslash = "/", mustWork = FALSE), prefix_args = character(0)))
    }

    if (grepl('^"[^"]+"', cmd)) {
      exe <- sub('^"([^"]+)".*$', "\\1", cmd)
      rest <- trimws(sub('^"[^"]+"', "", cmd))
      return(list(command = exe, prefix_args = if (nzchar(rest)) strsplit(rest, "\\s+")[[1]] else character(0)))
    }

    if (grepl("\\s", cmd)) {
      parts <- strsplit(cmd, "\\s+")[[1]]
      return(list(command = parts[1], prefix_args = if (length(parts) > 1) parts[-1] else character(0)))
    }

    list(command = cmd, prefix_args = character(0))
  }

  candidate_python_commands <- function(user_cmd) {
    user_cmd <- trimws(as.character(user_cmd %||% "auto"))
    env_cmd <- Sys.getenv("TFM_PYTHON", unset = "")
    candidates <- list()

    if (nzchar(env_cmd)) candidates <- c(candidates, list(split_python_command(env_cmd)))
    candidates <- c(candidates, list(split_python_command(user_cmd)))

    if (.Platform$OS.type == "windows") {
      candidates <- c(candidates, list(list(command = "py", prefix_args = "-3")))
      candidates <- c(candidates, list(split_python_command("python3"), split_python_command("python")))
    } else {
      candidates <- c(candidates, list(split_python_command("python3"), split_python_command("python")))
    }

    if (nzchar(Sys.which("python3"))) candidates <- c(candidates, list(list(command = unname(Sys.which("python3")), prefix_args = character(0))))
    if (nzchar(Sys.which("python"))) candidates <- c(candidates, list(list(command = unname(Sys.which("python")), prefix_args = character(0))))

    seen <- character(0)
    out <- list()
    for (cand in candidates) {
      key <- paste(cand$command, paste(cand$prefix_args, collapse = " "), sep = "|")
      if (!key %in% seen) {
        out <- c(out, list(cand))
        seen <- c(seen, key)
      }
    }
    out
  }

  run_python_command <- function(command, args) {
    out <- tryCatch(
      with_python_subprocess_env(system2(command, args = args, stdout = TRUE, stderr = TRUE)),
      error = function(e) structure(conditionMessage(e), status = 1)
    )
    cleaned <- sanitize_report_console(out)
    status <- attr(out, "status")
    if (!is.null(status)) attr(cleaned, "status") <- status
    cleaned
  }

  check_python_command <- function(cand, check_script) {
    # Usamos un fichero .py en vez de python -c "import ..." porque en algunas
    # combinaciones Windows + R + Anaconda la cadena -c se trocea y Python recibe
    # solo `import`, provocando: SyntaxError: invalid syntax.
    out <- run_python_command(cand$command, c(cand$prefix_args, check_script))
    status <- attr(out, "status")
    if (is.null(status)) status <- 0
    bad_alias <- any(grepl("Microsoft Store|no se encontr|No installed Python|not found|not recognized|no se reconoce|Alias", out, ignore.case = TRUE, useBytes = TRUE))
    ok <- identical(as.integer(status), 0L) && any(grepl("^PYTHON_OK=", out, useBytes = TRUE)) && any(grepl("^PYTHON_DEPS_OK=", out, useBytes = TRUE)) && !bad_alias
    missing <- grep("^PYTHON_MISSING=", out, value = TRUE, useBytes = TRUE)
    list(ok = ok, out = out, status = status, missing = if (length(missing)) sub("^PYTHON_MISSING=", "", missing[[1]]) else "")
  }

  resolve_python_command <- function(user_cmd) {
    check_script <- normalizePath(file.path("rag", "check_python_ok.py"), winslash = "/", mustWork = TRUE)
    checks <- list()
    best_missing <- NULL

    for (cand in candidate_python_commands(user_cmd)) {
      res <- check_python_command(cand, check_script)
      checks <- c(checks, list(list(candidate = cand, result = res)))
      if (isTRUE(res$ok)) return(cand)
      if (nzchar(res$missing) && is.null(best_missing)) {
        best_missing <- list(candidate = cand, missing = res$missing, out = res$out)
      }
    }

    details <- vapply(checks, function(x) {
      cand <- x$candidate
      res <- x$result
      paste0(
        "- ", paste(c(cand$command, cand$prefix_args), collapse = " "),
        " -> status ", res$status, "\n",
        paste(head(res$out, 8), collapse = "\n")
      )
    }, character(1))

    if (!is.null(best_missing)) {
      cmd_txt <- paste(c(best_missing$candidate$command, best_missing$candidate$prefix_args), collapse = " ")
      stop(paste(
        "Python sí se ha encontrado, pero faltan dependencias para generar informes.",
        paste0("Dependencias ausentes: ", best_missing$missing),
        "Instálalas desde la carpeta de la app con:",
        paste0('"', cmd_txt, '" -m pip install -r rag/requirements_minimal.txt'),
        "",
        "Comprobaciones realizadas:",
        paste(details, collapse = "\n\n"),
        sep = "\n"
      ))
    }

    stop(paste(
      "No se ha encontrado un ejecutable Python válido para generar informes.",
      "Con Anaconda, escribe en 'Ejecutable Python' la ruta completa del entorno, por ejemplo:",
      "C:/Users/aleja/anaconda3/envs/tfm-rag/python.exe",
      "",
      "También puedes fijarlo antes de lanzar Shiny:",
      "Sys.setenv(TFM_PYTHON = 'C:/Users/aleja/anaconda3/envs/tfm-rag/python.exe')",
      "",
      "Comprobaciones realizadas:",
      paste(details, collapse = "\n\n"),
      sep = "\n"
    ))
  }

  run_tfm_report <- function(report_type, player_ids, objective = "Comparativa general") {
    if (report_type == "individual") {
      shiny::validate(shiny::need(length(player_ids) >= 1, "Selecciona un jugador."))
      player_ids <- player_ids[1]
    } else {
      shiny::validate(shiny::need(length(player_ids) >= 2, "Selecciona al menos dos jugadores."))
      player_ids <- player_ids[seq_len(min(length(player_ids), 5))]
    }

    python <- resolve_python_command(input$report_python %||% "auto")

    script_path <- normalizePath(file.path("rag", "tfm_report_generator.py"), winslash = "/", mustWork = TRUE)
    data_dir <- normalizePath(file.path("data", "app"), winslash = "/", mustWork = TRUE)
    out_dir <- normalizePath(report_out_dir, winslash = "/", mustWork = FALSE)

    args <- c(python$prefix_args, script_path, "--data-dir", data_dir, "--report", report_type, "--out-dir", out_dir)
    for (pid in player_ids) {
      args <- c(args, "--player-id", as.character(pid))
    }
    if (report_type == "comparison") {
      objective_key <- objective %||% "general"
      objective_key <- dplyr::case_when(
        tolower(objective_key) %in% c("comparativa general", "general") ~ "general",
        tolower(objective_key) %in% c("fichaje") ~ "fichaje",
        tolower(objective_key) %in% c("renovación", "renovacion") ~ "renovacion",
        tolower(objective_key) %in% c("venta") ~ "venta",
        tolower(objective_key) %in% c("seguimiento") ~ "seguimiento",
        TRUE ~ as.character(objective_key)
      )
      args <- c(args, "--objective", objective_key)
    }
    # Generación determinista con RAG metodológico: sin agentes ni generación externa.

    out <- run_python_command(python$command, args)
    status <- attr(out, "status")
    if (is.null(status)) status <- 0
    json_line <- grep("^JSON_RESULT=", out, value = TRUE, useBytes = TRUE)
    if (length(json_line) == 0) {
      stop("No se recibió JSON_RESULT desde Python. Estado: ", status, "\nSalida:\n", paste(out, collapse = "\n"))
    }
    result <- jsonlite::fromJSON(sub("^JSON_RESULT=", "", tail(json_line, 1)), simplifyVector = TRUE)
    if (!isTRUE(result$ok)) {
      stop(result$error %||% paste(out, collapse = "\n"))
    }
    result$python_command <- paste(c(python$command, python$prefix_args), collapse = " ")

    preview_html <- NULL
    if (!is.null(result$html) && file.exists(result$html)) {
      preview_html <- paste(readLines(result$html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    }
    result$preview_html <- preview_html
    result$log <- paste(out, collapse = "\n")
    result
  }

  report_status_ui <- function(result) {
    if (is.null(result)) {
      return(div(class = "report-status report-status-idle", "Informe pendiente de generar."))
    }
    if (isTRUE(result$ok)) {
      return(div(class = "report-status report-status-ok", paste0("Generado: ", basename(result$html %||% result$markdown), " · listo para descargar")))
    }
    div(class = "report-status report-status-error", paste("Error:", result$error %||% "no disponible"))
  }

  report_preview_ui <- function(result) {
    if (is.null(result) || !isTRUE(result$ok)) return(NULL)
    if (is.null(result$preview_html) || !nzchar(result$preview_html)) {
      return(div(class = "report-status report-status-error", "No se pudo preparar la vista previa HTML."))
    }
    tags$iframe(class = "report-preview-frame", srcdoc = result$preview_html)
  }

  observeEvent(input$report_generate_individual, {
    report_individual_state(NULL)
    tryCatch({
      withProgress(message = "Generando informe individual...", value = 0.4, {
        result <- run_tfm_report("individual", as.integer(input$report_player))
        incProgress(1)
        report_individual_state(result)
      })
    }, error = function(e) {
      report_individual_state(list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  observeEvent(input$report_generate_comparison, {
    report_comparison_state(NULL)
    tryCatch({
      withProgress(message = "Generando informe comparativo...", value = 0.4, {
        result <- run_tfm_report("comparison", as.integer(input$report_compare_players), objective = input$report_objective)
        incProgress(1)
        report_comparison_state(result)
      })
    }, error = function(e) {
      report_comparison_state(list(ok = FALSE, error = conditionMessage(e)))
    })
  })

  output$report_status_individual <- renderUI(report_status_ui(report_individual_state()))
  output$report_status_comparison <- renderUI(report_status_ui(report_comparison_state()))
  output$report_preview_individual <- renderUI(report_preview_ui(report_individual_state()))
  output$report_preview_comparison <- renderUI(report_preview_ui(report_comparison_state()))

  output$download_report_individual_html <- downloadHandler(
    filename = function() basename(report_individual_state()$html %||% "informe_individual.html"),
    content = function(file) { req(report_individual_state()$html); file.copy(report_individual_state()$html, file, overwrite = TRUE) }
  )
  output$download_report_individual_md <- downloadHandler(
    filename = function() basename(report_individual_state()$markdown %||% "informe_individual.md"),
    content = function(file) { req(report_individual_state()$markdown); file.copy(report_individual_state()$markdown, file, overwrite = TRUE) }
  )
  output$download_report_individual_json <- downloadHandler(
    filename = function() basename(report_individual_state()$payload %||% "informe_individual_payload.json"),
    content = function(file) { req(report_individual_state()$payload); file.copy(report_individual_state()$payload, file, overwrite = TRUE) }
  )
  output$download_report_comparison_html <- downloadHandler(
    filename = function() basename(report_comparison_state()$html %||% "informe_comparativo.html"),
    content = function(file) { req(report_comparison_state()$html); file.copy(report_comparison_state()$html, file, overwrite = TRUE) }
  )
  output$download_report_comparison_md <- downloadHandler(
    filename = function() basename(report_comparison_state()$markdown %||% "informe_comparativo.md"),
    content = function(file) { req(report_comparison_state()$markdown); file.copy(report_comparison_state()$markdown, file, overwrite = TRUE) }
  )
  output$download_report_comparison_json <- downloadHandler(
    filename = function() basename(report_comparison_state()$payload %||% "informe_comparativo_payload.json"),
    content = function(file) { req(report_comparison_state()$payload); file.copy(report_comparison_state()$payload, file, overwrite = TRUE) }
  )

  observeEvent(input$position_filter, {
    valid_roles <- roles_for_positions(input$position_filter)
    selected <- intersect(input$role_filter %||% valid_roles, valid_roles)
    updateSelectInput(session, "role_filter", choices = valid_roles, selected = selected)
  }, ignoreInit = FALSE)

  observeEvent(input$search_position, {
    valid_roles <- if (is.null(input$search_position) || input$search_position == "Todas") role_choices else roles_for_positions(input$search_position)
    updateSelectInput(session, "search_role", choices = c("Todos", valid_roles), selected = "Todos")
  }, ignoreInit = FALSE)

  observeEvent(input$est_position, {
    valid_roles <- roles_for_positions(input$est_position)
    updateSelectInput(session, "est_role", choices = valid_roles, selected = valid_roles[1])
  }, ignoreInit = FALSE)

  # Estado interno del selector de métricas del comparador.
  # Se evita el tabsetPanel dinámico anterior porque en algunas sesiones de
  # Windows/Shiny podía devolver un índice vectorial y provocar el error:
  # "attempt to select more than one element in vector Index".
  compare_metric_state <- reactiveValues(
    selected = unique(intersect(compare_metric_default, all_compare_metric_cols)),
    current_group = names(compare_metric_groups)[1]
  )

  selected_compare_metric_cols <- reactive({
    vals <- compare_metric_state$selected %||% compare_metric_default
    vals <- unique(intersect(as.character(vals), all_compare_metric_cols))
    vals
  })

  set_compare_metric_selection <- function(selected) {
    compare_metric_state$selected <- unique(intersect(as.character(selected %||% character(0)), all_compare_metric_cols))
  }

  get_compare_current_group <- function() {
    grp <- as.character(compare_metric_state$current_group %||% names(compare_metric_groups)[1])
    grp <- grp[!is.na(grp) & nzchar(grp)]
    if (length(grp) == 0 || !grp[1] %in% names(compare_metric_groups)) {
      names(compare_metric_groups)[1]
    } else {
      grp[1]
    }
  }

  output$compare_metric_tabs_ui <- renderUI({
    current_group <- get_compare_current_group()
    tab_buttons <- lapply(names(compare_metric_groups), function(grp) {
      btn_id <- paste0("compare_metric_nav_", gsub("[^A-Za-z0-9_]", "_", grp))
      actionButton(
        btn_id,
        label = grp,
        class = paste("metric-tab-btn", if (identical(grp, current_group)) "active" else "")
      )
    })

    cols <- compare_metric_groups[[current_group]]
    selected <- intersect(selected_compare_metric_cols(), cols)
    choices <- stats::setNames(cols, unname(compare_metric_labels[cols]))

    div(
      class = "metric-selector-tabs-wrapper",
      div(class = "metric-block-nav", tab_buttons),
      div(class = "metric-block-panel metric-block-current",
        tags$p(class = "small-note", HTML(paste0(
          "Bloque activo: <b>", htmltools::htmlEscape(current_group), "</b> · ",
          length(cols), " métricas disponibles. Marca las variables que quieras incluir en el radar."
        ))),
        checkboxGroupInput(
          inputId = "compare_metric_current_checks",
          label = NULL,
          choices = choices,
          selected = selected,
          inline = FALSE
        )
      )
    )
  })

  for (grp_i in names(compare_metric_groups)) {
    local({
      grp <- grp_i
      btn_id <- paste0("compare_metric_nav_", gsub("[^A-Za-z0-9_]", "_", grp))
      observeEvent(input[[btn_id]], {
        compare_metric_state$current_group <- grp
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$compare_metric_current_checks, {
    grp <- get_compare_current_group()
    cols <- compare_metric_groups[[grp]]
    checked <- unique(intersect(as.character(input$compare_metric_current_checks %||% character(0)), cols))
    current <- selected_compare_metric_cols()
    set_compare_metric_selection(unique(c(setdiff(current, cols), checked)))
  }, ignoreInit = TRUE)

  observeEvent(input$compare_select_current_block, {
    grp <- get_compare_current_group()
    current <- selected_compare_metric_cols()
    set_compare_metric_selection(unique(c(current, compare_metric_groups[[grp]])))
  }, ignoreInit = TRUE)

  observeEvent(input$compare_clear_current_block, {
    grp <- get_compare_current_group()
    current <- selected_compare_metric_cols()
    set_compare_metric_selection(setdiff(current, compare_metric_groups[[grp]]))
  }, ignoreInit = TRUE)

  observeEvent(input$compare_select_default_metrics, {
    set_compare_metric_selection(compare_metric_default)
  }, ignoreInit = TRUE)

  observeEvent(input$compare_select_all_metrics, {
    set_compare_metric_selection(all_compare_metric_cols)
  }, ignoreInit = TRUE)

  observeEvent(input$compare_clear_metrics, {
    set_compare_metric_selection(character(0))
  }, ignoreInit = TRUE)

  output$compare_metric_selected_note <- renderUI({
    selected <- selected_compare_metric_cols()
    selected_preview <- compare_metric_lookup |>
      filter(metric_col %in% selected) |>
      arrange(metric_group, metric_label) |>
      mutate(txt = paste0("<span class='metric-chip'>", htmltools::htmlEscape(metric_group), " · ", htmltools::htmlEscape(metric_label), "</span>")) |>
      pull(txt)
    preview <- if (length(selected_preview) == 0) {
      "<span class='small-note'>Ninguna métrica seleccionada.</span>"
    } else {
      paste(head(selected_preview, 18), collapse = " ")
    }
    if (length(selected_preview) > 18) preview <- paste0(preview, " <span class='small-note'>+", length(selected_preview) - 18, " más</span>")

    div(class = "small-note metric-selected-note",
      HTML(paste0(
        "Métricas seleccionadas: <b>", length(selected), "</b>. ",
        "Para máxima legibilidad del radar, trabaja normalmente con 6-14 ejes; si seleccionas muchos, el gráfico seguirá funcionando pero las etiquetas serán más densas.<br>",
        preview
      ))
    )
  })

  compare_allowed_ids <- reactive({
    selected <- suppressWarnings(as.integer(input$compare_players %||% character()))
    selected <- selected[is.finite(selected)]
    eligible <- players$player_id
    if (length(selected) > 0) {
      if (isTRUE(input$compare_common_positions)) {
        pos_split <- roles_pca |>
          filter(player_id %in% selected) |>
          group_by(player_id) |>
          summarise(vals = list(unique(as.character(position))), .groups = "drop") |>
          pull(vals)
        common_pos <- if (length(pos_split) > 0) Reduce(intersect, pos_split) else character()
        if (length(common_pos) > 0) {
          eligible <- intersect(eligible, roles_pca |> filter(position %in% common_pos) |> pull(player_id) |> unique())
        } else {
          eligible <- selected
        }
      }
      if (isTRUE(input$compare_common_roles)) {
        role_split <- roles_pca |>
          filter(player_id %in% selected) |>
          group_by(player_id) |>
          summarise(vals = list(unique(as.character(role))), .groups = "drop") |>
          pull(vals)
        common_roles <- if (length(role_split) > 0) Reduce(intersect, role_split) else character()
        if (length(common_roles) > 0) {
          eligible <- intersect(eligible, roles_pca |> filter(role %in% common_roles) |> pull(player_id) |> unique())
        } else {
          eligible <- selected
        }
      }
    }
    unique(c(eligible, selected))
  })

  output$compare_players_ui <- renderUI({
    eligible <- compare_allowed_ids()
    choices_tbl <- player_choices |>
      filter(as.integer(value) %in% eligible) |>
      arrange(label)
    selected <- input$compare_players %||% as.character(head(players$player_id, 4))
    selected <- intersect(as.character(selected), choices_tbl$value)
    if (length(selected) == 0 && nrow(choices_tbl) > 0) {
      selected <- as.character(head(choices_tbl$value, min(3, nrow(choices_tbl))))
    }
    selectizeInput(
      "compare_players", "Jugadores a comparar",
      choices = setNames(choices_tbl$value, choices_tbl$label),
      selected = selected,
      multiple = TRUE,
      options = list(maxItems = 8, maxOptions = 3000, plugins = list("remove_button"))
    )
  })


  output$compare_color_ui <- renderUI({
    ids <- suppressWarnings(as.integer(input$compare_players %||% character()))
    ids <- ids[is.finite(ids)]
    if (length(ids) == 0) {
      return(div(class = "small-note", "Selecciona jugadores para personalizar los colores del radar."))
    }
    selected_players <- players |>
      filter(player_id %in% ids) |>
      mutate(.order = match(player_id, ids)) |>
      arrange(.order) |>
      select(player_id, player_name)

    controls <- lapply(seq_len(nrow(selected_players)), function(i) {
      id <- selected_players$player_id[i]
      default_col <- radar_default_palette[((i - 1) %% length(radar_default_palette)) + 1]
      column(
        3,
        div(
          class = "compare-color-control",
          colourpicker::colourInput(
            inputId = paste0("compare_color_", id),
            label = selected_players$player_name[i],
            value = input[[paste0("compare_color_", id)]] %||% default_col,
            showColour = "both",
            allowTransparent = FALSE,
            palette = "square"
          )
        )
      )
    })
    tagList(
      div(class = "small-note compare-color-note", "Color del área y de la línea de cada jugador. Se aplica de forma consistente a los dos radar plots."),
      do.call(fluidRow, controls)
    )
  })

  compare_player_colors <- reactive({
    ids <- suppressWarnings(as.integer(input$compare_players %||% character()))
    ids <- ids[is.finite(ids)]
    if (length(ids) == 0) return(NULL)
    selected_players <- players |>
      filter(player_id %in% ids) |>
      mutate(.order = match(player_id, ids)) |>
      arrange(.order) |>
      select(player_id, player_name)
    cols <- vapply(seq_len(nrow(selected_players)), function(i) {
      id <- selected_players$player_id[i]
      input[[paste0("compare_color_", id)]] %||% radar_default_palette[((i - 1) %% length(radar_default_palette)) + 1]
    }, character(1))
    stats::setNames(cols, selected_players$player_name)
  })

  # El selector del comparador se genera con renderUI para evitar bucles de actualización
  # que en algunas sesiones dejaban el campo vacío.

  pred_columns <- reactive({
    if (identical(input$prediction_type, "raw")) {
      list(pred = "pred_raw_mill", diff = "diff_raw_mill", ratio = "ratio_raw", label = "Original")
    } else {
      list(pred = "pred_iso_mill", diff = "diff_iso_mill", ratio = "ratio_iso", label = "Isotónica")
    }
  })

  filtered_players <- reactive({
    df <- players
    if (length(input$position_filter %||% character()) > 0) df <- df |> filter(primary_position %in% input$position_filter)
    if (length(input$team_filter %||% character()) > 0) df <- df |> filter(team %in% input$team_filter)
    if (length(input$role_filter %||% character()) > 0) df <- df |> filter(primary_role %in% input$role_filter)
    if (!is.null(input$value_range)) df <- df |> filter(market_value_mill >= input$value_range[1], market_value_mill <= input$value_range[2])
    if (!is.null(input$split_filter) && input$split_filter != "Todas") df <- df |> filter(split == input$split_filter)
    if (isTRUE(input$only_with_load)) df <- df |> filter(!is.na(risk_load_excess))
    df
  })

  selected_player_df <- reactive({
    req(input$selected_player)
    players |> filter(player_id == as.integer(input$selected_player))
  })

  filtered_roles <- reactive({
    ids <- filtered_players()$player_id
    roles_pca |> filter(player_id %in% ids)
  })

  output$kpi_cards <- renderUI({
    df <- filtered_players()
    pred <- pred_columns()
    mae <- mean(abs(df[[pred$diff]]), na.rm = TRUE)
    div(
      class = "kpi-grid",
      kpi_card("Jugadores", nrow(df), "muestra filtrada"),
      kpi_card("Valor mediano", fmt_mill(median(df$market_value_mill, na.rm = TRUE)), "observado"),
      kpi_card("MAE predicción", fmt_mill(mae), pred$label),
      kpi_card("Riesgo hist. medio", fmt_pct(mean(df$risk_hist_excess, na.rm = TRUE)), "exceso"),
      kpi_card("Riesgo carga medio", fmt_pct(mean(df$risk_load_excess, na.rm = TRUE)), "exceso")
    )
  })

  output$market_distribution <- renderPlotly({
    df <- filtered_players()
    shiny::validate(shiny::need(nrow(df) > 1, "No hay jugadores suficientes con los filtros actuales."))
    p <- ggplot(df, aes(x = market_value_mill, text = paste0("Valor: ", round(market_value_mill, 1), " M€"))) +
      geom_histogram(bins = 35, fill = APP_BLUE, color = "white", alpha = 0.88) +
      scale_x_continuous(labels = label_number(suffix = " M€")) +
      labs(title = "Distribución del valor de mercado", x = "Valor observado", y = "Jugadores") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$value_by_position <- renderPlotly({
    df <- filtered_players()
    shiny::validate(shiny::need(nrow(df) > 1, "No hay jugadores suficientes."))
    df <- df |> mutate(primary_position = factor(as.character(primary_position), levels = position_order))
    p <- ggplot(df, aes(x = primary_position, y = market_value_mill, text = paste0(player_name, "<br>", team, "<br>", round(market_value_mill, 1), " M€"))) +
      geom_boxplot(fill = APP_BLUE, alpha = 0.18, outlier.alpha = 0.25) +
      geom_jitter(width = 0.16, alpha = 0.45, size = 1.6, color = APP_BLUE) +
      scale_y_continuous(trans = "log10", labels = label_number(suffix = " M€")) +
      labs(title = "Valor por posición", x = "Posición", y = "Valor observado, escala log") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$value_by_role <- renderPlotly({
    df <- filtered_players() |>
      filter(!is.na(primary_role), !is.na(market_value_mill))
    shiny::validate(shiny::need(nrow(df) > 1, "No hay jugadores suficientes."))
    role_order <- df |>
      group_by(primary_role) |>
      summarise(mediana = median(market_value_mill, na.rm = TRUE), .groups = "drop") |>
      arrange(mediana) |>
      pull(primary_role)
    p <- ggplot(df, aes(x = factor(primary_role, levels = role_order), y = market_value_mill, text = paste0(player_name, "<br>", team, "<br>", primary_position, " - ", primary_role, "<br>", round(market_value_mill, 1), " M€"))) +
      geom_boxplot(fill = APP_BLUE, alpha = 0.18, outlier.alpha = 0.25) +
      geom_jitter(width = 0.16, alpha = 0.45, size = 1.4, color = APP_BLUE) +
      coord_flip() +
      scale_y_continuous(trans = "log10", labels = label_number(suffix = " M€")) +
      labs(title = "Valor por rol", x = "Rol", y = "Valor observado, escala log") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$team_group_value <- renderPlotly({
    df <- filtered_players() |>
      filter(!is.na(team_group_model)) |>
      group_by(team_group_model) |>
      summarise(n = n(), mediana = median(market_value_mill, na.rm = TRUE), media = mean(market_value_mill, na.rm = TRUE), .groups = "drop")
    shiny::validate(shiny::need(nrow(df) > 0, "No hay datos de grupo de equipo."))
    p <- ggplot(df, aes(x = reorder(team_group_model, mediana), y = mediana, text = paste0(team_group_model, "<br>Mediana: ", round(mediana, 1), " M€<br>n=", n))) +
      geom_col(fill = APP_BLUE) +
      coord_flip() +
      labs(title = "Valor mediano por grupo de equipo", x = NULL, y = "M€") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$age_value_plot <- renderPlotly({
    df <- filtered_players() |> filter(!is.na(age), !is.na(market_value_mill))
    shiny::validate(shiny::need(nrow(df) > 2, "No hay datos suficientes."))
    p <- ggplot(df, aes(x = age, y = market_value_mill, size = minutes_2324, color = contract_years, text = paste0(player_name, "<br>", team, "<br>Edad: ", age, "<br>Valor: ", round(market_value_mill, 1), " M€<br>Minutos: ", round(minutes_2324, 0)))) +
      geom_point(alpha = 0.75) +
      geom_smooth(method = "loess", se = FALSE, color = "black") +
      scale_y_continuous(trans = "log10", labels = label_number(suffix = " M€")) +
      labs(title = "Edad, minutos y contrato", x = "Edad", y = "Valor observado, escala log", color = "Contrato", size = "Minutos") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$players_table <- renderDT({
    df <- filtered_players() |>
      transmute(
        Jugador = player_name,
        Equipo = team,
        Posiciones = all_positions,
        Roles = all_roles,
        Edad = age,
        `Valor M€` = round(market_value_mill, 2),
        `Pred. iso M€` = round(pred_iso_mill, 2),
        `Error M€` = round(diff_iso_mill, 2),
        Split = split,
        Minutos = round(minutes_2324, 0),
        `Riesgo hist. %` = round(risk_hist_pct, 2),
        `Riesgo carga %` = round(risk_load_pct, 2),
        `Contrato años` = contract_years
      ) |>
      arrange(desc(`Valor M€`))
    make_dt(df)
  })

  output$role_axis_ui <- renderUI({
    df <- role_axis_interpretation |> filter(position == input$role_position)
    if (nrow(df) == 0) return(NULL)
    div(
      class = "info-box compact-info",
      HTML(paste0(
        "<b>Interpretación PCA (", htmltools::htmlEscape(input$role_position), ")</b><br>",
        "PC1 < 0: ", htmltools::htmlEscape(df$pc1_negative[1]), " · PC1 ≥ 0: ", htmltools::htmlEscape(df$pc1_positive[1]), "<br>",
        "PC2 < 0: ", htmltools::htmlEscape(df$pc2_negative[1]), " · PC2 ≥ 0: ", htmltools::htmlEscape(df$pc2_positive[1])
      ))
    )
  })

  output$role_scatter <- renderPlotly({
    df <- filtered_roles() |> filter(position == input$role_position)
    shiny::validate(shiny::need(nrow(df) > 2, "No hay datos de roles suficientes para esta posición."))
    selected <- as.integer(input$selected_player)
    df <- df |>
      mutate(
        label = ifelse(player_id == selected | isTRUE(input$label_role_players), player_name, ""),
        pct_events = 100 * as.numeric(event_share)
      )
    axis_info <- role_axis_interpretation |> filter(position == input$role_position)
    x_lab <- "PC1"
    y_lab <- "PC2"
    subtitle_txt <- NULL
    if (nrow(axis_info) > 0) {
      x_lab <- paste0("PC1: ", axis_info$pc1_negative[1], " ← 0 → ", axis_info$pc1_positive[1])
      y_lab <- paste0("PC2: ", axis_info$pc2_negative[1], " ← 0 → ", axis_info$pc2_positive[1])
      subtitle_txt <- paste0("PC1 < 0 = ", axis_info$pc1_negative[1], "; PC1 ≥ 0 = ", axis_info$pc1_positive[1], " | PC2 < 0 = ", axis_info$pc2_negative[1], "; PC2 ≥ 0 = ", axis_info$pc2_positive[1])
    }
    p <- ggplot(df, aes(x = PC1, y = PC2, color = role, text = paste0(player_name, "<br>", team, "<br>Rol: ", role, "<br>Cluster: ", cluster, "<br>Eventos: ", round(pct_events, 1), "%"))) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey65") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey65") +
      geom_point(aes(size = pct_events), alpha = 0.78) +
      ggrepel::geom_text_repel(aes(label = label), size = ifelse(isTRUE(input$label_role_players), 2.5, 3), max.overlaps = Inf, min.segment.length = 0, box.padding = 0.25, show.legend = FALSE) +
      labs(title = paste("Mapa de roles", input$role_position), subtitle = subtitle_txt, x = x_lab, y = y_lab, color = "Rol", size = "% eventos") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$role_premium <- renderPlotly({
    df <- role_summary |> filter(position == input$role_position) |> arrange(premium_vs_position_mill)
    shiny::validate(shiny::need(nrow(df) > 0, "No hay resumen de roles."))
    p <- ggplot(df, aes(x = reorder(role, premium_vs_position_mill), y = premium_vs_position_mill, text = paste0(role, "<br>n=", n_players, "<br>Prima: ", round(premium_vs_position_mill, 2), " M€"))) +
      geom_col(fill = APP_BLUE) +
      coord_flip() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Prima mediana frente a la posición", x = NULL, y = "Prima M€") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$loadings_heatmap <- renderPlotly({
    df <- role_loadings |> filter(position == input$role_position, PC %in% c("PC1", "PC2", "PC3", "PC4"))
    shiny::validate(shiny::need(nrow(df) > 0, "No hay cargas PCA."))
    p <- ggplot(df, aes(x = event_name, y = PC, fill = loading, text = paste0(event_name, "<br>", PC, ": ", round(loading, 3), "<br>Var.: ", round(100 * explained_variance, 1), "%"))) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#313695", mid = "#ffffbf", high = "#a50026", midpoint = 0) +
      labs(title = "Cargas PCA estimadas", subtitle = "Heatmap interpretativo desde matriz de eventos app-ready", x = NULL, y = NULL, fill = "Loading") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  output$pitch_events_plot <- renderPlotly({
    sid <- as.integer(input$selected_player)
    df <- events_pitch |>
      filter(player_id == sid, event_name %in% role_event_vars_all, is.finite(x), is.finite(y))
    if (length(input$pitch_event_filter %||% character()) > 0) {
      df <- df |> filter(event_name %in% input$pitch_event_filter)
    }
    shiny::validate(shiny::need(nrow(df) > 0, "No hay eventos para el jugador y filtro seleccionados."))
    if (!is.null(input$pitch_max_events) && nrow(df) > input$pitch_max_events) {
      set.seed(12345)
      df <- df |> slice_sample(n = input$pitch_max_events)
    }

    sectors_plot <- pitch_sectors |> mutate(zona = "normal")
    if (isTRUE(input$pitch_show_sectors)) {
      sectores_jugador <- events_pitch |>
        filter(player_id == sid, event_name %in% role_event_vars_all, sector_en_posicion) |>
        pull(sector) |>
        unique()
      sectors_plot$zona[sectors_plot$cell %in% sectores_jugador] <- "jugador"
    }
    if (isTRUE(input$pitch_show_neighbors)) {
      sectores_vecinos <- events_pitch |>
        filter(player_id == sid, event_name %in% role_event_vars_all, sector_en_vecino) |>
        pull(sector) |>
        unique()
      sectores_vecinos <- setdiff(sectores_vecinos, sectors_plot$cell[sectors_plot$zona == "jugador"])
      sectors_plot$zona[sectors_plot$cell %in% sectores_vecinos] <- "vecino"
    }

    circle_df <- tibble(theta = seq(0, 2*pi, length.out = 200), x = field_length/2 + 9.15*cos(theta), y = field_width/2 + 9.15*sin(theta))
    grid_x <- sort(unique(c(sectors_plot$xmin, sectors_plot$xmax))); grid_x <- grid_x[grid_x > 0 & grid_x < field_length]
    grid_y <- sort(unique(c(sectors_plot$ymin, sectors_plot$ymax))); grid_y <- grid_y[grid_y > 0 & grid_y < field_width]

    p <- ggplot() +
      geom_rect(data = sectors_plot, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = zona), color = "white", linewidth = 0.35) +
      scale_fill_manual(values = c("normal" = "#2E8B57", "vecino" = "#2E7D32", "jugador" = "#1B5E20"), guide = "none") +
      geom_vline(xintercept = grid_x, colour = "white", linewidth = 0.35) +
      geom_hline(yintercept = grid_y, colour = "white", linewidth = 0.35) +
      geom_rect(aes(xmin = 0, xmax = field_length, ymin = 0, ymax = field_width), fill = NA, color = "white", linewidth = 1) +
      geom_vline(xintercept = field_length / 2, color = "white", linewidth = 0.9) +
      geom_path(data = circle_df, aes(x = x, y = y), color = "white", linewidth = 0.8) +
      geom_point(aes(x = field_length/2, y = field_width/2), color = "white", size = 1.2) +
      geom_rect(aes(xmin = 0, xmax = 18, ymin = 18, ymax = 62), fill = NA, color = "white", linewidth = 0.8) +
      geom_rect(aes(xmin = field_length - 18, xmax = field_length, ymin = 18, ymax = 62), fill = NA, color = "white", linewidth = 0.8) +
      geom_rect(aes(xmin = 0, xmax = 6, ymin = 30, ymax = 50), fill = NA, color = "white", linewidth = 0.8) +
      geom_rect(aes(xmin = field_length - 6, xmax = field_length, ymin = 30, ymax = 50), fill = NA, color = "white", linewidth = 0.8) +
      geom_text(data = sectors_plot, aes(x = xcenter, y = ycenter, label = cell), colour = "white", size = 3.5, fontface = "bold", alpha = 0.9) +
      geom_point(data = df, aes(x = x, y = y, colour = event_name, text = paste0(event_name, "<br>", match, "<br>", game_date, "<br>Sector: ", sector, "<br>Posición evento: ", posicion_evento)), size = 1.5, alpha = 0.8) +
      coord_fixed(xlim = c(0, field_length), ylim = c(0, field_width), expand = FALSE) +
      scale_x_continuous(breaks = seq(0, field_length, by = 20)) +
      scale_y_continuous(breaks = seq(0, field_width, by = 10)) +
      labs(title = paste("Eventos de", unique(df$player_name)[1]), x = "Largo (m)", y = "Ancho (m)", colour = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.background = element_rect(fill = "#2E8B57", colour = NA), plot.background = element_rect(fill = "#2E8B57", colour = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(colour = "white", face = "bold"), axis.text = element_text(colour = "white"), plot.title = element_text(colour = "white", face = "bold", hjust = 0.5), legend.background = element_rect(fill = "#2E8B57", colour = NA), legend.key = element_rect(fill = "#2E8B57", colour = NA), legend.text = element_text(colour = "white"))
    ggplotly(p, tooltip = "text")
  })

  output$roles_table <- renderDT({
    df <- filtered_roles() |>
      transmute(
        Jugador = player_name,
        Equipo = team,
        Posición = position,
        Cluster = cluster,
        Rol = role,
        Interpretación = interpretation,
        `% eventos` = round(100 * event_share, 1),
        `Valor M€` = round(market_value_mill, 2),
        PC1 = round(PC1, 3),
        PC2 = round(PC2, 3)
      ) |>
      arrange(Posición, Rol, Jugador)
    make_dt(df)
  })

  output$player_risk_card <- renderUI({
    df <- selected_player_df()
    req(nrow(df) == 1)
    div(
      class = "kpi-grid",
      kpi_card("Jugador", df$player_name[1], paste(df$team[1], df$primary_position[1], df$primary_role[1], sep = " · ")),
      kpi_card("P(historial)", fmt_pct(df$prob_lesion_lasso[1]), paste("Grupo:", df$grupo_riesgo_lasso[1] %||% "-")),
      kpi_card("Exceso historial", fmt_pct(df$risk_hist_excess[1]), "LASSO"),
      kpi_card("Incidencia", fmt_num(df$incidencia[1], 1), "lesiones / 1000 h"),
      kpi_card("Días última lesión", fmt_num(df$dias_desde_ultima_lesion[1], 0), "historial médico")
    )
  })

  output$player_load_card <- renderUI({
    df <- selected_player_df()
    req(nrow(df) == 1)
    div(
      class = "kpi-grid",
      kpi_card("Jugador", df$player_name[1], paste(df$team[1], df$primary_position[1], df$primary_role[1], sep = " · ")),
      kpi_card("P(carga)", fmt_pct(df$prob_lesion_temporada[1]), paste("Ventanas:", df$n_ventanas_modelo[1] %||% "-")),
      kpi_card("Exceso carga", fmt_pct(df$risk_load_excess[1]), "XGBoost calibrado"),
      kpi_card("P última ventana", fmt_pct(df$prob_lesion_ultima[1]), "carga reciente"),
      kpi_card("P máxima ventana", fmt_pct(df$prob_lesion_max[1]), "máximo observado")
    )
  })

  output$risk_scatter <- renderPlotly({
    df <- filtered_players() |>
      filter(!is.na(risk_hist_pct), !is.na(risk_load_pct))
    shiny::validate(shiny::need(nrow(df) > 2, "No hay suficientes jugadores con ambos riesgos."))
    p <- ggplot(df, aes(x = risk_hist_pct, y = risk_load_pct, color = injured_2324_hist_label, size = market_value_mill, text = paste0(player_name, "<br>", team, "<br>Valor: ", round(market_value_mill, 1), " M€<br>Hist: ", round(risk_hist_pct, 2), "%<br>Carga: ", round(risk_load_pct, 2), "%<br>Lesionado: ", injured_2324_hist_label))) +
      geom_point(alpha = 0.75) +
      scale_color_manual(values = c("Sí" = APP_RED, "No" = APP_GREEN, "Sin dato" = APP_BLUE)) +
      labs(title = "Exceso de riesgo: historial vs carga", x = "Exceso historial (%)", y = "Exceso carga (%)", color = "Lesión 23/24", size = "Valor M€") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  hist_lasso_data <- reactive({
    ids <- filtered_players()$player_id
    df <- players |>
      filter(player_id %in% ids) |>
      select(player_id, player_name, team, any_of(c("lesionado", "incidencia", "dias_desde_ultima_lesion")))

    use_fallback <- !all(c("incidencia", "dias_desde_ultima_lesion") %in% names(df)) ||
      all(is.na(df$incidencia)) || all(is.na(df$dias_desde_ultima_lesion))

    if (use_fallback) {
      df <- p_lesion_context |>
        filter(player_id %in% ids) |>
        select(player_id, player_name, team, lesionado, incidencia, dias_desde_ultima_lesion)
    }

    df |>
      mutate(
        lesionado = as.character(lesionado),
        incidencia = suppressWarnings(as.numeric(incidencia)),
        dias_desde_ultima_lesion = suppressWarnings(as.numeric(dias_desde_ultima_lesion)),
        estado_lesion = ifelse(lesionado %in% c("X1", "1", "TRUE", "true", "Sí", "Si"), "Lesionado", "No lesionado"),
        estado_lesion = factor(estado_lesion, levels = c("No lesionado", "Lesionado"))
      )
  })

  output$hist_days_density <- renderPlot({
    df <- hist_lasso_data() |>
      filter(!is.na(dias_desde_ultima_lesion), !is.na(estado_lesion)) |>
      group_by(estado_lesion) |>
      filter(n() >= 2) |>
      ungroup()
    shiny::validate(shiny::need(nrow(df) > 2, "No hay datos suficientes de días desde última lesión."))
    ggplot(df, aes(x = dias_desde_ultima_lesion, fill = estado_lesion, color = estado_lesion)) +
      geom_density(alpha = 0.30, linewidth = 0.9, adjust = 1.05, na.rm = TRUE) +
      scale_fill_manual(values = c("No lesionado" = APP_GREEN, "Lesionado" = APP_RED), drop = FALSE) +
      scale_color_manual(values = c("No lesionado" = APP_GREEN, "Lesionado" = APP_RED), drop = FALSE) +
      scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      labs(
        title = "Densidad de días desde la última lesión",
        x = "Días desde última lesión",
        y = "Densidad",
        fill = "Estado 23/24",
        color = "Estado 23/24"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$hist_incidence_violin <- renderPlot({
    df <- hist_lasso_data() |>
      filter(!is.na(incidencia), !is.na(estado_lesion))
    shiny::validate(shiny::need(nrow(df) > 2, "No hay datos suficientes de incidencia."))
    ggplot(df, aes(x = estado_lesion, y = incidencia, fill = estado_lesion, color = estado_lesion)) +
      geom_violin(alpha = 0.30, trim = FALSE, na.rm = TRUE) +
      geom_boxplot(width = 0.13, alpha = 0.55, outlier.alpha = 0.25, na.rm = TRUE) +
      geom_jitter(width = 0.08, alpha = 0.35, size = 1.1, na.rm = TRUE) +
      scale_fill_manual(values = c("No lesionado" = APP_GREEN, "Lesionado" = APP_RED), drop = FALSE) +
      scale_color_manual(values = c("No lesionado" = APP_GREEN, "Lesionado" = APP_RED), drop = FALSE) +
      labs(
        title = "Distribución de la incidencia lesional",
        x = NULL,
        y = "Lesiones por 1000 horas",
        fill = "Estado 23/24",
        color = "Estado 23/24"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$risk_hist_distribution <- renderPlot({
    df <- filtered_players() |> filter(!is.na(risk_hist_pct))
    shiny::validate(shiny::need(nrow(df) > 2, "No hay datos suficientes de exceso histórico."))
    ggplot(df, aes(x = risk_hist_pct)) +
      geom_density(fill = APP_BLUE, color = APP_BLUE, alpha = 0.35, linewidth = 0.9, na.rm = TRUE) +
      labs(title = "Densidad del exceso de riesgo por historial", x = "%", y = "Densidad") +
      theme_minimal(base_size = 12)
  })

  output$risk_load_distribution <- renderPlot({
    df <- filtered_players() |> filter(!is.na(risk_load_pct))
    shiny::validate(shiny::need(nrow(df) > 2, "No hay datos suficientes de exceso por carga."))
    ggplot(df, aes(x = risk_load_pct)) +
      geom_density(fill = APP_BLUE, color = APP_BLUE, alpha = 0.35, linewidth = 0.9, na.rm = TRUE) +
      labs(title = "Densidad del exceso de riesgo por carga", x = "%", y = "Densidad") +
      theme_minimal(base_size = 12)
  })

  output$body_map <- renderPlot({
    req(input$selected_player)
    sid <- as.integer(input$selected_player)
    player_nm <- players |>
      filter(player_id == sid) |>
      pull(player_name) |>
      dplyr::first()
    if (length(player_nm) == 0 || is.na(player_nm)) player_nm <- "Jugador seleccionado"

    df <- injuries_long |>
      filter(player_id == sid) |>
      mutate(season_injured = normaliza_temporada(season_injured))

    min_days <- suppressWarnings(as.numeric(input$body_min_duration %||% 0))
    plot_injury_fillmap_by_player_app(
      data = df,
      player_name = player_nm,
      value = input$body_value %||% "count",
      mode = input$body_mode %||% "percentage",
      zones_keep = input$body_zones %||% unname(body_zone_choices),
      seasons_keep = input$body_seasons %||% character(),
      min_duration_days = ifelse(is.finite(min_days) && min_days > 0, min_days, NA_real_),
      show_labels = isTRUE(input$body_show_labels)
    )
  })

  output$injuries_by_season <- renderPlotly({
    sid <- as.integer(input$selected_player)
    df <- injuries_long |>
      filter(player_id == sid, !is.na(season_injured)) |>
      count(season_injured, sort = FALSE) |>
      arrange(season_injured)
    shiny::validate(shiny::need(nrow(df) > 0, "Sin lesiones registradas para el jugador seleccionado."))
    p <- ggplot(df, aes(x = season_injured, y = n, text = paste0(season_injured, ": ", n))) +
      geom_col(fill = APP_BLUE) +
      labs(title = "Lesiones por temporada", x = "Temporada", y = "Lesiones") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  output$injuries_by_group <- renderPlotly({
    sid <- as.integer(input$selected_player)
    df <- injuries_long |>
      filter(player_id == sid, !is.na(injury_group)) |>
      count(injury_group, sort = TRUE)
    shiny::validate(shiny::need(nrow(df) > 0, "Sin lesiones registradas para el jugador seleccionado."))
    p <- ggplot(df, aes(x = reorder(injury_group, n), y = n, text = paste0(injury_group, ": ", n))) +
      geom_col(fill = APP_BLUE) +
      coord_flip() +
      labs(title = "Lesiones por grupo", x = NULL, y = "Lesiones") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$selected_injuries_table <- renderDT({
    sid <- as.integer(input$selected_player)
    df <- injuries_long |>
      filter(player_id == sid) |>
      arrange(desc(date_from)) |>
      transmute(Temporada = season_injured, Lesión = injury, Grupo = injury_group, Desde = date_from, Hasta = date_until, `Días` = duration_days, `Partidos perdidos` = games_missed)
    make_dt(df, page_length = 8)
  })

  output$physical_timeline <- renderPlotly({
    sid <- as.integer(input$selected_player)
    metric <- input$physical_metric
    metric_label <- names(metric_choices)[metric_choices == metric]
    df <- physical_match |>
      filter(player_id == sid, !is.na(.data[[metric]])) |>
      arrange(date)
    shiny::validate(shiny::need(nrow(df) > 0, "No hay datos físicos por partido para el jugador seleccionado."))
    df <- df |>
      mutate(injury_status = dplyr::coalesce(injured_window_label, "Sin dato"))
    p <- ggplot(df, aes(x = date, y = .data[[metric]], group = 1, text = paste0(match, "<br>", date, "<br>", metric_label, ": ", round(.data[[metric]], 2), "<br>Min: ", round(minutes, 0), "<br>Ventana lesionada: ", injury_status))) +
      geom_line(color = "black", linewidth = 0.8, alpha = 0.9) +
      geom_point(aes(color = injury_status), size = 2.5) +
      scale_color_manual(values = c("Sí" = APP_RED, "No" = APP_GREEN, "Sin dato" = APP_BLUE)) +
      labs(title = paste("Evolución física:", unique(df$player_name)[1]), x = NULL, y = metric_label, color = "Ventana lesionada") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$physical_position_box <- renderPlotly({
    metric <- input$physical_metric
    metric_label <- names(metric_choices)[metric_choices == metric]
    df <- filtered_players() |>
      filter(!is.na(.data[[metric]]), !is.na(primary_position)) |>
      mutate(physical_position = ifelse(as.character(primary_position) %in% c("WF", "CF"), "Forward", as.character(primary_position)),
             physical_position = factor(physical_position, levels = physical_position_order))
    shiny::validate(shiny::need(nrow(df) > 2, "No hay suficientes datos físicos agregados."))
    p <- ggplot(df, aes(x = physical_position, y = .data[[metric]], text = paste0(player_name, "<br>", team, "<br>", metric_label, ": ", round(.data[[metric]], 2)))) +
      geom_boxplot(fill = APP_BLUE, alpha = 0.18, outlier.alpha = 0.25) +
      geom_jitter(width = 0.15, alpha = 0.45, size = 1.3, color = APP_BLUE) +
      labs(title = "Distribución por posición", x = "Posición", y = metric_label) +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$risk_load_windows <- renderPlotly({
    df <- filtered_players() |>
      filter(!is.na(n_ventanas_modelo), !is.na(risk_load_pct))
    shiny::validate(shiny::need(nrow(df) > 2, "No hay datos de ventanas suficientes."))
    p <- ggplot(df, aes(x = n_ventanas_modelo, y = risk_load_pct, color = injured_2324_load_label, text = paste0(player_name, "<br>", team, "<br>Ventanas: ", n_ventanas_modelo, "<br>Exceso carga: ", round(risk_load_pct, 3), "%<br>Lesionado: ", injured_2324_load_label))) +
      geom_point(alpha = 0.75, size = 2.2) +
      scale_color_manual(values = c("Sí" = APP_RED, "No" = APP_GREEN, "Sin dato" = APP_BLUE)) +
      labs(title = "Exceso de riesgo por carga vs ventanas", x = "Ventanas disponibles", y = "Exceso carga (%)", color = "Lesión por carga") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$physical_block_importance <- renderPlotly({
    df <- physical_block_importance |> mutate(block = factor(block, levels = block))
    p <- ggplot(df, aes(x = block, y = pct, text = paste0(block, "<br>", scales::percent(pct, accuracy = 0.1), "<br>", description))) +
      geom_col(fill = APP_BLUE, width = 0.65) +
      geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.3, size = 5) +
      scale_y_continuous(labels = scales::percent, limits = c(0, max(df$pct, na.rm = TRUE) * 1.18)) +
      labs(title = "Importancia por bloque del modelo de carga", x = NULL, y = "Gain total") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$physical_gain_categories <- renderPlot({
    df <- physical_gain_categories |>
      filter(!is.na(Gain), Gain > 0) |>
      group_by(group) |>
      arrange(Gain, .by_group = TRUE) |>
      mutate(feature_facet = paste(group, Feature, sep = "___")) |>
      ungroup() |>
      mutate(feature_facet = factor(feature_facet, levels = unique(feature_facet)))
    shiny::validate(shiny::need(nrow(df) > 0, "No hay datos de Gain por categorías."))
    ggplot(df, aes(x = feature_facet, y = Gain)) +
      geom_col(fill = APP_BLUE, width = 0.75) +
      coord_flip() +
      facet_wrap(~ group, scales = "free_y", ncol = 2) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(sub("^.*___", "", x), width = 26)) +
      labs(title = "Gain por categorías del modelo de carga", subtitle = "Top variables y resto agregadas por bloque fisiológico", x = NULL, y = "Gain importancia") +
      theme_minimal(base_size = 11) +
      theme(strip.text = element_text(face = "bold"), panel.spacing = grid::unit(1.2, "lines"))
  })

  output$physical_table <- renderDT({
    metric <- input$physical_metric
    phys_cols <- unique(c(metric, "distance_p90", "hsr_distance_p90", "sprint_distance_p90", "hi_distance_p90", "high_acc_count_p90", "high_dec_count_p90", "psv99"))
    df <- filtered_players() |>
      select(player_name, team, primary_position, primary_role, any_of(phys_cols)) |>
      distinct() |>
      rename(Jugador = player_name, Equipo = team, Posición = primary_position, Rol = primary_role) |>
      mutate(across(where(is.numeric), ~ round(.x, 2)))
    make_dt(df, page_length = 8)
  })

  output$predicted_vs_real <- renderPlotly({
    pred <- pred_columns()
    df <- filtered_players() |>
      filter(!is.na(.data[[pred$pred]]), !is.na(market_value_mill)) |>
      mutate(
        pred_value = .data[[pred$pred]],
        diff_value = .data[[pred$diff]],
        ratio_value = .data[[pred$ratio]],
        abs_error = abs(diff_value)
      )
    shiny::validate(shiny::need(nrow(df) > 2, "No hay predicciones suficientes."))
    thr <- as.numeric(stats::quantile(df$abs_error, probs = (input$outlier_pct %||% 95) / 100, na.rm = TRUE))
    df <- df |>
      mutate(
        tipo_error = case_when(
          abs_error >= thr & ratio_value > 1 ~ "Infravalorado",
          abs_error >= thr & ratio_value < 1 ~ "Sobrestimado",
          TRUE ~ "Resto"
        ),
        label = ifelse(tipo_error != "Resto" & isTRUE(input$label_top_errors), player_name, "")
      )
    maxv <- max(c(df$market_value_mill, df$pred_value), na.rm = TRUE)
    p <- ggplot(df, aes(x = market_value_mill, y = pred_value, color = tipo_error, text = paste0(player_name, "<br>", team, "<br>Real: ", round(market_value_mill, 1), " M€<br>Estimado: ", round(pred_value, 1), " M€<br>Error: ", round(diff_value, 1), " M€<br>Ratio: ", round(ratio_value, 2), "<br>Umbral: p", input$outlier_pct))) +
      geom_point(alpha = 0.82, size = 2.4) +
      ggrepel::geom_text_repel(aes(label = label), size = 3, max.overlaps = 50, show.legend = FALSE) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      scale_color_manual(values = c("Infravalorado" = APP_GREEN, "Sobrestimado" = APP_RED, "Resto" = APP_BLUE)) +
      coord_equal(xlim = c(0, maxv * 1.03), ylim = c(0, maxv * 1.03)) +
      labs(title = paste("Valor observado vs estimado -", pred$label), subtitle = paste0("Atípicos: |error| >= p", input$outlier_pct, "; ratio > 1 infravalorado, ratio < 1 sobrestimado"), x = "Valor observado (M€)", y = "Valor estimado (M€)", color = "Lectura") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$ratio_by_band <- renderPlotly({
    pred <- pred_columns()
    df <- filtered_players() |>
      filter(!is.na(.data[[pred$ratio]]), is.finite(.data[[pred$ratio]])) |>
      group_by(value_band) |>
      summarise(ratio_medio = mean(.data[[pred$ratio]], na.rm = TRUE), n = n(), .groups = "drop") |>
      filter(!is.na(value_band))
    shiny::validate(shiny::need(nrow(df) > 0, "No hay ratios suficientes."))
    p <- ggplot(df, aes(x = value_band, y = ratio_medio, text = paste0(value_band, "<br>Ratio: ", round(ratio_medio, 2), "<br>n=", n))) +
      geom_col(fill = APP_BLUE) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      labs(title = "Sesgo relativo por banda de valor", x = "Banda de valor", y = "Predicho / real") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$calibration_decile_plot <- renderPlotly({
    pred <- pred_columns()
    y_col <- if (identical(pred$pred, "pred_raw_mill")) "pred_raw_mean_mill" else "pred_iso_mean_mill"
    df <- calibration_deciles |>
      mutate(pred_mean = .data[[y_col]])
    p <- ggplot(df, aes(x = real_mean_mill, y = pred_mean, text = paste0(decile, "<br>Real medio: ", round(real_mean_mill, 1), " M€<br>Pred. medio: ", round(pred_mean, 1), " M€<br>n=", n))) +
      geom_point(size = 2.6, color = APP_BLUE) +
      geom_line(color = APP_BLUE) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = "Calibración por deciles", x = "Valor real medio (M€)", y = "Valor estimado medio (M€)") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$shap_global_plot <- renderPlotly({
    df <- shap_importance |>
      arrange(desc(mean_abs_shap)) |>
      slice_head(n = input$top_shap)
    p <- ggplot(df, aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap, text = paste0(feature, "<br>|SHAP| medio: ", round(mean_abs_shap, 4), "<br>Grupo: ", group))) +
      geom_col(fill = APP_BLUE) +
      coord_flip() +
      labs(title = "Importancia global SHAP", x = NULL, y = "|SHAP| medio") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$shap_distribution_plot <- renderPlotly({
    top_feats <- shap_dependence |>
      group_by(feature, feature_label) |>
      summarise(m = mean(abs(shap_value), na.rm = TRUE), .groups = "drop") |>
      arrange(desc(m)) |>
      slice_head(n = min(input$top_shap, 25)) |>
      pull(feature)
    df <- shap_dependence |>
      filter(feature %in% top_feats) |>
      mutate(feature_label = factor(feature_label, levels = rev(unique(shap_feature_tbl$feature_label[match(top_feats, shap_feature_tbl$feature)]))))
    shiny::validate(shiny::need(nrow(df) > 0, "No hay datos SHAP individuales para la distribución."))
    p <- ggplot(df, aes(x = shap_value, y = feature_label, color = feature_value_scaled, text = paste0(player_name, "<br>", feature_label, ": ", round(feature_value, 3), "<br>SHAP: ", round(shap_value, 4)))) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_jitter(width = 0, height = 0.18, alpha = 0.65, size = 1.4) +
      scale_color_gradient(low = APP_BLUE, high = "#e7298a", labels = scales::percent) +
      labs(title = "Distribución SHAP por jugador", x = "SHAP value", y = NULL, color = "Valor relativo") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$shap_dependence_plot <- renderPlotly({
    req(input$shap_feature)
    df <- shap_dependence |>
      filter(feature == input$shap_feature, is.finite(feature_value), is.finite(shap_value))
    shiny::validate(shiny::need(nrow(df) > 2, "No hay datos para esta variable."))
    color_var <- input$shap_color_var %||% ""
    if (!is.null(color_var) && nzchar(color_var) && color_var %in% shap_dependence$feature) {
      color_df <- shap_dependence |>
        filter(feature == color_var) |>
        select(player_id, color_value = feature_value, color_label = feature_label)
      df <- df |> left_join(color_df, by = "player_id")
      p <- ggplot(df, aes(x = feature_value, y = shap_value, color = color_value, text = paste0(player_name, "<br>", feature_label, ": ", round(feature_value, 3), "<br>SHAP: ", round(shap_value, 4), "<br>Color: ", round(color_value, 3)))) +
        geom_point(alpha = 0.75, size = 2) +
        geom_smooth(method = "loess", se = FALSE, color = "black") +
        scale_color_gradient(low = APP_BLUE, high = "#e7298a") +
        labs(title = paste("SHAP dependence:", unique(df$feature_label)[1]), x = unique(df$feature_label)[1], y = paste0("SHAP(", unique(df$feature_label)[1], ")"), color = unique(df$color_label)[1]) +
        theme_minimal(base_size = 12)
    } else {
      p <- ggplot(df, aes(x = feature_value, y = shap_value, text = paste0(player_name, "<br>", feature_label, ": ", round(feature_value, 3), "<br>SHAP: ", round(shap_value, 4)))) +
        geom_point(alpha = 0.75, size = 2, color = APP_BLUE) +
        geom_smooth(method = "loess", se = FALSE, color = "black") +
        labs(title = paste("SHAP dependence:", unique(df$feature_label)[1]), x = unique(df$feature_label)[1], y = paste0("SHAP(", unique(df$feature_label)[1], ")")) +
        theme_minimal(base_size = 12)
    }
    ggplotly(p, tooltip = "text")
  })

  output$shap_group_plot <- renderPlotly({
    df <- shap_importance |>
      group_by(group) |>
      summarise(
        total_shap = sum(mean_abs_shap, na.rm = TRUE),
        n_vars = n(),
        .groups = "drop"
      ) |>
      mutate(
        perc_shap = 100 * total_shap / sum(total_shap)
      ) |>
      arrange(perc_shap)
    
    p <- ggplot(
      df,
      aes(
        x = reorder(group, perc_shap),
        y = perc_shap,
        text = paste0(
          group,
          "<br>Importancia: ", round(perc_shap, 1), "%",
          "<br>|SHAP| total: ", round(total_shap, 3),
          "<br>Variables: ", n_vars
        )
      )
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Importancia relativa de los grupos de variables",
        x = NULL,
        y = "Importancia relativa (%)"
      ) +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$model_metrics_table <- renderDT({
    df <- metrics_final |>
      select(modelo, etapa_modelo, calibracion, best_iter, n, MAE_mill, RMSE_mill, MAPE, R2_eur, R2_log, ratio_medio, sesgo_mill) |>
      mutate(across(where(is.numeric), ~ round(.x, 4))) |>
      rename(Modelo = modelo, Etapa = etapa_modelo, Calibración = calibracion, Iter = best_iter)
    make_dt(df, page_length = 9)
  })

  output$market_table <- renderDT({
    pred <- pred_columns()
    df0 <- filtered_players() |>
      mutate(
        pred_value = .data[[pred$pred]],
        diff_value = .data[[pred$diff]],
        ratio_value = .data[[pred$ratio]],
        abs_error = abs(diff_value)
      )
    thr <- as.numeric(stats::quantile(df0$abs_error, probs = (input$outlier_pct %||% 95) / 100, na.rm = TRUE))
    df <- df0 |>
      mutate(
        Lectura = case_when(
          abs_error >= thr & ratio_value > 1 ~ "Infravalorado",
          abs_error >= thr & ratio_value < 1 ~ "Sobrestimado",
          TRUE ~ "Resto"
        )
      ) |>
      transmute(
        Jugador = player_name,
        Equipo = team,
        Posición = primary_position,
        Rol = primary_role,
        Split = split,
        `Real M€` = round(market_value_mill, 2),
        `Estimado M€` = round(pred_value, 2),
        `Diferencia M€` = round(diff_value, 2),
        Ratio = round(ratio_value, 2),
        Lectura = Lectura,
        Edad = age,
        Minutos = round(minutes_2324, 0),
        `Contrato años` = contract_years,
        `Riesgo hist. %` = round(risk_hist_pct, 2),
        `Riesgo carga %` = round(risk_load_pct, 3)
      ) |>
      arrange(desc(abs(`Diferencia M€`)))
    make_dt(df)
  })

  compare_scope <- reactive({
    ids <- suppressWarnings(as.integer(input$compare_players %||% character()))
    ids <- ids[is.finite(ids)]
    shiny::validate(shiny::need(length(ids) > 0, "Selecciona al menos un jugador."))
    rp <- roles_pca |>
      filter(player_id %in% ids)
    pos_split <- rp |>
      group_by(player_id) |>
      summarise(vals = list(unique(as.character(position))), .groups = "drop") |>
      pull(vals)
    role_split <- rp |>
      group_by(player_id) |>
      summarise(vals = list(unique(as.character(role))), .groups = "drop") |>
      pull(vals)
    common_positions <- if (length(pos_split) > 0) Reduce(intersect, pos_split) else character()
    common_roles <- if (length(role_split) > 0) Reduce(intersect, role_split) else character()
    list(ids = ids, common_positions = common_positions, common_roles = common_roles)
  })

  output$compare_scope_note <- renderUI({
    sc <- compare_scope()
    pos_txt <- if (length(sc$common_positions) > 0) paste(sc$common_positions, collapse = ", ") else "sin común"
    role_txt <- if (length(sc$common_roles) > 0) paste(sc$common_roles, collapse = ", ") else "sin común"
    div(class = "small-note", HTML(paste0("Pos. comunes: <b>", htmltools::htmlEscape(pos_txt), "</b><br>Roles comunes: <b>", htmltools::htmlEscape(role_txt), "</b>")))
  })

  compare_filtered_percentiles <- reactive({
    sc <- compare_scope()
    df <- event_percentiles |> filter(player_id %in% sc$ids)
    if (isTRUE(input$compare_common_positions)) {
      shiny::validate(shiny::need(length(sc$common_positions) > 0, "Los jugadores seleccionados no tienen posiciones comunes."))
      df <- df |> filter(position %in% sc$common_positions)
    }
    if (isTRUE(input$compare_common_roles)) {
      shiny::validate(shiny::need(length(sc$common_roles) > 0, "Los jugadores seleccionados no tienen roles comunes."))
      df <- df |> filter(rol %in% sc$common_roles)
    }
    df
  })

  compare_radar_data <- reactive({
    req(input$compare_players)
    ids <- suppressWarnings(as.integer(input$compare_players %||% character()))
    ids <- ids[is.finite(ids)]
    shiny::validate(shiny::need(length(ids) > 0, "Selecciona al menos un jugador."))

    selected_metrics <- selected_compare_metric_cols()
    selected_metrics <- unique(intersect(unlist(as.character(selected_metrics), use.names = FALSE), all_compare_metric_cols))
    shiny::validate(shiny::need(length(selected_metrics) >= 3, "Selecciona al menos tres métricas principales para construir el radar."))

    base_players <- players |>
      filter(player_id %in% ids) |>
      select(player_id, player_name, team, all_positions, all_roles)

    metric_group_map <- compare_metric_lookup |>
      select(metric_col, metric_group) |>
      distinct(metric_col, .keep_all = TRUE)
    metric_label_tbl <- tibble(metric_col = selected_metrics) |>
      left_join(metric_group_map, by = "metric_col") |>
      mutate(
        metric_group = dplyr::coalesce(metric_group, "Otros"),
        raw_label = vapply(metric_col, lookup_compare_metric_radar_label, character(1))
      )
    duplicated_labels <- duplicated(metric_label_tbl$raw_label) | duplicated(metric_label_tbl$raw_label, fromLast = TRUE)
    metric_label_tbl <- metric_label_tbl |>
      mutate(category_label = ifelse(duplicated_labels, paste0(raw_label, " · ", metric_group), raw_label))
    category_map <- stats::setNames(metric_label_tbl$category_label, metric_label_tbl$metric_col)

    metric_df <- dplyr::bind_rows(lapply(selected_metrics, function(metric_col) {
      vals <- suppressWarnings(as.numeric(players[[metric_col]]))
      valid_vals <- vals[is.finite(vals)]
      if (length(valid_vals) <= 1) return(NULL)
      tibble(
        player_id = players$player_id,
        metric_col = metric_col,
        category = unname(category_map[as.character(metric_col)[1]]),
        value = vals,
        percentile = 100 * dplyr::percent_rank(vals)
      )
    }))

    shiny::validate(shiny::need(nrow(metric_df) > 0, "No hay métricas numéricas disponibles para el radar."))

    metric_levels <- unname(category_map[selected_metrics])
    metric_levels <- metric_levels[metric_levels %in% unique(metric_df$category)]

    tidyr::expand_grid(base_players, metric_col = selected_metrics) |>
      left_join(metric_df, by = c("player_id", "metric_col")) |>
      mutate(
        category = factor(category, levels = metric_levels),
        percentile_plot = ifelse(is.na(percentile), 0, pmax(0, pmin(100, percentile))),
        tooltip = paste0(
          player_name, "<br>", category, ": ",
          ifelse(is.na(value), "sin dato", round(value, 2)),
          "<br>Percentil: ", ifelse(is.na(percentile), "sin dato", round(percentile, 0)),
          "<br>", team,
          "<br>Posiciones: ", all_positions,
          "<br>Roles: ", all_roles
        )
      ) |>
      arrange(player_name, category)
  })

  output$compare_bar <- renderPlotly({
    df <- compare_radar_data() |>
      filter(!is.na(player_name)) |>
      arrange(player_name, category)
    shiny::validate(shiny::need(nrow(df) > 0, "Selecciona al menos un jugador."))
    make_radar_plotly(
      df,
      category_col = "category",
      value_col = "percentile_plot",
      group_col = "player_name",
      title = "Radar comparativo de métricas generales",
      subtitle = "Percentiles sobre la muestra completa; métricas del modelo LightGBM seleccionadas por bloques.",
      label_width = 15,
      palette = compare_player_colors(),
      player_label_size = 17,
      area_alpha = 0.15,
      axis_label_size = NULL
    )
  })

  output$compare_table <- renderDT({
    req(input$compare_players)
    ids <- as.integer(input$compare_players)
    roles_collapsed <- roles_pca |>
      filter(player_id %in% ids) |>
      group_by(player_id) |>
      summarise(
        Posiciones = paste(sort(unique(position)), collapse = " | "),
        Roles = paste(sort(unique(role)), collapse = " | "),
        .groups = "drop"
      )
    df <- players |>
      filter(player_id %in% ids) |>
      left_join(roles_collapsed, by = "player_id") |>
      transmute(
        Jugador = player_name,
        Equipo = team,
        Posiciones = dplyr::coalesce(Posiciones, all_positions),
        Roles = dplyr::coalesce(Roles, all_roles),
        Edad = age,
        `Valor M€` = round(market_value_mill, 2),
        `Pred iso M€` = round(pred_iso_mill, 2),
        Minutos = round(minutes_2324, 0),
        `Contrato años` = contract_years,
        `Riesgo hist. %` = round(risk_hist_pct, 2),
        `Riesgo carga %` = round(risk_load_pct, 3),
        `Goles/90` = round(goles90, 2),
        `xG/90` = round(xg90, 2)
      )
    make_dt(df, page_length = 10)
  })

  output$compare_events <- renderPlotly({
    req(input$compare_players)
    ids <- suppressWarnings(as.integer(input$compare_players %||% character()))
    ids <- ids[is.finite(ids)]
    shiny::validate(shiny::need(length(ids) > 0, "Selecciona al menos un jugador."))

    base_players <- players |>
      filter(player_id %in% ids) |>
      select(player_id, player_name, team, all_positions, all_roles)

    event_levels <- role_event_vars_all
    shiny::validate(shiny::need(length(event_levels) == 16, "No se pudieron cargar los 16 eventos base utilizados en roles."))

    df_raw <- compare_filtered_percentiles() |>
      filter(event_name %in% event_levels) |>
      group_by(player_id, event_name) |>
      summarise(percentile = safe_mean(percentile_position), .groups = "drop")

    if (nrow(df_raw) == 0) {
      df_raw <- tibble(player_id = integer(), event_name = character(), percentile = numeric())
    }

    df <- tidyr::expand_grid(base_players, event_name = event_levels) |>
      left_join(df_raw, by = c("player_id", "event_name")) |>
      mutate(
        event_name = factor(event_name, levels = event_levels),
        percentile_plot = ifelse(is.na(percentile), 0, pmax(0, pmin(100, percentile)))
      ) |>
      arrange(player_name, event_name)

    make_radar_plotly(
      df,
      category_col = "event_name",
      value_col = "percentile_plot",
      group_col = "player_name",
      title = "Radar de los 16 eventos utilizados para roles",
      subtitle = "Mismos 16 eventos del Weighted PCA, el clustering y la sectorización del campo.",
      label_width = 13,
      palette = compare_player_colors(),
      player_label_size = 17,
      area_alpha = 0.15,
      axis_label_size = 13
    )
  })

  output$search_stats_filters <- renderUI({
    stats <- input$search_stats_selected %||% character()
    stats <- intersect(stats, wyscout_feature_candidates)
    if (length(stats) == 0) {
      return(div(class = "small-note", "Selecciona estadísticas para añadir umbrales dinámicos al buscador."))
    }
    controls <- lapply(stats, function(st) {
      x <- suppressWarnings(as.numeric(players[[st]]))
      rng <- range(x, na.rm = TRUE)
      if (!all(is.finite(rng)) || diff(rng) <= 0) return(NULL)
      column(
        4,
        sliderInput(
          stat_input_id("search_stat", st),
          pretty_feature_label(st),
          min = round(rng[1], 3),
          max = round(rng[2], 3),
          value = c(round(rng[1], 3), round(rng[2], 3)),
          step = stat_step(x)
        )
      )
    })
    controls <- controls[!vapply(controls, is.null, logical(1))]
    if (length(controls) == 0) return(NULL)
    do.call(fluidRow, controls)
  })

  output$est_stats_inputs <- renderUI({
    stats <- input$est_stats_selected %||% character()
    stats <- intersect(stats, wyscout_feature_candidates)
    if (length(stats) == 0) {
      return(div(class = "small-note", "Selecciona estadísticas Wyscout para añadir umbrales al estimador. Las no seleccionadas se imputan con la media del rol."))
    }
    controls <- lapply(stats, function(st) {
      x <- suppressWarnings(as.numeric(players[[st]]))
      rng <- range(x, na.rm = TRUE)
      if (!all(is.finite(rng)) || diff(rng) <= 0) return(NULL)
      column(
        4,
        sliderInput(
          stat_input_id("est_stat", st),
          pretty_feature_label(st),
          min = round(rng[1], 3),
          max = round(rng[2], 3),
          value = c(round(rng[1], 3), round(rng[2], 3)),
          step = stat_step(x)
        )
      )
    })
    controls <- controls[!vapply(controls, is.null, logical(1))]
    if (length(controls) == 0) return(NULL)
    do.call(fluidRow, controls)
  })

  search_results <- reactive({
    df <- players
    if (!is.null(input$search_position) && input$search_position != "Todas") {
      ids_pos <- roles_pca |> filter(position == input$search_position) |> pull(player_id) |> unique()
      df <- df |> filter(player_id %in% ids_pos)
    }
    if (!is.null(input$search_role) && input$search_role != "Todos") {
      ids_role <- roles_pca |> filter(role == input$search_role) |> pull(player_id) |> unique()
      df <- df |> filter(player_id %in% ids_role)
    }
    if (!is.null(input$search_team_group) && input$search_team_group != "Todos") df <- df |> filter(team_group_model == input$search_team_group)
    if (!is.null(input$search_country) && input$search_country != "Todos") df <- df |> filter(country_group == input$search_country)
    df <- df |>
      filter(
        age >= input$search_age[1], age <= input$search_age[2],
        minutes_2324 >= input$search_minutes[1], minutes_2324 <= input$search_minutes[2],
        contract_years >= input$search_contract[1], contract_years <= input$search_contract[2],
        market_value_mill >= input$search_value[1], market_value_mill <= input$search_value[2],
        risk_hist_pct >= input$search_risk_hist[1], risk_hist_pct <= input$search_risk_hist[2],
        risk_load_pct >= input$search_risk_load[1], risk_load_pct <= input$search_risk_load[2]
      )
    stats <- intersect(input$search_stats_selected %||% character(), wyscout_feature_candidates)
    for (st in stats) {
      id <- stat_input_id("search_stat", st)
      rng <- input[[id]]
      if (length(rng) == 2 && all(is.finite(rng)) && st %in% names(df)) {
        df <- df |> filter(!is.na(.data[[st]]), .data[[st]] >= rng[1], .data[[st]] <= rng[2])
      }
    }
    df
  })

  output$search_table <- renderDT({
    raw <- search_results() |>
      arrange(desc(market_value_mill))
    stats <- intersect(input$search_stats_selected %||% character(), wyscout_feature_candidates)
    df <- raw |>
      transmute(
        Jugador = player_name,
        Equipo = team,
        `Grupo equipo` = team_group_model,
        País = country_group,
        Posiciones = all_positions,
        Roles = all_roles,
        Edad = age,
        `Valor M€` = round(market_value_mill, 2),
        `Pred. M€` = round(pred_iso_mill, 2),
        Minutos = round(minutes_2324, 0),
        Contrato = contract_years,
        `Riesgo hist. %` = round(risk_hist_pct, 2),
        `Riesgo carga %` = round(risk_load_pct, 3)
      )
    for (st in stats) {
      df[[pretty_feature_label(st)]] <- round(raw[[st]], 3)
    }
    make_dt(df)
  })

  estimate_input_vector <- reactive({
    role_ids <- roles_pca |>
      filter(position == input$est_position, role == input$est_role) |>
      pull(player_id) |>
      unique()
    role_mean <- players |>
      filter(player_id %in% role_ids) |>
      summarise(across(any_of(estimator_numeric_features), ~ mean(.x, na.rm = TRUE)))
    if (nrow(role_mean) == 0 || all(is.na(role_mean))) {
      role_mean <- players |> summarise(across(any_of(estimator_numeric_features), ~ mean(.x, na.rm = TRUE)))
    }
    vec <- as.list(role_mean[1, , drop = FALSE])
    team_group_info <- players |>
      filter(team_group_model == input$est_team_group) |>
      summarise(team_points_1a = median(team_points_1a, na.rm = TRUE), team_group_model = first(na.omit(team_group_model)))
    vec$age <- input$est_age
    vec$minutes_2324 <- input$est_minutes
    vec$contract_years <- input$est_contract
    vec$team_points_1a <- ifelse(is.finite(team_group_info$team_points_1a[1]), team_group_info$team_points_1a[1], mean(players$team_points_1a, na.rm = TRUE))
    optional_values <- list()
    risk_hist_rng <- suppressWarnings(as.numeric(input$est_risk_hist_range))
    if (length(risk_hist_rng) == 2 && all(is.finite(risk_hist_rng)) && diff(range(players$risk_hist_pct, na.rm = TRUE)) > 0) {
      full_rng <- c(0, ceiling(max(players$risk_hist_pct, na.rm = TRUE)))
      if (abs(risk_hist_rng[1] - full_rng[1]) > 1e-8 || abs(risk_hist_rng[2] - full_rng[2]) > 1e-8) {
        optional_values$risk_hist_excess <- mean(risk_hist_rng) / 100
      }
    }
    risk_load_rng <- suppressWarnings(as.numeric(input$est_risk_load_range))
    if (length(risk_load_rng) == 2 && all(is.finite(risk_load_rng)) && diff(range(players$risk_load_pct, na.rm = TRUE)) > 0) {
      full_rng <- c(0, max(0.1, ceiling(max(players$risk_load_pct, na.rm = TRUE) * 10) / 10))
      if (abs(risk_load_rng[1] - full_rng[1]) > 1e-8 || abs(risk_load_rng[2] - full_rng[2]) > 1e-8) {
        optional_values$risk_load_excess <- mean(risk_load_rng) / 100
      }
    }
    stats <- intersect(input$est_stats_selected %||% character(), wyscout_feature_candidates)
    for (st in stats) {
      rng <- suppressWarnings(as.numeric(input[[stat_input_id("est_stat", st)]]))
      if (length(rng) == 2 && all(is.finite(rng))) {
        optional_values[[st]] <- mean(rng)
      }
    }
    for (nm in names(optional_values)) {
      val <- suppressWarnings(as.numeric(optional_values[[nm]]))
      if (length(val) == 1 && is.finite(val)) vec[[nm]] <- val
    }
    list(
      numeric = vec,
      position = input$est_position,
      role = input$est_role,
      country = input$est_country,
      team_group = input$est_team_group
    )
  })

  estimate_neighbors <- reactive({
    inp <- estimate_input_vector()
    df <- players |> filter(!is.na(market_value_mill))
    risk_hist_rng <- suppressWarnings(as.numeric(input$est_risk_hist_range))
    if (length(risk_hist_rng) == 2 && all(is.finite(risk_hist_rng))) {
      df <- df |> filter(risk_hist_pct >= risk_hist_rng[1], risk_hist_pct <= risk_hist_rng[2])
    }
    risk_load_rng <- suppressWarnings(as.numeric(input$est_risk_load_range))
    if (length(risk_load_rng) == 2 && all(is.finite(risk_load_rng))) {
      df <- df |> filter(risk_load_pct >= risk_load_rng[1], risk_load_pct <= risk_load_rng[2])
    }
    stats <- intersect(input$est_stats_selected %||% character(), wyscout_feature_candidates)
    for (st in stats) {
      rng <- suppressWarnings(as.numeric(input[[stat_input_id("est_stat", st)]]))
      if (length(rng) == 2 && all(is.finite(rng)) && st %in% names(df)) {
        df <- df |> filter(!is.na(.data[[st]]), .data[[st]] >= rng[1], .data[[st]] <= rng[2])
      }
    }
    shiny::validate(shiny::need(nrow(df) > 0, "No hay jugadores que cumplan los umbrales estadísticos seleccionados."))
    num_cols <- intersect(estimator_numeric_features, names(df))
    dist_num <- rep(0, nrow(df))
    used <- 0
    for (cl in num_cols) {
      val <- suppressWarnings(as.numeric(inp$numeric[[cl]]))
      if (!is.finite(val)) next
      sdv <- sd(df[[cl]], na.rm = TRUE)
      if (!is.finite(sdv) || sdv == 0) next
      z <- (df[[cl]] - val) / sdv
      z[!is.finite(z)] <- 0
      dist_num <- dist_num + z^2
      used <- used + 1
    }
    dist_num <- sqrt(dist_num / max(1, used))
    has_pos <- vapply(df$player_id, function(id) any(roles_pca$player_id == id & roles_pca$position == inp$position), logical(1))
    has_role <- vapply(df$player_id, function(id) any(roles_pca$player_id == id & roles_pca$role == inp$role), logical(1))
    has_pair <- vapply(df$player_id, function(id) any(roles_pca$player_id == id & roles_pca$position == inp$position & roles_pca$role == inp$role), logical(1))
    dist_cat <- 0.65 * as.numeric(!has_pos) +
      0.35 * as.numeric(!has_role) +
      0.90 * as.numeric(!has_pair) +
      0.25 * as.numeric(coalesce(df$country_group, "") != inp$country) +
      0.35 * as.numeric(coalesce(df$team_group_model, "") != (inp$team_group %||% ""))
    df |>
      mutate(
        distancia = dist_num + dist_cat,
        similitud_pct = 100 * exp(-distancia),
        peso_vecino = 1 / (distancia + 0.05)
      ) |>
      arrange(distancia) |>
      slice_head(n = as.integer(input$est_k %||% 12))
  })

  estimated_price_value <- reactive({
    nb <- estimate_neighbors()
    shiny::validate(shiny::need(nrow(nb) > 0, "No hay vecinos suficientes."))
    sum(nb$market_value_mill * nb$peso_vecino, na.rm = TRUE) / sum(nb$peso_vecino, na.rm = TRUE)
  })

  output$estimate_price_card <- renderUI({
    est <- estimated_price_value()
    nb <- estimate_neighbors()
    div(
      class = "estimate-card",
      div(class = "estimate-subtitle", "Valor estimado por vecinos similares"),
      div(class = "estimate-value", fmt_mill(est)),
      div(class = "estimate-subtitle", paste0("Rango vecinos p25-p75: ", fmt_mill(quantile(nb$market_value_mill, 0.25, na.rm = TRUE)), " – ", fmt_mill(quantile(nb$market_value_mill, 0.75, na.rm = TRUE)))),
      div(class = "estimate-subtitle", paste0("Similitud media vecinos: ", round(mean(nb$similitud_pct, na.rm = TRUE), 1), "%"))
    )
  })

  output$estimate_neighbors_plot <- renderPlotly({
    nb <- estimate_neighbors() |>
      mutate(Jugador = paste0(player_name, " (", team, ")")) |>
      arrange(market_value_mill)
    shiny::validate(shiny::need(nrow(nb) > 0, "No hay vecinos."))
    p <- ggplot(nb, aes(x = reorder(Jugador, market_value_mill), y = market_value_mill, text = paste0(player_name, "<br>", team, "<br>Valor: ", round(market_value_mill, 2), " M€<br>Distancia: ", round(distancia, 3), "<br>Similitud: ", round(similitud_pct, 1), "%"))) +
      geom_col(fill = APP_BLUE) +
      coord_flip() +
      labs(title = "Vecinos utilizados por el estimador", x = NULL, y = "Valor M€") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$estimate_neighbors_table <- renderDT({
    nb <- estimate_neighbors() |>
      transmute(
        Jugador = player_name,
        Equipo = team,
        Posiciones = all_positions,
        Roles = all_roles,
        Edad = age,
        `Valor M€` = round(market_value_mill, 2),
        Minutos = round(minutes_2324, 0),
        Contrato = contract_years,
        Similitud = paste0(round(similitud_pct, 1), "%"),
        Distancia = round(distancia, 3)
      )
    make_dt(nb)
  })


  output$data_files_table <- renderDT({
    files <- list.files(app_data_dir, full.names = TRUE)
    df <- tibble(
      archivo = basename(files),
      kb = round(file.info(files)$size / 1024, 1),
      modificado = as.character(file.info(files)$mtime)
    ) |>
      arrange(archivo)
    make_dt(df, page_length = 20)
  })

  output$download_filtered_players <- downloadHandler(
    filename = function() paste0("jugadores_filtrados_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(filtered_players(), file)
  )

  output$download_role_data <- downloadHandler(
    filename = function() paste0("roles_filtrados_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(filtered_roles(), file)
  )

  output$download_predictions <- downloadHandler(
    filename = function() paste0("predicciones_valor_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(market_predictions, file)
  )
}

shinyApp(ui, server)
