# Helpers para generar informes automaticos con Python + RAG.

report_python_cmd <- function() {
  cmd <- Sys.getenv("TFM_PYTHON", unset = "auto")
  if (!nzchar(cmd)) cmd <- "auto"
  cmd
}

report_clean_output <- function(x) {
  if (length(x) == 0) return(character())
  status <- attr(x, "status")
  x <- as.character(x)
  y <- tryCatch(iconv(x, from = "", to = "UTF-8", sub = "byte"), error = function(e) x)
  y[is.na(y)] <- "<salida no decodificable>"
  if (!is.null(status)) attr(y, "status") <- status
  y
}

split_python_command <- function(cmd) {
  cmd <- trimws(as.character(cmd %||% "auto"))
  if (!nzchar(cmd)) cmd <- "auto"
  if (grepl('^"[^"]+"', cmd)) {
    exe <- sub('^"([^"]+)".*$', "\\1", cmd)
    rest <- trimws(sub('^"[^"]+"', "", cmd))
    extra <- if (nzchar(rest)) strsplit(rest, "\\s+")[[1]] else character()
  } else {
    parts <- strsplit(cmd, "\\s+")[[1]]
    exe <- parts[1]
    extra <- if (length(parts) > 1) parts[-1] else character()
  }
  list(command = exe, extra_args = extra)
}

run_python_command_safe <- function(command, args) {
  old_env <- Sys.getenv(c("PYTHONIOENCODING", "PYTHONDONTWRITEBYTECODE"), unset = NA)
  on.exit({
    if (is.na(old_env[["PYTHONIOENCODING"]])) Sys.unsetenv("PYTHONIOENCODING") else Sys.setenv(PYTHONIOENCODING = old_env[["PYTHONIOENCODING"]])
    if (is.na(old_env[["PYTHONDONTWRITEBYTECODE"]])) Sys.unsetenv("PYTHONDONTWRITEBYTECODE") else Sys.setenv(PYTHONDONTWRITEBYTECODE = old_env[["PYTHONDONTWRITEBYTECODE"]])
  }, add = TRUE)
  Sys.setenv(PYTHONIOENCODING = "utf-8", PYTHONDONTWRITEBYTECODE = "1")
  out <- tryCatch(
    suppressWarnings(system2(command, args = args, stdout = TRUE, stderr = TRUE)),
    error = function(e) structure(conditionMessage(e), status = 1)
  )
  report_clean_output(out)
}

resolve_python_command <- function(user_cmd = report_python_cmd()) {
  user_cmd <- trimws(as.character(user_cmd %||% "auto"))
  if (!nzchar(user_cmd)) user_cmd <- "auto"
  custom_python <- !tolower(user_cmd) %in% c("auto", "python")
  candidates <- if (custom_python) {
    list(split_python_command(user_cmd))
  } else if (.Platform$OS.type == "windows") {
    list(
      split_python_command("py -3"),
      split_python_command("python3"),
      split_python_command("python"),
      split_python_command("py")
    )
  } else {
    list(
      split_python_command("python3"),
      split_python_command("python")
    )
  }
  log <- character()
  for (cand in candidates) {
    check <- run_python_command_safe(cand$command, c(cand$extra_args, "--version"))
    status <- attr(check, "status")
    if (is.null(status)) status <- 0
    log <- c(log, paste0("$ ", cand$command, " ", paste(c(cand$extra_args, "--version"), collapse = " "), "\n", paste(check, collapse = "\n")))
    if (identical(status, 0) && any(grepl("Python", check, fixed = TRUE))) return(cand)
  }
  stop(
    "No se ha encontrado un ejecutable Python valido. En Windows suele solucionarse escribiendo ",
    "la ruta completa a python.exe o 'py -3' si tienes instalado el Python Launcher.\n\nDiagnostico:\n",
    paste(log, collapse = "\n\n")
  )
}

parse_report_result <- function(lines) {
  lines <- report_clean_output(lines)
  marker <- grep("^JSON_RESULT=", lines, value = TRUE, useBytes = TRUE)
  if (length(marker) == 0) {
    stop(paste(c("El generador Python no devolvio JSON_RESULT.", lines), collapse = "\n"))
  }
  jsonlite::fromJSON(sub("^JSON_RESULT=", "", marker[[length(marker)]]), simplifyVector = TRUE)
}

run_tfm_report <- function(report_type, player_ids, objective = "Comparativa general", python_cmd = report_python_cmd()) {
  # Guardar fuera de la carpeta de la app evita que Shiny/RStudio active autoreload
  # al generar nuevos ficheros.
  out_dir <- file.path(tempdir(), paste0("tfm_shiny_reports_", Sys.getpid()))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path("rag", "tfm_report_generator.py")
  if (!file.exists(script)) stop("No se encuentra el script Python: ", script)

  py <- resolve_python_command(python_cmd)
  objective_key <- objective %||% "general"
  objective_key <- dplyr::case_when(
    tolower(objective_key) %in% c("comparativa general", "general") ~ "general",
    tolower(objective_key) %in% c("fichaje") ~ "fichaje",
    tolower(objective_key) %in% c("renovación", "renovacion") ~ "renovacion",
    tolower(objective_key) %in% c("venta") ~ "venta",
    tolower(objective_key) %in% c("seguimiento") ~ "seguimiento",
    TRUE ~ as.character(objective_key)
  )
  args <- c(
    py$extra_args,
    script,
    "--data-dir", file.path("data", "app"),
    "--report", report_type,
    "--out-dir", out_dir,
    "--objective", objective_key
  )
  for (id in player_ids) {
    args <- c(args, "--player-id", as.character(id))
  }

  lines <- run_python_command_safe(py$command, args)
  result <- parse_report_result(lines)
  if (!isTRUE(result$ok)) {
    stop(result$error %||% "Error desconocido al generar el informe")
  }
  result
}

report_preview_ui <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(div(class = "info-box", "Genera un informe para ver la vista previa."))
  }
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tags$iframe(srcdoc = html, class = "report-preview-frame")
}
