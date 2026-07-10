# Helpers para la app Shiny del TFM -------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

fmt_mill <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    "-",
    paste0(format(round(x, digits), nsmall = digits, big.mark = ".", decimal.mark = ","), " M€")
  )
}

fmt_eur <- function(x, digits = 1) {
  fmt_mill(suppressWarnings(as.numeric(x)) / 1e6, digits = digits)
}

fmt_pct <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    "-",
    paste0(format(round(100 * x, digits), nsmall = digits, big.mark = ".", decimal.mark = ","), "%")
  )
}

fmt_pct_already <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    "-",
    paste0(format(round(x, digits), nsmall = digits, big.mark = ".", decimal.mark = ","), "%")
  )
}

fmt_num <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    "-",
    format(round(x, digits), nsmall = digits, big.mark = ".", decimal.mark = ",")
  )
}

as_bool_label <- function(x) {
  xx <- as.character(x)
  dplyr::case_when(
    is.na(x) ~ "Sin dato",
    xx %in% c("TRUE", "true", "1", "X1", "Sí", "Si", "SI", "SÍ") ~ "Sí",
    TRUE ~ "No"
  )
}

kpi_card <- function(title, value, subtitle = NULL) {
  shiny::div(
    class = "kpi-card",
    shiny::div(class = "kpi-title", title),
    shiny::div(class = "kpi-value", value),
    if (!is.null(subtitle)) shiny::div(class = "kpi-subtitle", subtitle)
  )
}

make_dt <- function(df, page_length = 15) {
  page_length <- 15
  DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    extensions = c("Buttons"),
    class = "compact stripe hover nowrap",
    options = list(
      pageLength = page_length,
      lengthMenu = list(c(15, 25, 50, -1), c("15", "25", "50", "Todos")),
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel")
    )
  )
}


rescale01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / diff(rng)
}

value_band_levels <- c("0-1 M€", "1-5 M€", "5-15 M€", "15-40 M€", ">40 M€")
