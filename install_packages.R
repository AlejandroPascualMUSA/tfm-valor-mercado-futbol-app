packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly", "DT",
  "readr", "scales", "stringr", "ggrepel", "tibble", "htmltools", "png", "colourpicker", "jsonlite"
)
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)
message("Dependencias listas.")
