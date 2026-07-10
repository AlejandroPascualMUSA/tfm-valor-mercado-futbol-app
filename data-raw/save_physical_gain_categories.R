# Export exact gain-by-category data for the Shiny app from Lesiones4_corregidoP4_reinicio.qmd.
# Run this at the end of the load-injury model script, after model_3B_opt exists.

library(dplyr)
library(stringr)
library(readr)

imp <- model_3B_opt$importance
imp$Feature <- as.character(imp$Feature)

imp <- imp %>%
  mutate(group = case_when(
    str_detect(Feature, "Sprint|HSR") ~ "Speed / High Intensity",
    str_detect(Feature, "Running|HI.Distance") ~ "Volume",
    str_detect(Feature, "Acc|Dec") ~ "Neuromuscular Load",
    str_detect(Feature, "Explosive") ~ "Explosiveness",
    str_detect(Feature, "PSV|M.min") ~ "Performance Capacity",
    str_detect(Feature, "days_since|days_in|densidad") ~ "Temporal",
    str_detect(Feature, "load|chronic|acwr|spike") ~ "External Load",
    str_detect(Feature, "cluster|rol|position") ~ "Role / Position",
    TRUE ~ "Other"
  ))

imp_top <- imp %>%
  group_by(group) %>%
  arrange(desc(Gain), .by_group = TRUE) %>%
  mutate(rank = row_number())

top5 <- imp_top %>% filter(rank <= 5) %>% select(group, Feature, Gain)
other <- imp_top %>%
  filter(rank > 5) %>%
  summarise(Gain = sum(Gain, na.rm = TRUE), .groups = "drop") %>%
  mutate(Feature = "other")

bind_rows(top5, other) %>%
  arrange(group, desc(Gain)) %>%
  readr::write_csv("data/app/physical_gain_categories.csv")
