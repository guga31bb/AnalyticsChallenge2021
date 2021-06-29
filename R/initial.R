library(tidyverse)

pbp <- read_csv("data/PlayByPlay.csv") %>%
  janitor::clean_names()
events <- read_csv("data/SkillPositionPlayers.csv") %>%
  janitor::clean_names() %>%
  filter(route != "NULL", on_field_position != "B", side_of_center != "NULL")

events %>%
  mutate(
    # fix all these weird classifications that aren't really different things
    route = case_when(
      route == "Fade - Back Shoulder" ~ "Back Shoulder Fade",
      route == "Screen - Bubble" ~ "Screen",
      route == "Screen - Quick" ~ "Screen",
      route == "Flat - Right" ~ "Flat",
      route == "Flat - Left" ~ "Flat",
      route == "Chip - Flat" ~ "Flat",
      route == "Screen - Tunnel" ~ "Screen",
      route == "Chip - Curl" ~ "Curl",
      route == "Chip - Drag" ~ "Drag",
      route == "Swing - Left" ~ "Swing",
      route == "Swing - Right" ~ "Swing",
      route == "Chip - Seam" ~ "Seam",
      TRUE ~ route
      )
  ) %>%
  select(game_id, event_id, side_of_center, order_outside_to_inside, route) %>%
  pivot_wider(
    id_cols = c(game_id, event_id, side_of_center),
    names_from = order_outside_to_inside,
    values_from = route
  ) %>%
  filter(!is.na(`2`)) %>%
  mutate(
    combo = glue::glue("{`1`} // {`2`} {ifelse(is.na(`3`), '', '//')} {ifelse(!is.na(`3`), `3`, '')}")
  ) %>%
  mutate(
    combo = case_when(
      # group the different types of screen with blocking together
      combo %in% c("Screen // Blocking  ", "Blocking // Blocking // Screen", "Blocking // Screen  ", "Screen // Blocking // Blocking") ~ "Screen with blockers",
      TRUE ~ paste0(combo)
    )) %>%
  group_by(combo) %>%
  summarize(n = n()) %>%
  arrange(-n) %>%
  head(20) %>%
  ngscleanR:::make_table()
