# Create DF of plays where ball was hit into play

# Libraries and Read in data
library(tidyverse)

game_events <- read_csv("Data/game_events.csv")

# Find Plays where ball is hit in play
bip_plays <- game_events %>% 
  group_by(game_str, play_id) %>% 
    filter(event_code == 4) %>% 
  ungroup() %>%
  select(game_str, play_id) %>% 
  mutate(key = paste(game_str, play_id, sep = "_"))

rm(game_events)
