# Find the inning each play occured in 
library(tidyverse)
# Read in data
game_info <- read_csv("Data/game_info.csv")
game_events <- read_csv("Data/game_events.csv")

# Join Data
game_info_events <- full_join(game_info, game_events,
                              by = c("game_str", "at_bat", "play_per_game"))

inning_finder <- game_info_events %>% 
  select(game_str, play_id, inning) %>% 
  distinct()

rm(game_info, game_events, game_info_events)
