# Make Player Position Look Up Table

# Libraries and read data
library(tidyverse)

game_info <- read_csv("Data/game_info.csv")
game_events <- read_csv("Data/game_events.csv")

# Join game_info and game_events together
game_info_events <- full_join(game_info, game_events,
                              by = c("game_str", "at_bat", "play_per_game")) 


# Filter down datasets as needed
player_pos_lookup <- game_info_events %>% 
  select(game_str, play_id, pitcher:third_baserunner) %>% 
  pivot_longer(cols = pitcher:third_baserunner,
               names_to = "position",
               values_to = "player_id") %>% 
  mutate(player_position = case_when(
    position == "pitcher" ~ 1,
    position == "catcher" ~ 2,
    position == "first_base" ~ 3,
    position == "second_base" ~ 4,
    position == "third_base" ~ 5,
    position == "shortstop" ~ 6,
    position == "left_field" ~ 7,
    position == "center_field" ~ 8,
    position == "right_field" ~ 9,
    position == "batter" ~ 10,
    position == "first_baserunner" ~ 11,
    position == "second_baserunner" ~ 12,
    position == "third_baserunner" ~ 13,
    TRUE ~ 0
  )) %>% 
  distinct() %>% 
  mutate(key = paste(game_str, play_id, sep = "_")) %>%
  select(key, game_str, play_id, player_position, player_id)

# Clear Environment a bit
rm(game_events, game_info, game_info_events)
