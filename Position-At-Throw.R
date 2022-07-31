# Source code to animate play and then went through and labeled manually

library(tidyverse)

################################################################
### This gives the distance at all times
################################################################

ball_pos <- read_csv("Data/ball_pos.csv")
player_pos <- read_csv("Data/player_pos.csv")

# Join all tracking data together
tracking_data <- full_join(ball_pos, player_pos,
                           by = c("game_str", "play_id", "timestamp")) 

distance <- function(x1, x2, y1, y2) {
  output <- sqrt((y2-y1)^2 + (x2-x1)^2)
  return(output)
}

plotting_data <- tracking_data %>% 
  mutate(ball_to_2 = distance(x1 = ball_position_x, 
                              y1 = ball_position_y,
                              x2 = 0,
                              y2 = 90*sqrt(2)),
         ball_to_3 = distance(x1 = ball_position_x, 
                              y1 = ball_position_y,
                              x2 = -sqrt(4050),
                              y2 = sqrt(4050)),
         player_to_2 = distance(x1 = field_x, 
                                y1 = field_y,
                                x2 = 0,
                                y2 = 90*sqrt(2)),
         player_to_3 = distance(x1 = field_x, 
                                y1 = field_y,
                                x2 = -sqrt(4050),
                                y2 = sqrt(4050)),
         player_color = case_when(player_position <= 9 ~"navy",
                                  TRUE ~ "lightblue")) %>% 
  filter(player_position <= 13) %>% 
  mutate(key = paste(game_str, play_id, sep = "_"))

################################################################
### This gives the time we need the distance at (time of throw)
################################################################

source("Ball-2-OF.R")
game_events <- read_csv('Data/game_events.csv') %>% 
  mutate(key = paste(game_str, play_id, sep = "_"))

time_of_throw <- game_events %>% 
  # In of_plays
  filter(key %in% of_plays$key) %>% 
  # The game event where the of makes the throw
  filter(player_position %in% c(7,8,9) & event_code == 3) %>% 
  select(game_str, play_id, key, timeOfThrow = timestamp, 
         thrower = player_position)

################################################################
### Get positions at time of throw
################################################################

position_at_throw <- left_join(plotting_data, time_of_throw,
          by = c("game_str", "play_id", "key")) %>% 
  filter(timeOfThrow == timestamp)

# Clean environment
rm(game_events, of_plays, ball_pos, player_pos, tracking_data, plotting_data, 
   time_of_throw, distance)