# Ball to Base
library(tidyverse)
source("Position-At-Throw.R")

distance <- function(x1, x2, y1, y2) {
  
  output <- sqrt((x2-x1)^2 + (y2-y1)^2)
  
  return(output)
}

throw_distances <- position_at_throw %>%
  arrange(game_str, play_id) %>%
  filter(thrower == player_position) %>% 
  # Replace ball location with thrower location if needed
  mutate(ball_position_x = case_when(is.na(ball_position_x) ~ field_x,
                                     TRUE ~ ball_position_x),
         ball_position_y = case_when(is.na(ball_position_y) ~ field_y,
                                     TRUE ~ ball_position_y)) %>% 
  select(game_str, play_id, ball_position_x, ball_position_y) %>%
  mutate(ball2Second = distance(x1 = ball_position_x,
                               y1 = ball_position_y,
                               x2 = 0,
                               y2 = 90*sqrt(2)),
         ball2Third = distance(x1 = ball_position_x,
                               y1 = ball_position_y,
                               x2 = -1*sqrt(4050),
                               y2 = sqrt(4050)),
         ball2Home = distance(x1 = ball_position_x,
                              y1 = ball_position_y,
                              x2 = 0,
                              y2 = 0),
         ) %>%
  distinct() %>% 
  filter(!is.na(ball2Home)) %>% 
  select(game_str, play_id, starts_with("ball2"))

rm(position_at_throw, distance)
