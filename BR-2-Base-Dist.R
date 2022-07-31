# Fielder to base distance
library(tidyverse)
source("Position-At-Throw.R")

distance <- function(x1, x2, y1, y2) {
  
  output <- sqrt((x2-x1)^2 + (y2-y1)^2)
  
  return(output)
}

br_distances <- position_at_throw %>%
  arrange(game_str, play_id) %>% 
  # Baserunners excluding batters
  filter(player_position %in% c(11,12,13)) %>% 
  select(game_str, play_id, player_position, field_x, field_y) %>%
  mutate(BR2Second = distance(x1 = field_x,
                                y1 = field_y,
                                x2 = 0,
                                y2 = 90*sqrt(2)),
         BR2Third = distance(x1 = field_x,
                               y1 = field_y,
                               x2 = -1*sqrt(4050),
                               y2 = sqrt(4050)),
         BR2Home = distance(x1 = field_x,
                              y1 = field_y,
                              x2 = 0,
                              y2 = 0)) %>%
  # Need to go back and case_when for if they've already passed a base
  # Doing this in 2 separate steps because it's easier to understand
  mutate(BR2Second = case_when(player_position %in% c(11) ~ BR2Second,
                               TRUE ~ 0),
         BR2Third = case_when(player_position %in% c(11,12) ~ BR2Third,
                              TRUE ~ 0),
         BR2Home = case_when(player_position %in% c(11,12, 13) ~ BR2Home,
                             TRUE ~ 0)) %>% 
  # Distance to the next base (or 2nd base for batters)
  mutate(BR2NextBase = case_when(player_position %in% c(11) ~ BR2Second,
                                 player_position == 12 ~ BR2Third,
                                 player_position == 13 ~ BR2Home)) %>% 
  filter(!is.na(BR2Home)) %>% 
  select(game_str:player_position, BR2NextBase) %>% 
  # Make currentBase variable
  mutate(currentBase = case_when(player_position == 11 ~ 1,
                                 player_position == 12 ~ 2,
                                 player_position == 13 ~ 3))

rm(position_at_throw, distance)
