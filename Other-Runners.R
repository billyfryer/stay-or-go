# Calculate other runners on
library(tidyverse)
source("BR-2-Base-Dist.R")

other_runners <- br_distances %>% 
  group_by(game_str, play_id) %>% 
  summarize(onFirst = case_when(11 %in% player_position ~ 1,
                                TRUE ~ 0),
            onSecond = case_when(12 %in% player_position ~ 1,
                                TRUE ~ 0),
            onThird = case_when(13 %in% player_position ~ 1,
                                TRUE ~ 0),
            ) %>% 
  ungroup() %>% 
  mutate(gameState = paste0(onFirst, onSecond, onThird)) %>% 
  select(game_str, play_id, gameState) %>% 
  distinct()
  

rm(br_distances)
