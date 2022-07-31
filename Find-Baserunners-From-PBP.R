library(tidyverse)

# Read in Data 
player_pos <- read_csv("Data/player_pos.csv")

baserunners <- player_pos %>% 
  filter(player_position >= 11) %>% 
  filter(player_position <= 13)

base_situation <- baserunners %>% 
  group_by(game_str, play_id) %>% 
    summarise(player_position = unique(player_position)) %>% 
  ungroup()
  
base_situation[is.na(base_situation)] <- 0

rm(baserunners, player_pos)

# Source in ID matching
source("Player-Position-Lookup.R")

fixed_base_runners <- left_join(base_situation, player_pos_lookup,
                  by= c("game_str", "play_id", "player_position")) %>% 
  mutate(player_id_lag = dplyr::lag(player_id, order_by = player_position),
         player_id_lead = dplyr::lead(player_id, order_by = player_position),
         # Figure out missing player ids
         player_id = case_when(
           # Case 1: player_id already exists
           !is.na(player_id) ~ player_id,
           # Case 2: id before == id after
           player_id_lag == player_id_lead ~ player_id_lag,
           # First batter of game
           play_id == 1 ~ player_id_lead,
           TRUE ~ NA_real_
         )) %>%
  select(-c(player_id_lag, player_id_lead)) 

problems <- fixed_base_runners %>% 
  dplyr::group_by(game_str, play_id, player_position) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n == 1L) %>% 
  ungroup() %>% 
  mutate(key = paste(game_str, play_id, player_position, sep = "_")) %>% 
  select(-n)
  

game_state <- fixed_base_runners %>% 
  mutate(key = paste(game_str, play_id, player_position, sep = "_")) %>% 
  filter(key %in% problems$key) %>%
  pivot_wider(names_from = player_position,
              values_from = player_id) %>% 
  rename("onFirst" = "11",
         "onSecond" = "12",
         "onThird" = "13")

onFirst <- game_state %>% 
  group_by(game_str, play_id, key) %>% 
    summarise(across(c("onFirst"), ~na.omit(.x)))

onSecond <- game_state %>% 
  group_by(game_str, play_id, key) %>% 
  summarise(across(c("onSecond"), ~na.omit(.x)))

onThird <- game_state %>% 
  group_by(game_str, play_id, key) %>% 
  summarise(across(c("onThird"), ~na.omit(.x)))


baserunners_final <- full_join(onFirst, onSecond,
                        by = c("game_str", "play_id", "key")) %>% 
              full_join(., onThird,
                        by = c("game_str", "play_id", "key")) %>% 
  mutate(onFirst = case_when(is.na(onFirst) ~ 0,
                             TRUE ~ onFirst),
         onSecond = case_when(is.na(onSecond) ~ 0,
                             TRUE ~ onSecond),
         onThird = case_when(is.na(onThird) ~ 0,
                             TRUE ~ onThird))

rm(base_situation, fixed_base_runners,player_pos_lookup, problems,
   game_state, onFirst, onSecond, onThird)
