# Ball Hit to OF Plays
library(tidyverse)
game_info <- read_csv("Data/game_info.csv")
game_events <- read_csv("Data/game_events.csv")

# Join game_info and game_events together
game_info_events <- full_join(game_info, game_events,
                              by = c("game_str", "at_bat", "play_per_game")) 

of_plays <- game_info_events %>% 
  # Group by game and play
  group_by(game_str, play_id) %>% 
  summarize(
    LF.Catch.YN = case_when(
      # Yes/No variable where the LF acquires the ball
      7 %in% player_position & event_code == 2 ~ 1,
      TRUE ~ 0),
    # Yes/No variable where the CF acquires the ball
    CF.Catch.YN = case_when(
      8 %in% player_position & event_code == 2 ~ 1,
      TRUE ~ 0),
    # Yes/No variable where the RF acquires the ball
    RF.Catch.YN = case_when(
      9 %in% player_position & event_code == 2 ~ 1,
      TRUE ~ 0)
  ) %>% 
  ungroup() %>% 
  # Filters down rows that appear more than once
  distinct() %>% 
  # Only plays where an outfielder makes a catch
  filter(LF.Catch.YN == 1 | CF.Catch.YN == 1 | RF.Catch.YN == 1) %>% 
  # This limits us to only plays where one outfielder touches the ball.
  # This was necessary because of some instances in the data set where
  # the ball bounced off the glove of one outfielder and went to another
  # outfielder. There was an instance there was a throw by the catcher
  # trying to backpick a runner where a throw went to the outfielder
  mutate(one.outfielder = LF.Catch.YN + CF.Catch.YN + RF.Catch.YN) %>%
  filter(one.outfielder == 1) %>% 
  # This is to make it easier for me to filter tracking_data_final
  mutate(key = paste(game_str, play_id, sep = "_")) %>% 
  select(game_str, play_id, key)

rm(game_events, game_info, game_info_events)
