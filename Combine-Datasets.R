# Combine all files into 1 for analysis
# As a note, I remove files as I go along to clear memory
library(tidyverse)

# For other runners on base
source("Other-Runners.R")

# For inning that play occurred in
source("Inning-Finder.R")

# Create modeling dataset
modeling_dataset <- full_join(other_runners, inning_finder,
                              by = c("game_str", "play_id"))
# Get rid of unneeded dfs
rm(inning_finder, other_runners)

# For Distance from Ball to base
source("Ball-2-Base-Dist.R")
modeling_dataset <- left_join(modeling_dataset, throw_distances,
                              by = c("game_str", "play_id"))
# Clean environment
rm(throw_distances)

# For Distance from baserunner to next base
source("BR-2-Base-Dist.R")
modeling_dataset <- left_join(modeling_dataset, br_distances,
                              by = c("game_str", "play_id")) %>% 
  # Going this distance plus 90 to advance 2 bases
  mutate(BR2NextBase = BR2NextBase + 90)
# Get ball going to correct base
modeling_dataset <- modeling_dataset %>% 
  mutate(ball2Base = case_when(currentBase == 1 ~ ball2Third,
                                   currentBase == 2 ~ ball2Home)) %>% 
  select(-c(ball2Second, ball2Third, ball2Home))

# Clear Environment a little bit
rm(br_distances)

# Baserunners from PBP
source("Find-Baserunners-From-PBP.R")

modeling_dataset <- left_join(modeling_dataset, baserunners_final,
                               by = c("game_str", "play_id")) %>% 
  mutate(baserunner = case_when(player_position == 11 ~ onFirst,
                                player_position == 12 ~ onSecond,
                                player_position == 13 ~ onThird)) %>% 
  # Only runners on 1st or 2nd at start of play
  filter(player_position %in% c(11, 12)) %>% 
  select(-c(onFirst, onSecond, onThird, player_position))

# Clear environment one mo gin

rm(baserunners_final)
# For baserunning speeds
source("Base-Running-Speed.R")
modeling_dataset <- left_join(modeling_dataset, final_baserunning,
                               by = c("baserunner" = "player_id")) %>% 
  rename("adj_baserunning_speed" = "avg_speed_fps_adj") %>% 
  mutate(key = paste(game_str, play_id, sep = "_"))

# Clear environment one mo gin
rm(final_baserunning)

# Finally, filter down to only of_plays
source("Ball-2-OF.R")
modeling_dataset <- modeling_dataset %>% 
  filter(key %in% of_plays$key) %>% 
  filter(baserunner != 0) %>% 
  # If unknown, inning = 1
  mutate(inning = case_when(!is.na(inning)~ inning,
                            TRUE ~ 1)) %>% 
  # Ball 2 Next Base Distance cannot be NA
  filter(!is.na(ball2Base)) %>%
  # Reorder Variables
  select(game_str, play_id, key, inning, gameState, currentBase, BR2NextBase,
         adj_baserunning_speed, ball2Base) 

rm(of_plays)

# Outputted this data to CSV and then manually watched every play  using
# the Animate-Play.R file and tracked whether runner was safe or out 
# in OUTCOME column (1 for safe, 0 for out)

# write.csv(modeling_dataset_final,
#          file = "Data/MODELING-DATA.csv",
#          row.names = FALSE)

# Also found out in this step that I need to make sure the ball was hit
# aka not a bad throw on a stolen base

# Need to do only balls in play
source("BIP-Plays.R")
modeling_dataset <- modeling_dataset %>% 
  filter(key %in% bip_plays$key)
  
rm(bip_plays)

# Finally Join on the data that I went through and watched every play
play_results <- read_csv("Data/play_results.csv")

modeling_dataset_final <- left_join(modeling_dataset, play_results,
          by = c("game_str", "play_id", "currentBase")) %>% 
  select(-key)

clean_data <- modeling_dataset_final %>% 
  # Making Catagorical Variables Factors
  mutate(gameState = as.factor(gameState),
         currentBase = as.factor(currentBase))

rm(play_results, modeling_dataset)

# write.csv(clean_data,
#          file = "Data/Clean-Modeling-Data.csv",
#          row.names = FALSE)
