# Save GIFs of all the plays

library(tidyverse)
source("Animate-Play.R")

modeling_data <- read_csv("Data/play_results.csv")

plays_needed <- modeling_data %>% 
  filter(is.na(ADVANCED_2)) %>% 
  select(game_str, play_id) %>% 
  distinct()

for (i in 1:nrow(plays_needed)) {
  # Pull the play_id and game_str
  game_str_needed <- as.character(plays_needed[i, 1])
  play_id_needed <- as.numeric(plays_needed[i, 2])
  
  # Print play_id and game_str
  print(paste0(i, "/", nrow(plays_needed)))
  # Create GIF
  gif <- plot_play(game_str_needed,
                   play_id_needed)
  # Create file name and save in the Play-GIFs folder
  filename <- paste0("Play-GIFs/", game_str_needed, "-",
                     play_id_needed,".gif")
  
  # Save Animation as a gif
  anim_save(filename = filename,
            animation  = gif)
  rm(game_str_needed, play_id_needed, gif, filename)
}
