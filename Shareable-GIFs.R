# Nice Animations
# Distance Between Player and Ball
library(tidyverse)
library(ggplot2)
library(gganimate)
library(geomtextpath)
library(sportyR)

################################################################
### This gives the distance at all times
################################################################

# Didn't go play
# 1902_02_TeamMG_TeamA3 Play ID:313
# Did go play
# 1903_27_TeamNK_TeamB Play ID: 39

# Distance Between Player and Ball
library(tidyverse)
library(ggplot2)
library(gganimate)
library(geomtextpath)
library(sportyR)

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

# For the 2 examples, all I need is 1st to 3rd so it cleans up a little nicer
plotting_data <- tracking_data %>% 
  mutate(ball_to_3 = distance(x1 = ball_position_x, 
                              y1 = ball_position_y,
                              x2 = -sqrt(4050),
                              y2 = sqrt(4050)),
         player_to_3 = distance(x1 = field_x, 
                                y1 = field_y,
                                x2 = -sqrt(4050),
                                y2 = sqrt(4050)),
         player_color = case_when(player_position <= 9 ~"navy",
                                  TRUE ~ "lightblue")) %>% 
  filter(player_position <= 13) %>% 
  mutate(key = paste(game_str, play_id, sep = "_"),
         linealpha = case_when(player_position == 11 ~ 1,
                               TRUE ~ 0))

# Get our two plays
no_go_play <- plotting_data %>% 
  filter(game_str == "1902_02_TeamMG_TeamA3", play_id == 313)

go_play <- plotting_data %>% 
  filter(game_str == "1903_27_TeamNK_TeamB", play_id == 39)

# no_go_play animation
geom_baseball(league = "MLB") +
  # Fielders
  geom_point(data = no_go_play,
             aes(x = field_x,
                 y = field_y,
                 color = player_color),
             size = 3) +
  # Scale color by value
  scale_color_identity() +
  # Line Telling Ball to 3rd
  geom_textsegment(data = no_go_play,
                   aes(x = ball_position_x, xend = -sqrt(4050),
                       y = ball_position_y, yend = sqrt(4050),
                       label = paste(round(ball_to_3), "ft.")),
                   color = "yellow") +
  # Line Telling Player to 3rd
  geom_textsegment(data = no_go_play,
                   aes(x = field_x, xend = -sqrt(4050),
                       y = field_y, yend = sqrt(4050),
                       label = paste(round(player_to_3), "ft."),
                       alpha = linealpha),
                   color = "blue") +
  geom_point(data = no_go_play,
             aes(x = ball_position_x,
                 y = ball_position_y),
             color = "white",
             size = 2) +
  # gganimate code!
  transition_time(
    # Variable that you want each state to represent
    time = no_go_play$timestamp) +
  # Labs and theme
  labs(title = "Probability Safe at Third: 3%",
       caption = "Game: 1902_02_TeamMG_TeamA3 Play ID: 313 ~ Data from SMT",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5))

anim_save(filename = "Graphics/Stay-Example.gif")

# go_play animation
geom_baseball(league = "MLB") +
  # Fielders
  geom_point(data = go_play,
             aes(x = field_x,
                 y = field_y,
                 color = player_color),
             size = 3) +
  # Scale color by value
  scale_color_identity() +
  # Line Telling Ball to 3rd
  geom_textsegment(data = go_play,
                   aes(x = ball_position_x, xend = -sqrt(4050),
                       y = ball_position_y, yend = sqrt(4050),
                       label = paste(round(ball_to_3), "ft.")),
                   color = "yellow") +
  # Line Telling Player to 3rd
  geom_textsegment(data = go_play,
                   aes(x = field_x, xend = -sqrt(4050),
                       y = field_y, yend = sqrt(4050),
                       label = paste(round(player_to_3), "ft."),
                       alpha = linealpha),
                   color = "blue") +
  geom_point(data = go_play,
             aes(x = ball_position_x,
                 y = ball_position_y),
             color = "white",
             size = 2) +
  # gganimate code!
  transition_time(
    # Variable that you want each state to represent
    time = go_play$timestamp) +
  # Labs and theme
  labs(title = "Probability Safe at Third: 88%",
       caption = "Game: 1903_27_TeamNK_TeamB Play ID: 39 ~ Data from SMT",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5))

anim_save(filename = "Graphics/Go-Example.gif")

