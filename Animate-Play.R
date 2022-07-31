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

plot_play <- function(game, play, 
                      df = plotting_data) {
  
  game <- as.character(game)
  play <- as.numeric(play)

single_play <- df %>% 
  filter(game_str == game & play_id == play) %>% 
  mutate(line_alpha = case_when(player_position <= 10 ~ 0,
                                TRUE ~ 1))

geom_baseball(league = "MLB") +
  # Fielders
  geom_point(data = single_play,
             aes(x = field_x,
                 y = field_y,
                 color = player_color),
             size = 3) +
  # Scale color by value
  scale_color_identity() +
  # Line Telling Ball to 2nd
  geom_textsegment(data = single_play,
                   aes(x = ball_position_x, xend = 0,
                       y = ball_position_y, yend = 90*sqrt(2),
                       label = paste(round(ball_to_2), "ft.")),
                   color = "yellow") +
  # Line Telling Ball to 3rd
  geom_textsegment(data = single_play,
                   aes(x = ball_position_x, xend = -sqrt(4050),
                       y = ball_position_y, yend = sqrt(4050),
                       label = paste(round(ball_to_3), "ft.")),
                   color = "yellow") +
  # Line Telling Player to 2nd
  geom_textsegment(data = single_play,
                   aes(x = field_x, xend = 0,
                       y = field_y, yend = 90*sqrt(2),
                       label = paste(round(player_to_2), "ft."),
                       alpha = line_alpha),
                   color = "blue") +
  # Line Telling Player to 3rd
  geom_textsegment(data = single_play,
                   aes(x = field_x, xend = -sqrt(4050),
                       y = field_y, yend = sqrt(4050),
                       label = paste(round(player_to_3), "ft."),
                       alpha = line_alpha),
                   color = "blue") +
  geom_point(data = single_play,
             aes(x = ball_position_x,
                 y = ball_position_y),
             color = "white",
             size = 2) +
  # gganimate code!
  transition_time(
    # Variable that you want each state to represent
    time = single_play$timestamp) +
  # Labs and theme
  labs(title = "Fielder Distance to Ball",
       subtitle = paste("Game:", game, 
                        "Play ID:", play),
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5))
}

rm(ball_pos, player_pos, tracking_data, distance)

plot_play("1903_16_TeamNI_TeamA3",
          205)
