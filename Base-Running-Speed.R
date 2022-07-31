# Average speed for each player as a baserunner
library(tidyverse)

# Calculates Distance Given 2 xs and 2ys
distance <- function(x1, x2, y1, y2) {
  output <- sqrt((y2-y1)^2 + (x2-x1)^2)
  return(output)
}

# Read in Data
ball_pos <- read_csv("Data/ball_pos.csv")
player_pos <- read_csv("Data/player_pos.csv")

position_data <- full_join(ball_pos, player_pos,
                           by = c("game_str", "play_id", "timestamp"))

# Join position_data and player position lookup
source("Player-Position-Lookup.R")

position_data_full <- left_join(position_data, player_pos_lookup,
                    by = c("game_str", "play_id", "player_position")) %>% 
  mutate(key = paste(game_str, play_id, sep = "_"))

# Only plays where ball was hit into play
source("BIP-Plays.R")
position_data_filtered <- position_data_full %>% 
  filter(key %in% bip_plays$key)

# Caluculate Average Baserunning speed
calculate_delta_dist <- position_data_filtered %>% 
  # Need to only be base runners
  filter(player_position %in% 10:13) %>% 
  # Get all change in distance and time for every timestamp
  group_by(game_str, play_id, player_id) %>% 
  # Get Change in distance ata a timestamp
  summarize(delta_dist = abs(distance(x1 = field_x, x2 = lead(field_x),
                               y1 = field_y, y2 = lead(field_y))),
            timestamp = timestamp) %>% 
  ungroup() %>% 
  # Get rid of when delta_dist is NA
  filter(!is.na(delta_dist)) %>% 
  # Get rid of when player_id is NA
  filter(!is.na(player_id))

calculate_total_dist <- calculate_delta_dist %>% 
  # Get total distance traveled/time by player BY PLAY
  group_by(game_str, play_id, player_id) %>% 
    summarize(total_dist = sum(delta_dist, na.rm = TRUE),
           total_time = max(timestamp, na.rm = TRUE) -
                        min(timestamp, na.rm = TRUE)) %>% 
  ungroup() %>%
  # Only select useful variables for debugging
  select(game_str, play_id, player_id, 
         total_dist, total_time) %>% 
  # Only need one copy of every row
  distinct() %>% 
  # Only when they've traveled over 30 feet on a single play
  filter(total_dist >= 30)

# Get big average speed 
br_speed_lookup <- calculate_total_dist %>% 
  # Average the speed by player
  group_by(player_id) %>% 
  # Sum distance traveled/time by player ONLY
    summarize(big_total_dist = sum(total_dist, na.rm = TRUE),
              big_total_time = sum(total_time, na.rm = TRUE),
              # number of attempts
              count = n()) %>% 
  # Calculate average speed by player
    mutate(avg_speed = big_total_dist / big_total_time) %>% 
    ungroup() %>% 
  # Convert to feet per seconds
    mutate(avg_speed_fps = avg_speed * 1000)

# Plotting avg_speed density
ggplot(br_speed_lookup, aes(avg_speed_fps)) +
 geom_density() +
  labs(title = "Average Speed of Runners",
       x = "Average Speed (feet/second)",
       y = "Density",
       caption = "Data from SMT") +
  scale_x_continuous() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("segment", 
           x = 200, xend = 50, 
           y = 0.08, yend = 0.105,
         arrow = arrow()) +
  geom_label(x = 300, y = 0.0775, label = "Average Speed is\naround 26 ft/sec")

# ggsave(filename = "Graphics/Average-Speed.jpg",
#        width = 4,
#        height = 4)

#' I this looks pretty normal
#' so I'm going to roughly check by emperical rule

# Mu and Sigma
mu <- mean(br_speed_lookup$avg_speed_fps)
sigma <- sd(br_speed_lookup$avg_speed_fps)

emperical_rule_check <- function(mean, sd, how_many, 
                                 df = br_speed_lookup) {

lower <- mean - how_many*sd
upper <- mean + how_many*sd

temp <- df %>% 
  mutate(lowerYN = case_when(avg_speed_fps >= lower ~ 1,
                             TRUE ~ 0),
         upperYN = case_when(avg_speed_fps <= upper ~ 1,
                             TRUE ~ 0),
         both = case_when(lowerYN + upperYN == 2 ~ 1,
                          TRUE ~ 0)
         )

return(sum(temp$both) / nrow(temp))
}

emperical_rule_check(mean = mu, sd = sigma, how_many = 1)
# 80.84291% are within 1 standard deviation so that works with me

emperical_rule_check(mean = mu, sd = sigma, how_many = 2)
# 97.31801% are within 2 standard deviations so that works with me

emperical_rule_check(mean = mu, sd = sigma, how_many = 3)
# 99.23372% Are within 3 standard deviations so that works with me


# Overall this is pretty close to Normal, and CLT probably
# has me covered otherwise

# Replace people that are crazier than 3 sds with mu
br_speed_lookup_adj <- br_speed_lookup %>% 
  select(player_id, avg_speed_fps, count) %>% 
  mutate(avg_speed_fps_adj = case_when(avg_speed_fps <= mu -3*sigma ~ mu,
                                       avg_speed_fps >= mu + 3*sigma ~ mu,
                                       TRUE ~ avg_speed_fps))

# Plotting avg_speed to total dist
# ggplot(br_speed_lookup_adj) +
#   # Normal
#   stat_function(fun = dnorm, 
#                 n = 101, 
#                 args = list(mean = mu, sd = sigma)) +
#   # Adjusted
#   geom_density(aes(x = avg_speed_fps_adj),
#                color = "yellow") +
#   # Actual
#   geom_density(aes(x = avg_speed_fps),
#                color = "red") +
#   theme_dark() +
#   xlim(0, 100)

final_baserunning <- player_pos_lookup %>% 
  select(player_id) %>% 
  distinct() %>% 
  left_join(. , br_speed_lookup_adj, by = "player_id") %>% 
  # Replace NAs in avg_speed_fps_adj with mu,
  # Other columns don't really matter tbh
  mutate(avg_speed_fps_adj = case_when(is.na(avg_speed_fps) ~ mu,
                                       TRUE ~ avg_speed_fps)) %>% 
  select(player_id, avg_speed_fps_adj)

# Clean Environment for sourcing
rm(ball_pos, bip_plays, player_pos, player_pos_lookup, br_speed_lookup)
rm(br_speed_lookup_adj, calculate_delta_dist, calculate_total_dist)
rm(position_data, position_data_filtered, position_data_full)
rm(mu, sigma, distance, emperical_rule_check)


### THINGS TO REMEMBER TO WRITE
#'
#' * Speed only measured when ball is hit into play
#' and runner travels over 30 feet
#' 
#' * Speed calculated by total distance traveled / total time traveled.
#' This is NOT speed by play, averaged together
#' 
#' * Players with completely missing data or who were more than 3 sds
#' away from the mean were replaced with the mean speed in f/sec

