#' Sample MiLB Run Expectancy
#'
#' How to calculate Run Expectancy
#' ___________________________________________________
#' 1) Get PBP
#' 2) Runs at current moment
#' 3) Get Runs at end of half inning
#' 4) Runs yet to be scored = runs at current moment - runs at end of 1/2 inning
#' 5) Get State of game (runners on and outs)
#' 
#' Ideally dataframe would be:
#' RUN ON 1ST | RUN ON 2ND | RUN ON 3RD | OUTS | RUNS YET TO BE SCORES
#' 
#' 6) Average by state

# Libraries
library(tidyverse)
library(baseballr)
library(progressr)

# Change year and level inputs here
year <- 2015
level <- 12

# level_ids key
#' 1 = MLB
#' 11 = Triple-A
#' 12 = Double-A
#' 13 = Class A Advanced
#' 14 = Class A
#' 15 = Class A Short Season
#' 5442 = Rookie Advanced
#' 16 = Rookie
#' 17 = Winter League

### Step 1: Get Data
# Get All ids of a certain level
schedule <- mlb_schedule(season = year,
                  level_ids = level)

# Get some Game IDs from 5 games
# Eventually, this will be all IDs
ids <- unique(schedule$game_pk)

# Help prevent errors
daily_stats <- function(game_id){
  tryCatch(
    mlb_pbp(game_pk = game_id) # |> 
    # Data cleaning/manipulation
    ,
    error = function(cond){tibble()}
  )
}

# Get All pbp
pbp <- map(ids, ~{ daily_stats(.x)}) %>%
  bind_rows()

### Steps 2-5) Data Cleaning
run_expectancies <- pbp %>% 
  # Only last pitch of each at bat, to reduce size of data
  # and thus improve speed.
  group_by(game_pk, about.inning, about.halfInning, atBatIndex) %>%
  # Take last row of each at bat
  slice_max(order_by = index, n = 1) %>% 
  ### Step 2: Runs at Current Moment
  mutate(
    CURRENT.RUNS = case_when(
      # Bottom of Inning == Home Hits, so only home score changes
      about.halfInning == "bottom" ~ result.homeScore,
      # Top of Inning == Away Hits, so only away score changes
      about.halfInning == "top" ~ result.awayScore
    )) %>% 
  ungroup() %>% 
  ### Step 3: Get Runs at end of half inning
  group_by(game_pk, about.inning, about.halfInning) %>% # Not grouped at atbat level
  mutate(
    END.HALF.INNING.RUNS = case_when(
      # Bottom of Inning == Home Hits, so only home score changes
      about.halfInning == "bottom" ~ max(result.homeScore),
      # Top of Inning == Away Hits, so only away score changes
      about.halfInning == "top" ~ max(result.awayScore)
    )) %>% 
  ungroup() %>% 
  ### Step 4: Calculate Runs Yet to Be Scored in 1/2 inning
  mutate(RUNS.REMAINING = END.HALF.INNING.RUNS - CURRENT.RUNS, 
  ### Step 5: Get State of game (runners on and outs)
  RUNNER.ON.1 = case_when(is.na(matchup.postOnFirst.fullName) ~ 0,
                            TRUE ~ 1),
  RUNNER.ON.2 = case_when(is.na(matchup.postOnSecond.fullName) ~ 0,
                       TRUE ~ 1),
  RUNNER.ON.3 = case_when(is.na(matchup.postOnThird.fullName) ~ 0,
                       TRUE ~ 1),
  OUTS = count.outs.end) %>% 
  # Only want to select created columns
  select(starts_with("RUNNER.ON"), OUTS, RUNS.REMAINING) %>% 
  # Can't score any more runs if there are 3 outs
  filter(OUTS != 3) %>% 
### Step 6:  Average by state
# aka Calculate Run Expectancies
  group_by(RUNNER.ON.1, RUNNER.ON.2, RUNNER.ON.3, OUTS) %>%
    summarize(AVG.RUNS = mean(RUNS.REMAINING, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Make a state variable by pasting runners on
  mutate(game_state = paste0(RUNNER.ON.1, RUNNER.ON.2, RUNNER.ON.3)) %>% 
  select(game_state, OUTS, AVG.RUNS) %>% 
  pivot_wider(names_from = OUTS,
              names_prefix = "outs_",
              values_from = AVG.RUNS)
  
# For some reason, there was a typo somewhere in the pbp
# So I had to get rid of a column where outs == 4
run_expectancies <- run_expectancies %>% 
  select(-outs_4)

write.csv(x = run_expectancies,
          file = "Data/AA-2015-RE.csv",
          row.names = FALSE)