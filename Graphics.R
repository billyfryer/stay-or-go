# Graphics!

# Animated Plays in Shareable-GIFs.R

# Variable Importance Plot
library(tidyverse)
library(ggplot2)
library(gt)
source("Modeling.R")

game_state_importance <- importance %>% 
  select(Feature, Gain) %>% 
  mutate(Game_State = case_when(str_detect(Feature, "gameState") ~ 1,
                                TRUE ~ 0)) %>% 
  group_by(Game_State) %>% 
  summarize(Gain = sum(Gain)) %>% 
  filter(Game_State == 1) %>% 
  pull(Gain)

importance_clean <- importance %>% 
  select(Feature, Gain) %>% 
  filter(str_detect(Feature, "gameState", negate = TRUE)) %>% 
  as.data.frame() %>% 
  rbind(., c(Feature = "gameState", Gain = game_state_importance)) %>% 
  mutate(Gain = as.numeric(Gain)) %>% 
  mutate(FeatureName = case_when(Feature == "inning" ~ "Inning",
                                 Feature == "gameState" ~ "Game State",
                                 Feature == "currentBase" ~ "Current Base",
                                 Feature == "BR2NextBase" ~ "Distance to Target Base (Runner)",
                                 Feature == "ball2Base" ~ "Distance to Target Base (Ball)",
                                 TRUE ~ "Baserunner Avg Speed"
                                 ))

ggplot(importance_clean, aes(x = Gain,
                             y = reorder(FeatureName, Gain))) +
  geom_bar(stat = "identity") +
  scale_y_discrete(labels = scales::label_wrap(15)) +
  scale_x_continuous(labels = scales::label_percent()) +
  theme_classic() +
  labs(title = "Accuracy Gained by Variable",
       x = "Accuracy Gained",
       y = "",
       caption = "Data from SMT")

ggsave(filename = "Graphics/Accuracy-Gained.png",
       width = 4,
       height = 4)

# Run Expectancy Table
run_expectancy <- read_csv("Data/AA-2015-RE.csv")

run_expectancy %>% 
  gt() %>% 
  # Change Column Labels
  cols_label(
    "game_state" = "Game State",
    "outs_0" = "0 Outs",
    "outs_1" = "1 Outs",
    "outs_2" = "2 Outs"
  ) %>% 
  tab_header(
    title = md("**2015 AA Run Expectancies**"),
  ) %>% 
  # Footnote
  tab_source_note(source_note = "Data from milb.com via baseballr") %>%
  # Tab Options
  tab_options(
    # Bold Title
    heading.title.font.weight = "bold",
    # Change Subtitle Font Size
    heading.subtitle.font.size = 12,
    # Align Heading
    heading.align = "center",
    column_labels.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
  ) %>% 
  cols_align(align = "center") %>%
  fmt_number(c(outs_0, outs_1, outs_2), decimals = 3) %>%
gtsave("Graphics/Run-Expectancies.png")

# Accuracy/log Loss table
data.frame(model = c("Logistic", "Naive Bayes", "Random Forest", "xgboost"),
           test_accuracy = c(.6209, .6494, .7337, .7630)) %>% 
  gt() %>% 
  # Change Column Labels
  cols_label(
    "model" = "Model Type",
    "test_accuracy" = "Test Accuracy"
  ) %>% 
  tab_header(
    title = md("**Test Data Accuracy by Model**"),
  ) %>% 
  # Footnote
  tab_source_note(source_note = "Data from SMT") %>%
  # Tab Options
  tab_options(
    # Bold Title
    heading.title.font.weight = "bold",
    # Change Subtitle Font Size
    heading.subtitle.font.size = 12,
    # Align Heading
    heading.align = "center",
    column_labels.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
  ) %>% 
  cols_align(align = "center") %>%
  fmt_percent(test_accuracy)  %>%
  gtsave("Graphics/Model-Accuracy.png")
  
