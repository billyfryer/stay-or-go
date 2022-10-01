library(shiny)
library(xgboost)
library(parsnip)
library(tidyverse)
library(rsconnect)
library(bslib)

# Deploying Shiny App Details
# rsconnect::setAccountInfo(name='b4billy', 
#                          token='B968395C4FA3905980F693BFF8D1C2FF', 
#                          secret='WqvhsB+dwGClCa5+cGLmzSdhNfVPVJ48zBJAWrXL')
# deployApp()

xgboost.model <- readRDS("final-model.rds")
aa2015_re <- read_csv("AA-2015-RE.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bg = "lightgrey",
                   fg = "black",
                   primary = "red",
                   base_font = 'Helvetica Neue',
                   code_font = 'Helvetica Neue',
                   heading_font = 'Helvetica Neue'),
    # Application title
    titlePanel("Should I Stay or Should I Go Recommendations"),
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
  ### PROBABILITY CALCULATOR TAB
                      tabPanel("Probability Calculator",
                               # Inning Input
                               numericInput(
                                 inputId = "inning.calc.input",
                                 label = "Inning",
                                 value = 1,
                                 min = 1,
                                 step = 1
                               ),
                               # Game State Input
                               selectInput(
                                 inputId = "gameState.calc.input",
                                 label = "Game State",
                                 choices = c("100", "110", "101", "010", "011", "111")
                               ),
                               # Current Base Input
                               selectInput(
                                 inputId = "currentBase.calc.input",
                                 label = "Runner is Currently on ___ Base",
                                 choices = c("1st", "2nd")
                               ),
                               # BR2NextBase Input
                               numericInput(
                                 inputId = "BR2NextBase.calc.input",
                                 label = "Estimated Distance from Base Runner to Target Base (ft)",
                                 value = 90,
                                 min = 0,
                                 max = 180,
                                 step = 1
                               ),
                               # Base running Speed Input
                               numericInput(
                                 inputId = "baserunning_speed.calc.input",
                                 label = "Estimated Baserunning Speed (ft/sec)",
                                 value = 27,
                                 min = 0
                               ),
                               # ball2Base Input
                               numericInput(
                                 inputId = "ball2Base.calc.input",
                                 label = "Estimated Distance from Ball to Target Base (ft)",
                                 value = 200,
                                 min = 0,
                                 step = 1
                               ),
                               # Submit Button
                               actionButton("submit.calc", label = "Submit"),
                               br(),
                               br(),
                               textOutput("probability")),
  ### RUN EXPECTANCY TABE
  #                    tabPanel("Change in Run Expectancy",
  #                             ### RISK SUCCESSFUL
  #                             h3("Risk Successful"),
  #                             # Game State for Failed Risk
  #                             selectInput(
  #                               inputId = "gameState.successful.input",
  #                               label = "Game State",
  #                               choices = c("100", "110", "101", 
  #                                           "010", "011", "111",
  #                                           "000", "001")
  #                             ),
  #                             # Current Base Input
  #                             selectInput(
  #                               inputId = "outs.successful.input",
  #                               label = "Outs",
  #                               choices = c(0, 1, 2)
  #                             ),
  #                             ### RISK FAILED
  #                             h3("Risk Failed"),
  #                             # Game State for Failed Risk
  #                             selectInput(
  #                               inputId = "gameState.failed.input",
  #                               label = "Game State",
  #                               choices = c("100", "110", "101", 
  #                                           "010", "011", "111",
  #                                           "000", "001")
  #                             ),
  #                             # Current Base Input
  #                             selectInput(
  #                               inputId = "outs.failed.input",
  #                               label = "Outs",
  #                               choices = c(1, 2,3)
  #                             ),
  #                             ### No Risk Taken
  #                             h3("No Risk Taken"),
  #                             # Game State for No Risk
  #                             selectInput(
  #                               inputId = "gameState.neutral.input",
  #                               label = "Game State",
  #                               choices = c("100", "110", "101", 
  #                                           "010", "011", "111",
  #                                           "000", "001")
  #                             ),
  #                             # Current Base Input
  #                             selectInput(
  #                               inputId = "outs.neutral.input",
  #                               label = "Outs",
  #                               choices = c(0, 1, 2)
  #                             ),
  #                             # Probability Input
  #                             numericInput(
  #                               inputId = "probability.input",
  #                               label = "Probability of Being Safe at Target Base",
  #                               value = 0.500,
  #                               min = 0,
  #                               max = 1,
  #                               step = 0.01
  #                             ),
  #                             # Submit Button
  #                             actionButton("submit.re", label = "Submit"),
  #                             
  #                             textOutput("re_output")))
  #      )
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
 safe_prob_calculation <- eventReactive(input$submit.calc, {
   # Take all the inputs
   inning <- as.numeric(input$inning.calc.input)
   gameState <- input$gameState.calc.input
   currentBase <- input$currentBase.calc.input
   BR2NextBase <- as.numeric(input$BR2NextBase.calc.input)
   adj_baserunning_speed <- as.numeric(input$baserunning_speed.calc.input)
   ball2Base <- as.numeric(input$ball2Base.calc.input)
   
   # Change currentBase to numeric
   if(currentBase == "1st") {
     currentBase <- 1
   } else {
     currentBase <- 2
   }
   
   # Need to make Game State a Dummy Variable...
   # Nested it in the if 1==1 so that I can collapse it
   if(gameState == "100") {
     gameState100 <- 1
     gameState110 <- 0
     gameState101 <- 0
     gameState010 <- 0
     gameState011 <- 0
     gameState111 <- 0
   } else if(gameState == "110") {
     gameState100 <- 0
     gameState110 <- 1
     gameState101 <- 0
     gameState010 <- 0
     gameState011 <- 0
     gameState111 <- 0
   } else if(gameState == "101") {
     gameState100 <- 0
     gameState110 <- 0
     gameState101 <- 1
     gameState010 <- 0
     gameState011 <- 0
     gameState111 <- 0
   } else if(gameState == "010") {
     gameState100 <- 0
     gameState110 <- 0
     gameState101 <- 0
     gameState010 <- 1
     gameState011 <- 0
     gameState111 <- 0
   } else if(gameState == "011") {
     gameState100 <- 0
     gameState110 <- 0
     gameState101 <- 0
     gameState010 <- 0
     gameState011 <- 1
     gameState111 <- 0
   } else if(gameState == "111") {
     gameState100 <- 0
     gameState110 <- 0
     gameState101 <- 0
     gameState010 <- 0
     gameState011 <- 0
     gameState111 <- 1
   }
   
   # Put all inputs in a MATRIX and bind to data frame
   # xgboost requires a matrix to perform calculations
   # matrix needs to have exactly 1 row
   input_matrix <- matrix(c(inning, gameState010, gameState011,
                             gameState100, gameState101,gameState110, 
                             gameState111, currentBase, BR2NextBase, 
                             adj_baserunning_speed, ball2Base),
                          nrow = 1)
   
   prediction <- predict(xgboost.model, input_matrix)
   
   
   # return prediction
   return(prediction)
 })
 
 target_base <- eventReactive(input$submit.calc, {
   
   if(input$currentBase.calc.input == "1st") {
     final_base <- "third"
   } else {
     final_base <- "home"
   }
   
   final_base
 })
 
 re_calculator <- eventReactive(input$submit.re, {
   # Get inputs
   outs.successful <- paste("outs", input$outs.successful.input, sep = "_")
   outs.failed <- paste("outs", input$outs.failed.input, sep = "_")
   outs.neutral <- paste("outs", input$outs.neutral.input, sep = "_")
   prob <- input$probability.input
   
    # Pull Correct Run Expectancies
   success <- aa2015_re %>% 
     filter(game_state == input$gameState.successful.input) %>% 
     pull(outs.successful)
   
   failure <- if(outs.failed == "outs_3") { 0 } else {
     aa2015_re %>% 
       filter(game_state == input$gameState.failed.input) %>% 
       pull(outs.failed)
   }
   
   neutral <- aa2015_re %>% 
     filter(game_state == input$gameState.neutral.input) %>% 
     pull(outs.neutral)
   
   # Perform Calculations
   a <- prob * success
   b <- (1-prob) * failure
   c <- -1*neutral
   
   a + b + c
 })
 
  
 output$probability <- renderText({
    paste0("Probability of being safe at ",
           target_base(), 
           " is ", round(100*safe_prob_calculation(),2), "%")
    })
 
 # output$re_output <- renderText({
 #   paste("Change in Run Expectancy is:", re_calculator())
 # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
