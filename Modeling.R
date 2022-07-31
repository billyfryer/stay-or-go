# Modeling!!!
library(tidyverse)
library(ggplot2)
library(performance)
library(e1071)
library(caTools)
library(caret)
library(randomForest)
library(xgboost)

clean_data <- read_csv("Data/Clean-Modeling-Data.csv")

modeling_data <- clean_data %>% 
  select(-c(game_str, play_id)) %>% 
  # Scaling Numeric Variables
  mutate(inning = scale(inning)[,1],
         BR2NextBase = scale(BR2NextBase)[,1],
         adj_baserunning_speed = scale(adj_baserunning_speed)[,1],
         ball2Base = scale(ball2Base)[,1]) %>% 
  # Order columns
  select(ADVANCED_2, everything())


### Splitting Data into training and testing sets
set.seed(6) # The number of consecutive playoffs the Biscuits
            # have made without a series win aka why I'm depressed.
n <- nrow(modeling_data)
prop <- 0.6
train <- sample(n, size = n * prop)
data.train <- modeling_data[train,]
data.test <- modeling_data[-train,]

######################### Logistic Regression ######################### 
# Create Model
logistic_model <- glm(ADVANCED_2 ~ .,
                      data = data.train,
                      family = "binomial")
# Quick Summary
summary(logistic_model)

# Prediction on the test test
predict.test <- predict(logistic_model,
                        data.test,
                        type = "response")

data.test <- bind_cols(data.test, predictions = predict.test)

# Performance checks
performance::model_performance(logistic_model) # Log Loss = 0.626
performance::performance_accuracy(logistic_model) # 62.09%

######################### Naive Bayes Classifier ######################### 
# From Example here: https://www.geeksforgeeks.org/naive-bayes-classifier-in-r-programming/
# Fitting Naive Bayes Model to training dataset
set.seed(2004)  # Montgomery Biscuit's First Year
classifier_cl <- naiveBayes(ADVANCED_2 ~ ., data = data.train)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = data.test)

# Confusion Matrix
cm <- table(data.test$ADVANCED_2, y_pred)
cm
cm %>% prop.table()

# Model Evaluation
caret::confusionMatrix(cm) # Test Set Accuracy 64.94%

# This is only slightly more accurate than the Logistic Regression Model and
# less interpretable, so in the words of Randy: "That's going to be a
# no from me dawg"

######################### Random Forest ######################### 
set.seed(9) # Jackie Robinson in the Minors
# Random Forest needs the response as a factor for classification
modeling_data2 <- modeling_data %>% 
  mutate(ADVANCED_2 = as.factor(ADVANCED_2))
data.train2 <- modeling_data2[train,]
data.test2 <- modeling_data2[-train,]
rf.model <- randomForest(ADVANCED_2 ~ ., 
                        data = data.train2, 
                        ntree = 100,
                        importance = TRUE,
                        proximity = TRUE)
rf.model

# Classification Predictions
prediction <- predict(rf.model, data.test2)

# Confusion Matrix
table(prediction, data.test2$ADVANCED_2) %>% prop.table()

sum(prediction == data.test2$ADVANCED_2) / nrow(data.test2)
# Test Set Accuracy = 73.37% 
# 10% better than the Logisitc Regression Model
# So definitely worth strong consideration

# Probability predictions
# These are the percentage of votes 
rf.probs <-  rf.model$votes %>% as.data.frame() %>% .$`1`

varImpPlot(rf.model)


######################### xgboost ######################### 
# Following example here: 
# https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html

# xgboost only works with numeric variables 
# so this will take some data manipulation
dummy <- dummyVars(" ~ .", 
                   data = clean_data %>% 
                     select(-c(game_str, play_id)))

# Perform one-hot encoding on data frame
xgboost.clean_data <- data.frame(predict(dummy, newdata = clean_data))

# Separate data from label
xgboost.matrix <- xgboost.clean_data %>% select(-ADVANCED_2) %>% as.matrix()
xgboost.label <- xgboost.clean_data$ADVANCED_2

### Splitting Data into training and testing sets
n <- nrow(xgboost.matrix)
prop <- 0.6
set.seed(26) # Joey Wiemer
train <- sample(n, size = n * prop)
xgboost.matrix.train <- xgboost.matrix[train,]
xgboost.matrix.test <- xgboost.matrix[-train,]
xgboost.label.train <- xgboost.label[train]
xgboost.label.test <- xgboost.label[-train]

# Train Model
xgboost.model <- xgboost(data = xgboost.matrix.train, 
                         label = xgboost.label.train, 
                         nrounds = 10,
                         objective = "binary:logistic")
# Logloss = 0.290046

# Predictions
xgb_preds <- predict(xgboost.model, xgboost.matrix.test) %>% 
  as.data.frame()

outcomes <- bind_cols(xgboost.label.test, xgb_preds) 
names(outcomes) <- c("actual", "expected_prob")
outcomes <- outcomes %>% 
  mutate(expected_class = case_when(expected_prob >= 0.5 ~ 1,
                                    TRUE ~ 0))
sum(outcomes$actual == outcomes$expected_class) / nrow(outcomes)
# Test Set Accuracy = 76.30%

# Feature Importance
importance <- xgb.importance(feature_names = colnames(xgboost.matrix.train),
                             model = xgboost.model)
head(importance)
xgb.plot.importance(importance_matrix = importance)

# The xgboost model seems to slightly outperform the random forest
# model so we're going to go with that!
#rm(list=setdiff(ls(), c("importance", "xgboost.model", "clean_data", "xgboost.matrix")))


# Get all predictions from xgboost
xgb_preds <- predict(xgboost.model, xgboost.matrix) %>% 
  as.data.frame()

# Add predictions to clean_data
clean_data_w_prob <- cbind(clean_data, xgb_preds) %>% 
  as.data.frame()

clean_data_w_prob <- clean_data_w_prob %>% 
  select(game_str, play_id, ADVANCED_2,  "safe_prob" = ".", everything())

rm(list=setdiff(ls(), c("importance", "xgboost.model", "clean_data_w_prob")))

# write.csv(clean_data_w_prob,
#           file = "Data/Data-With-Prob.csv",
#           row.names = FALSE)
