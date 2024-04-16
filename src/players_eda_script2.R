library(tidyverse)
library(caret)
library(corrplot)
library(ggplot2)
library(e1071)

# Load and inspect the dataset
players <- read.csv("C:/Users/olko/Desktop/OZNAL/data/players_22.csv")
glimpse(players) # Provides a brief overview of the dataset
summary(players) # Summarizes the data giving quick insights

# Data Cleaning

# Remove players that play as a goalkeeper
players <- players %>% 
  filter(!str_detect(player_positions, "GK")) 

# Identify and handle missing values, outliers, and possibly convert data types as needed.
players <- players %>% 
  filter(!is.na(overall)) %>%
  filter(!is.na(potential)) %>%
  filter(!is.na(wage_eur)) %>%
  filter(!is.na(value_eur))

# Prepare Data for Classification

# Assuming player_positions column exists and "B" stands for defenders like "CB", "LB", "RB"
players$IsDefender <- ifelse(grepl("B", players$player_positions), 1, 0) 

# Select predictors - replace this with your actual predictor columns
predictors <- c("pace", "shooting", "passing", "dribbling", "defending", "physic", "height_cm", "weight_kg")

# Prepare the dataset (ensure that you have no missing values in the predictors and the target variable)
classification_data <- players %>% 
  select(IsDefender, all_of(predictors)) %>% 
  na.omit()

# Create training and testing sets
set.seed(123) # for reproducibility
training_rows <- createDataPartition(classification_data$IsDefender, p = 0.8, list = FALSE)
train_data <- classification_data[training_rows, ]
test_data <- classification_data[-training_rows, ]

# Logistic Regression Model
logit_model <- glm(IsDefender ~ ., data = train_data, family = "binomial")
logit_predictions <- predict(logit_model, newdata = test_data, type = "response")
logit_predicted_classes <- ifelse(logit_predictions > 0.5, 1, 0)
# Convert both predicted classes and actual classes to factors with the same levels
logit_predicted_classes <- factor(logit_predicted_classes, levels = c(0, 1))
test_data$IsDefender <- factor(test_data$IsDefender, levels = c(0, 1))
# Create confusion matrix
logit_confMatrix <- confusionMatrix(logit_predicted_classes, test_data$IsDefender)

# SVM Model
svm_model <- svm(IsDefender ~ ., data = train_data, kernel = "radial")
svm_predictions <- predict(svm_model, newdata = test_data)
svm_confMatrix <- confusionMatrix(svm_predictions, test_data$IsDefender)

# Naive Bayes Model
naive_model <- naiveBayes(IsDefender ~ ., data = train_data)
naive_predictions <- predict(naive_model, newdata = test_data)
naive_confMatrix <- confusionMatrix(naive_predictions, test_data$IsDefender)

# Collecting metrics
evaluation_metrics <- tibble(
  Model = c("Logistic Regression", "SVM", "Naive Bayes"),
  Accuracy = c(logit_confMatrix$overall['Accuracy'], svm_confMatrix$overall['Accuracy'], naive_confMatrix$overall['Accuracy']),
  Precision = c(logit_confMatrix$byClass['Precision'], svm_confMatrix$byClass['Precision'], naive_confMatrix$byClass['Precision']),
  Recall = c(logit_confMatrix$byClass['Recall'], svm_confMatrix$byClass['Recall'], naive_confMatrix$byClass['Recall']),
  F1 = c(logit_confMatrix$byClass['F1'], svm_confMatrix$byClass['F1'], naive_confMatrix$byClass['F1'])
)

# Display the evaluation metrics table
print(evaluation_metrics)

