#XGBoost

Final_RF_frame.csv <- read.csv("Final_RF_frame.csv")

library(xgboost)
library(caTools)
library(dplyr)
library(cvms)
library(caret)

set.seed(42)
sample_split <- sample.split(Y = iris$Species, SplitRatio = 0.7)
train_set <- subset(x = iris, sample_split == TRUE)
test_set <- subset(x = iris, sample_split == FALSE)

y_train <- as.integer(train_set$Species) - 1
y_test <- as.integer(test_set$Species) - 1
X_train <- train_set %>% select(-Species)
X_test <- test_set %>% select(-Species)