library(tidyverse)
library(janitor)
library(healthcareai)

red <- read_delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", delim = ";")
white <- read_delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", delim = ";'")

wine <- bind_rows(red, white) %>% 
  clean_names() %>% 
  mutate(quality = as_factor(quality)) %>% 
  split_train_test(quality, percent_train = 0.7, seed = 123)

test <- wine$test
train <- wine$train

task_train <- makeClassifTask(data = train, target = "quality")

my_measures <- list(auc, logloss, f1, acc)

xgb <- makeLearner("classif.xgboost", predict.type = "prob") %>%
  makeDummyFeaturesWrapper()

model_xgb <- mlr::train(xgb, task_train)
predict_xgb <- predict(model_xgb, task_train)

xgb1 <- performance(predict_xgb, auc)
xgb2 <- performance(predict_xgb, logloss)
xgb3 <- performance(predict_xgb, acc)
xgb4 <- performance(predict_xgb, f1)

decision_tree <- makeLearner("classif.rpart", predict.type = "prob")

model_dt <- mlr::train(decision_tree, task_train)
predict_dt <- predict(model_dt, task_train)

dt1 <- performance(predict_dt, auc)
dt2 <- performance(predict_dt, logloss)
dt3 <- performance(predict_dt, acc)
dt4 <- performance(predict_dt, f1)

ada_boost <- makeLearner("classif.boosting", predict.type = "prob")

model_ab <- mlr::train(ada_boost, task_train)
predict_ab <- predict(model_ab, task_train)

ab1 <- performance(predict_ab, auc)
ab2 <- performance(predict_ab, logloss)
ab3 <- performance(predict_ab, acc)
ab4 <- performance(predict_ab, f1)


boost <- makeLearner("classif.gbm", predict.type = "prob")

model_boost <- mlr::train(boost, task_train)
predict_boost <- predict(model_boost, task_train)

boost1 <- performance(predict_boost, auc)
boost2 <- performance(predict_boost, logloss)
boost3 <- performance(predict_boost, acc)
boost4 <- performance(predict_boost, f1)

DT <- data.table(
  Algorithm = c("XGBoost","Decision Tree","AdaBoost","Gradient Boosting Machine"),
  AreaUnderCurve = c(xgb1, dt1, ab1, boost1),
  LogLoss = c(xgb2, dt2, ab2, boost2),
  Accuracy = c(xgb3, dt3, ab3, boost3),
  f1Score = c(xgb4, dt4, ab4, boost4)
)

