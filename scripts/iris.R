library(ucimlr)
library(actools)
library(mlr)
library(mlrMBO)
library(tidyverse)
library(data.table)
library(caTools)

my_measures <- list(logloss, acc)
resample_desc <- makeResampleDesc("CV", iters = 10)

## xgboost
data(iris)
task <- makeClassifTask(data = iris, target = "Species")

xgboost <- makeLearner("classif.xgboost", predict.type = "prob") %>%
  makeDummyFeaturesWrapper()

model_xgb <- mlr::train(xgboost, task)
predict_xgb <- predict(model_xgb, task)

f1_score <- function(predicted, expected, positive.class="1") {
  predicted <- factor(as.character(predicted), levels=unique(as.character(expected)))
  expected  <- as.factor(expected)
  cm = as.matrix(table(expected, predicted))
  
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  
  #Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  
  #Binary F1 or Multi-class macro-averaged F1
  ifelse(nlevels(expected) == 2, f1[positive.class], mean(f1))
}

xgb1 <- colAUC(getPredictionProbabilities(predict_xgb), predict_xgb$data$truth)
xgb2 <- performance(predict_xgb, logloss)
xgb3 <- performance(predict_xgb, acc)
xgb4 <- f1_score(predict_xgb$data$response, predict_xgb$data$truth, positive.class="1")

## tuning parameters (xgboost)
xgbParams <- getParamSet("classif.xgboost")

params.xgboost <- makeParamSet(
  makeIntegerParam("nrounds", lower = 10, upper = 50),
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  makeNumericParam("eta", lower = .1, upper = .5),
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)

control <- makeTuneControlRandom(maxit = 10)

tr_xgb_rand <- tuneParams(learner = xgboost,
                          task = task,
                          resampling = resample_desc,
                          measures = my_measures,
                          par.set = params.xgboost,
                          control = control)

xgb_tuned_learner <- setHyperPars(
  learner = xgboost,
  par.vals = tr_xgb_rand$x
)

xgb_model <- mlr::train(xgb_tuned_learner, task)
predict_xgb <- predict(xgb_model, task)

tuned_xgb1 <- colAUC(getPredictionProbabilities(predict_xgb), predict_xgb$data$truth)
tuned_xgb2 <- performance(predict_xgb, logloss)
tuned_xgb3 <- performance(predict_xgb, acc)
tuned_xgb4 <- f1_score(predict_xgb$data$response, predict_xgb$data$truth, positive.class="1")

## decision tree
decision_tree <- makeLearner("classif.rpart", predict.type = "prob")

model_dt <- mlr::train(decision_tree, task)
predict_dt <- predict(model_dt, task)

dt1 <- colAUC(getPredictionProbabilities(predict_dt), predict_dt$data$truth)
dt2 <- performance(predict_dt, logloss)
dt3 <- performance(predict_dt, acc)
dt4 <- f1_score(predict_dt$data$response, predict_dt$data$truth, positive.class="1")

## tuning parameters (dt)
dtParams <- getParamSet("classif.rpart")

params.dt <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

control <- makeTuneControlRandom(maxit = 100)

tr_dt_rand <- tuneParams(learner = decision_tree,
                         task = task,
                         resampling = resample_desc,
                         measures = my_measures,
                         par.set = params.dt,
                         control = control)

dt_tuned_learner <- setHyperPars(
  learner = decision_tree,
  par.vals = tr_dt_rand$x
)

dt_model <- mlr::train(dt_tuned_learner, task)
predict_dt <- predict(dt_model, task)

tuned_dt1 <- colAUC(getPredictionProbabilities(predict_dt), predict_dt$data$truth)
tuned_dt2 <- performance(predict_dt, logloss)
tuned_dt3 <- performance(predict_dt, acc)
tuned_dt4 <- f1_score(predict_dt$data$response, predict_dt$data$truth, positive.class="1")

## ada boost
ada_boost <- makeLearner("classif.boosting", predict.type = "prob")

model_ab <- mlr::train(ada_boost, task)
predict_ab <- predict(model_ab, task)

ab1 <- colAUC(getPredictionProbabilities(predict_ab), predict_ab$data$truth)
ab2 <- performance(predict_ab, logloss)
ab3 <- performance(predict_ab, acc)
ab4 <- f1_score(predict_ab$data$response, predict_ab$data$truth, positive.class="1")
ab5 <- 1.0000000

## decision stumps
params.ds <- makeParamSet(
  makeNumericParam("eta", lower = 0, upper = 1),
  makeNumericParam("gamma", lower = 0, upper = 1),
  makeIntegerParam("max_depth", lower = 1, upper = 1),
  makeNumericParam("min_child_weight", lower = 0, upper = 100),
  makeNumericParam("subsample", lower = 0.1, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.1, upper = 1),
  makeIntegerParam("nrounds", lower = 10, upper = 1000)
)

control <- makeTuneControlRandom(maxit = 25)

tr_ds_rand <- 
  tuneParams(learner = xgboost,
             task = task, 
             resampling = resample_desc,
             measures = my_measures, 
             par.set = params.ds,
             control = control)

ds_tuned_learner <- setHyperPars(xgboost, par.vals = tr_ds_rand$x) %>%
  setLearnerId("xgb tuned random")

model_ds <- mlr::train(ds_tuned_learner, task)
predict_ds <- predict(model_ds, task)

ds1 <- colAUC(getPredictionProbabilities(predict_ds), predict_ds$data$truth)
ds2 <- performance(predict_ds, logloss)
ds3 <- performance(predict_ds, acc)
ds4 <- f1_score(predict_ds$data$response, predict_ds$data$truth, positive.class="1")

DT <- data.table(
  Algorithm = c("XGBoost","Decision Tree", "AdaBoost", "Decision Stump"),
  AreaUnderCurve = c(mean(tuned_xgb1), mean(tuned_dt1), ifelse(ab3 == 1 && ab4 == 1, ab5, mean(ab1)), mean(ds1)),
  LogLoss = c(tuned_xgb2, tuned_dt2, ab2, ds2),
  Accuracy = c(tuned_xgb3, tuned_dt3, ab3, ds3),
  f1Score = c(tuned_xgb4, tuned_dt4, ab4, ds4)
)
