
# load libraries

library("iml")
library("future")
library("future.callr")
library("ggplot2")
library("Metrics")
library(tidyverse)

#clear the global environment
rm(list = ls(all.names = TRUE))

 # load the models for each rf and logit 

load("./output/ml_object/caret/partial/logit_partial_model.RData")
load("./output/ml_object/caret/partial/rf_partial_model.RData")
#plot(rf_train_output_partial)



# load test/train data which were the basis for the models
load("./output/ml_object/caret/partial/test_train_partial_data.RData")

#  create matrices for generating the featimp  scores

X <- lgbti_partial_train[!names(lgbti_partial_train) %in% c("open_at_work_bin")]

y <- lgbti_partial_train$open_at_work_bin

predictor_logit <- Predictor$new(logit_train_output_partial, data = X, y = y)

predictor_rf    <- Predictor$new(rf_train_output_partial, data = X, y = y)



# run the scores generation with parallelisation
plan("callr", workers = 2)

system.time(
logit_partial_featimp_ce <- FeatureImp$new(predictor_logit, loss = "ce")
 )

system.time(
rf_partial_featimp_ce    <- FeatureImp$new(predictor_rf, loss = "ce")
)

#plot(rf_partial_featimp_auc)

# save the feat imp objects of both models 

save(logit_partial_featimp_ce,file = "./output/ml_object/caret/partial/logit_partial_featimp_ce.RData")
save(rf_partial_featimp_ce,file = "./output/ml_object/caret/partial/rf_partial_featimp_ce.RData")




