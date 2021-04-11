
# load libraries

library("iml")
library("future")
library("future.callr")
library("ggplot2")
library("Metrics")

#clear the global environment
rm(list = ls(all.names = TRUE))
 # load the models for each rf and logit 
load("./output/ml_object/caret/full/rf_full_model.RData")
load("./output/ml_object/caret/full/logit_full_model.RData")

# load test/train data which were the basis for the models
load("./output/ml_object/caret/full/test_train_full_data.RData")

#  create matrices for generating the featimp  scores

X <- lgbti_full_train[!names(lgbti_full_train) %in% c("open_at_work_bin")]

y <- lgbti_full_train$open_at_work_bin

predictor_logit <- Predictor$new(logit_train_output_full, data = X, y = y)

predictor_rf    <- Predictor$new(rf_train_output_full, data = X, y = y)

#auc_error = function(actual, predicted) 1 - Metrics::auc(actual, predicted)

# run the scores generation with parallelisation
plan("callr", workers = 2)

system.time(
logit_full_featimp_ce <- FeatureImp$new(predictor_logit, loss = "ce")
)

system.time(
rf_full_featimp_ce    <- FeatureImp$new(predictor_rf, loss = "ce")
)

#plot(rf_full_featimp_auc)
# save the feat imp objects of both models 

save(logit_full_featimp_ce,file = "./output/ml_object/caret/full/logit_full_featimp_ce.RData")
save(rf_full_featimp_ce,file = "./output/ml_object/caret/full/rf_full_featimp_ce.RData")




