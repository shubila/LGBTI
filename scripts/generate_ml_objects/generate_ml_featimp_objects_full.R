
# load libraries

library("iml")
library("future")
library("future.callr")
library("ggplot2")

#clear the global environment
rm(list = ls(all.names = TRUE))
 # load the models for each rf and logit 
load("./output/ml_object/caret/full/rf_full_model.RData")
load("./output/ml_object/caret/full/logit_full_model.RData")

# load test/train data which were the basis for the models
load("./output/ml_object/caret/full/test_train_full_data.RData")

#  create matrices for generating the featimp  scores

X <- lgbti_partial_train[!names(lgbti_partial_train) %in% c("open_at_work_bin")]

y <- lgbti_partial_train$open_at_work_bin

predictor_logit <- Predictor$new(logit_train_output_partial, data = X, y = y)

predictor_rf    <- Predictor$new(rf_train_output_partial, data = X, y = y)

# run the scores generation with parallelisation
plan("callr", workers = 2)

system.time(
logit_full_featimp <- FeatureImp$new(predictor_logit, loss = "ce")
)

system.time(
rf_full_featimp    <- FeatureImp$new(predictor_rf, loss = "ce")
)


# save the feat imp objects of both models 

save(logit_full_featimp,file = "./output/ml_object/caret/full/logit_full_featimp.RData")
save(rf_full_featimp,file = "./output/ml_object/caret/full/rf_full_featimp.RData")




