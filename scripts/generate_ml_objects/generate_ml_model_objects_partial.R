
# load libraries
library(tidyverse)
library(caret)
library(doParallel)

#clear the global environment
rm(list = ls(all.names = TRUE))
# load data
lgbti_ML_partial <- readRDS("./data/clean/lgbti_ML_partial_v2.rds")
# Show dimensions
dim(lgbti_ML_partial)



lgbti_ML_partial <- lgbti_ML_partial %>%
  select(-one_of("open_at_work_multi"))
# Show dimensions
dim(lgbti_ML_partial)

# print variables to console
names(lgbti_ML_partial)


# ------------------------- split into train/test ------------------------------------------------------

set.seed(49043)

inTrain <- createDataPartition(lgbti_ML_partial$open_at_work_bin, 
                               p = .7, 
                               list = FALSE, 
                               times = 1)
inTrain <- as.integer(inTrain)

lgbti_partial_train <- lgbti_ML_partial[inTrain,]
lgbti_partial_test  <- lgbti_ML_partial[-inTrain,]

lgbti_partial_train_v2 <- lgbti_partial_train
lgbti_partial_test_v2  <- lgbti_partial_test

# ------------------------- generate ml train object ------------------------------------------------------


 cl <- makePSOCKcluster(5)
 registerDoParallel(cl)

set.seed(49043)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     verboseIter = TRUE)

# logistic regression 

# system.time(
#   logit_train_output_partial <- train(
#     x = lgbti_partial_train[!names(lgbti_partial_train) %in% c("open_at_work_bin")],
#     y = lgbti_partial_train$open_at_work_bin,
#     method="glm",family=binomial(),
#     trControl = ctrl,
#     metric = "ROC"
#   )
# )



# random forest

# set grid for hyper parameter mtry and min.node.size 


rf_grid <- expand.grid(mtry = 15,
                       splitrule = "hellinger",
                       min.node.size = 10
)

# rf_grid <- expand.grid(mtry = seq.int(5, 45, by=5),
#                        splitrule = c("hellinger","gini"),
#                        min.node.size = c(5,10)
# )


system.time(
  rf_train_output_partial_v2 <-
    train(
      x = lgbti_partial_train[!names(lgbti_partial_train) %in% c("open_at_work_bin")],
      y = lgbti_partial_train$open_at_work_bin,
      method = "ranger",
      trControl = ctrl,
      tuneGrid = rf_grid,
      importance = "permutation",
      metric = "ROC"
    )
)


stopCluster(cl)

# ------------------------- print results to console ----------------

# logit_train_output_partial
# rf_train_output_partial

plot(rf_train_output_partial_v2)


# ------------------------- save to folder----------------

# save the models
#save(logit_train_output_partial, file = "./output/ml_object/caret/partial/logit_partial_model.RData")
save(rf_train_output_partial_v2, file = "./output/ml_object/caret/partial/rf_partial_model_v2.RData")

# save the test/train data

save(lgbti_partial_test_v2,lgbti_partial_train_v2, file = "./output/ml_object/caret/partial/test_train_partial_data_v2.RData")





 



