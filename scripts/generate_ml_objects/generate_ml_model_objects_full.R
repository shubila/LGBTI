
# load libraries
library(tidyverse)
library(caret)
library(doParallel)

#clear the global environment
rm(list = ls(all.names = TRUE))
# load data
lgbti_ML_full <- readRDS("./data/clean/lgbti_ML_full.rds")
# Show dimensions
dim(lgbti_ML_full)

lgbti_ML_full <- lgbti_ML_full %>%
  select(-one_of("open_at_work_multi"))
# Show dimensions
dim(lgbti_ML_full)

# print variables to console
names(lgbti_ML_full)


# ------------------------- split into train/test ------------------------------------------------------

set.seed(49043)

inTrain <- createDataPartition(lgbti_ML_full$open_at_work_bin, 
                               p = .7, 
                               list = FALSE, 
                               times = 1)
inTrain <- as.integer(inTrain)

lgbti_full_train <- lgbti_ML_full[inTrain,]
lgbti_full_test  <- lgbti_ML_full[-inTrain,]

# ------------------------- generate ml train object ------------------------------------------------------

# use parallelisation
 cl <- makePSOCKcluster(5)
 registerDoParallel(cl)

set.seed(49043)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     verboseIter = TRUE)

# logistic regression 

system.time(
  logit_train_output_full <- train(
    x = lgbti_full_train[!names(lgbti_full_train) %in% c("open_at_work_bin")],
    y = lgbti_full_train$open_at_work_bin,
    method="glm",family=binomial(),
    trControl = ctrl,
    metric = "ROC"
  )
)



# random forest

# set grid for hyper parameter mtry and min.node.size 


rf_grid <- expand.grid(mtry = seq.int(5, 45, by=5),
                       splitrule = c("hellinger","gini"),
                       min.node.size = c(5,10)
)

system.time(
  rf_train_output_full <-
    train(
      x = lgbti_full_train[!names(lgbti_full_train) %in% c("open_at_work_bin")],
      y = lgbti_full_train$open_at_work_bin,
      method = "ranger",
      trControl = ctrl,
      tuneGrid = rf_grid,
      # importance = "permutation",
      metric = "ROC"
    )
)


stopCluster(cl)

# ------------------------- print results to console ----------------

logit_train_output
rf_train_output


# ------------------------- save to folder----------------
# save the models
save(logit_train_output_full, file = "./output/ml_object/caret/full/logit_full_model.RData")
save(rf_train_output__full,   file = "./output/ml_object/caret/full/rf_full_model.RData")

# save the test/train data

save(lgbti_full_test,lgbti_full_train, file = "./output/ml_object/caret/full/test_train_full.RData")











