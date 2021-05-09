
library(tidyverse)
library(caret)

#clear the global environment
rm(list = ls(all.names = TRUE))

# load partial featimp objects


load("./output/ml_object/caret/partial/rf_partial_model.RData")
source("./scripts/data_manipulation/import_rename_predictors.R")


# extract the results table from the featimp object


rf_featimp_partial          <- varImp(rf_train_output_partial_v2)$importance 
rf_featimp_partial          <- rownames_to_column(rf_featimp_partial, var = "feature")%>%
                                                arrange(., desc(Overall))

# join the new names of predictors

rf_featimp_partial    <- inner_join(rf_featimp_partial,rename_pred,    by = c("feature" = "old"))

# how many variables to include in the plot 

number_vars <- 30



# generate rf plot 

plot_rf_featimp <- rf_featimp_partial %>%
  slice_max(Overall, n = number_vars) %>%
  ggplot(., aes(Overall, reorder(new, Overall))) +
  geom_point(size = 2) +
  #geom_linerange(aes(xmin = importance.05, xmax = importance.95)) +
  ggtitle("randomforest") +
  ylab("feature") +
  xlab("importance") +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 10)
  )



 plot_rf_featimp





