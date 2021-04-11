
library(tidyverse)

#clear the global environment
rm(list = ls(all.names = TRUE))

# load partial featimp objects

load("./output/ml_object/caret/partial/logit_partial_featimp_ce.RData")
load("./output/ml_object/caret/partial/rf_partial_featimp_ce.RData")
source("./scripts/data_manipulation/import_rename_predictors.R")


# extract the results table from the featimp object

logit_featimp_partial       <- logit_partial_featimp_ce$results

rf_featimp_partial          <- rf_partial_featimp_ce$results 

# join the new names of predictors

logit_featimp_partial <- inner_join(logit_featimp_partial,rename_pred, by = c("feature" = "old"))
rf_featimp_partial    <- inner_join(rf_featimp_partial,rename_pred,    by = c("feature" = "old"))

# generate logit plot 

plot_logit_featimp <- logit_featimp_partial2 %>%
  slice_max(importance, n = 25) %>%
  ggplot(., aes(importance, reorder(new, importance))) +
  geom_point(size = 2) +
  geom_linerange(aes(xmin = importance.05, xmax = importance.95)) +
  ggtitle("logit") +
  ylab("feature") +
  theme(
        axis.text.y = element_text(size = 8, face = "bold"),
        plot.title = element_text(size = 10)
        )



# generate rf plot 

plot_rf_featimp <- rf_featimp_partial %>%
  slice_max(importance, n = 25) %>%
  ggplot(., aes(importance, reorder(new, importance))) +
  geom_point(size = 2) +
  geom_linerange(aes(xmin = importance.05, xmax = importance.95)) +
  ggtitle("randomforest") +
  ylab("feature") +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 10)
  )



(plot_featimp_ce <- gridExtra::grid.arrange(plot_logit_featimp, plot_rf_featimp, nrow = 1))





