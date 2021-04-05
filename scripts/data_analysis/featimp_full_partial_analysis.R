
library(tidyverse)

#clear the global environment
rm(list = ls(all.names = TRUE))

# load full and partial featimp objects

load("./output/ml_object/caret/full/logit_full_featimp.RData")
load("./output/ml_object/caret/full/rf_full_featimp.RData")

load("./output/ml_object/caret/partial/logit_partial_featimp.RData")
load("./output/ml_object/caret/partial/rf_partial_featimp.RData")


# create the individual plots


plot_featimp_full_logit  <- plot(featimp_full_logit) + ggtitle("logit_full")

plot_featimp_full_rf       <- plot(featimp_full_rf)  + ggtitle("rf_full") 


plot_featimp_partial_logit  <- plot(featimp_partial_logit) + ggtitle("logit_partial")

plot_featimp_partial_rf       <- plot(featimp_partial_rf)  + ggtitle("rf_partial") 

# create the combined plots

plot_comb_logit <- gridExtra::grid.arrange(plot_featimp_partial_logit, plot_featimp_full_logit, nrow = 1)
plot_comb_rf <- gridExtra::grid.arrange(plot_featimp_partial_rf, plot_featimp_full_rf, nrow = 1)


# check the tables 

tab_featimp_full_logit    <- featimp_full_logit$results %>% 
  rename_with(~paste0(.,"_logit_full"))
tab_featimp_partial_logit <- featimp_partial_logit$results %>% 
  rename_with(~paste0(.,"_logit_partial"))

tab_featimp_full_rf    <- featimp_full_rf$results %>% 
  rename_with(~paste0(.,"_rf_full"))

tab_featimp_partial_rf <- featimp_partial_rf$results %>% 
  rename_with(~paste0(.,"_rf_partial"))

# set their ranking and join

tab_featimp_full_logit$ranking_logit_full        <- 1:nrow(tab_featimp_full_logit)

tab_featimp_partial_logit$ranking_logit_partial  <- 1:nrow(tab_featimp_partial_logit)

tab_featimp_full_rf$ranking_rf_full               <- 1:nrow(tab_featimp_full_rf)

tab_featimp_partial_rf$ranking_rf_partial         <- 1:nrow(tab_featimp_partial_rf)

# join

comb_logit <-
  inner_join(
    tab_featimp_full_logit,
    tab_featimp_partial_logit,
    by = c("feature_logit_full" = "feature_logit_partial"))

comb_rf <-
  inner_join(
    tab_featimp_full_rf,
    tab_featimp_partial_rf,
    by = c("feature_rf_full" = "feature_rf_partial"))


