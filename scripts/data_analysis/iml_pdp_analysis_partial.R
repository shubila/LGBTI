
library("iml")
library("future")
library("future.callr")
library("ggplot2")

 # load the models for each rf and logit 
load("./output/ml_object/caret/partial/rf_partial_model.RData")
load("./output/ml_object/caret/partial/logit_partial_model.RData")

# load test/train data which were the basis for the models
load("./output/ml_object/caret/partial/test_train_partial.RData")

#  create matrices for generating the featimp  scores

X <- lgbti_partial_test[!names(lgbti_partial_test) %in% c("open_at_work_bin")]

y <- lgbti_partial_test$open_at_work_bin

predictor_logit <- Predictor$new(logit_train_output_partial, data = X, y = y)

predictor_rf    <- Predictor$new(rf_train_output_partial, data = X, y = y)


# run the scores generation with parallelisation
plan("callr", workers = 2)

system.time(
pdp_logit_a13 <- Partial$new(predictor_logit, feature = "A13",ice = TRUE, grid.size = 50)
)

system.time(
  pdp_rf_a13 <- Partial$new(predictor_rf, feature = "A13",ice = TRUE, grid.size = 50)
)

# generate the plots for each model

plot_pdp_logit_a13 <- plot(pdp_logit_a13) + ggtitle("logit")

plot_pdp_rf_a13    <- plot(pdp_rf_a13)    + ggtitle("rf")

plot_comb_partial <-
  gridExtra::grid.arrange(plot_pdp_logit_a13, plot_pdp_rf_a13, nrow = 1)
 
# save the feat imp objects of both models 

# save(featimp_partial_logit,file = "./output/ml_object/caret/partial/featimp_partial_logit.RData")
# save(featimp_partial_rf,file = "./output/ml_object/caret/partial/featimp_partial_rf.RData")
# 
# # pdf
# ggsave(filename = "./output/fig/caret/plot_featimp_partial_logit.pdf", plot = plot_featimp_partial_logit)
# ggsave(filename = "./output/fig/caret/plot_featimp_partial_rf.pdf",    plot = plot_featimp_partial_rf)
# ggsave(filename = "./output/fig/caret/plot_comb_partial.pdf",          plot = plot_comb_partial)
# 
# # jpeg
# ggsave(filename = "./output/fig/caret/plot_featimp_partial_logit.jpg", plot = plot_featimp_partial_logit)
# ggsave(filename = "./output/fig/caret/plot_featimp_partial_rf.jpg",    plot = plot_featimp_partial_rf)
# ggsave(filename = "./output/fig/caret/plot_comb_partial.jpg",          plot = plot_comb_partial)

  ggsave(filename = "./output/fig/caret/plot_pdp_a13_partial_logit.pdf", plot = plot_pdp_logit_a13)
  ggsave(filename = "./output/fig/caret/plot_pdp_a13_partial_rf.pdf"   , plot = plot_pdp_rf_a13)
