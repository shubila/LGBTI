rm(list = ls(all.names = TRUE))
library(iml)
library(shapper)
library(tidyverse)
library(readxl)
load("./output/ml_object/caret/partial/test_train_partial_data.RData")
load("./output/ml_object/caret/partial/rf_partial_model.RData")
rename_pred <- read_excel("./documentation/Rename_predictors.xlsx",sheet = "Rename",col_names = TRUE)

testset <- lgbti_partial_test_v2

predictions_open <- predict(rf_train_output_partial_v2, newdata = lgbti_partial_test_v2, type = "prob")

testset$predictions_open <- predictions_open$Open

df_mean <- testset %>% 
  summarise(mean_pred = mean(predictions_open))

ggplot(
  testset,
  aes(predictions_open, colour = RESPONDENT_CATEGORY, fill = RESPONDENT_CATEGORY)
) +
  geom_density() +
  facet_wrap(~ RESPONDENT_CATEGORY, ncol = 1) + 
  geom_vline(data = df_mean, mapping = aes(xintercept = mean_pred)) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size=10)) +
  xlab("probability predictions of random forest model") 

ggplot(testset, aes(x=RESPONDENT_CATEGORY, y=predictions_open)) + 
  geom_violin()+
  geom_hline(data = df_mean, mapping = aes(yintercept = mean_pred)) +
  ylab("probability predictions of random forest") +
  xlab("") 

testset <- testset %>% 
  group_by(RESPONDENT_CATEGORY)%>% 
  mutate(quartile_within = ntile(predictions_open, 4))%>% 
  ungroup()

set.seed(300000)
ind_trans    <- testset %>% filter(., RESPONDENT_CATEGORY == "trans" & predictions_open < 0.75) %>% select(A1:A14_fct)%>% sample_n(1) 
ind_intersex <- testset %>% filter(., RESPONDENT_CATEGORY == "intersex" & predictions_open < 0.75) %>% select(A1:A14_fct)%>% sample_n(1) 
bisexf       <- testset %>% filter(., RESPONDENT_CATEGORY == "bisexual (female)" & predictions_open < 0.75) %>% select(A1:A14_fct)%>% sample_n(1) 
bisexm       <- testset %>% filter(., RESPONDENT_CATEGORY == "bisexual (male)" & predictions_open < 0.75) %>% select(A1:A14_fct)%>% sample_n(1) 

p_function <- function(model, data) predict(model, newdata = data, type = "prob")

predictor_rf    <-
  Predictor$new(
    rf_train_output_partial_v2,
    data = lgbti_partial_test_v2[!names(lgbti_partial_test_v2) %in% c("open_at_work_bin")],
    y = lgbti_partial_test_v2$open_at_work_bin,
    class = "Open",
    type = "prob",
    predict.fun = p_function,
  )

shapley_trans    = Shapley$new(predictor_rf, x.interest = ind_trans)
shapley_intersex = Shapley$new(predictor_rf, x.interest = ind_intersex)
shapley_bisexf   = Shapley$new(predictor_rf, x.interest = bisexf)
shapley_bisexm   = Shapley$new(predictor_rf, x.interest = bisexm )


trans_tab <- shapley_trans$results %>% 
                      inner_join(.,rename_pred,    by = c("feature" = "old")) %>%
                      mutate(feature_value2 = str_replace(feature.value,"[^=]+",new))%>%
                      mutate(abs_phi = abs(phi))%>%
                      slice_max(n = 15, abs_phi)
                      

intersex_tab <- shapley_intersex$results %>% 
  inner_join(.,rename_pred,    by = c("feature" = "old")) %>%
  mutate(feature_value2 = str_replace(feature.value,"[^=]+",new))%>%
  mutate(abs_phi = abs(phi))%>%
  slice_max(n = 15, abs_phi)

bisexf_tab <- shapley_bisexf$results %>% 
  inner_join(.,rename_pred,    by = c("feature" = "old")) %>%
  mutate(feature_value2 = str_replace(feature.value,"[^=]+",new))%>%
  mutate(abs_phi = abs(phi))%>%
  slice_max(n = 15, abs_phi)

bisexm_tab <- shapley_bisexm$results %>% 
  inner_join(.,rename_pred,    by = c("feature" = "old")) %>%
  mutate(feature_value2 = str_replace(feature.value,"[^=]+",new))%>%
  mutate(abs_phi = abs(phi))%>%
  slice_max(n = 15, abs_phi)

trans_plot <- trans_tab %>%
  ggplot(.) +
  geom_col(aes(y = phi, x = reorder(feature_value2,phi,sum))) +
  ggtitle("Trans",
          paste("Actual prediction :",round(shapley_trans$y.hat.interest,digits = 2))) +
  coord_flip() +
  xlab("") +
  theme(plot.title = element_text(size = 8),
        plot.subtitle=element_text(size= 8),
        text = element_text(size = 8,face = "bold"))
  
intersex_plot <- intersex_tab %>%
  ggplot(.) +
  geom_col(aes(y = phi, x = reorder(feature_value2,phi,sum))) +
  ggtitle("Intersex",
          paste("Actual prediction :",round(shapley_intersex$y.hat.interest,digits = 2))) +
  coord_flip() +
  xlab("")+
  theme(plot.title = element_text(size = 8),
        plot.subtitle=element_text(size= 8),
        text = element_text(size = 8,face = "bold"))


bisexf_plot <- bisexf_tab %>%
  ggplot(.) +
  geom_col(aes(y = phi, x = reorder(feature_value2,phi,sum))) +
  ggtitle("Bisex, female",
          paste("Actual prediction :",round(shapley_bisexf$y.hat.interest,digits = 2))) +
  coord_flip() +
  xlab("")+
  theme(plot.title = element_text(size = 8),
        plot.subtitle=element_text(size= 8),
        text = element_text(size = 8,face = "bold"))


bisexm_plot <- bisexm_tab %>%
  ggplot(.) +
  geom_col(aes(y = phi, x = reorder(feature_value2,phi,sum))) +
  ggtitle("Bisex, male",
          paste("Actual prediction :",round(shapley_bisexm$y.hat.interest,digits = 2))) +
  coord_flip() +
  xlab("")+
  theme(plot.title = element_text(size = 8),
        plot.subtitle=element_text(size= 8),
        text = element_text(size = 8,face = "bold"))

plot_comb_1 <-
  gridExtra::grid.arrange(
    trans_plot ,
    intersex_plot,
    ncol = 2,
    top = "Shapley values for average prediction: 0.75"
  )


plot_comb_2 <-
  gridExtra::grid.arrange(
    bisexf_plot,
    bisexm_plot,
    ncol = 2,
    top = "Shapley values for average prediction: 0.75"
  )


plot_comb_3 <-
  gridExtra::grid.arrange(
    trans_plot ,
    bisexm_plot,
    ncol = 2,
    top = "Shapley values for average prediction: 0.75"
  )







# plot_comb_ <-
#   gridExtra::grid.arrange(
#     trans_plot ,
#     intersex_plot,
#     bisexf_plot,
#     bisexm_plot,
#     nrow = 2,
#     ncol = 2,
#     top = "Shapley values for average prediction: 0.75"
#   )








