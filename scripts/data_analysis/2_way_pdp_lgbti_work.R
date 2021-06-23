
#clear the global environment
rm(list = ls(all.names = TRUE))
library(iml)
library(tidyverse)

load("./output/ml_object/caret/partial/test_train_partial_data.RData")
load("./output/ml_object/caret/partial/rf_partial_model.RData")


pdp2_df <- function(tab,feature1,lab1,feature2,lab2) {
  
  
  pdp2    <- tab %>%
    rename({{lab1}} := {{feature1}},
           {{lab2}} := {{feature2}},
    )
  return(pdp2)
}



predictor_rf_caret    = Predictor$new(rf_train_output_partial_v2, data = lgbti_partial_test_v2, class = "Open",type = "prob")


plan("callr", workers = 2)

pdp2_rf1      = FeatureEffect$new(predictor_rf_caret, feature = c("C6_1_D","RESPONDENT_CATEGORY"), method = "pdp")

tab1      <- pdp2_rf1$results %>%
                  rename(variable = C6_1_D,
                         resp_cat = RESPONDENT_CATEGORY) %>% 
                  mutate(predictor = "work_gen_neg")

pdp2_rf2      = FeatureEffect$new(predictor_rf_caret, feature = c("C7_B","RESPONDENT_CATEGORY"), method = "pdp")

tab2     <- pdp2_rf2$results %>%
                       rename(variable = C7_B,
                              resp_cat = RESPONDENT_CATEGORY) %>%
                      mutate(predictor = "work_supp")

tab_comb <- rbind.data.frame(tab1,tab2)

tab_comb %>%
ggplot(., aes(variable,resp_cat, fill =.value)) +
        geom_tile() +
  geom_rug(alpha = 1/2, position = "jitter") +
  facet_wrap(~ predictor)


  





