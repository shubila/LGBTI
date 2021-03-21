
library(tidyverse)
library(openxlsx)

#clear the global environment
rm(list = ls(all.names = TRUE))
# load data
lgbti_ML <- readRDS("./data/clean/lgbti_ML.rds")
lgbti_ML <- lgbti_ML %>%
  select(-one_of("open_at_work_multi"))
# Show dimensions
dim(lgbti_ML)

# generate a vector with names of factors in our dataset

fct_vars <- lgbti_ML %>%
  select_if(., is.factor)%>%
  names(.)

# generate a dataframe with all pairwise combinations of the factors

pair_vars <- as.data.frame(combn(fct_vars,2,simplify = T))

# perform a chisquare test for dependence  with each pairwise combination of factors

chi_output <- map(pair_vars, ~ table(lgbti_ML[.x])) %>%
  map_df(., ~ chisq.test(.x)[c("statistic","p.value","parameter")])

# put everything  with variables, chisquare value, p value and degrees of freedom into df

chisq <- cbind(
  as.data.frame(t(pair_vars)),
  chi_output
)
# set the names of the output dataframe
names(chisq) <- c("VarOne","VarTwo","chisquare","p_value","degrees_freedom")

# order the data frame by the descending chisq statistic

chisq <- chisq[order(-chisq$chisq),]

# reduce the decimal digits

chisq <- chisq %>% 
  mutate(across(where(is.numeric), ~ round(.,digits = 4)))


write.xlsx(chisq,"./paper/collinearity_factors.xlsx")



