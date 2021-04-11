#clear the global environment
rm(list = ls(all.names = TRUE))

# Set-up  -----------------------------------------------------------------

# load packages
library(foreign)
library(haven)
library(tidyverse)
library(naniar)

# Read the raw files, .dta and .sav 

lgbti_dta <- read_dta("./data/raw/ZA7604_v1-0-0.dta")

lgbti_sav <- as.data.frame(read.spss("./data/raw/ZA7604_v1-0-0.sav"))


# tidy up the data so that we can have a clean dataset to directly run the ML.
# To generate a clean dataset it is the easiest to use the SPSS dataset as starting point

# ------------------------- columns------------------------------------------------------

# We start by selecting the variables we will actually use 

cols <-
  c(
    
    # section A
    "A1", "A2", "A7", "A8", "A10","A10_1","A13","A14",
    # section TR
    "TR1",
    # section IX
    "IX3",
    # section B
    "B1","B2","B5",
    # section C
    "C1_A","C1_C","C1_D","C1_E","C1_F","C1_G","C1_H", "C6_1_D","C7_B","C8_A","C8_B","C8_C","C8_D",
    "C9_1","C10_A","C10_B","C10_1","C11_A","C11_B","C11_C","C11_D","C11_E","C11_F","C11_G","C11_H",
    "C11_I","C12",
    #"C6_1_C","C7_A","C1_B"
    
    # section D
    "D1","D2","D4",
    # section E
    "E1",
    # section F
    "F1_A","F1_B","F1_C","F1_D","F1_E","F1_F","F1_G",
    # section G
    "G1_A","G1_B","G1_C","G1_E","G1_H","G2",
    # section H
    "H1","H2","H3","H4", "H5", "H10","H12_A","H12_B","H12_C",
    "H12_D","H12_E","H15_A","H15_B","H15_C","H15_D","H15_E","H15_F","H16",
    "H17","H18","H19", "H20", "H21_1","H21_2","H21_3","H21_4","H21_5",
    # section I
    "I3_A","I3_B","I3_C","I3_D", "I3_E",
    # derived and other variables
    "open_at_work","RESPONDENT_ID","RESPONDENT_CATEGORY"
  )

# keep only the listed variables above

lgbti_ML <- lgbti_sav %>% 
  select(all_of(cols))


#--------------------------- sections ------------------------------------------


#  Section A


# set A13 to numeric, take age from TR1, IX3 for trans and intersex where possible

lgbti_ML$A13 <- as.character(lgbti_ML$A13)
lgbti_ML$IX3  <- as.character(lgbti_ML$IX3)
lgbti_ML$TR1  <- as.character(lgbti_ML$TR1)

lgbti_ML <- lgbti_ML %>%
  mutate(A13 = case_when(A13 == "Not applicable" & RESPONDENT_CATEGORY == "intersex" ~ IX3,
                         TRUE  ~ A13),
         A13 = case_when(A13 == "Not applicable" & RESPONDENT_CATEGORY == "trans" ~ TR1,
                         TRUE  ~ A13)
  )

lgbti_ML$A13 <- as.numeric(lgbti_ML$A13)


# make age groups out of A14, take age from TR1, IX3 for trans and intersex where possible,and keep the category "I have not told anybody"

A14_df <- lgbti_dta %>%
  select(RESPONDENT_ID,RESPONDENT_CATEGORY,A14,IX3_1,TR2) %>%
  mutate(A14 = case_when(A14 == -2 & RESPONDENT_CATEGORY == 7 ~ IX3_1,
                         TRUE  ~ A14),
         A14 = case_when(A14 == -2 & RESPONDENT_CATEGORY == 17 ~ TR2,
                         TRUE  ~ A14)
  )

A14_df <- A14_df %>%
  select(RESPONDENT_ID,A14) %>%
  mutate(A14_fct = as.factor(case_when(A14 == -998           ~ "I have not told anybody",
                                       A14 == -999           ~ NA_character_,
                                       A14 == -2             ~ NA_character_,
                                       A14 <= 14             ~    "-14",
                                       A14 >= 15 & A14 <= 17 ~  "15-17",
                                       A14 >= 18 & A14 <= 24 ~  "18-24",
                                       A14 >= 25 & A14 <= 29 ~  "25-29",
                                       A14 >= 30 & A14 <= 34 ~  "30-34",
                                       A14 >= 35 & A14 <= 39 ~  "35-39",
                                       A14 >= 40 & A14 <= 44 ~  "40-44",
                                       A14 >= 45             ~    "45+",
                                       TRUE  ~ as.character(A14)))) %>%
  select(RESPONDENT_ID,A14_fct)

lgbti_ML <- inner_join(lgbti_ML,A14_df, by = "RESPONDENT_ID")

# A14 >= 45 & A14 <= 49 ~  "45-49",
# A14 >= 50 & A14 <= 54 ~  "50-54",
# A14 >= 55 & A14 <= 59 ~  "55-59",
# A14 >= 60 & A14 <= 64 ~  "60-64",
# A14 >= 65             ~    "65+",

#---------------------------- generate two versions of the dependent variable(collapsing the open categories), one binary and one multi class ------------------



lgbti_ML$open_at_work_bin <- fct_collapse(
  lgbti_ML$open_at_work,
  Hide = "Hide being LGBTI",
  Open = c("Very open", "Selectively open")
)


lgbti_ML$open_at_work_multi <- fct_recode(
  lgbti_ML$open_at_work,
  Hide = "Hide being LGBTI",
  Sel_open = "Selectively open",
  Very_open = "Very open"
)




#----------------------------- rows ---------------------------------------------


#  

lgbti_ML <- lgbti_ML %>%
  mutate(across(where(is.factor), ~ fct_recode(.,
                  "Other"                  = "Other, please specify",
                  "Only women"             = "Only women (or with one woman)",
                  "Only men"               = "Only men (or with one man)",
                  "Not same-sex partner"   = "I do not have a same-sex partner",
                  "10+"                    = "More than 10 times",
                  "At all times"           = "All of the time",
                  "In paid work"           = "In paid work (including on paternity or other temporary leave)",
                  "Christian_Catholic"     = "Christian <U+0096> Catholic",
                  "Christian_Protestant"   = "Christian <U+0096> Protestant",
                  "Christian_Orthodox"     = "Christian <U+0096> Orthodox",
                  "Christian_Other"        = "Christian <U+0096> Other",
          "Married/same-sex partner"       = "Married/ registered partnership with same-sex partner",
          "Married/different-sex partner"  = "Married/ registered partnership with different sex partner",
          "Unpaid/voluntary work"          = "In unpaid or voluntary work",
          "Unable to work-health problems" = "Unable to work due to long-standing health problems",
          "Military/Civilian service"      = "Compulsory military or civilian service",
          "Domestic tasks"                 = "Fulfilling domestic tasks",
          "No formal"                      = "No formal education",
          "Primary"                        = "Primary education",
          "Lower secondary"                = "Lower secondary education",
          "Upper secondary"                = "Upper secondary education",
          "Post-secondary"                 = "Post-secondary education other than college/university",
          "Bachelor"                       = "Bachelor or equivalent",
          "Master"                         = "Master or equivalent",
          "Doctoral"                       = "Doctoral or equivalent",
          "Not told"                       = "I have not told anybody",
          "Yes,positive"                   = "Yes, in a positive way",
          "Yes,positive/negative"          = "Yes, both in a positive and negative way",
          "Yes,neutral/balanced"           = "Yes, in a neutral and balanced way",
          "Yes,negative"                   = "Yes, in a negative way",
          "Co-living with partner/spouse"  = "Living together with a partner /spouse",
          "Relationship without co-living" = "Involved in a relationship without living together",
          "No relationship/partner"        = "Have no relationship / do not have a partner",
          "Other"                          = "Any other religion",
          "More than half"                 = "More than half of the time",
          "Less than half"                 = "Less than half of the time")
        )
    )
  



# Generate two versions of the dataset, one including "Does not apply to me" and one without

# Replace all "Do not know" for the factors of the dataset. 
# For the variable H3 I found the factor levels "-999" and "-888" so converting "-888" to "Prefer not to say"
# Generate the version which include "Does not apply to me"


lgbti_ML_full <- lgbti_ML %>%  
  mutate(across(where(is.factor), ~ fct_recode(.,NULL = "Don’t know",
                                                 NULL = "Do not know",
                                                 NULL = "-999",
                                                 NULL = "Missing",
                                                 NULL = "Prefer not to say",
                                                 NULL = "-888")))

# lgbti_ML_full <- lgbti_ML %>%  
#   mutate(across(where(is.factor), ~ fct_recode(.,NULL = "Don’t know",
#                                                NULL = "Do not know",
#                                                NULL = "-999",
#                                                NULL = "Missing",
#                                                `Prefer not to say`= "-888")))



# Generate the version which excludes "Does not apply to me"

lgbti_ML_partial <- lgbti_ML %>%  
  mutate(across(where(is.factor), ~ fct_recode(.,NULL = "Don’t know",
                                                 NULL = "Do not know",
                                                 NULL = "-999",
                                                 NULL = "Missing",
                                                 NULL = "Prefer not to say",
                                                 NULL = "-888",
                                                 NULL = "Does not apply to me")))



# ------------------------- filter out population of interest, drop NA:s  and unused factor levels ----------------

# filter population of interest
lgbti_ML_full <- lgbti_ML_full %>%
  select(-one_of("RESPONDENT_ID","open_at_work","IX3","TR1","A14")) %>% 
  drop_na() %>%
  droplevels()

lgbti_ML_partial <- lgbti_ML_partial %>%
  select(-one_of("RESPONDENT_ID","open_at_work","IX3","TR1","A14")) %>% 
  drop_na() %>%
  droplevels()


# ------------------------- save to folder ----------------

saveRDS(lgbti_sav,"./data/clean/lgbti_sav.rds")
saveRDS(lgbti_dta,"./data/clean/lgbti_dta.rds")
saveRDS(lgbti_ML_full,"./data/clean/lgbti_ML_full.rds")
saveRDS(lgbti_ML_partial,"./data/clean/lgbti_ML_partial.rds")
