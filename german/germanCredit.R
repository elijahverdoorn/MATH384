library(dplyr)
library(glmnet)
library(ggplot2)

###
# Cost Matrix:
# False Positive - 5
# False Negative - 1
# True Negative - 0
# True Positive - 0
###

germanCreditNumeric.df <- read.csv("germanNumeric.csv", header = T) # read the numeric data
germanCredit.df <- read.csv("german.csv", header = F, sep = "") # read the original data
names(germanCredit.df) <- c("chk_ac_status_1",
                            "duration_month_2", "credit_history_3", "purpose_4",
                            "credit_amount_5","savings_ac_bond_6","p_employment_since_7",
                            "instalment_pct_8", "personal_status_9","other_debtors_or_grantors_10",
                            "present_residence_since_11","property_type_12","age_in_yrs_13",
                            "other_instalment_type_14", "housing_type_15",
                            "number_cards_this_bank_16","job_17","no_people_liable_for_mntnance_18",
                            "telephone_19", "foreign_worker_20",
                            "good_bad_21")

germanCreditNumeric.df <- germanCreditNumeric.df %>% # add the response variable to the numeric data
    mutate(response = germanCredit.df[,21])

# Lasso
# TODO: use lasso to select variables, then feed those variables to LDA and logistic regression

# Linear Discrim. Analysis

# Ridge Regression

# Logistic Regression
