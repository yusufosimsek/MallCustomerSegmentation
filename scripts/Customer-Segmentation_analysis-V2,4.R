                          ## Customer Segmentation Analysis V2.4##
                      ## Inferential Statistic: Regression Tests (H3) ##
## Our third hypoteses is significant different test. According to the this test, we will be test that
## are we have a difference between gender for spending score and annual income.


df_normal  <- read.csv("C:\\Users\\Yusuf\\Desktop\\MallCustomerSegmentation\\data\\Mall_Customers_EDA_DONE.csv", header = TRUE, , sep=";")

library(dplyr)
library(ggplot2)
library(car)
library(rstatix)


## Changing categories to factors

df_normal$Gender <- as.factor(df_normal$Gender)


## Manova model
manova_model <- manova(cbind(Spending_Score, Annual_Income) ~ Gender, data = df_normal)
summary(manova_model, test = "Wilks")
    ## According to MANOVA test result we can't reject h0.
    ## So we couldn't found significant difference for spending score and annual income according to genders.

## Two sapmles t-test for genders and spending score
    ## We didn't find any significant with spending score and annual income but maybe one of them have 
    ## significant value on the gender. Therefore we will be look at one by one.

t_test_spending <- t.test(Spending_Score ~ Gender, data = df_normal, var.equal = TRUE)
print(t_test_spending)
    ## We couldn't find any significant with gender and spending score.

## Two sapmles t-test for genders and annual income
t_test_income <- t.test(Annual_Income ~ Gender, data = df_normal, var.equal = TRUE)
print(t_test_income)
    ## We couldn't find any significant with gender and annual income.

## We couldn't say between gender, annual income, and spending score have significant result.
## At that result our 5th hypothesis h0 couldn't reject.