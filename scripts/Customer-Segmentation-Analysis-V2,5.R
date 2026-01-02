                        ## Customer Segmentation Analysis V2.5##
                    ## Inferential Statistic: Regression Tests (H4) ##
## Our 4th hypothesis will be test that is there significant result in age groups for spending score and annual income.


df_normal  <- read.csv("C:\\Users\\Yusuf\\Desktop\\MallCustomerSegmentation\\data\\Mall_Customers_EDA_DONE.csv", header = TRUE, , sep=";")

library(dplyr)
library(ggplot2)
library(car)
library(rstatix)

## Changing to factor from value.
df_normal$Age_Group <- as.factor(df_normal$Age_Group)

## We will be MANOVA test for the statistical test.
## We will be setup to MANOVA model

manova_age_model <- manova(cbind(Spending_Score, Annual_Income) ~ Age_Group, data = df_normal)
summary(manova_age_model, test = "Wilks")
    ## We have powerfull significant differences between age groups. So age grroups have different
    ## spending score and annual income.

## Source of significant differents.
summary.aov(manova_age_model)
    ## Both spending score and annual income have significant differences for age groups.
    ## So we can reject h0.

## Looking means for age groups:

df_normal %>%
  group_by(Age_Group) %>%
  summarise(
    Count = n(),
    Mean_Spending = mean(Spending_Score),
    Mean_Income = mean(Annual_Income)
  )
    ## Before the post hoc tests we see that young people spending more than according to other groups.
    ## Middle ages have annual income according to other groups.
    ## To be sure we will be post hoc test.

## Post hoc Tukey test

    ## Before to the post hoc, we should make two anova types (So one way ANOVA).
anova_spending <- aov(Spending_Score ~ Age_Group, data = df_normal)
anova_income <- aov(Annual_Income ~ Age_Group, data = df_normal)

    ## Start to Tukey post hoc tests.
    ## Spending Score post hoc test
tukey_spending <- TukeyHSD(anova_spending)
print(tukey_spending)
    ## According to these results we say that group 3 (olds) spending score have significant differents
    ## from other groups. BUt we can't say for young and middle age groups.

    ## Annual Income post hoc test
tukey_income <- TukeyHSD(anova_income)
print(tukey_income)
    ## Accordin to these results we say 2 and 1 have significant differences for annual income
    ## but 3 and 1 don't have significant differences. Also, 3 and 2 have significant differences.
    ## So group 2 has different annual income to other groups.

### According to the results we can say that:
### Group 1 (youngs) have higher than spending score to the other groups.
### Group 2 (middle) have higher than annual score to the other groups.
### To the results our customers young people but the richest customers middle ages.