                              ## Customer Segmentation Analysis V2.3##
                          ## Inferential Statistic: Regression Tests (H2) ##
## Our second hypothesis: As age increases, spending score and annual income increase
    ## We saw that age and spending score have a negative correlation and annual income has no correlation
    ## both of them. Therefore we can't make a regression test between these. Therefore we will be make
    ## regression test with age and spending score but we should to not forget that we have not powerfull
    ## correlation between the two values. So the regression model won't be give us reliable a result.

df_normal  <- read.csv("C:\\Users\\Yusuf\\Desktop\\MallCustomerSegmentation\\data\\Mall_Customers_EDA_DONE.csv", header = TRUE, , sep=";")

library(dplyr)
library(ggplot2)
library(car)      # For VIF
library(rstatix)

## Regulation to categorical datas

model_reg <- lm(Spending_Score ~ Age, data = df_normal)

## Looking summary
summary(model_reg)
    ## When we look at the result, we can say that definetly we have evidences for as age increase, spending
    ## score loss. We can say that in %95 level of test.
    ## Every increasing of age spending score lost 0,6051 point.
    ## This model explain just %10,85. So age, explains spending score but that explain is so limited.
    ## Likewise our standar error (e) 23,37 points. So this error value is so big.
          ## Regression Equation:
            ## y=a+bx+e
            ## y=(73,7529)+(-0,6051)x+24,37

## The regression model graph
ggplot(df_normal, aes(x = Age, y = Spending_Score)) +
  geom_point(color = "darkblue", alpha = 0.6) + # Real Datas
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1.2) + # Regression Line
  labs(title = "Impact of age on to the spending score",
       subtitle = paste("Correlation Coefficient:", round(cor(df_normal$Age, df_normal$Spending_Score), 2)),
       x = "Age",
       y = "Spending Score") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
    ## When we look at the real datas, we can understand why the model not enough for explain spending score.
    ## So we need to more than variable for explain spending score but we don't have another variables.

## We will be look at the SEM casulaty test.
    ## We saw age can explain spending score but not enough. Now we will be look at is that casualty valid?

library(lavaan) # SEM

sem_model_strutucture <- 'Spending_Score ~ Age'
fit_sem <- sem(sem_model_strutucture, data = df_normal)
summary(fit_sem, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
    ## We saw that age a casual of spending score but not enough for explain to spending score.