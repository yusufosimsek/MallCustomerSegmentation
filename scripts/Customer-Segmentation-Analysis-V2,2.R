                ## Customer Segmentation Analysis V2.2##
                        ## Inferential Statistic ##

df_normal  <- read.csv("C:\\Users\\Yusuf\\Desktop\\MallCustomerSegmentation\\data\\Mall_Customers_EDA_DONE.csv", header = TRUE, , sep=";")


# Install and Load Function

install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Libraries

libs <- c(
  "dplyr", "sqldf", "readr", "tidyr",
  "ggplot2", "corrplot", "ggpubr",
  "e1071", "psych", "pastecs",
  "car", "rstatix", "stats",
  "lme4", "caret", "lavaan"
)

install_and_load(libs)

# Control
print(colnames(df_normal))

## We will be start to make correlation analysis. Therefore we will be call our numeric datas and 
## we will be ensure a correlation matrix.


# Correlation Analysis

Numeric_Data <- df_normal %>% select(Age, Annual_Income, Spending_Score)
cor_matrix <- cor(Numeric_Data)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", # Write on coefficents.
         tl.col = "black", 
         title = "Relations between variables", 
         mar = c(0,0,2,0))
## We see that Age and Spending_Score have negative correlation between thmesselves.
## So when age increases Spending_Score looses. When SPending_Score increases Age loosses.
## But that is not enought and powerfull a correlation but middle a correlation. 
## Also, we can't see any correlation for Afe and Annual_Income and Annual_Income and Spending_Score.

    ## NOTE: H6 rejected. Because annual income and spending score has not a correlation.

## Before start to hypothesess tests we will make test of normality and homogenity for using
## parametric or non-parametric tests.

# Test of normality

shapiro_age <- shapiro.test(df_normal$Age)
shapiro_income <- shapiro.test(df_normal$Annual_Income)
shapiro_score <- shapiro.test(df_normal$Spending_Score)

test_of_normality_all <- c(shapiro_age, shapiro_income, shapiro_score)
print(test_of_normality_all)

    ## According to W value the three values are so near to normal distribution and they can be assume normal.
    ## But when we look at the p values (important value is that) we don't have normal distribution.
    ## Except Age column.
    ## But when we look at our EDA report, our describing statistic script and, in here W values
    ## We can accept normal distribution for these 3 values.
    ## Because our std. deviasion values between +1,5 and -1,5, we are ensuring CLT.

# Test of homogenity

library(car)

levene_spending <- leveneTest(Spending_Score ~ Gender, data = df_normal)
levene_income <- leveneTest(Annual_Income ~ Gender, data = df_normal)

levene_result <- c(levene_spending, levene_income)

print(levene_result)

    ## We can ensure homogenity for both of them.

## We can start to hypotheses tests.

# First Hypotheses: To estimate population mean/variance confidence intervals regarding the spending
# behavior of the sample

## 1.1 Mean Point Estimation for Spending Score
t_test_result1 <- t.test(df_normal$Spending_Score, conf.level = 0.95)
print(t_test_result1)
    ## Our spending score mean in 0,95 confidence level as statistical 50,19697.

## 1.2 Mean Point Estimation for Annual Income
t_test_result2 <- t.test(df_normal$Annual_Income, conf.level = 0.95)
print(t_test_result2)
    ## Our annual income score mean in 0,95 confidence level as statistical 59,78788.

## 1.3 Mean Point Estimation for Age
t_test_result3 <- t.test(df_normal$Age, conf.level = 0.95)
print(t_test_result3)
    ## Our annual age mean in 0,95 confidence level as statistical 38,92929

## 1.4 Variance and Standart Deviasion Point Estimation for Spending Score
var_spending <- var(df_normal$Spending_Score)
print(var_spending)
sd_spending <- sqrt(var_spending)
print(sd_spending)
    ## Our spending score result (50,19697) can deviate 25,74685 point.
    ## This result is high for 0-100 a value.

## 1.5 Variance and Standart Deviasion Point Estimation for Annual Income
var_income <- var(df_normal$Annual_Income)
print(var_income)
sd_income <- sqrt(var_income)
print(sd_income)
    ## Annual income actually is not deviate so much. Because this value can be go to forever.
    ## It's just deviating 25,23726 points.

## 1.6 Variance and Standart Deviasion Point Estimation for Age
var_age <- var(df_normal$Age)
print(var_age)
sd_age <- sqrt(var_age)
print(sd_age)
    ## We have bigger a deviasion for years. Because 14,01685 is so big.
    ## Example, we should work 14-year generation for making a campaing, ad, or anything.


## 1.7 Confidence Interval Calculating
    ## We will be write a function for calculate to variance.
calc_var_ci <- function(data, conf.level = 0.95) {
  df <- length(data) - 1
  v  <- var(data)
  lower <- (df * v) / qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
  upper <- (df * v) / qchisq((1 - conf.level)/2, df, lower.tail = TRUE)
  return(c(lower, upper))
}


## 1.7.1 Spending Score Confidence Interval Mean and Variance

ci_mean_spending <- t.test(df_normal$Spending_Score, conf.level = 0.95)$conf.int
ci_var_spending  <- calc_var_ci(df_normal$Spending_Score, 0.95)
print(ci_mean_spending)
print(ci_var_spending)
    ## Spending score of our customers 46,58856-53,80538 between and this mean is our customers mean
    ## is not VIP or low type. Our customers are standard.

## 1.7.2 Annual Income Mean and Variance
ci_mean_income <- t.test(df_normal$Annual_Income, conf.level = 0.95)$conf.int
ci_var_income  <- calc_var_ci(df_normal$Annual_Income, 0.95)
print(ci_mean_income)
print(ci_var_income)
    ## Our customers have standard incomes like their spending scroes.

## 1.7.3 Age Mean and Variance
ci_mean_age <- t.test(df_normal$Age, conf.level = 0.95)$conf.int
ci_var_age <- calc_var_ci(df_normal$Age, 0.95)
print(ci_mean_age)
print(ci_var_age)
    ## Our customers are between 36,96484-40,89375. So our customers middle age and we should make some
    ## campaing for that.

###################################################################################################################
####      RESULT OF F??RST HYPOTHES:
####      Our customers middle age and they aren't making musch nor less spending. Also, they are middle class.
####      Therefore we should focus on the group. Because our customers in here.