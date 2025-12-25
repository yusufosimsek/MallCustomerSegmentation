                              ##Second Step: Detail Analysis##

## Uploading file.

df_final <- read.csv("C:\\Users\\Yusuf\\Desktop\\MallCustomerSegmentation\\data\\Mall_Customers_SQL_Final.csv", header = FALSE, , sep=";")


## When we made output from SQL we lost headers. Therefore we will be add them.
colnames(df_final) <- c(
  "CustomerID", 
  "Gender", 
  "Age", 
  "Annual_Income", 
  "Spending_Score", 
  "Age_Group",
  "Spending_Group",
  "Income_Quartile",
  "Spending_Quartile"
)

## We will be make categorical datas for some columns.

cols_to_factor <- c("Age_Group", "Spending_Group", "Income_Quartile", "Spending_Quartile")
df_final[cols_to_factor] <- lapply(df_final[cols_to_factor], as.factor)

## Import to libraries.

install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

libs <- c(
  "dplyr",
  "sqldf",
  "readr",
  "tidyr",

  "ggplot2",
  "corrplot",
  "ggpubr",

  "e1071",
  "psych",
  "pastecs",

  "car",
  "rstatix",
  "stats",

  "lme4",
  "caret",
  "lavaan"
)

install_and_load(libs)




---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

  
  library(e1071)

get_mode <- function(v) { uniqv <- unique(v); uniqv[which.max(tabulate(match(v, uniqv)))] }
get_se <- function(x) sd(x) / sqrt(length(x))


AimColumns <- c("Age", "Annual_Income", "Spending_Score")


DescribingStatistic <- data.frame(
  Variable1 = AimColumns,
  Avarage   = sapply(df_final[AimColumns], mean),
  Med       = sapply(df_final[AimColumns], median),
  Mod       = sapply(df_final[AimColumns], get_mode),
  StdDev    = sapply(df_final[AimColumns], sd),
  Varian    = sapply(df_final[AimColumns], var), 
  StdErr    = sapply(df_final[AimColumns], get_se), 
  Skewness  = sapply(df_final[AimColumns], skewness), 
  Curtosis  = sapply(df_final[AimColumns], kurtosis), 
  Min       = sapply(df_final[AimColumns], min),
  Max       = sapply(df_final[AimColumns], max)
)

DescribingStatistic[, -1] <- round(DescribingStatistic[, -1], 2)
print(DescribingStatistic)

    ## We can say mean and median values so near. Mode values little far bur not so much.
    ## Standard deviation is so high for every columns.
    ## First result is enough for normal distribution but second result is not.
    ## This is show us simple and the best slack normal distribution accepting can't ensure.


# Describing Graphs

library(ggplot2)
library(ggpubr)

plot_histogram <- function(data, column, title, fill_color) {
  ggplot(data, aes(x = .data[[column]])) +
    geom_histogram(aes(y = ..density..), bins = 20, fill = fill_color, color = "black", alpha = 0.7) +
    geom_density(color = "red", size = 1) +
    labs(title = title, x = column, y = "Yo??unluk") +
    theme_minimal()
}


plot_boxplot <- function(data, column, fill_color) {
  ggplot(data, aes(y = .data[[column]])) +
    geom_boxplot(fill = fill_color, color = "black", outlier.colour = "red", outlier.shape = 16) +
    labs(y = column) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}


hist_age <- plot_histogram(df_final, "Age", "Ya?? Da????l??m??", "lightblue")
box_age  <- plot_boxplot(df_final, "Age", "lightblue")

hist_income <- plot_histogram(df_final, "Annual_Income", "Gelir Da????l??m??", "lightgreen")
box_income  <- plot_boxplot(df_final, "Annual_Income", "lightgreen")

hist_score <- plot_histogram(df_final, "Spending_Score", "Harcama Skoru Da????l??m??", "pink")
box_score  <- plot_boxplot(df_final, "Spending_Score", "pink")

ggarrange(
  hist_age, box_age, 
  hist_income, box_income, 
  hist_score, box_score,
  ncol = 2, nrow = 3, # 2 s??tun, 3 sat??r
  widths = c(2, 1)    # Histogram geni??, Boxplot dar olsun
)

    ## When we look at the box plot and histogram graphs we see not enought normal distribution.
    ## But when we look at the graphs we can say we don't have outliers values (This is important for for regression analysis).
    ## We will be last control for normality when further time.

# Cleaning outlier values.

outlier_val <- boxplot.stats(df_final$Annual_Income)$out
df_clean <- subset(df_final, Annual_Income != outlier_val)

compare_models <- function(data, numeric_col, category_col, target_col) {
  
  formula_num <- as.formula(paste(target_col, "~", numeric_col))
  model_num <- lm(formula_num, data = data)
  
  formula_cat <- as.formula(paste(target_col, "~", category_col))
  model_cat <- lm(formula_cat, data = data)
  
  results <- data.frame(
    Model_Turu = c("Say??sal (Numeric)", "Kategorik (Categorical)"),
    AIC_Skoru = c(AIC(model_num), AIC(model_cat)),
    BIC_Skoru = c(BIC(model_num), BIC(model_cat)),
    R_Kare    = c(summary(model_num)$r.squared, summary(model_cat)$r.squared)
  )
  
  return(results)
}

print(compare_models(df_clean, "Age", "Age_Group", "Spending_Score"))
print(compare_models(df_clean, "Annual_Income", "Income_Quartile", "Spending_Score"))
    ## Categorical and numeric datas near. Therefore we can say categorical datas have little knowledge losing.
    ## Therefore we can use our categorical datas.

# Finished describing statistic and EDA step.
# We can continue inferentical statistic.
# Before we should save our analysis.

write.csv(df_clean, "C:/Users/Yusuf/Desktop/MallCustomerSegmentation/Mall_Customers_EDA_DONE.csv", row.names = FALSE)
