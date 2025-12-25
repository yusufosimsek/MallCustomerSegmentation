## Uploading file.

dataFrameCustomers <- read.csv("C:\\Users\\Yusuf\\Desktop\\MallCustomerSegmentation\\data\\mall_customers.csv", sep=";")


## Looking at column names.
head(dataFrameCustomers)

## Looking at data types.
str(dataFrameCustomers)
##NOTE: We will be changed int for Gender column. Male=1, Female=2
dataFrameCustomers$Gender_Numeric <- ifelse(dataFrameCustomers$Gender == "Male", 1, 2)
head(dataFrameCustomers)
## We ensured numeric types for genders. We will be use Gender_Numeric for further times statistical workings.

## Summary
summary(dataFrameCustomers)
## When we look at the summery table we can say our data has normal distribution for all column.
## Because median and mean so near for every column and quartiles are increasing linear.
## We can say that for first table but we will be look at graphs and normality tests for to be sure.

## We will be look at histograms before test of normality.
## Therefore R's commands for us but we can use ggplot2 library why more proffesinal graphs. But for now we won't be use it.
## NOTE: We can assume normal distribution due to CLT (because our datas bigger than 30).
## But if we want to be more sure to normality. We will use these methods.

# Annual Income Histogram
hist(dataFrameCustomers$Annual.Income..k.., 
     main = "Annual Income Distribution",
     xlab = "Annual Income (k$)",
     col = "lightblue", 
     border = "white")
## We have left scewed a graph. But same time it's so near to normal distribution.

# Spending Score Histogram
hist(dataFrameCustomers$Spending.Score..1.100., 
     main = "Spending Score Distribution",
     xlab = "Spending Score (1-100)",
     col = "lightgreen", 
     border = "white")
## Some hists are breaking normality but despite that, we can say we have normal distribution.

# Now we will be look at a scatter plot graph for Annual Income and Spending Score.
plot(dataFrameCustomers$Annual.Income..k.., 
     dataFrameCustomers$Spending.Score..1.100.,
     main = "Annual Income vs Spending Score",
     xlab = "Annual Income (k$)",
     ylab = "Spending Score (1-100)",
     pch = 19,
     col = "darkblue")
## We can say both of them have a clustering between together.
## We can see (as predict) 5 gorups.

# Looking at the how many groups.
set.seed(123)
kmeans_model <- kmeans(dataFrameCustomers[, 4:5], centers = 5)
plot(dataFrameCustomers$Annual.Income..k.., 
     dataFrameCustomers$Spending.Score..1.100.,
     col = kmeans_model$cluster, 
     pch = 19,
     cex = 1.5,
     main = "Customer Segments(K-Means)",
     xlab = "Annual Income (k$)",
     ylab = "Spending Score (1-100)")

## We saw the 5 groups.