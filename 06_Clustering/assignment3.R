
 setwd("c:/Study/Analy_Edge/06_Clustering/")
# Load StocksCluster.csv into a data frame called "stocks". How many observations are in the dataset?
stocks = read.csv("StocksCluster.csv")


# What is the maximum correlation between any two return variables in the dataset? You should look at the pairwise correlations between ReturnJan, ReturnFeb, ReturnMar, ReturnApr, ReturnMay, ReturnJune, ReturnJuly, ReturnAug, ReturnSep, ReturnOct, and ReturnNov.
cor(stocks)

prob1 <- cor(stocks)
diag(prob1) = NA
max(prob1, na.rm = T)


# Which month (from January through November) has the largest mean return across all observations in the dataset? 
sort(sapply(stocks[1:11], mean))


##  Problem 2.1 - Initial Logistic Regression Model 
# Run the following commands to split the data into a training set and testing set, putting 70% of the data in the training set and 30% of the data in the testing set:
library(caTools)
set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)

#   Then, use the stocksTrain data frame to train a logistic regression model (name it StocksModel) to predict PositiveDec using all the other variables as independent variables. Don't forget to add the argument family=binomial to your glm command.
stocksModel <- glm(PositiveDec ~ . , data = stocksTrain, family = "binomial")
#What is the overall accuracy on the training set, using a threshold of 0.5?
prLog_stocks = predict(stocksModel, type = "response") # Since the model was logit_reg I will not use ' type = "class" '

# Accuracy
table(stocksTrain$PositiveDec, prLog_stocks >= .5)

sum(diag(table(stocksTrain$PositiveDec, prLog_stocks >= .5)))/sum(table(stocksTrain$PositiveDec, prLog_stocks >= .5))

# Now obtain test set predictions from StocksModel. What is the overall accuracy of the model on the test, again using a threshold of 0.5?

prLog_stock_test <- predict(stocksModel, newdata = stocksTest, type = "response")

table(stocksTest$PositiveDec, prLog_stock_test >= .5)

sum(diag(table(stocksTest$PositiveDec, prLog_stock_test >= .5)))/sum(table(stocksTest$PositiveDec, prLog_stock_test >= .5))

# What is the accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)?
table(stocksTrain$PositiveDec)
4427/(3679+4427)


#  Problem 3.1 - Clustering Stocks 
# Now, let's cluster the stocks. The first step in this process is to remove the dependent variable using the following commands:

limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

# In the market segmentation assignment in this week's homework, you were introduced to the preProcess command from the caret package, which normalizes variables by subtracting by the mean and dividing by the standard deviation.
library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

# What is the mean of the ReturnJan variable in normTrain?
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

# Set the random seed to 144 (it is important to do this again, even though we did it earlier). Run k-means clustering with 3 clusters on normTrain, storing the result in an object called km.
set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster)

# Recall from the recitation that we can use the flexclust package to obtain training set and testing set cluster assignments for our observations (note that the call to as.kcca may take a while to complete):
library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

# How many test-set observations were assigned to Cluster 2?
table(clusterTest)


##  Problem 4.1 - Cluster-Specific Predictions 
# Using the subset function, build data frames stocksTrain1, stocksTrain2, and stocksTrain3, containing the elements in the stocksTrain data frame assigned to clusters 1, 2, and 3, respectively (be careful to take subsets of stocksTrain, not of normTrain). Similarly build stocksTest1, stocksTest2, and stocksTest3 from the stocksTest data frame.

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

# Which training set data frame has the highest average value of the dependent variable?
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# Build logistic regression models StocksModel1, StocksModel2, and StocksModel3, which predict PositiveDec using all the other variables as independent variables. StocksModel1 should be trained on stocksTrain1, StocksModel2 should be trained on stocksTrain2, and StocksModel3 should be trained on stocksTrain3.

StockModel1 <- glm(PositiveDec ~ ., data = stocksTrain1, family = "binomial")
StockModel2 <- glm(PositiveDec ~ ., data = stocksTrain2, family = "binomial")
StockModel3 <- glm(PositiveDec ~ ., data = stocksTrain3, family = "binomial")

# Which variables have a positive sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3 and a negative sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3? Select all that apply.
cbind(
    summary(StockModel1)$coefficients[,1],
    summary(StockModel2)$coefficients[,1],
    summary(StockModel3)$coefficients[,1]
)

# Using StocksModel1, make test-set predictions called PredictTest1 on the data frame stocksTest1. Using StocksModel2, make test-set predictions called PredictTest2 on the data frame stocksTest2. Using StocksModel3, make test-set predictions called PredictTest3 on the data frame stocksTest3.

pr_stocks_md1 = predict(StockModel1, newdata = stocksTest1, type = "response")
pr_stocks_md2 = predict(StockModel2, newdata = stocksTest2, type = "response")
pr_stocks_md3 = predict(StockModel3, newdata = stocksTest3, type = "response")

# Accuracy
sum(diag(table(stocksTest1$PositiveDec, pr_stocks_md1 > .5)))/sum(table(stocksTest1$PositiveDec, pr_stocks_md1 > .5))

sum(diag(table(stocksTest2$PositiveDec, pr_stocks_md2 > .5)))/sum(table(stocksTest2$PositiveDec, pr_stocks_md2 > .5))

sum(diag(table(stocksTest3$PositiveDec, pr_stocks_md3 > .5)))/sum(table(stocksTest3$PositiveDec, pr_stocks_md3 > .5))

##  Problem 4.4 - Cluster-Specific Predictions 

# To compute the overall test-set accuracy of the cluster-then-predict approach, we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector:
Allpredictions = c(pr_stocks_md1, pr_stocks_md2, pr_stocks_md3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
sum(diag(table(AllOutcomes, Allpredictions >= .5)))/sum(table(AllOutcomes, Allpredictions >= .5))
