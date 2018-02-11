##### Starting with Analytics edge an MITx, course is relatively basic but it is still worth taking.  In addition there are more advanced Masters course, such as Introdction to Analyics Modeling, a GTx course which covers the Analytics modeling in more breadth, plus UMich's Data Science and Predictive analytics course.  Still both of these courses are not with the Mathematical rigor of say Shalazi or Wasserman's courses that as of this date are still available online on .edu websites.

### This Repo is deticated to my work from courses in Analytics 
_____
_____

# The Analytics Edge in `AnalyticEdge` folder
This course is taught based on R and is relatively easy. However, it is still worth taking because it is very practical and contains plenty of interesting data sets. Here I list some points, useful and for future reference. I recommend this course to anyone who is interested in analytics.


## An Introduction to Analytics
`table()`
>`table` uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels.

`tapply()`
>Apply a function to each cell of a ragged array, that is to each (non-empty) group of values given by a unique combination of the levels of certain factors.


## Linear Regression
```r
spl = sample.split(data$class, SplitRatio = 0.7)
train = subset(data, spl == TRUE)
test = subset(data, spl == FALSE)
model = lm(y ~ ., data = train)
pred = predict(model, newdata = test)

# out-of-sample R squared
SSE = sum((test$y - pred)^2)
SST = sum((test$y - mean(train$y))^2)
RSquared = 1 - SSE / SST
```

`subset()`
>Return subsets of vectors, matrices or data frames which meet conditions.

`step()`
>Select a formula-based model by AIC.

`na.omit()`
>`na.omit` returns the object with incomplete cases removed.

`coredata()` in `zoo`
>Generic functions for extracting the core data contained in a (more complex) object and replacing it.

It can be used to avoid explicitly transforming data types such as using as.vector().

`mice()` in `mice`
>Generates Multivariate Imputations by Chained Equations (MICE)

`complete()` in `mice`
>Takes an object of class mids, fills in the missing data, and returns the completed data in a specified format.`


## Logistic Regression
```r
LogModel = glm(formula, data = train, family = "binomial")
pred = predict(LogModel, newdata = test, type = "response")
predObj = prediction(pred, test$class)
auc = as.numeric(performance(predObj, "auc")@y.vallues)
perf = performance(predObj, "tpr", "fpr")
plot(perf) # roc curve
```

definition of [sensitivity, specificity and accuracy](https://en.wikipedia.org/wiki/Sensitivity_and_specificity)

interpretation of auc
* The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator.
* The AUC of a model has the following nice interpretation: given a random patient from the dataset who actually received poor care, and a random patient from the dataset who actually received good care, the AUC is the perecentage of time that our model will classify which is which correctly.

Sometimes the accuracy will not improve a lot compared to a baseline prediction (always predicting the majority), but the model is still valuable since it gives more information and different cutoffs can be used in different situations. The quality of the model is mainly evaluated by auc.


## Trees
```r
tree = rpart(formula, data = train, method = "class", minbucket = 25)
prp(tree)
pred = predict(tree, newdata = test)
predObj = prediction(pred[, 2], test$class)

# cross-validation
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01)) 
train(formula, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
```

`method = "class"` should be removed for regression trees. `type = "class"` can be added in the `predict()` function if we want to predict the labels when the cutoff is 0.5.


## Text Analytics
```r
corpus = Corpus(VectorSource(data))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)
dataSparse = as.data.frame(as.matrix(spdtm))
names(dataSparse) = make.names(names(dataSparse))
```

`ifelse()`
>ifelse returns a value with the same shape as test which is filled with elements selected from either yes or no depending on whether the element of test is TRUE or FALSE.

It is convenient to use and can save me some code.

`nchar()`
>`nchar` takes a character vector as an argument and returns a vector whose elements contain the sizes of the corresponding elements of x.

Note there is no length() function for string.


## Clustering
```r
# standardization
library(caret)
preproc = preProcess(train)
normTrain = predict(preproc, train)
normTest = predict(preproc, test)

# prediction with k-means
library(flexclust)
km = kmeans(normTrain, centers = k)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
```

`rect.hclust()`
>Draws rectangles around the branches of a dendrogram highlighting the corresponding clusters. First the dendrogram is cut at a certain level, then a rectangle is drawn around selected branches.
It helps visualize the sizes of clusters.

`image()`
>Creates a grid of colored or gray-scale rectangles with colors corresponding to the values in z. This can be used to display three-dimensional or spatial data aka images. This is a generic function.

`dim()`
>Retrieve or set the dimension of an object.

It can be used to transform a vector into a matrix.

`split()` 
>`split` divides the data in the vector x into the groups defined by f.

A list is returned and then `lapply()`, `sapply()` often can be used.


## Visualization
[`ggplot2`](http://docs.ggplot2.org/current/) documentation

`ggmap()`
>`ggmap` plots the raster object produced by `get_map()`.

`factor()` can be used to reorder factor levels.

`strptime()` can be used to extract time and date from a string.

`weekdays()`
>Extract the weekday, month or quarter, or the Julian time (days since some origin). 


## Linear Optimization & Integer Optimization
solver in Excel
