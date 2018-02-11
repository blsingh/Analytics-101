
emails <- read.csv("emails.csv", stringsAsFactors = F)
dim(emails)

corpus <- VCorpus(VectorSource(emails$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)
dtm
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))

sort(colSums(emailsSparse),decreasing = F)

emailsSparse$spam <- emails$spam

sort(colSums(subset(emailsSparse, spam == 0)))


#

emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)

spl <-sample.split(emailsSparse$spam, SplitRatio = 0.7)
train <- subset(emailsSparse, spl == T)
test <- subset(emailsSparse, spl == F)
set.seed(123)
spamLog <- glm(spam ~ ., data = train, family = "binomial")
set.seed(123)
spamCART <- rpart(spam ~ ., data = train, method = "class")
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data = train)

problog <- predict(spamLog, data = train)
probCART <- predict(spamCART)[,2]
probRF <- predict(spamRF, type = "prob")[,2]


table(train$spam, problog > .5)
sum(diag(table(train$spam, problog > .5)))/sum(table(train$spam, problog > .5))


# 
library(ROCR)
predROCR <- prediction(problog, train$spam)
performance(predROCR, "auc")@y.values


table(train$spam, probCART > .5)
sum(diag(table(train$spam, probCART > .5)))/sum(table(train$spam, probCART > .5))
predROCR <- prediction(probCART, train$spam)
performance(predROCR, "auc")@y.values


table(train$spam, probRF > .5)
sum(diag(table(train$spam, probRF > .5)))/sum(table(train$spam, probRF > .5))

predROCR <- prediction(probRF, train$spam)
performance(predROCR, "auc")@y.values

predLog_test <- predict(spamLog, newdata = test)
table(test$spam, predLog_test>.5)
sum(diag(table(test$spam, predLog_test>.5)))/sum(table(test$spam, predLog_test>.5))

ROCRpred <- prediction(predLog_test, test$spam)
performance(ROCRpred, "auc")@y.values


predCART_test <- predict(spamCART, newdata = test)[,2]
table(test$spam,predCART_test > .5)
sum(diag(table(test$spam,predCART_test > .5)))/sum(table(test$spam,predCART_test > .5))

ROCRpred <- prediction(predCART_test, test$spam)
performance(ROCRpred, "auc")@y.values


predRF_test <- predict(spamRF, newdata = test, type = "prob")[,2]
table(test$spam, predRF_test > .5)
sum(diag(table(test$spam, predRF_test > .5)))/sum(table(test$spam, predRF_test > .5))

ROCRpred <- prediction(predRF_test, test$spam)
performance(ROCRpred, "auc")@y.values
