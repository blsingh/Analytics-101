##
    setwd("c:/Study/Analy_Edge/03_LOGIT/")
    imputed <- read.csv("loans_imputed.csv")
    set.seed(144)
    library(caTools)
    split <- sample.split(imputed$not.fully.paid, SplitRatio = 0.7)
    train <- subset(imputed, split == TRUE)
    test <- subset(imputed, split == FALSE)
    model <- glm(not.fully.paid ~ ., data = train, family = binomial)
    test$predicted.risk <- predict(model, newdata = test, type = "response")
    
    ## Confusion matrix
    table(test$not.fully.paid, test$predicted.risk > 0.5)
    table(test$not.fully.paid) # Baseline
    ## Accuracy
    (2400 + 3)/sum(table(test$not.fully.paid, test$predicted.risk > 0.5))
    
    ## Area Under the ROC Curve
    library(ROCR)
    Rocrpred <- prediction(test$predicted.risk, test$not.fully.paid)
    auc <- as.numeric(performance(Rocrpred, "auc")@y.values)
    auc
    
