
## In the folders for each module are scripts, assignments, dataset, etc

#### What are the different models for analytics and what **PACKAGES** are used in each.

####    # caTools :: for spliting data
        # glm :: for logistics regression
        # ROCR :: for ROCR and AUC 
        # rpart :: for TRESS; regrssion/classification
        # rpart.plot ::
        # caret :: cross validataion
        # e1071 :: cross validation
        # randomforrest :: randomforrest
        


##   Logistic Regression
####
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
            # Performance function
        ROCRperf = performance(ROCRpred, "tpr", "fpr")
        
        # Plot ROC curve
        plot(ROCRperf)
        
        # Add colors
        plot(ROCRperf, colorize=TRUE)
        
        # Add threshold labels 
        plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
        
        # To calculate the AUC:
        auc = as.numeric(performance(ROCRpred, "auc")@y.values)
        auc  # Compute the AUC
    

##    (Classification and Regression) Trees
####
    
    setwd("C:/Study/Analy_Edge/04_CART/")
    stevens <- read.csv("stevens.csv")
    
    # Split the data
    library(caTools)
    set.seed(3000)
    spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
    Train = subset(stevens, spl==TRUE)
    Test = subset(stevens, spl==FALSE)

    library(rpart)
    library(rpart.plot)
    
    StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=5)

    prp(StevensTree) #plot the trees
    # Make predictions
    PredictCART = predict(StevensTree, newdata = Test, type = "class")
    # * Note the prediction here is 1 or 0
    
########
######## I logistic regression is highly accurate vis-a-vi AUC and with variables that are significant, we can gauge which variables are relevant for this prediction task. However, it is not immediately clear which variables are more important than the others, especially with large number of factor variables in logistics regression.  Introduce CART model.
    
########
######## Let us now consider the ROC curve and AUC for the CART model on the test set. You will need to get predicted probabilities for the observations in the test set to build the ROC curve and compute the AUC. Remember that you can do this by removing the type="class" argument when making predictions, and taking the second column of the resulting object.

########
    ####    RandomForest & Variable used -- check exercise3 under 04_CART
    
########
    ####    Selecting cp by Cross-Validation  -- check exercise3 under 04_CART
