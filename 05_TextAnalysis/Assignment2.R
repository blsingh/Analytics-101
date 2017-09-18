"
automating reviews in medicine

The medical literature is enormous. Pubmed, a database of medical publications maintained by the U.S. National Library of Medicine, has indexed over 23 million medical publications. Further, the rate of medical publication has increased over time, and now there are nearly 1 million new publications in the field each year, or more than one per minute.

The large size and fast-changing nature of the medical literature has increased the need for reviews, which search databases like Pubmed for papers on a particular topic and then report results from the papers found. While such reviews are often performed manually, with multiple people reviewing each search result, this is tedious and time consuming. In this problem, we will see how text analytics can be used to automate the process of information retrieval.

The dataset consists of the titles (variable title) and abstracts (variable abstract) of papers retrieved in a Pubmed search. Each search result is labeled with whether the paper is a clinical trial testing a drug therapy for cancer (variable trial). These labels were obtained by two people reviewing each search result and accessing the actual paper if necessary, as part of a literature review of clinical trials testing drug therapies for advanced and metastatic breast cancer.
"

####    
getwd() # "C:/Study/Analy_Edge/05_TextAnalysis"

trails <- read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
str(trails)

# How many characters are there in the longest abstract? (Longest here is defined as the abstract with the largest number of characters.) 

head(sort(nchar(trails$abstract), decreasing = T))

# How many search results provided no abstract? (HINT: A search result provided no abstract if the number of characters in the abstract field is zero.)

table(nchar(trails$abstract) == 0)

# Find the observation with the minimum number of characters in the title (the variable "title") out of all of the observations in this dataset. What is the text of the title of this article? Include capitalization and punctuation in your response, but don't include the quotes.

trails[which(nchar(trails$title) == min(nchar(trails$title))), 'title']

#  Problem 2.1 - Preparing the Corpus 

# Because we have both title and abstract information for trials, we need to build two corpora instead of one. Name them corpusTitle and corpusAbstract.

#Following the commands from lecture, perform the following tasks (you might need to load the "tm" package first if it isn't already loaded). Make sure to perform them in this order.
library(tm)
#1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpusTitle <- VCorpus(VectorSource(trails$title))
corpusAbstract <- VCorpus(VectorSource(trails$abstract))
#2) Convert corpusTitle and corpusAbstract to lowercase.
corpusTitle <- tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract <- tm_map(corpusAbstract, content_transformer(tolower))
#3) Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
#4) Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract,removeWords, stopwords("english"))
#5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
#6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
#7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)
#8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

# How many terms remain in dtmTitle after removing sparse terms (aka how many columns does it have)?
dim(dtmTitle)
dim(dtmAbstract)

# What is the most frequent word stem across all the abstracts? Hint: you can use colSums() to compute the frequency of a word across all the abstracts.
sort(colSums(dtmAbstract),decreasing = TRUE)

# We want to combine dtmTitle and dtmAbstract into a single data frame to make predictions. However, some of the variables in these data frames have the same names. To fix this issue, run the following commands:

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#  Problem 3.2 - Building a Model 
#Using cbind(), combine dtmTitle and dtmAbstract into a single data frame called dtm:

dtm = cbind(dtmTitle, dtmAbstract)

# As we did in class, add the dependent variable "trial" to dtm, copying it from the original data frame called trials. How many columns are in this combined data frame?
dtm$trail <- trails$trial
dim(dtm)

#  Problem 3.3 - Building a Model 
# Now that we have prepared our data frame, it's time to split it into a training and testing set and to build regression models. Set the random seed to 144 and use the sample.split function from the caTools package to split dtm into data frames named "train" and "test", putting 70% of the data in the training set.

set.seed(144)
spl <- sample.split(dtm$trail, SplitRatio = 0.7)
train <- subset(dtm, spl == TRUE)
test <- subset(dtm, spl == FALSE)


# What is the accuracy of the baseline model on the training set? (Remember that the baseline model predicts the most frequent outcome in the training set for all observations.)
table(train$trail)

# Build a CART model called trialCART, using all the independent variables in the training set to train the model, and then plot the CART model. Just use the default parameters to build the model (don't add a minbucket or cp value). Remember to add the method="class" argument, since this is a classification problem.
trailCART <- rpart(trail ~ . , data = train, method = "class")

# What is the name of the first variable the model split on?
prp(trailCART)

# Obtain the training set predictions for the model (do not yet predict on the test set). Extract the predicted probability of a result being a trial (recall that this involves not setting a type argument, and keeping only the second column of the predict output). What is the maximum predicted probability for any result?
predTrain = predict(trailCART)[,2]

summary(predTrain) 

# For these questions, use a threshold probability of 0.5 to predict that an observation is a clinical trial.

# What is the training set accuracy of the CART model?
sum(diag(table(train$trail, predTrain > .5)))/sum(table(train$trail, predTrain > .5))

# What is the training set sensitivity of the CART model?
# What is the training set specificity of the CART model?
table(train$trail, predTrain >= .5)
table(train$trail, predTrain >= 0.5)

#Evaluate the CART model on the testing set using the predict function and creating a vector of predicted probabilities predTest.
predTest <- predict(trailCART, newdata = test)[,2]
#What is the testing set accuracy, assuming a probability threshold of 0.5 for predicting that a result is a clinical trial?
table(test$trail, predTest >= .5)
(260+178)/(260+53+67+178)

