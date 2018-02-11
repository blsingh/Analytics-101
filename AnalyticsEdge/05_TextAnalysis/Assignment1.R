########        Assignments week 5  Text Analytics

setwd("C:/Study/Analy_Edge/05_TextAnalysis")
wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)

# convert vandal column to factor
wiki$Vandal <- as.factor(wiki$Vandal)

table(wiki$Vandal)

# 1) Create the corpus for the Added column, and call it "corpusAdded".
library(tm)
corpus <- VCorpus(VectorSource(wiki$Added))

#2) Remove the English-language stopwords.

corpus <- tm_map(corpus, removeWords, stopwords("english"))

#3) Stem the words.
corpus <- tm_map(corpus, stemDocument)

#4) Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded <- DocumentTermMatrix(corpus)

# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions, and call the new matrix sparseAdded. How many terms appear in sparseAdded?
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded


# Problem 1.4  Bag of words
"Convert sparseAdded to a data frame called wordsAdded, and then prepend all the words with the letter A, by using the command:

colnames(wordsAdded) = paste("A", colnames(wordsAdded))

Now repeat all of the steps we've done so far (create a corpus, remove stop words, stem the document, create a sparse document term matrix, and convert it to a data frame) to create a Removed bag-of-words dataframe, called wordsRemoved, except this time, prepend all of the words with the letter R:

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

How many words are in the wordsRemoved data frame?"

wordsAdded <- as.data.frame(as.matrix(sparseAdded))

colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#       create wordsRemoved
corpus <- VCorpus(VectorSource(wiki$Removed))

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpus)

sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#Combine the two data frames into a data frame called wikiWords with the following line of code:

wikiWords = cbind(wordsAdded, wordsRemoved) 

# The cbind function combines two sets of variables for the same observations into one data frame. Then add the Vandal column (HINT: remember how we added the dependent variable back into our data frame in the Twitter lecture). Set the random seed to 123 and then split the data set using sample.split from the "caTools" package to put 70% in the training set.

wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)
sp <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)

train <- subset(wikiWords, sp==TRUE)
test <- subset(wikiWords, sp==FALSE)
# Problem 1.5
# What is the accuracy on the test set of a baseline method that always predicts "not vandalism" (the most frequent outcome)?
table(test$Vandal)


#  Build a CART model to predict Vandal, using all of the other variables as independent variables. Use the training set to build the model and the default parameters (don't set values for minbucket or cp). 

library(rpart)
library(rpart.plot)
wikiCART <- rpart(Vandal ~ . , data = train, method = "class")
wikipre <- predict(wikiCART, newdata = test, type = "class")
table(test$Vandal, wikipre)

# Plot the CART tree. How many word stems does the CART model use?
prp(wikiCART)





####     Problem 2.1 - Problem-specific Knowledge 
########
"We weren't able to improve on the baseline using the raw textual information. More specifically, the words themselves were not useful. There are other options though, and in this section we will try two techniques - identifying a key class of words, and counting words.

The key class of words we will use are website addresses. "Website addresses" (also known as URLs - Uniform Resource Locators) are comprised of two main parts. An example would be "http://www.google.com". The first part is the protocol, which is usually "http" (HyperText Transfer Protocol). The second part is the address of the site, e.g. "www.google.com". We have stripped all punctuation so links to websites appear in the data as one word, e.g. "httpwwwgooglecom". We hypothesize that given that a lot of vandalism seems to be adding links to promotional or irrelevant websites, the presence of a web address is a sign of vandalism.

We can search for the presence of a web address in the words added by searching for "http" in the Added column. The grepl function returns TRUE if a string is found in another string, e.g.

grepl("cat","dogs and cats",fixed=TRUE) # TRUE

grepl("cat","dogs and rats",fixed=TRUE) # FALSE

Create a copy of your dataframe from the previous question:"
wikiWords2 <- wikiWords

#Make a new column in wikiWords2 that is 1 if "http" was in Added:

wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

# Based on this new column, how many revisions added a link?
table(wikiWords2$HTTP)

# In problem 1.5, you computed a vector called "spl" that identified the observations to put in the training and testing sets. Use that variable (do not recompute it with sample.split) to make new training and testing sets:

wikiTrain2 = subset(wikiWords2, sp==TRUE)

wikiTest2 = subset(wikiWords2, sp==FALSE)

# Then create a new CART model using this new variable as one of the independent variables.
# What is the new accuracy of the CART model on the test set, using a threshold of 0.5?

wiki2CART <- rpart(Vandal ~ . , data = wikiTrain2, method = "class")
wiki2pred <- predict(wiki2CART, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, wiki2pred)
(609+57)/(609+9+488+57)

# Another possibility is that the number of words added and removed is predictive, perhaps more so than the actual words themselves. We already have a word count available in the form of the document-term matrices (DTMs).

#Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your data frame wikiWords2 (called NumWordsAdded and NumWordsRemoved) by using the following commands:

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

#What is the average number of words added?
mean(wikiWords2$NumWordsAdded)

# In problem 1.5, you computed a vector called "spl" that identified the observations to put in the training and testing sets. Use that variable (do not recompute it with sample.split) to make new training and testing sets with wikiWords2. Create the CART model again (using the training set and the default parameters).

wikiTrain2 = subset(wikiWords2, sp==TRUE)

wikiTest2 = subset(wikiWords2, sp==FALSE)

wiki3CART <- rpart(Vandal ~ . , data = wikiTrain2, method = "class")
wiki3pred <- predict(wiki3CART, newdata = wikiTest2, type = "class")

# What is the new accuracy of the CART model on the test set?
table(wikiTest2$Vandal, wiki3pred)
(514 + 248)/(514+104+297+248)


####     Problem 3.1 - Using Non-Textual Data 
########

# We have two pieces of "metadata" (data about data) that we haven't yet used. Make a copy of wikiWords2, and call it wikiWords3:

wikiWords3 = wikiWords2

#Then add the two original variables Minor and Loggedin to this new data frame:

wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin

# In problem 1.5, you computed a vector called "spl" that identified the observations to put in the training and testing sets. Use that variable (do not recompute it with sample.split) to make new training and testing sets with wikiWords3.
wikiTrain3 <- subset(wikiWords3, sp == TRUE)
wikiTest3 <- subset(wikiWords3, sp == FALSE)

#Build a CART model using all the training data. What is the accuracy of the model on the test set?

wiki3CART <- rpart(Vandal ~ ., data = wikiTrain3, method = "class")
wiki3pred <- predict(wiki3CART, newdata = wikiTest3, type = "class")

table(wikiTest3$Vandal, wiki3pred)
(595+241)/(595+23+304+241)

prp(wiki3CART)
