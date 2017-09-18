
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
names(tweets)

library(tm)
corpus = VCorpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus ,content_transformer(tolower))
corpus = tm_map(corpus , removePunctuation)
corpus = tm_map(corpus , removeWords, c("apple",stopwords("english")))

dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
dim(allTweets)

library(wordcloud)

?wordcloud
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(2,0.25))
