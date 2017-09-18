
library(ggplot2)
library(maps)
library(ggmap)


# load the US map and save it
statesMap = map_data("state")

unique(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 


#   Now, let's color the map of the US according to our 2012 US presidential election predictions from the Unit 3 Recitation. We'll rebuild the model here, using the dataset PollingImputed.csv. Be sure to use this file so that you don't have to redo the imputation to fill in the missing values, like we did in the Unit 3 Recitation.

polling = read.csv("PollingImputed.csv")


#   Load the data using the read.csv function, and call it "polling". Then split the data using the subset function into a training set called "Train" that has observations from 2004 and 2008, and a testing set called "Test" that has observations from 2012. 
Train = subset(polling, Year <2012)
Test = subset(polling, Year == 2012)

#  Then, create a logistic regression model and make predictions on the test set using the following commands:
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")


# TestPrediction gives the predicted probabilities for each state, but let's also create a vector of Republican/Democrat predictions by using the following command:

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

# Now, put the predictions and state labels in a data.frame so that we can use ggplot:

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

# get ready to merge
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
#merge
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

# Lastly, we need to make sure the observations are in order so that the map is drawn properly, by typing the following:

predictionMap = predictionMap[order(predictionMap$order),]# What happens is the when we merge the order was messed up


#  Now we are ready to color the US map with our predictions! You can color the states according to our binary predictions by typing the following in your R console:

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")    

#   We see that the legend displays a blue gradient for outcomes between 0 and 1. However, when plotting the binary predictions there are only two possible outcomes: 0 or 1. Let's replot the map with discrete outcomes. We can also change the color scheme to blue and red, to match the blue color associated with the Democratic Party in the US and the red color associated with the Republican Party in the US. This can be done with the following command:

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black", alpha =.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
