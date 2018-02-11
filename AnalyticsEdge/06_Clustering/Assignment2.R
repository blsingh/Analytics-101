list.files()

airlines <- read.csv("AirlinesCluster.csv")

#       Normalilze the data 
# Let's go ahead and normalize our data. You can normalize the variables in a data frame by using the preProcess function in the "caret" package. You should already have this package installed from Week 4, but if not, go ahead and install it with install.packages("caret"). Then load the package with library(caret).

library(caret)
preproc = preProcess(airlines)

airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)


# Compute the distances between data points (using euclidean distance) and then run the Hierarchical clustering algorithm (using method="ward.D") on the normalized data. It may take a few minutes for the commands to finish since the dataset has a large number of observations for hierarchical clustering.
distance <- dist(airlinesNorm, method = "euclidean")
airlinesClus <- hclust(distance, method = "ward.D")
plot(airlinesClus)

# Suppose that after looking at the dendrogram and discussing with the marketing department, the airline decides to proceed with 5 clusters. Divide the data points into 5 clusters by using the cutree function. How many data points are in Cluster 1?

air_5clus <- cutree(airlinesClus, k = 5)

table(air_5clus)

# Now, use tapply to compare the average values in each of the variables for the 5 clusters (the centroids of the clusters). You may want to compute the average values of the unnormalized data so that it is easier to interpret. You can do this for the variable "Balance" with the following command:
tapply(airlines$Balance, air_5clus, mean)
tapply(airlines$QualMiles, air_5clus, mean)
tapply(airlines$BonusMiles, air_5clus, mean)
tapply(airlines$BonusTrans, air_5clus, mean)
tapply(airlines$FlightMiles, air_5clus, mean)
tapply(airlines$FlightTrans, air_5clus, mean)
tapply(airlines$DaysSinceEnroll, air_5clus, mean)

# OR
sapply(airlines, function(x){tapply(x, air_5clus, mean)})

##  Problem 3
# Now run the k-means clustering algorithm on the normalized data, again creating 5 clusters. Set the seed to 88 right before running the clustering algorithm, and set the argument iter.max to 1000. 
set.seed(88)
kmc = kmeans(airlinesNorm, centers = 5, iter.max = 1000)

table(kmc$cluster)


# Now, compare the cluster centroids to each other either by dividing the data points into groups and then using tapply, or by looking at the output of kmeansClust$centers, where "kmeansClust" is the name of the output of the kmeans function. (Note that the output of kmeansClust$centers will be for the normalized data. If you want to look at the average values for the unnormalized data, you need to use tapply like we did for hierarchical clustering.)
sapply(airlines, function(x){tapply(x, kmc$cluster, mean)})
