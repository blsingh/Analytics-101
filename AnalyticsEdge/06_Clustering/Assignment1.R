

setwd("c:/Study/Analy_Edge/06_Clustering/")

kos <- read.csv("dailykos.csv", header = TRUE)


# Let's start by building a hierarchical clustering model. First, read the data set into R. Then, compute the distances (using method="euclidean"), and use hclust to build the model (using method="ward.D"). You should cluster on all of the variables.

distance = dist(kos, method = "euclidean")
kosClust = hclust(distance, method = "ward.D")

dev.new()
plot(kosClust)

kos_grs = cutree(kosClust, k = 7)


# Let's pick 7 clusters. This number is reasonable according to the dendrogram, and also seems reasonable for the application. Use the cutree function to split your data into 7 clusters.

table(kos_grs)

# Instead of looking at the average value in each variable individually, we'll just look at the top 6 words in each cluster. To do this for cluster 1, type the following in your R console (where "HierCluster1" should be replaced with the name of your first cluster subset):

HClus1 = subset(kos, kos_grs ==1)
tail(sort(colMeans(HClus1)))

# Now repeat the command given in the previous problem for each of the other clusters, and answer the following questions.

HClust2 = subset(kos, kos_grs == 2)
tail(sort(colMeans(HClust2)))

########
####    write a little function
kos_Hclust_sort = function(x){
    tail(sort(colMeans(subset(kos, kos_grs == x))))
}


lapply(1:7, kos_Hclust_sort)



##      Prob 2  K-Means Clustering
# Now, run k-means clustering, setting the seed to 1000 right before you run the kmeans function. Again, pick the number of clusters equal to 7. You don't need to add the iters.max argument.

set.seed(1000)

kmc <- kmeans(kos, centers = 7)
# How many observations are in Cluster 3?
table(kmc$cluster)

# Subset your data into the 7 clusters (7 new datasets) by using the "cluster" variable of your kmeans output. 

kmc_subset <- function(x){
    subset(kos, kmc$cluster ==x)
}

clus1 <- kmc_subset(1)
clus2 <- kmc_subset(2)
clus3 <- kmc_subset(3)
clus4 <- kmc_subset(4)
clus5 <- kmc_subset(5)
clus6 <- kmc_subset(6)
clus7 <- kmc_subset(7)


# Now, output the six most frequent words in each cluster, like we did in the previous problem, for each of the k-means clusters.
 tail(sort(colMeans(clus1)))
 tail(sort(colMeans(clus2)))
  tail(sort(colMeans(clus3)))
   tail(sort(colMeans(clus4)))
    tail(sort(colMeans(clus5)))
     tail(sort(colMeans(clus6)))
      tail(sort(colMeans(clus7)))
      
# For the rest of this problem, we'll ask you to compare how observations were assigned to clusters in the two different methods. Use the table function to compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering. 
      table(kmc$cluster)
        table(kos_grs)
    table(kos_grs, kmc$cluster == 2) 
    table(kos_grs, kmc$cluster == 3) 
    table(kos_grs, kmc$cluster == 6) 
    