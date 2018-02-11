# Unit 6 - Introduction to Clustering

# Video 6

# After following the steps in the video, load the data into R
movies = read.table("movieLens.txt", header=FALSE, sep="|",quote="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)

########
#       Question
"Using the table function in R, please answer the following questions about the dataset 'movies'.

How many movies are classified as comedies?"
table(movies$Comedy)
"How many movies are classified as westerns?"
table(movies$Western)
"How many movies are classified as romance AND drama?"
table(movies$Romance & movies$Drama)



# Video 7

# Compute distances
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward.D") 

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)

#Now let's figure out what the clusters are like.

# Let's use the tapply function to compute the percentage of movies in each genre and cluster

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods


# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# (teCreate a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]

#########
#       Question
"Run the cutree function again to create the cluster groups, but this time pick k = 2 clusters. It turns out that the algorithm groups all of the movies that only belong to one specific genre in one cluster (cluster 2), and puts all of the other movies in the other cluster (cluster 1). What is the genre that all of the movies in cluster 2 belong to?"

Clus_k2 = cutree(clusterMovies, k = 2)

temp <- subset(movies, Clus_k2 == 1)
temp2 <- subset(movies, Clus_k2 == 2)
head(temp)
head(temp2)
# it is Comedy by the looks of it else I am a bit confused.
tapply(movies$Drama, Clus_k2, mean)
