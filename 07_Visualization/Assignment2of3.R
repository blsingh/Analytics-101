

###
#       Faceboook data
#   Edges and Verticies:: visualize networks using igraph packages.

edges = read.csv("edges.csv")
users = read.csv("users.csv")

# In our dataset, what is the average number of friends per user? Hint: this question is tricky, and it might help to start by thinking about a small example with two users who are friends.
str(edges)
a = merge(edges, users, by.x=edges$V1,by.y=users$id)
library(dplyr)
edges

##  Problem 2
" Problem 2.1 - Creating a Network
1 point possible (graded)

We will be using the igraph package to visualize networks; install and load this package using the install.packages and library commands.

We can create a new graph object using the graph.data.frame() function. Based on ?graph.data.frame, which of the following commands will create a graph g describing our social network, with the attributes of each user correctly loaded?

Note: A directed graph is one where the edges only go one way -- they point from one vertex to another. The other option is an undirected graph, which means that the relations between the vertices are symmetric. "

install.packages("igraph")
library(igraph)
?graph.data.frame

g = graph.data.frame(edges, FALSE, users)

plot(g, vertex.size=5, vertex.label=NA)
degree(g)

V(g)$size = degree(g)/2+2  # Change the size of vertices    
plot(g, vertex.label=NA)

"Thus far, we have changed the 'size' attributes of our vertices. However, we can also change the colors of vertices to capture additional information about the Facebook users we are depicting.

When changing the size of nodes, we first obtained the vertices of our graph with V(g) and then accessed the the size attribute with V(g)$size. To change the color, we will update the attribute V(g)$color.

To color the vertices based on the gender of the user, we will need access to that variable. When we created our graph g, we provided it with the data frame users, which had variables gender, school, and locale. These are now stored as attributes V(g)$gender, V(g)$school, and V(g)$locale.

We can update the colors by setting the color to black for all vertices, than setting it to red for the vertices with gender A and setting it to gray for the vertices with gender B:"

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label = NA)


#  Now, color the vertices based on the school that each user in our network attended.
V(g)$color[V(g)$school == "A"] = "yellow"
V(g)$color[V(g)$school == "AB"] = "green"
plot(g, vertex.label = NA)

# Now, color the vertices based on the locale of the user.

V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "yellow"

plot(g, vertex.label = NA)


##  How to plot in 3-D
?igraph.plotting
