# Reference: https://ocw.mit.edu/courses/sloan-school-of-management/15-071-the-analytics-edge-spring-2017/clustering/
# Intrinsic Meausure: RMSE

# The Netflix Prize
# Used collaborative filtering approach to determine A and B have the similar preferences
# Used content filtering to find movie C is classified in the same set of genres as movie D
# where user A watched movie C but not movie D; while user B has not watched C nor D

{
# Amazon Recommendation (hypothetically)

# If Amazon.com constructs a recommendation system for books, 
# and would like to use the same exact algorithm for shoes, what type would it have to be?
# A. Collaborative Filtering
# B. Content Filtering

# If Amazon.com would like to suggest books to users based on the previous books they have 
# purchased, what type of recommendation system would it be?
# A. Collaborative Filtering
# B. Content Filtering

# Answers: A, B

# Collaborative Filtering using Clustering
# Dataset: movielense
# movie: genres = 1:n relationship
# "Unsupervised" learning: to segment the data into similar groups instead of predictions
# clustering can improve the predictive methods, techniques include: Hierarchical and K-means


########## DISTANCE CALCULTIONS ########## 
# To compute the distances between datapoints
# Euclidean Distance:
# dij = sqrt ((Xi1-Xj1)^2 + (Xi2 - Xj2)^2 + ... + (Xin - Xjn)^2)

# Other distances: Manhattan Distances

# To compute the distances between clusters
# Centroid Distance:centroid is the point that has the average of all data points in each component

# example
v1 = c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
v2 = c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)
dist(rbind(v1, v2))
#1.414214

dist(rbind(1, 3))

v1 = c(0,1)
v2 = c(1,1)
dist(rbind(v1, v2))

########## NORMALIZE ########## 
}

movies = read.table("~/Desktop/movieLens.txt", header=FALSE, sep="|",quote="\"")
str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", 
                     "VideoReleaseDate", "IMDB", "Unknown", 
                     "Action", "Adventure", "Animation", 
                     "Childrens", "Comedy", "Crime", 
                     "Documentary", "Drama", "Fantasy", 
                     "FilmNoir", "Horror", "Musical", 
                     "Mystery", "Romance", "SciFi", 
                     "Thriller", "War", "Western")
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

table(movies$Comedy)
table(movies$Western)
table(movies$Romance)
table(movies$Drama)

table(movies$Romance&movies$Drama)
table(movies$Romance|movies$Drama)

# To cluster by genre
distances = dist(movies[2:20], method = "euclidean")
clusterMovies = hclust(distances, method = "ward") # use centroids and variances
plot(clusterMovies)

# We can potentially pick 2-3 clusters but too few for recommendations
# so we would better to pick a bigger number, for example 10

clusterGroups = cutree(clusterMovies, k = 10)

# Compute % of movies of their genres fall into each of the clusters
tapply(movies$Action, clusterGroups, mean)
# 1         2         3         4         5         6         7         8         9        10 
# 0.1784512 0.7839196 0.1238532 0.0000000 0.0000000 0.1015625 0.0000000 0.0000000 0.0000000 0.0000000 

# Compute % of movies of their genres fall into each of the clusters
tapply(movies$Drama, clusterGroups, mean)
# 1         2         3         4         5         6         7         8         9        10 
# 0.3063973 0.1105528 0.3807339 1.0000000 0.0000000 0.6640625 0.0000000 0.0000000 1.0000000 0.0000000 


subset(movies, Title=="Men in Black (1997)")
# Title Unknown Action Adventure Animation Childrens Comedy Crime Documentary Drama Fantasy FilmNoir Horror Musical Mystery Romance SciFi
# 257 Men in Black (1997)       0      1         1         0         0      1     0           0     0       0        0      0       0       0       0     1
# Thriller War Western
# 257        0   0       0

clusterGroups[257]
# 257 
# 2 #cluster2 is an Action-Adventure-SciFi genre

cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

colMeans(subset(movies[2:20], clusterGroups == 1))
spl = split(movies[2:20], clusterGroups)
spl[[1]] # the same as subset(movies[2:20], clusterGroups == 1)

lapply(spl, colMeans)

# Experiment with 2 clusters
clusterGroups = cutree(clusterMovies, k = 2)
cluster2 = subset(movies, clusterGroups == 2)
cluster2

# $`1`
# Unknown      Action   Adventure   Animation   Childrens      Comedy       Crime Documentary       Drama     Fantasy    FilmNoir      Horror     Musical     Mystery     Romance       SciFi 
# 0.001545595 0.192426584 0.102782071 0.032457496 0.092735703 0.387944359 0.082689335 0.038639876 0.267387944 0.017001546 0.018547141 0.069551777 0.043276662 0.046367852 0.188562597 0.077279753 
# Thriller         War     Western 
# 0.191653787 0.054868624 0.020865533 
# 
# $`2`
# Unknown      Action   Adventure   Animation   Childrens      Comedy       Crime Documentary       Drama     Fantasy    FilmNoir      Horror     Musical     Mystery     Romance       SciFi 
# 0           0           0           0           0           0           0           0           1           0           0           0           0           0           0           0 
# Thriller         War     Western 
# 0           0           0 

# "If I have 3 million customers on the web, I should have 3 million stores on the web" - Jeff Bezos


