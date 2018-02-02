# DATA 630 Assignment 5
# Written by Daanish Ahmed
# Semester Summer 2017
# July 31, 2017
# Professor Edward Herranz

# This R script implements the k-means clustering method on a dataset containing
# leaf species information.  The purpose of this assignment is to build a model
# that splits the instances into clusters according to similar characteristics. 
# Four models will be created, each using a different number of clusters.  The 
# first model uses the formula k = sqrt(n/2) to find k.  The second and third 
# models will use the "elbow method" to determine k.  The final model will set 
# k equal to the number of classes (i.e. number of species, which is 30).



# This section of code covers opening the dataset and initializing the packages 
# that are used in this script.

# Sets the working directory for this assignment.  Please change this directory 
# to whichever directory you are using, and make sure that all files are placed 
# in that location.
setwd("~/Class Documents/2016-17 Summer/DATA 630/R/Assignment 5")

# In order to run the clustering commands, we need to install the "cluster" 
# package:

# If you have not installed this package yet, remove the # symbol below.
# install.packages("cluster")

# Loads the cluster package into the system.
library("cluster")

# Opens the CSV file "leaf.csv".
leaf <- read.csv("leaf.csv", head=TRUE, sep=",")

# Creates a copy of the dataset.
newleaf <- leaf

# End of opening the dataset.



# This section of code covers data preprocessing.  It includes exploration of 
# the original dataset, removing variables, and dealing with missing values.

# Previews the dataset.
View(newleaf)

# Shows the initial structure of the dataset.
str(newleaf)

# Removes the species and specimen number variables from the dataset, since 
# they represent categorical variables.
newleaf$Species <- NULL
newleaf$SpecimenNumber <- NULL

# Verifies that the variables have been removed.
str(newleaf)

# Shows the descriptive statistics of the dataset.
summary(newleaf)

# Sets the variables to the same scale, such that they have the same
# mean and standard deviation.
newleaf[1:14] <- scale(newleaf[1:14])

# Verifies that the variables are set to the same scale (see the mean).
summary(newleaf)

# End of data preprocessing.



# This section of code covers the implementation of the k-means clustering 
# algorithm on the dataset using k = 13 clusters.  This k-value was 
# obtained using the formula k = sqrt(n/2), where n equals the number of 
# instances (340 in this dataset).

# Generates a random seed to allow us to reproduce the results.
set.seed(1234)

# Implements k-means clustering on the dataset using k = 13 clusters.
kc <- kmeans(newleaf, 13)

# Shows the output of the clustered dataset, including the number of 
# instances in each cluster, the average values for each variable in all 
# four clusters, and the sum of squares for each cluster.
kc

# Shows the between clusters sum of squares.
kc$betweenss

# Shows the number of iterations required to cluster the dataset.
kc$iter

# Shows the clustering of instances according to the actual leaf species.
table(leaf$Species, kc$cluster)

# Creates the cluster plot for the dataset.
clusplot(newleaf, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# End of creating the first clustering model.



# This section of code covers the implementation of the "elbow method" to
# find the number of clusters to use.  I will create a clustering model 
# for each possible "elbow" indicated by the method.

# The following code is from "Finding Optimal Number of Clusters"
# Based on Anand (2017)
# https://www.r-bloggers.com/finding-optimal-number-of-clusters/
# Modified for UMUC DATA 630 by Daanish Ahmed

# Generates a random seed to allow us to reproduce the results.
set.seed(1234)

# Computes the within clusters sum of squares with a minimum k of 2 and 
# a maximum k of 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(newleaf, k, nstart=50, 
                                 iter.max = 15)$tot.withinss})

# Plots the graph of the within clusters sum of squares vs. the number 
# of clusters.
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

# We see that possible "elbows" are k = 3 and k = 4.

# End of Anand code.



# First elbow: k = 3

# Generates a random seed to allow us to reproduce the results.
set.seed(1234)

# Implements k-means clustering on the dataset using k = 3 clusters.
kc <- kmeans(newleaf, 3)

# Shows the number of instances in each cluster.
kc$size

# Shows the between clusters sum of squares.
kc$betweenss

# Shows the within clusters sum of squares.
kc$withinss

# Shows the number of iterations required to cluster the dataset.
kc$iter

# Shows the clustering of instances according to the actual leaf species.
table(leaf$Species, kc$cluster)

# Creates the cluster plot for the dataset.
clusplot(newleaf, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# End of creating clustering model for the first elbow.



# Second elbow: k = 4

# Generates a random seed to allow us to reproduce the results.
set.seed(1234)

# Implements k-means clustering on the dataset using k = 4 clusters.
kc <- kmeans(newleaf, 4)

# Shows the number of instances in each cluster.
kc$size

# Shows the between clusters sum of squares.
kc$betweenss

# Shows the within clusters sum of squares.
kc$withinss

# Shows the number of iterations required to cluster the dataset.
kc$iter

# Shows the clustering of instances according to the actual leaf species.
table(leaf$Species, kc$cluster)

# Creates the cluster plot for the dataset.
clusplot(newleaf, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# End of creating clustering model for the second elbow.



# This section of code covers the creation of an additional clustering
# model that uses k = 30, meaning that there is one cluster for every 
# leaf species.

# Generates a random seed to allow us to reproduce the results.
set.seed(1234)

# Implements k-means clustering on the dataset using k = 30 clusters.
kc <- kmeans(newleaf, 30)

# Shows the number of instances in each cluster.
kc$size

# Shows the between clusters sum of squares.
kc$betweenss

# Shows the within clusters sum of squares.
kc$withinss

# Shows the number of iterations required to cluster the dataset.
kc$iter

# Shows the clustering of instances according to the actual leaf species.
table(leaf$Species, kc$cluster)

# Creates the cluster plot for the dataset.
clusplot(newleaf, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# End of creating clustering model for k = 30.

# End of script.

