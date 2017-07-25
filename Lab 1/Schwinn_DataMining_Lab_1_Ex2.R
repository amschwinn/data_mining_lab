#############################
#Data Mining Practical Session
#Lab 1 Exercise 2
#
#Subject: Clustering with Iris Dataset. 
#K-Means & Heirarchial Clustering
#
#Author: Austin Schwinn
#
#Jan 17, 2017
#############################


#install.packages('rstudioapi')
library(rstudioapi)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load in the data
data("iris")
irisFeat  <- iris[1:4]
irisLabel <- as.matrix(iris[5])

####
#Data Preparation
#Normalize data with z-score transformation
normalize <- function(row) { (row - mean(row))/ sd(row)}
iris_zs   <- data.frame(apply(irisFeat,2,normalize))

####
#K-Means
#Unprocessed Cluster
cl <- kmeans(irisFeat, 3)
print(cl)
plot(irisFeat[3:4], col = cl$cluster)

#Normalizaed Cluster
cl_norm <- kmeans(iris_zs, 3)
print(cl_norm)
plot(iris_zs[3:4], col = cl_norm$cluster)

#The results seem to be less stable with normalized data than
#with non-preprocessed data. This is due to additional noise
#between the distance between the first two feature columns

####
#Heirarchical Clustering
#Using euclidean distance and average for linkage criteria
hc <- hclust(dist(irisFeat, method="euclidean"), "ave")
print(hc)
plot(hc)

#Cluster with ward linkage criteria and manhattan distance
hc2 <- hclust(dist(irisFeat, method="manhattan"), "ward")
plot(hc2)

#Manhattan distance with ward linkage performed better for this
#use case. This H.Clus dendrogram has a clear 3 way split that 
#matches the 3 plant species present in this use case
