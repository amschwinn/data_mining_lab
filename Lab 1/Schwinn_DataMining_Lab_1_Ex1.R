
#############################
#Data Mining Practical Session
#Lab 1 Exercise 1
#
#Subject: Data Exploration in R 
#with Iris Dataset. Correlation, 
#Pairwise Visualization, and PCA.
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
#Statistical Exploration
summary(iris)
help(iris)

#look at correlation
cor(irisFeat)

####
#Visualization
#plot the pairwise
pairs(irisFeat)

#pairs with changed colors by species
pairs(irisFeat, pch=21, 
      bg=c("red","green3","blue")[unclass(irisLabel)])

####
#Principle Component Analysis (PCA)
#use prcomp to compute princ comp analysis
iris.pca <- prcomp(irisFeat)

#Which attribute is best represented by first component?
iris.pca
#PC1 and petal.length has 85% correlation

#Is a reduced representation space a good rep for the 4 attrib?
summary(iris.pca)
#Yes, the representation explains the majority of the data present
# in a smaller dimensional space. PC1 represents 92% of the data in 
# a 1 dimensional space, and PC 1 & 2 combinged represent 98% of the
# data variance in a 2D space. That is good representation while
# cutting the dimensionality in half

#Plot pairwise rep of PCA, then just with first 2 components
win.graph(800,600,10)
pairs(iris.pca$x, main="PCA_Plot", font.main=4, pch=19)
plot(iris.pca$x, main="PCA_Plot", font.main=4, pch=19)

#pca with colors
plot(iris.pca$x, main="PCA-Plot", font.main=4,pch=21, 
     bg=c("red","green3","blue")[unclass(irisLabel)])

#Use the princomp function for computing the PCA and biplot
#function to represent in 2D space
iris.pca2 <- princomp(irisFeat)
iris.pca2

win.graph(800,600,10)
biplot(iris.pca2)
summary(iris.pca2)
