
#############################
#Data Mining Practical Session
#
#Subject: Practicing data mining techniques
#   on iris dataset and breast cancer detection.
#   Included in this lab is: clustering, zscore normalization,
#   correlation analysis, PCA, kmeans, hierarchial clustering,
#   knn, decision trees, over/underfitting
#   Also, introduction to Rattle
#
#Author: Austin Schwinn
#
#Jan 17, 2017
#############################
#############################
#Exercise 1: 
#Data Exploration with Iris Dataset

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

#############################
#Exercise 2: 
#Clustering with Iris Dataset

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

#############################
#Exercise 3: 
#Classfication with Iris Dataset

library(rpart)
help(rpart)

#Load in the data
data("iris")
irisFeat  <- iris[1:4]
irisLabel <- as.matrix(iris[5])
#Load in the data
data("iris")
irisFeat  <- iris[1:4]
irisLabel <- as.matrix(iris[5])

####
#Decision Tree
#Split into training/test
set.seed(1991)
split       <- .5
train       <- sort(sample(1:nrow(iris), floor(nrow(iris)*split)))
iris.train  <- iris[train,]
iris.test   <- iris[-train,]

#Decision Tree on the training set
iris.dt <- rpart(Species ~.,
                 data = iris,
                 subset = train,
                 method = "class",
                 parms = list(split="information"),
                 maxsurrogate=0,
                 cp=0,
                 minsplit=5,
                 minbucket=2)
summary(iris.dt)

#Plot the decision tree
win.graph(800,600,10)
plot(iris.dt,
     uniform = TRUE,
     compress = TRUE,
     margin = .2)
text(iris.dt,
     use.n=TRUE,
     all=TRUE,
     fancy = TRUE)

#What are the attributes used for prediction?
#pedal length and width

#Are those results consistent with conclusion with PCA and clustering?
#Yes

#Evaluate decision tree on test set
pred.dt <- predict(iris.dt,
                   newdata = iris.test,
                   type="class")
pred.dt

#Additional test info
#Probability of classes for each observ
pred.prob <- predict(iris.dt,
                newdata = iris.test,
                type="prob")
pred.prob
#Vector of numeric class prediction
pred.vect <- predict(iris.dt,
                     newdata = iris.test,
                     type="vector")
pred.vect
#Complete info matrix
pred.matr <- predict(iris.dt,
                     newdata = iris.test,
                     type="matrix")
pred.matr

#print the cross tabulation contingency table
table(iris.test$Species, pred.dt)

#How many examples are well predicted?
#All but 5 are properly predicted (3 vers & 2 virg)

#What classes are easier to predict?
#Setosa is easier as it was the only one completely
#Correctly classified

####
#kNN

library(class)
data(iris3)
help(iris3)

#Move from 3D Array to Dataframe
iris3 <- data.frame(rbind(iris3[,,1],iris3[,,2],iris3[,,3]))
#Add labels to data
iris3$Species <- factor(c(rep("Setosa",50), rep("Versicolor",50), rep("Virginica",50)))

#Train/test split
split     <- .5
train     <- sort(sample(1:nrow(iris3), floor(nrow(iris3)*split)))
i3.train  <- data.frame(iris3[train,])
i3.test   <- data.frame(iris3[-train,])

#Use train to fit KNN Algorithm to classify and make predictions on test set
pred <- knn(i3.train[,1:4],i3.test[,1:4],i3.train$Species,k=3)

#print the cross tabulation contingency table
table(pred,i3.test$Species)

#How many examples are well predicted? 
#All except 2 vers incorectly classified as virgi

#What class is easier to predict?
#Setosa is the easiest as it is completely classified correctly
#and have no other species incorectly classified as it

#What can we conclude on the iris dataset?
#Length and width are good predictors of the plant species

#############################
#Exercise 4 
#Testing overfitting on regression models

#generate a sample dataset
x <- 1:10
y <- x + c(-.5,.5)
plot(x,y)

#try different regression models
#simple linear regression
model1 <- lm(y~x)
lines(x, predict(model1, data.frame(x)), lty=1, col="blue")
#Polynomial regression of order 3
model2 <- lm(y~poly(x,3))
lines(x, predict(model2, data.frame(x)), lty=1, col="green")
#Polynomial Regression of order 9
model3 <- lm(y~poly(x,9))
lines(x, predict(model3, data.frame(x)), lty=1, col="red")

#############################
#Exercise 5 
#Diagnose Breast Cancer with Decision Trees

library(rpart)

#Load data
bcw <- read.csv("Data and Dependancies/breast_cancer_wisconsin.data", header = FALSE, quote = "")
summary(bcw)

#Print the characteristics of the compact structure
str(bcw[2:10])
#Upon inspection, there are ?'s to signify missing values

#Encode the ?'s with proper NA
bcw.ok <- bcw
#Iterate through rows
for(r in 1:nrow(bcw)){
  #Iterate through columns
  for(c in 1:ncol(bcw)){
    #If value is ?
    if(bcw[r,c]=="?"){
      #Replace with proper NA
      bcw.ok[r,c] <- NA
    }
  }
}
#Reset all cols to numeric
bcw.ok <- data.frame(lapply(bcw.ok, FUN = as.numeric))
#Verify that correct changes were made
str(bcw.ok)

#Although we have other options to manipulate missing data,
#we are going to remove incomplete observations for this 
#case study

#Remove incomplete observations
bcw.ok <- na.exclude(bcw.ok)
str(bcw.ok)
#Went from 699 observations to 683

#Compute correlation between features now that it complete
cor(bcw.ok[2:10])
summary(bcw.ok)
class(bcw.ok)

#Change current label column that is numerical to be categorical
#Iterate through rows
for (i in 1:nrow(bcw.ok)){
  #If label is 2, it is begnin. Otherwise malignant
  if(bcw.ok[i,ncol(bcw.ok)]==2)
    {bcw.ok[i,ncol(bcw.ok)] <- "begnin"}
  else 
    {bcw.ok[i,ncol(bcw.ok)] <- "malignant"}
}

#Add column names
colnames(bcw.ok) <- c("ID","Clump_Thickness","Uniformity_Cell_Size",
                       "Uniformity_Cell_Shape", "Marginal_Adhesion",
                       "Single_Epithelial_Cell_Size", "Bare_Nuclei", 
                       "Bland_Chromatin", "Normal_Nucleoli", "Mitoses",
                       "Class")

#Remove ID col b/c it is irrelevant
bcw.ok <- bcw.ok[,-1]

#Train/Test Split
set.seed(1991)
split     <- .5
train     <- sort(sample(1:nrow(bcw.ok), floor(nrow(bcw.ok)*split)))
bcw.train <- bcw.ok[train,]
bcw.test  <- bcw.ok[-train,]

#Create the decision tree on the test data
bcw.dt     <- rpart(bcw.ok$Class~. , 
                     data = bcw.ok[,-ncol(bcw.ok)],
                     subset = train,
                     method = "class",
                     parms = list(split="information"),
                     maxsurrogate = 0,
                     cp = 0,
                     minsplit = 5,
                     minbucket = 2)

summary(bcw.dt)

#plot the decision tree
win.graph(800,600,10) 
plot(bcw.dt,
     uniform = TRUE,
     compress = TRUE,
     margin = .2)
text(bcw.dt,
     use.n = TRUE,
     all = TRUE,
     fancy = TRUE)

#Now we can  predict onto the test data
pred.dt <- predict(bcw.dt,
                   newdata = bcw.test,
                   type = "class")
pred.dt

#Evaluate the test data predictions
table(bcw.test$Class, pred.dt)

