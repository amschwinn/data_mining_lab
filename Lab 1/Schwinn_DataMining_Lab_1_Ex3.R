
#############################
#Data Mining Practical Session
#Lab 1 Exercise 3
#
#Subject: Classification wtih 
#Iris and Iris3 Datasets. 
#Decision Tree & K Nearest Neighbor (KNN)
#
#Author: Austin Schwinn
#
#Jan 17, 2017
#############################

#install.packages('rpart')
#install.packages('rstudioapi')
library(rstudioapi)
library(rpart)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
