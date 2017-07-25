#############################
#Data Mining Practical Session
#Lab 3 Exercise 1
#
#Subject: Recommender Systems
#using movie ratings from
#MovieLense
#
#Author: Austin Schwinn
#
#Feb 1, 2017
#############################

#install.packages('rstudioapi')
#install.packages('recommenderlab')
#install.packages('ggplot2')
library(rstudioapi)
library(recommenderlab)
library(ggplot2)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


####
#Problem Understanding
#Practice recommendation process on random dataset

#create small artificial dataset
mat <- matrix(sample(c(as.numeric(0:5), NA), 50,
            replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
            dimnames = list(user=paste("u", 1:5, sep=''),
            item=paste("i", 1:10, sep='')))

#coerce M to realRatingMatrix object which stores the data
#in sparse format
rMat <- as(mat, "realRatingMatrix")
rMat

#Explore rating matrix
as(rMat, "list")
as(rMat, "data.frame")

#Normalize rating matrix entries
rMat.norm <- normalize(rMat)

#Visually inspect small portions of rating matrix
image(rMat, main = "Raw_Ratings")
image(rMat.norm, main = "Normalized_Ratings")

#Which ratings are above a certain threshold
r_b <- binarize(rMat, minRating = 4)
as(r_b, "matrix")

####
#Now on an actual dataset

#We will use MovieLense dataset consisting of 100k ratings 
#from 943 users on 1664 movies collected from 
#sept 1997 - april 1998 through the movie lens website

#Load the data
data("MovieLense")
MovieLense

#Visualize a sample of this
image(MovieLense, main = "Raw_ratings")
summary(getRatings(MovieLense))
#It appears some movies were not rated at all by first few users

#Visualizing ratings
qplot(getRatings(MovieLense), binwidth=1,
      main="Histogram_of_ratings", xlab="Rating")
#The rating plot looks skewed to the right

#Normalization
qplot(getRatings(normalize(MovieLense, method = "Z-score")),
      main="Histogram_of_normalized_raings", xlab="Rating")

summary(getRatings(normalize(MovieLense, method="Z-score")))
#Seems better, normalization will take care of user bias

#How many movies did people rate on average?
qplot(rowCounts(MovieLense), binwidth=10,
      main="Movies_rated_on_average",
      xlab = "#_of_users",
      ylab = "#_of_movies_rated")
#Some people get tired of rating movies at a logarithmic pace.

#What is the mean rating of each movie?
qplot(colMeans(MovieLense), binwidth=.1,
      main = "Mean_rating_of_movies",
      xlab = "Rating",
      ylab = "#_of_movies")
#The big spike on 1 suggest that this could be interpreted as binary.
#In other words, some people don't want to see certain movies at all
#Same on 5 and 3

####
#Deploy recommendation algorithms

#Algorithms we can test
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
#It looks like IBCF, POPULAR, RANDOM, and UBCF could all be
#good options for our needs
#Potentially, we could add more recommender methods (based on
#singular value decomposition or principal component analysis
#decomposition (see Bhatnagar, 2013))

#Train/test split
scheme <- evaluationScheme(MovieLense, method="split", train=.9,
                           k=1, given=10, goodRating=4)
scheme
#For testing, it will take any 10 movie ratings by the user and predict
#n others. Then compare to see if they match

#Iterate through potential algorithms to compare evaluation results
# and find optimal model. 
#Using internal model normalization
algorithms <- list(
  "random_items" = list(name="RANDOM", param=list(normalize="Z-score")),
  "popular_items" = list(name="POPULAR", param=list(normalize="Z-score")),
  "user_based_CF" = list(name="UBCF", param=list(normalize="Z-score",
                                                 method="Cosine",
                                                 nn=50, minRating=3)),
  "item_based_CF" = list(name="IBCF", param=list(normalize="Z-score"))
)

#Run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1,3,5,10,15,20))

#Draw ROC curve
plot(results, annotate=1:4, legend="topleft")
#To see precision/recall
plot(results, "prec/rec", annotate=3)

#UBCF performs the best but it very resource intensive. It saves
#the whole matrix and then generates recommendation at prediction 
#by finding the closest user. For large number of user-items,
#this becomes an issue. IBCF, on the other hand, saves only the
#k closest items in the matrix and doesn't have to save everything.
#It is precalculated and predict simply reads off the closest item.
#Random predictably performs the worst. Popular does a good job 
#but doesn't give very original recommendations.

#Create 4-fold cross validation scheme with the given-3 protocol
scheme2   <- evaluationScheme(MovieLense, method="cross", k=4, given=3,
                              goodRating=5)
scheme2
results2  <- evaluate(scheme2, method="POPULAR", n=c(1,3,5,10,15,20))

#return the confusion matrices for the 4 runs as a list
getConfusionMatrix(results2)[[1]]
