#############################
#Data Mining Practical Session
#
#Subject: Recommender Systems and Crime Analysis
#
#Author: Austin Schwinn
#
#Feb 1, 2017
#############################
#############################
#Exercise 1: 
#Recommender Systems 

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
r_b <- binarize(r, minRating = 4)
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

#############################
#Exercise 2: 
#Crime Analysis

#install.packages('rstudioapi')
#install.packages('RgoogleMaps')
#install.packages('sp')
#install.packages('ggplot2')
#install.packages('ggmap')
#install.packages('maps')
library(rstudioapi)
library(RgoogleMaps)
library(sp)
library(ggplot2)
library(ggmap)
library(maps)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load the data
#There are two options to get the data.
#The first is directly through City of Chicago's data portal
#Note the data file is large, >1.5GB so download takes a while
rm(list=ls())
url.data    <- "https://data.cityofchicago.org/api/views/6zsd-86xi/rows.csv?accessType=DOWNLOAD&bom=true&query=select+*"
crime.data  <- read.csv(url.data, na.strings = '')
#Once file is downloaded store as CSV for later
write.csv(crime.data, "crime.data.csv", row.names = FALSE)

#The second option is if you have already stored the data in csv
#to read csv instead of redownloading
crime.data <- read.csv('Data and Dependencies/crime.data.csv')

#Understand the data fields
str(crime.data)
summary(crime.data)

####
#Data Preparation

#Remove duplicate observations
crime.data <- subset(crime.data, !duplicated(crime.data$CASE))
summary(crime.data)

#Remove observations with missing values for geo location
crime.data <- subset(crime.data, !is.na(crime.data$Latitude))
crime.data <- subset(crime.data, !is.na(crime.data$Ward))
summary(crime.data)

#Remove observations with illogical values
crime.data <- crime.data[crime.data$Case.Number != 'CASE#',]
crime.data <- crime.data[crime.data$PRIMARY.DESCRIPTION != "PRIMARY",]
summary(crime.data)

#Observe how data and time stamp are stored
head(crime.data$DATE..OF.OCCURANCE)
tail(crime.data$DATE..OF.OCCURANCE)

#Date is currently stored as facor, need to change data type
crime.data$date <- as.POSIXlt(crime.data$DATE..OF.OCCURRENCE,
                              format="%m/%d/%Y_%I:%M:%S_%p")
head(crime.data$date)
head(crime.data$date)

#Process time further with chron package
crime.data$time <- times(format(crime.data$date,
                                format = '%H:%M:%S'))
head(crime.data$time)

#Create time partitions to bucket occurances by part of the day
#Split on 6 hour intervals
time.tag <- chron(times = c("00:00:00", "06:00:00", "12:00:00",
                            "18:00:00", "23:59:59"))
time.tag

#Bin the occurance times back on time buckets
crime.data$time.tag <- cut(crime.data$time, breaks = time.tag,
                           labels=c("00-06", "06-12", "12-18",
                                    "18-00"), include.lowest = TRUE)
table(crime.data$time.tag)

#The crime distribution is uneven throughout the day, there 
#are more occurances later in the day (This is logical)

#Since we have timestamps stored in seperate var, recode
#date variable to just contain the data without time
crime.data$date <- as.POSIXlt(strptime(crime.data$date,
                                       format = "%Y-%m-%d"))
head(crime.data$date)

#Look to see if occurance frequencies are seasonal by 
#observing the months
crime.data$month <- months(crime.data$date, abbreviate=TRUE)
table(crime.data$month)

#Look to see if occurance frequencies follow certain
#trends throughout the week
crime.data$day = weekdays(crime.data$date, abbreviate=TRUE)
table(crime.data$month)

#Observe first of two fields with description of crime committed
table(crime.data$PRIMARY.DESCRIPTION)

#There are 31 different types of crimes listed in the primary
#description. However, many of these are similar and can be combined
#into groups. This will cut down on the number of fields in the feature
#space, reducing the dimensionality

#coerce to character type
crime.data$crime <- as.character(crime.data$PRIMARY.DESCRIPTION)

#Group crimes
crime.data$crime <- ifelse(crime.data$crime %in% c("CRIM_SEXUAL_ASSULT",
                      "KIDNAPPING", "SEX_OFFENSE","OFFENSE INVOLVING CHILDREN"),
                      'vio_personal',crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("INTIMIDATION","STALKING",
                      "PROSTITUTION", "PUBLIC_INDECENCY","OBSCENITY"),
                      'nv_personal',crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("CRIM_SEXUAL_ASSULT",
                      "KIDNAPPING", "SEX_OFFENSE","OFFENSE INVOLVING CHILDREN",
                      "WEAPONS_VIOLATION","ASSULT", "BATTERY"),'violent',
                      crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("MOTOR_VEHICLE_THEFT",
                      "CRIMINAL_DAMAGE", "DAMAGE","BURGLARY","THEFT","ARSON",
                      "CRIMINAL_TRESPASS", "TRESPASS"),'property', 
                      crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("GAMBLING",
                      "INTERFERENCE_WITH_PUBLIC_OFFICER","LIQUOR_LAW_VIOLATION",
                      "PUBLIC_PEACE_VIOLATION","NON-CRIMINAL_(SUBJECT SPECIFIED)",
                      "NON-CRIMINAL"),'public_code', crime.data$crime)



crime.data$crime <- ifelse(crime.data$crime %in% c("NARCOTICS", 
                      "OTHER_NARCOTIC_VIOLATION"), "drug", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("OTHER_OFFENSE",
                      "OTHER_OFFENSE_"), "other", crime.data$crime)

table(crime.data$crime)

#Futher combine into violent and nonviolent groups
crime.data$type <- ifelse(crime.data$crime %in% c("vio_personal", "violent"), "V",
                          ifelse(crime.data$crime %in% c( "nv_personal","property",
                            "public_code", "drug"),"nv","Other"))
table(crime.data$crime.type)

#Coerce arrest field from yes/no to binary
crime.data$ARREST <- ifelse(as.character(crime.data$ARREST)=="Y",1,0)

####
#Visualization