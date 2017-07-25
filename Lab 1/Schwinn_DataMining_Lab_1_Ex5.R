
#############################
#Data Mining Practical Session
#Lab 1, Exercise 5
#
#Subject: Diagnose Breast Cancer with Decision Trees
#
#Author: Austin Schwinn
#
#Jan 17, 2017
#############################

#install.packages('rstudioapi')
#install.packages('rpart')
library(rstudioapi)
library(rpart)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load data
bcw <- read.csv("Data and Dependencies/breast_cancer_wisconsin.data", header = FALSE, quote = "")
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
win.graph(1600,1200,10) 
plot(bcw.dt,
     uniform = TRUE,
     compress = TRUE,
     margin = .2)
?text(bcw.dt,
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

