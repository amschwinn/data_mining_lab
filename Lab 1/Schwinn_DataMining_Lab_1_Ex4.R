
#############################
#Data Mining Practical Session
#Lab 1 Exercise 4
#
#Subject: Testing overfitting on regression models
#
#Author: Austin Schwinn
#
#Jan 17, 2017
#############################

#install.packages('rstudioapi')
library(rstudioapi)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
