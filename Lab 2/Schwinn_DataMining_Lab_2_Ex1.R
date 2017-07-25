
#############################
#Data Mining Practical Session
#Lab 2 Exercise 1
#
#Subject: Association rule 
#mining with Apiori and Eclat Algorithms 
#on Last.fm dataset
#
#Author: Austin Schwinn
#
#Jan 24, 2017
#############################
#install.packages('rstudioapi')
#install.packages('arules')
#install.packages('arulesViz')
#install.packages('Rgraphviz')
#install.packages('diptest')
library(rstudioapi)
library(arules)
library(arulesViz)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load dataset
lastfm <- read.csv("Data and Dependencies/lastfm.csv")

#Explore the data a bit
head(lastfm)
length(lastfm$user)
lastfm$user <- factor(lastfm$user)
levels(lastfm$user)
levels(lastfm$artist)
#There are 289,995 records, 15k users, & 1004 artists


#First step is to transform data into incidence matrix
# with each listener as row with binary features if they
# listened to each artist. 

#Split data in vector x into groups defined by vector y
playlist <- split(x=lastfm[,"artist"], f=lastfm$user)
playlist[1:2]

#Remove duplicated of listeners playing an artists 
#multiple times.
playlist <- lapply(playlist,unique)

#View this as a list of transactions wtih arules data class
playlist <- as(playlist,"transactions")

#List the support of bands by computing relative frequency of
# arists listened to by users
artistFrequency <- itemFrequency(playlist)

#Plot item frequencies (onlyy bands with > a given support)
win.graph(800,600,10)
itemFrequencyPlot(playlist,support=.08,cex.names=1.5)

#Build the association rules with Apiori Algo
#Only associate with support > .01 and confidence >.5
lastRules <- apriori(playlist,parameter=list(support=.01,
                                              confidence=.05))
inspect(lastRules)

#Filter by lift >5
inspect(subset(lastRules, subset=lift > 5))

#save to Rdata file for exercise 2
save(lastRules,file="Data and Dependencies/lastRules.RData")

