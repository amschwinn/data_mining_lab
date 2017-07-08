
#############################
#Data Mining Practical Session
#
#Subject: Practicing association
#   rule mining with Apiori algorithm
#   with last.fm datasetS
#
#Author: Austin Schwinn
#
#Jan 24, 2017
#############################
#############################
#Exercise 1: 
#Association rule mining

#install.packages('rstudioapi')
#install.packages('arules')
#install.packages('arulesViz')
#install.packages('Rgraphviz')
#install.packages('diptest')
library(rstudioapi)
library(arules)
library(arulesViz)
library(Rgraphviz)


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

#############################
#Exercise 2: 
#Association Rule Visualization

#Plot confidence vs support of rules
win.graph(800,600,10)
plot(lastRules, interactive = TRUE)

#With shading
win.graph(800,600,10)
plot(lastRules, shading="order", control=list(main="Two-key_plot"))

#This works better with fewer rules
subRules <- lastRules[quality(lastRules)$confidence > .5]

#2D matrix with shading
win.graph(800,600,10)
plot(subRules,method="matrix", measure="lift")
win.graph(800,600,10)
plot(subRules, method="matrix", measure="lift", control=list(reorder=TRUE))

#3D matrix
win.graph(800,600,10)
plot(subRules, method="matrix3D", measure="lift")
win.graph(800,600,10)
plot(subRules, method="matrix3D", measure="lift", control=list(reorder=TRUE))

#Matrix with 2 measures
win.graph(800,600,10)
plot(subRules, method="matrix3D", measure=c("lift","confidence"))
win.graph(800,600,10)
plot(subRules, method="matrix3D", measure=c("lift","confidence"), 
     control=list(reorder=TRUE))

#Interactive with 2 measures
win.graph(800,600,10)
plot(subRules, method="matrix", measure="lift", interactive = TRUE,
     control=list(reorder=TRUE))

#grouped matrix plot
win.graph(800,600,10)
plot(lastRules,method="grouped")
win.graph(800,600,10)
plot(lastRules,method="grouped", control=list(k=30))

#Grouped interactive
win.graph(800,600,10)
plot(lastRules,method="grouped",interactive = TRUE)

#Graphs only work well with very few rules
subRules2 <- sample(lastRules,10)
win.graph(800,600,10)
plot(subRules2, method="graph")
win.graph(800,600,10)
plot(subRules2, method="graph",
     control=list(type="items"))

#subRules2 interactive
win.graph(800,600,10)
plot(subRules2, method = "graph", interactive = TRUE)

#Parallel coordinates plot
win.graph(800,600,10)
plot(subRules2, method = "paracoord")
win.graph(800,600,10)
plot(subRules2, method = "paracoord", control=list(reorder=TRUE))

#for itemsets using eclat algorithm
itemsets <- eclat(lastfm,parameter = list(support=0.02))
win.graph(800,600,10)
plot(itemsets, method = "paracoord", control=list(alpha=.5, reorder=TRUE))
win.graph(800,600,10)
plot(itemsets, method = "graph")
