
#############################
#Data Mining Practical Session
#Lab 2 Exercise 2
#
#Subject: Association rule 
#visualization on Last.fm dataset
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
library(Rgraphviz)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load rules created in exercise 1
load(file="Data and Dependencies/lastRules.RData")

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
