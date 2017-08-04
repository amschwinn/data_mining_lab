#############################
#Data Mining Practical Session
#Lab 5 Exercise 1
#
#Subject: Adjusting backend
#parameters to work more efficienctly
#in R
#
#Author: Austin Schwinn
#
#Feb 15, 2017
#############################


#install.packages('rstudioapi')
#install.packages('compiler')
#install.packages('microbenchmark')
library(rstudioapi)
library(compiler)
library(microbenchmark)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Explore memory limiti
memory.limit()

#Able to chance memory limit
memory.limit(size=18000)

####
#Code Compilation Example

#First write inefficient loop
mean_r = function(x){
  m = 0
  n = length(x)
  for(i in seq_len(n)){
    m = m + x[i]/n
  m
  }
}

#Combile the function, resulting in byte code which
#should run faster
cmp_mean_r = cmpfun(mean_r)

#Generate some data
x = rnorm(1000)

#Compare compiled and non-compiled
microbenchmark(times=10, unit="ms", mean_r(x), cmp_mean_r(x),
               mean(x))

#Results: Compiled runs quicker than inefficient loop but built in
#mean function stills runs the  quickest