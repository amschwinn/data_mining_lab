#############################
#Data Mining Practical Session
#Lab 5 Exercise 2
#
#Subject: Databases in R:
# SQL, MySQL, and
# NoSQL
#
#Author: Austin Schwinn
#
#Feb 15, 2017
#############################


#install.packages('rstudioapi')
#install.packages('sqldf')
#install.packages('hflights')
#install.packages('RMySQL')
library(rstudioapi)
library(sqldf)
library(hflights)
library(RMySQL)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#SQLDF allows us to use SQL on dataframes
#Irisi example
iris_ex <- read.csv.sql("Data and Dependencies/iris.csv",
              sql="SELECT * FROM file WHERE Species = 'setosa'")

#Example using hflights package, commercial domestic flights
# that depart from Houston
#Export data to CSV
data("hflights")
write.csv(hflights, "Data and Dependencies/hflights.csv", 
          row.names=FALSE)

#Query csv
#Destination selection: NYC JFK
#Month selection: December
DecJFKflgihts <- read.csv.sql("Data and Dependencies/hflights.csv",
                      sql="SELECT * FROM file where Dest='\"JFK\"' and Month=12")

####
# Import MySql File

#Connect database
conn <- dbConnect(MySQL(), dbname='cities',username="root",password="")

#Verify table loaded correctly
dbListTables(conn)

