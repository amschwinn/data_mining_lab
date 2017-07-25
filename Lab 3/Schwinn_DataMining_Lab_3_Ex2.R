#############################
#Data Mining Practical Session
#Lab 3 Exercise 2
#
#Subject: Crime Analysis
#from City of Chicago's 
#Data portal
#
#Author: Austin Schwinn
#
#Feb 1, 2017
#############################

#install.packages('rstudioapi')
#install.packages('RgoogleMaps')
#install.packages('sp')
#install.packages('ggplot2')
#install.packages('ggmap')
#install.packages('maps')
#install.packages('chron')
#install.packages('doBy')
library(rstudioapi)
library(RgoogleMaps)
library(sp)
library(ggplot2)
library(ggmap)
library(maps)
library(chron)
library(doBy)

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
write.csv(crime.data, "Data and Dependencies/crime.data.csv", row.names = FALSE)

#The second option is if you have already stored the data in csv
#to read csv instead of redownloading
crime.data <- read.csv('Data and Dependencies/crime.data.csv')

#For backup
#crime.backup <- crime.data
#crime.data <- crime.backup

#Understand the data fields
str(crime.data)
summary(crime.data)

####
#Data Preparation

#Remove duplicate observations
crime.data <- subset(crime.data, !duplicated(crime.data$Case.Number))
summary(crime.data)

#Remove observations with missing values for geo location
crime.data <- subset(crime.data, !is.na(crime.data$Latitude))
crime.data <- subset(crime.data, !is.na(crime.data$Ward))
summary(crime.data)

#Remove observations with illogical values
crime.data <- crime.data[crime.data$Case.Number != 'CASE#',]
crime.data <- crime.data[crime.data$Description != "PRIMARY",]
summary(crime.data)

#Observe how data and time stamp are stored
head(crime.data$Date)
tail(crime.data$Date)

#Date is currently stored as facor, need to change data type
crime.data$date_form <- as.POSIXlt(crime.data$Date,
                          format="%m/%d/%Y %I:%M:%S %p")
head(crime.data$date_form)

#Extract time from date with chron
crime.data$time <- times(format(crime.data$date_form,
                    format = '%H:%M:%S'))
head(crime.data$time)

#Create time partitions to bucket occurances by part of the day
#Split on 6 hour intervals
time.tag <- chron(times = c("00:00:00", "03:00:00", "06:00:00", 
                            "09:00:00", "12:00:00", "15:00:00",
                            "18:00:00", "21:00:00", "23:59:59"))
time.tag

#Bin the occurance times back on time buckets
crime.data$time.tag <- cut(crime.data$time, breaks = time.tag,
                           labels=c("00-03","03-06", "06-09", 
                            "09-12","12-15","15-18", "18-21",
                            "21-00"), include.lowest = TRUE)
table(crime.data$time.tag)

#The crime distribution is uneven throughout the day, there 
#are more occurances later in the day (This is logical)

#Since we have timestamps stored in seperate var, recode
#date variable to just contain the data without time
crime.data$date_form <- as.POSIXlt(strptime(crime.data$date_form,
                                       format = "%Y-%m-%d"))
head(crime.data$date_form)

#Look to see if occurance frequencies are seasonal by 
#observing the months
crime.data$month <- months(crime.data$date_form, abbreviate=TRUE)
table(crime.data$month)

#Look to see if occurance frequencies follow certain
#trends throughout the week
crime.data$day = weekdays(crime.data$date_form, abbreviate=TRUE)
table(crime.data$day)

#Observe first of two fields with description of crime committed
table(crime.data$Primary.Type)
table(crime.data$Description)

#There are 31 different types of crimes listed in the primary
#description. However, many of these are similar and can be combined
#into groups. This will cut down on the number of fields in the feature
#space, reducing the dimensionality

#coerce to character type
crime.data$crime <- as.character(crime.data$Primary.Type)

#Group crimes
crime.data$crime <- ifelse(crime.data$crime %in% c("CRIM SEXUAL ASSAULT",
                      "KIDNAPPING", "SEX OFFENSE","OFFENSE INVOLVING CHILDREN",
                      "DOMESTIC VIOLENCE", "HOMICIDE", "HUMAN TRAFFICKING",
                      "WEAPONS VIOLATION","ASSAULT", "BATTERY","ROBBERY"),
                      'violent',crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("INTIMIDATION","STALKING",
                      "PROSTITUTION", "DECEPTIVE PRACTICE"),
                      'nv_personal',crime.data$crime)


crime.data$crime <- ifelse(crime.data$crime %in% c("MOTOR VEHICLE THEFT",
                      "CRIMINAL DAMAGE", "DAMAGE","BURGLARY","THEFT","ARSON",
                      "CRIMINAL TRESPASS", "TRESPASS"),'property', 
                      crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("GAMBLING",
                      "INTERFERENCE WITH PUBLIC OFFICER","LIQUOR LAW VIOLATION",
                      "PUBLIC PEACE VIOLATION","NON-CRIMINAL (SUBJECT SPECIFIED)",
                      "NON - CRIMINAL","NON-CRIMINAL",
                      "CONCEALED CARRY LICENSE VIOLATION","PUBLIC INDECENCY",
                      "OBSCENITY", "RITUALISM"),'public_code', crime.data$crime)



crime.data$crime <- ifelse(crime.data$crime %in% c("NARCOTICS", 
                      "OTHER NARCOTIC VIOLATION"), "drug", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("OTHER_OFFENSE",
                      "OTHER OFFENSE"), "other", crime.data$crime)

table(crime.data$crime)

#Futher combine into violent and nonviolent groups
crime.data$crime.type <- ifelse(crime.data$crime %in% c("violent"), "V",
                          ifelse(crime.data$crime %in% c( "nv_personal",
                          "property", "public_code", "drug"),"nv", "other"))
table(crime.data$crime.type)

#Coerce arrest field from yes/no to binary
crime.data$Arrest <- ifelse(as.character(crime.data$Arrest)=="true",1,0)
table(crime.data$Arrest)

####
#Visualization

#Frequency of crime types
qplot(crime.data$crime, xlab="crimes", main="Crimes in Chicago") + 
  scale_y_continuous("Number of Crimes")
#property and violent crimes are the two highest, however they have the 
#most types of crimes within the type

#Time series
#Time of day distribution
qplot(crime.data$time.tag, xlab="Time of Day", main="Crimes by time of day")+
  scale_y_continuous("Number of Crimes")
#Peak is between 6-9PM and grows throughout the day until then. Sharp drop
#after 3am

#Crimes by day of the week
crime.data$day <- factor(crime.data$day, levels = c("Mon", "Tue", "Wed",
                    "Thu", "Fri", "Sat", "Sun"))
qplot(crime.data$day, xlab="Day of Week", main="Crimes by day of the week")+
  scale_y_continuous("Number of Crimes")
#Pretty evenly distributed except for slight spike on Friday

#By month
crime.data$month <- factor(crime.data$month, levels = c("Jan", "Feb", 
                      "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                      "Oct", "Nov", "Dec"))
qplot(crime.data$month, xlab="Month", main="Crimes by Month") +
  scale_y_continuous("Number of Crimes")
#Looks like a bell shpaed curve with biggest spike during the summer months
#with an additional spike around the holidays/end/beginning of the year

#There are distinct trends for each of the time attributes, but it
#is important to see the interplay between these features.

#Combine and explore
comb <- aggregate(crime.data$crime, by=list(crime.data$crime, 
          crime.data$time.tag), FUN=length)
names(comb) <- c("crime","time.tag", "count")

#Heat map of combined crime groups and time of day buckets
ggplot(comb, aes(x=crime, y=factor(time.tag)))+
  geom_tile(aes(fill=count))+
  scale_x_discrete("Crime", expand=c(0,0))+
  scale_y_discrete("Time of day", expand=c(0,-2))+
  scale_fill_gradient("Number of Crimes", low="white", high="steelblue")+
  theme_bw()+
  ggtitle("Crimes by time of day")+
  theme(panel.grid.major = element_line(color = NA),
        panel.grid.minor = element_line(color = NA))

#Similiar heat map with respect to month and day of the week
crime.data$mo
comb <- summaryBy(Case.Number~ crime + month, data=crime.data, FUN=length)
names(comb)[3] <- 'count'
#Heatmap by month
ggplot(comb, aes(x=crime, y=month, fill=count))+
  geom_tile(aes(fill=count))+
  scale_x_discrete("Crime", expand=c(0,0))+
  scale_y_discrete("Month", expand=c(0,-2))+
  scale_fill_gradient("Names of crimes", low="white", high='steelblue')+
  theme_bw()+
  ggtitle("Crimes by Month")+
  theme(panel.grid.major = element_line(color=NA),
        panel.grid.minor = element_line(color=NA))

####
#Plot on google map of chicago

#Load map of chicago
chicago_map <- get_map(location = "Chicago",maptype = "satellite",
                  zoom = 10)
#Just chicago
win.graph(800,600,10)
plot(chicago_map)

#Plot crimes on map
win.graph(800,600,10)
ggmap(chicago_map, extent="device")+
  geom_point(aes(x=crime.data$Longitude,y=crime.data$Latitude),
    color="red", alpha=.1, size=1, data=crime.data)+
  ggtitle("Crimes by location")

#These are too many points for one plot. Subset and look by type groups
for(i in 1:length(unique(crime.data$crime))){
  #Take subset of crime type
  type_select <- unique(crime.data$crime)[i]
  crime_type  <- crime.data[crime.data$crime==type_select,]
  
  #Plot crime type locations on map
  win.graph(800,600,10)
  print(
    ggmap(chicago_map, extent="device")+
      geom_point(aes(x=crime_type$Longitude,y=crime_type$Latitude),
                 color="red", alpha=.1, size=1, data=crime_type)+
      ggtitle(paste(type_select," crime group geographic location",sep="")))
  
  #Plot heatmap of crime type
  win.graph(800,600,10)
  print(
    ggmap(chicago_map, extent="device")+
      geom_density2d(data=crime_type, aes(x = Longitude,
                                          y = Latitude), size = 0.3)+
      stat_density2d(data=crime_type, aes(x = Longitude,
                                          y = Latitude, fill = ..level.., alpha = ..level..),
                     size = 0.01, geom = "polygon")+
      scale_fill_gradient(low = "green", high = "red")+
      scale_alpha(range=c(0,0.3), guide = FALSE)+
      ggtitle(paste(type_select," crime group heatmap",sep="")))  
}
