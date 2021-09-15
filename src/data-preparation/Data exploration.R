###DPREP PROJECT - TEAM 3###
###Data exploration - InsideAirbnb###
###Research question: ...### 

##READ DATA EXPLORATION TUTORIAL IN R##
##1: Loading and inspecting data## 
#Load: Listing data
setwd("C:/Users/danie/OneDrive/Documents/dPREP folder new/dPREP-project-team-3/data")
ams_listings<-read.csv('Listings Amsterdam.csv')
#Inspect
View(ams_listings)
summary(ams_listings)
head(ams_listings)

#Load: Reviews data
ams_reviews<-read.csv('Reviews Amsterdam.gz')
#Inspect
View(ams_reviews)
summary(ams_reviews)
head(ams_reviews)

#Load: Calendar data
ams_calendar<-read.csv('Calendar Amsterdam.gz')
#Inspect
View(ams_calendar)
summary(ams_calendar)
head(ams_calendar)

##2: Data cleaning and transformation##
#Drop columns 




##3: Data exploration## 
#Visualize using plot() 


