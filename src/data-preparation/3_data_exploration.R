##################################
###### 3. DATA EXPLORATION  ######
##################################

# Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Create directories
dir.create('../../gen/data-preparation/data')

df_ams5 <- read.csv('../../gen/data-preparation/temp/ams_amenities_dv.csv', as.is = TRUE) 
View(df_ams5)
df_ams5 <- df_ams5[-1] #remove first index column

## Data wrangling
#Arranging dataset based on price
df_ams6<-df_ams5%>%arrange(price) 
#Creating new column for average star rating based on the 7 categories
ams_complete<-df_ams6%>%mutate(mean_review = ((rev_accuracy+rev_clean+rev_checkin+rev_comm+rev_location+rev_value)/6))

## Data exploration
summary(df_ams6)


#Summary statistics
#Average overall rating per price class 
overallrating_price<-df_ams6%>%group_by(price)%>%summarize(mean_rating=mean(rev_rating)) 
#Average review scores per price class
df_ams6%>%group_by(price)%>%summarize(mean_accuracy=mean(rev_accuracy),
                                  mean_comm=mean(rev_comm),
                                  mean_clean=mean(rev_clean),
                                  mean_location=mean(rev_location),
                                  mean_checkin=mean(rev_checkin),
                                  mean_value=mean(rev_value))


#export cleaned dataset as csv file
write.csv(ams_complete, "../../gen/data-preparation/data/ams_complete.csv")

