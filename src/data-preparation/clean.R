### DPREP PROJECT - TEAM 3 - InsideAirbnb - DOWNLOAD FILE ###
## Merges, preprocesses and aggregates the data into a dataframe and writes it to aggregated_df.csv
## This file should load the listings data from the data folder and store the output in gen/temp (is done by the makefile I think)

# STEP 0: STARTING-UP
# Loading packages 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
# Loading the data (from the data folder) 
df<-read.csv('Listings.csv') #if we add multiple datasets for different cities, of course change the csv file names! (this is for Amsterdam)
head(df)
View(df)
summary(df) #inspecting the data

# STEP 1: DATA TRANSFORMATION
# Filtering columns 
cols_to_keep <- c('id', 'name', 'host_id', 'neighbourhood_cleansed', 'room_type','accommodates', 'price','review_scores_rating','review_scores_accuracy','review_scores_cleanliness','review_scores_checkin','review_scores_communication', 'review_scores_location', 'review_scores_value','reviews_per_month','number_of_reviews')
price_quality_ratio<-listings[,which(colnames(listings)%in%cols_to_keep)]
head(price_quality_ratio) 
ncol(price_quality_ratio) #we retain only 16 columns
colnames(price_quality_ratio)
# Renaming columns 
price_quality_ratio <- price_quality_ratio %>%
  rename(neigbourhood = neighbourhood_cleansed,
         rev_accuracy = review_scores_accuracy,
         rev_comm = review_scores_communication,
         rev_clean = review_scores_cleanliness,
         rev_location = review_scores_location,
         rev_value = review_scores_value,
         rev_checkin = review_scores_checkin,
         rev_rating = review_scores_rating,
         n_reviews = number_of_reviews,
         n_reviews_month = reviews_per_month,
         list_name = name)

# STEP 2: CLEANING DATA
# Checking datatypes columns and correcting some 
lapply(price_quality_ratio, class)
price_quality_ratio$list_name <- as.character(price_quality_ratio$list_name)
price_quality_ratio$price <- as.numeric(price_quality_ratio$price)
class(price_quality_ratio$price)
# Filter for missings/0 values. 
cleaned_pq_ratio<-price_quality_ratio%>%filter(price != '$0.00') #removing listings with price=$0.00
cleaned_pq_ratio<-cleaned_pq_ratio%>%filter(n_reviews!=0) #removing listings with 0 reviews: maybe 1 review is still not representative of the quality of the listing? (THINK ABOUT THIS!)
cleaned_pq_ratio<-cleaned_pq_ratio%>%filter(rev_rating != 0.00, rev_clean !=0.00, rev_accuracy !=0.00, rev_comm !=0.00, rev_location !=0.00,rev_value !=0.00) #when rev_rating = 0.00, all other ratings for all other categories were NA so this data isnt useable -> now the review columns don't contain NA values anymore either. #all 7 categories of the review must be filled in (the value for one of the categories can't be 0.00) 
# Checking range constraints: do star ratings really fall between 1-5? 
breaks<-unique(c(min(cleaned_pq_ratio$rev_rating),1,5,max(cleaned_pq_ratio$rev_rating))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(cleaned_pq_ratio,aes(rev_rating))+geom_histogram(breaks=breaks) #rev_rating passed the test. 
breaks<-unique(c(min(cleaned_pq_ratio$rev_accuracy),1,5,max(cleaned_pq_ratio$rev_accuracy)))
ggplot(cleaned_pq_ratio,aes(rev_accuracy))+geom_histogram(breaks=breaks) #rev_accuracy passed the test. 
breaks<-unique(c(min(cleaned_pq_ratio$rev_clean),1,5,max(cleaned_pq_ratio$rev_clean)))
ggplot(cleaned_pq_ratio,aes(rev_clean))+geom_histogram(breaks=breaks) #rev_clean passed the test. 
breaks<-unique(c(min(cleaned_pq_ratio$rev_checkin),1,5,max(cleaned_pq_ratio$rev_checkin))) 
ggplot(cleaned_pq_ratio,aes(rev_checkin))+geom_histogram(breaks=breaks) #rev_checkin passed the test. 
breaks<-unique(c(min(cleaned_pq_ratio$rev_comm),1,5,max(cleaned_pq_ratio$rev_comm))) 
ggplot(cleaned_pq_ratio,aes(rev_comm))+geom_histogram(breaks=breaks) #rev_comm passed the test. 
breaks<-unique(c(min(cleaned_pq_ratio$rev_location),1,5,max(cleaned_pq_ratio$rev_location))) 
ggplot(cleaned_pq_ratio,aes(rev_location))+geom_histogram(breaks=breaks) #rev_location passed the test. 
breaks<-unique(c(min(cleaned_pq_ratio$rev_value),1,5,max(cleaned_pq_ratio$rev_value))) 
ggplot(cleaned_pq_ratio,aes(rev_value))+geom_histogram(breaks=breaks) #rev_value passed the test. 
# Checking uniqueness constraints
duplicated(cleaned_pq_ratio)
sum(duplicated(cleaned_pq_ratio)) #0 full duplicates: passed test
cleaned_pq_ratio%>%count(id)%>%filter(n>1) #0 partial duplicates (passed test)
cleaned_pq_ratio%>%count(list_name,host_id,price)%>%filter(n>1) #some list names are the same (for n=22)
#needs to be looked at again, maybe some hosts are selling multiple similar apartments so they name them similarly and the price will be the same, maybe we need to keep these (only partial duplicates) or remove them?)
# Cleaning text data: is already clean! 

# STEP 3: Data wrangling
# Arranging dataset based on price
pq_ratio<-cleaned_pq_ratio%>%arrange(price) 
# Creating new column for average star rating based on the 7 categories
pq_ratio<-pq_ratio%>%mutate(review = ((rev_rating+rev_accuracy+rev_clean+rev_checkin+rev_comm+rev_location+rev_value)/7))
# (here were some simple plots of the data, maybe we need to add those to the analysis file?)

##STEP 4: Data exploration (maybe also add this step to analysis already?)
# Summary statistics
# Average overall rating per price class
overallrating_price<-pq_ratio%>%group_by(price)%>%summarize(mean_rating=mean(rev_rating)) 
# Average review scores per price class
pq_ratio%>%group_by(price)%>%summarize(mean_accuracy=mean(rev_accuracy),
                                       mean_comm=mean(rev_comm),
                                       mean_clean=mean(rev_clean),
                                       mean_location=mean(rev_location),
                                       mean_checkin=mean(rev_checkin),
                                       mean_value=mean(rev_value))








