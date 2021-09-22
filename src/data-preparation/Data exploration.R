###DPREP PROJECT - TEAM 3###
###Data exploration - InsideAirbnb###
###Research question: Which cities in Europe have the best quality-price ratio ('value-for-money') in terms of Airbnb listings?### 

##READ DATA EXPLORATION TUTORIAL IN R##
###Session: 22/09/2021###

#FIRST, TRY OUT ANALYSIS ONLY FOR AMSTERDAM HERE

##1: Loading and inspecting data## 
#Load: Listing data
setwd("C:/Users/danie/OneDrive/Documents/Repositories/dPREP-project-team-3/data")
listings<-read.csv('Listings.gz')
#Inspect
View(listings)
summary(listings)
head(listings)

##2: Data transformation##
#Narrowing down the data to what we like to focus on
#FILTERING COLUMNS
#Keeping a set of columns, dropping the rest
cols_to_keep <- c('id', 'name', 'host_id', 'neighbourhood_cleansed', 'room_type','accommodates', 'price','review_scores_rating','review_scores_accuracy','review_scores_cleanliness','review_scores_checkin','review_scores_communication', 'review_scores_location', 'review_scores_value','reviews_per_month','number_of_reviews')
price_quality_ratio<-listings[,which(colnames(listings)%in%cols_to_keep)]
View(price_quality_ratio)
head(price_quality_ratio) 
ncol(price_quality_ratio) #we retain only 16 columns
colnames(price_quality_ratio)
#Renaming columns (clarity, shorter)
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='neighbourhood_cleansed')]<-'neighbourhood'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='review_scores_accuracy')]<-'rev_accuracy'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='review_scores_communication')]<-'rev_comm'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='review_scores_cleanliness')]<-'rev_clean'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='review_scores_location')]<-'rev_location'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='review_scores_checkin')]<-'rev_checkin'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='review_scores_value')]<-'rev_value'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='review_scores_rating')]<-'rev_rating'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='number_of_reviews')]<-'n_reviews'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='reviews_per_month')]<-'n_reviews_month'
colnames(price_quality_ratio)[which(colnames(price_quality_ratio)=='name')]<-'list_name'
View(price_quality_ratio)

##3: Cleaning the data##
library(tidyverse)
library(dplyr)
#CHECKING DATATYPES OF ALL COLUMNS
class(price_quality_ratio$id) #integer
class(price_quality_ratio$list_name) #factor (changed)
class(price_quality_ratio$host_id) #integer
class(price_quality_ratio$neighbourhood) #factor
class(price_quality_ratio$room_type)#factor
class(price_quality_ratio$accommodates) #integer
class(price_quality_ratio$price) #factor (needs to be changed)
class(price_quality_ratio$n_reviews) #integer
class(price_quality_ratio$rev_rating) #numeric
class(price_quality_ratio$rev_accuracy) #numeric
class(price_quality_ratio$rev_clean) #numeric
class(price_quality_ratio$rev_checkin) #numeric
class(price_quality_ratio$rev_comm) #numeric
class(price_quality_ratio$rev_location) #numeric
class(price_quality_ratio$rev_value) #numeric
class(price_quality_ratio$n_reviews_month) #numeric
#Correcting datatypes
price_quality_ratio<-price_quality_ratio%>%mutate(list_name=as.character(list_name)) #since it's an unique name, not a category
View(price_quality_ratio)
as.numeric(as.character(price_quality_ratio$price)) #code doesn't work properly as it induces all NAs (needs to be looked at)

#FILTER VERB
#Cleaning up the dataset, removing missing/0 values. 
cleaned_pq_ratio<-price_quality_ratio%>%filter(price != '$0.00') #removing listings with price=$0.00
cleaned_pq_ratio<-cleaned_pq_ratio%>%filter(n_reviews!=0) #removing listings with 0 reviews
#But maybe 1 review is still not representative of the quality of the listing? (THINK ABOUT THIS!)
#all 7 categories of the review must be filled in (the value for one of the categories can't be 0.00)
cleaned_pq_ratio<-cleaned_pq_ratio%>%filter(rev_rating != 0.00,rev_accuracy !=0.00, rev_clean !=0.00, rev_checkin !=0.00, rev_comm !=0.00, rev_location !=0.00,rev_value !=0.00) #when rev_rating = 0.00, all other ratings for all other categories were NA so this data isnt useable
#now the review columns don't contain NA values anymore either. 

#RANGE CONSTRAINTS
#Checking whether the star ratings do really fall between 1 and 5
#for rev_rating (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_rating),1,5,max(cleaned_pq_ratio$rev_rating))) #wrapped with unique() to omit the error of 'breaks are not unique'message
ggplot(cleaned_pq_ratio,aes(rev_rating))+geom_histogram(breaks=breaks)
#for rev_accuracy (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_accuracy),1,5,max(cleaned_pq_ratio$rev_accuracy))) #wrapped with unique() to omit the error of 'breaks are not unique'message
ggplot(cleaned_pq_ratio,aes(rev_accuracy))+geom_histogram(breaks=breaks)
#for rev_clean (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_clean),1,5,max(cleaned_pq_ratio$rev_clean))) #wrapped with unique() to omit the error of 'breaks are not unique'message
ggplot(cleaned_pq_ratio,aes(rev_clean))+geom_histogram(breaks=breaks)
#for rev_checkin (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_checkin),1,5,max(cleaned_pq_ratio$rev_checkin))) #wrapped with unique() to omit the error of 'breaks are not unique'message
ggplot(cleaned_pq_ratio,aes(rev_checkin))+geom_histogram(breaks=breaks)
#for rev_comm (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_comm),1,5,max(cleaned_pq_ratio$rev_comm))) #wrapped with unique() to omit the error of 'breaks are not unique'message
ggplot(cleaned_pq_ratio,aes(rev_comm))+geom_histogram(breaks=breaks)
#for rev_location (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_location),1,5,max(cleaned_pq_ratio$rev_location))) #wrapped with unique() to omit the error of 'breaks are not unique'message
ggplot(cleaned_pq_ratio,aes(rev_location))+geom_histogram(breaks=breaks)
#for rev_value (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_value),1,5,max(cleaned_pq_ratio$rev_value))) #wrapped with unique() to omit the error of 'breaks are not unique'message
ggplot(cleaned_pq_ratio,aes(rev_value))+geom_histogram(breaks=breaks)

#UNIQUENESS CONSTRAINTS
#Checking for full duplicates
duplicated(cleaned_pq_ratio)
sum(duplicated(cleaned_pq_ratio)) #0 full duplicates (passed test)
#Checking for partial duplicates
cleaned_pq_ratio%>%count(id)%>%filter(n>1) #0 partial duplicates (passed test)
cleaned_pq_ratio%>%count(list_name,host_id,price)%>%filter(n>1) #some list names are the same (for n=22)
#(needs to be looked at again, maybe some hosts are selling multiple similar apartments so they name them similarly and the price will be the same, maybe we need to keep these (only partial duplicates) or remove them?)

#CLEANING TEXT DATA
#text data is already clean, so no need to change anything

##4: Data wrangling##
#ARRANGE VERB
#Arranging the dataset on the basis of price
pq_ratio<-cleaned_pq_ratio%>%arrange(price) #doesn't work as well as class(price)=factor
View(pq_ratio) 
class(pq_ratio$price)

#MUTATE VERB
#Creating a new column for the average star rating based on the 7 categories
pq_ratio<-pq_ratio%>%mutate(review = ((rev_rating+rev_accuracy+rev_clean+rev_checkin+rev_comm+rev_location+rev_value)/7))

#SUMMARIZE VERB
#Creating a tibble with the average star rating (overall) for each price category
meanreview_byprice<-pq_ratio%>%group_by(price)%>%summarize(meanRating=mean(review))
#Visualizing our summarized data
library(ggplot2)
ggplot(meanreview_byprice,aes(x=price,y=meanRating))+geom_point() #but although price is a factor variable, there are very many categories so the plot doesn't look very clean
#We can also distinguish between different room types and how that differs in rating per price
meanreview_bypriceroom<-pq_ratio%>%group_by(price,room_type)%>%summarize(meanRating=mean(review))
ggplot(meanreview_bypriceroom,aes(x=price,y=meanRating,color=room_type))+geom_point() #same problem with the price-axis still (needs fixing)

#ADDING MORE GENERATED COLUMNS
#add.code# 

##5: Data exploration## 
summary(cleaned_pq_ratio)
#SOME SUMMARY STATISTICS
#Average overall rating per price class 
overallrating_price<-cleaned_pq_ratio%>%group_by(price)%>%summarize(mean_rating=mean(rev_rating)) 
#Average accuracy score per price class
cleaned_pq_ratio%>%group_by(price)%>%summarize(mean_rating=mean(rev_accuracy)) 
#Average communication score per price class
cleaned_pq_ratio%>%group_by(price)%>%summarize(mean_rating=mean(rev_comm)) 
#Average cleanliness score per price class
cleaned_pq_ratio%>%group_by(price)%>%summarize(mean_rating=mean(rev_clean)) 
#Average location score per price class
cleaned_pq_ratio%>%group_by(price)%>%summarize(mean_rating=mean(rev_location)) 
#Average checkin score per price class
cleaned_pq_ratio%>%group_by(price)%>%summarize(mean_rating=mean(rev_checkin)) 
#Average value score per price class
cleaned_pq_ratio%>%group_by(price)%>%summarize(mean_rating=mean(rev_value)) 

#Maybe group price into 3 classes - expensive, average, cheap - and then look at summary statistics? But this should then be comparable across cities in Europe, since not everywhere 'cheap' will contain the same price values approx..

#Visualize using plot()

##6: Adding other datasets## (not needed, only use the listings set for other cities in europe)
#Load: Review data (not sure if needed, only displays specific review comments)
reviews<-read.csv('Reviews.gz')
#Inspect
View(reviews)
summary(reviews)
head(reviews)

#Load: Calendar data 
calendar<-read.csv('Calendar.gz') #not really needed either, all important info is already in the listings set.
#Inspect
View(calendar)
summary(calendar)
head(calendar)



