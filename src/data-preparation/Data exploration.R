###DPREP PROJECT - TEAM 3###
###Data exploration - InsideAirbnb###
###Research question: Which cities in Europe have the best quality-price ratio ('value-for-money') in terms of Airbnb listings?### 

##READ DATA EXPLORATION TUTORIAL IN R##
###Session: 16/09/2021###

#FIRST, TRY OUT ANALYSIS ONLY FOR AMSTERDAM HERE

##1: Loading and inspecting data## 
#Load: Listing data
setwd("C:/Users/danie/OneDrive/Documents/dPREP folder new/dPREP-project-team-3/data")
listings<-read.csv('Listings.gz')
#Inspect
View(listings)
summary(listings)
head(listings)

##2: Data cleaning and transformation##
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

#FILTERING ROWS
library(dplyr)
cleaned_pq_ratio<-price_quality_ratio%>%filter(price != '$0.00') #removing listings with price=$0.00
cleaned_pq_ratio<-cleaned_pq_ratio%>%filter(n_reviews!=0) #removing listings with 0 reviews
cleaned_pq_ratio<-cleaned_pq_ratio%>%filter(rev_rating != 0.00) #when rev_rating = 0.00, all other ratings for all other categories were NA so this data isnt useable

#ADDING OUR OWN GENERATED COLUMNS
#add.code# 


##3: Data exploration## 
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



