###DPREP PROJECT - TEAM 3###
###Data exploration - InsideAirbnb###
###Research question: Which cities in Europe have the best quality-price ratio ('value-for-money') in terms of Airbnb listings?### 

#ANALYSIS FOR AMSTERDAM

##0: Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

##1: Loading and inspecting data
#Load: Listing data
setwd("C:/Users/danie/OneDrive/Documents/Repositories/dPREP-project-team-3/data")
listings<-read.csv('Listings.gz')
#Inspect
View(listings)
summary(listings)
head(listings)


##2: Data transformation
#Narrowing down the data to what we like to focus on
#FILTERING COLUMNS
#Keeping a set of columns, dropping the rest
cols_to_keep <- c('id', 'name', 'host_id', 'neighbourhood_cleansed', 'room_type','accommodates', 'price','amenities', 'review_scores_rating','review_scores_accuracy','review_scores_cleanliness','review_scores_checkin','review_scores_communication', 'review_scores_location', 'review_scores_value','reviews_per_month','number_of_reviews')
price_quality_ratio<-listings[,which(colnames(listings)%in%cols_to_keep)]
head(price_quality_ratio) 
ncol(price_quality_ratio) #we retain only 17 columns
colnames(price_quality_ratio)
View(price_quality_ratio)

#Renaming columns (clarity, shorter)
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

#splitting column 'amenities' for each amenity
View(price_quality_ratio)
#first, clean up the amenities column to prevent errors in splitting columns
price_quality_ratio$amenities<-gsub('\\[', '',price_quality_ratio$amenities)
price_quality_ratio$amenities<-gsub('\\]', '',price_quality_ratio$amenities)
price_quality_ratio$amenities<-gsub('\\"', '',price_quality_ratio$amenities)
price_quality_ratio$amenities<-gsub('\\"', '',price_quality_ratio$amenities)
price_quality_ratio$amenities<-gsub(',', ';',price_quality_ratio$amenities)
price_quality_ratio$amenities<-as.character(price_quality_ratio$amenities)
price_quality_ratio<-price_quality_ratio%>%filter(price_quality_ratio$amenities!='') #filtered out listings where amenities ='' (which solved the problem)
#NOW FIXED-> ERROR: this last line with pivot_wider returns error: column 211 must be named (tibble column names can't be empty)
head(price_quality_ratio$amenities)

#then, split the columns
pq_ratio_split <- price_quality_ratio %>%
  separate_rows(amenities, sep = ";") %>%
  replace_na(list(amenities = "no_amenities")) %>%
  mutate(amenities_logical = TRUE) %>%
  pivot_wider(names_from = amenities,
              values_from = amenities_logical,
              values_fill = list(amenities_logical = FALSE),
              names_repair='unique')
nrow(pq_ratio_split)
View(pq_ratio_split)

#then, create a subset of the dataset with only the columns we want to keep; which amenities do we want to research? 
#e.g. this list based on what''s on the Airbnb site: kitchen, heating, air conditioning, washer, dryer, wifi, breakfast, indoor fireplace, iron, hair dryer, dedicated workspace, TV, crib, high chair, smoke alarm, carbon monoxide alarm, ski-in/ski-out, beachfront, waterfront
cols_to_keep1 <- c('id','host_id', 'neighbourhood', 'room_type','accommodates', 'price', 'Kitchen','Heating', 'Air conditioning', 'Washer', 'Dryer', 'Wifi', 'Breakfast', 'Indoor fireplace','Iron','Hair dryer','Dedicated workspace','TV', 'Crib','High chair','Smoke alarm', 'Carbon monoxide alarm', 'Ski-in/Ski-out', 'Beachfront', 'Waterfront', 'rev_rating','rev_accuracy','rev_clean','rev_checkin','rev_comm', 'rev_location', 'rev_value','n_reviews_month','n_reviews')
pq_ratio_split2<-pq_ratio_split[,which(colnames(pq_ratio_split)%in%cols_to_keep1)]
View(pq_ratio_split2)#cols_to_keep1 didn't work for all variables as some may be named slightly different (ADJUST THIS!!)
#or instead make a correlation matrix and look at the correlations between the amenities and the price or quality (reviews), and look which ones are the highest correlated (these may be the most interesting to look at for our analysis!)



##3: Cleaning the data (UPDATE: update dataset names from here on as new code was added before!)
#CHECKING DATATYPES OF ALL COLUMNS
lapply(pq_ratio_split, class)

#Correcting datatypes
price_quality_ratio$list_name <- as.character(price_quality_ratio$list_name)
price_quality_ratio$price<-as.numeric(gsub('\\$','',price_quality_ratio$price)) #remove the dollar sign such that price doesn't introduce ERRORS anymore
class(price_quality_ratio$price)

#FILTER VERB
#Cleaning up the dataset, removing missing/0 values. 
cleaned_pq_ratio<-price_quality_ratio%>%filter(price != '0.00') #removing listings with price=$0.00
cleaned_pq_ratio<-cleaned_pq_ratio%>%filter(n_reviews!=0) #removing listings with 0 reviews
#maybe 1 review is still not representative of the quality of the listing? (THINK ABOUT THIS!)

#all 7 categories of the review must be filled in (the value for one of the categories can't be 0.00)
cleaned_pq_ratio<-cleaned_pq_ratio%>%filter(rev_rating != 0.00, rev_clean !=0.00, rev_accuracy !=0.00, rev_comm !=0.00, rev_location !=0.00,rev_value !=0.00) #when rev_rating = 0.00, all other ratings for all other categories were NA so this data isnt useable -> now the review columns don't contain NA values anymore either. 
View(cleaned_pq_ratio)

#RANGE CONSTRAINTS
#Checking whether the star ratings do really fall between 1 and 5
#for rev_rating (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_rating),1,5,max(cleaned_pq_ratio$rev_rating))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(cleaned_pq_ratio,aes(rev_rating))+geom_histogram(breaks=breaks)
#for rev_accuracy (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_accuracy),1,5,max(cleaned_pq_ratio$rev_accuracy))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(cleaned_pq_ratio,aes(rev_accuracy))+geom_histogram(breaks=breaks)
#for rev_clean (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_clean),1,5,max(cleaned_pq_ratio$rev_clean))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(cleaned_pq_ratio,aes(rev_clean))+geom_histogram(breaks=breaks)
#for rev_checkin (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_checkin),1,5,max(cleaned_pq_ratio$rev_checkin))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(cleaned_pq_ratio,aes(rev_checkin))+geom_histogram(breaks=breaks)
#for rev_comm (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_comm),1,5,max(cleaned_pq_ratio$rev_comm))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(cleaned_pq_ratio,aes(rev_comm))+geom_histogram(breaks=breaks)
#for rev_location (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_location),1,5,max(cleaned_pq_ratio$rev_location))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(cleaned_pq_ratio,aes(rev_location))+geom_histogram(breaks=breaks)
#for rev_value (passed test)
breaks<-unique(c(min(cleaned_pq_ratio$rev_value),1,5,max(cleaned_pq_ratio$rev_value))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(cleaned_pq_ratio,aes(rev_value))+geom_histogram(breaks=breaks)


#UNIQUENESS CONSTRAINTS
#Checking for full duplicates
duplicated(cleaned_pq_ratio)
sum(duplicated(cleaned_pq_ratio)) #0 full duplicates (passed test)
#Checking for partial duplicates
cleaned_pq_ratio%>%count(id)%>%filter(n>1) #0 partial duplicates (passed test)
cleaned_pq_ratio%>%count(list_name,host_id,price)%>%filter(n>1) #some list names are the same (for n=22)
#needs to be looked at again, maybe some hosts are selling multiple similar apartments so they name them similarly and the price will be the same, maybe we need to keep these (only partial duplicates) or remove them?)


#CLEANING TEXT DATA
#text data is already clean, so no need to change anything



##4: Data wrangling##
#ARRANGE VERB
#Arranging the dataset on the basis of price
pq_ratio<-cleaned_pq_ratio%>%arrange(price) 

#MUTATE VERB
#Creating a new column for the average star rating based on the 7 categories
pq_ratio<-pq_ratio%>%mutate(review = ((rev_rating+rev_accuracy+rev_clean+rev_checkin+rev_comm+rev_location+rev_value)/7))

#SUMMARIZE VERB
#Creating a tibble with the average star rating (overall) for each price category
meanreview_byprice<-pq_ratio%>%group_by(price)%>%summarize(meanRating=mean(review))
ggplot(meanreview_byprice,aes(x=price,y=meanRating))+geom_point() #there are very many categories so the plot doesn't look very clean

#plot for meanRating's above 4
meanreviewhigh_byprice<-pq_ratio%>%group_by(price)%>%summarize(meanRating=mean(review))%>%filter(meanRating>4)
ggplot(meanreviewhigh_byprice,aes(x=price,y=meanRating))+geom_point()

#we can also distinguish between different room types and how that differs in rating per price
meanreview_bypriceroom<-pq_ratio%>%group_by(price,room_type)%>%summarize(meanRating=mean(review))
ggplot(meanreview_bypriceroom,aes(x=price,y=meanRating,color=room_type))+geom_point() #same problem with the price-axis still (needs fixing)


#ADDING MORE GENERATED COLUMNS
#add.code# 

##5: Data exploration## 
summary(pq_ratio)
#SOME SUMMARY STATISTICS
#Average overall rating per price class 
overallrating_price<-pq_ratio%>%group_by(price)%>%summarize(mean_rating=mean(rev_rating)) 
#Average review scores per price class
pq_ratio%>%group_by(price)%>%summarize(mean_accuracy=mean(rev_accuracy),
                                               mean_comm=mean(rev_comm),
                                               mean_clean=mean(rev_clean),
                                               mean_location=mean(rev_location),
                                               mean_checkin=mean(rev_checkin),
                                               mean_value=mean(rev_value))
                                               

#Maybe group price into 3 classes - expensive, average, cheap - and then look at summary statistics? But this should then be comparable across cities in Europe, since not everywhere 'cheap' will contain the same price values approx..

#Visualize using plot()




