####################################
###### 1. DATA TRANSFORMATION ######
####################################

# Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Create directories
dir.create('../../gen')
dir.create('../../gen/data-preparation')
dir.create('../../gen/data-preparation/temp')

######### Analysis for Amsterdam ##########
# Merges, pre-processes and aggregates the data into a dataframe and writes it to aggregated_df.csv
list_ams <- read.csv(gzfile('../../data/listings-amsterdam.csv.gz')) 

# For our analysis we use different amenities categories, which are all price determinants:
# 1. space attributes; 2. listing quality attributes; 3. common listing attributes; 4. extra listing attributes; 5. host quality attributes; 6. convenience attributes; 7. safety attributes

#Filtering columns 
cols_to_keep <- c('id', 'host_id', 'price', 'room_type', 'bedrooms', 'beds', 'amenities','number_of_reviews', 'review_scores_rating','review_scores_accuracy','review_scores_cleanliness','review_scores_checkin','review_scores_communication', 'review_scores_location', 'review_scores_value', 'license','host_listings_count','host_is_superhost','host_acceptance_rate','host_response_time','host_response_rate','instant_bookable')
df_ams1<-list_ams[,which(colnames(list_ams)%in%cols_to_keep)]
colnames(df_ams1)

#Renaming columns 
df_ams1 <- df_ams1 %>%
  rename(rev_accuracy = review_scores_accuracy,
         rev_comm = review_scores_communication,
         rev_clean = review_scores_cleanliness,
         rev_location = review_scores_location,
         rev_value = review_scores_value,
         rev_checkin = review_scores_checkin,
         rev_rating = review_scores_rating,
         n_reviews = number_of_reviews,
         n_host_listings = host_listings_count,
         host_accept_rate = host_acceptance_rate,
         superhost = host_is_superhost)

#Creating binary variables
df_ams1$superhost<-ifelse(df_ams1$superhost=='t',1,0)
df_ams1$instant_bookable<-ifelse(df_ams1$instant_bookable=='t',1,0)
df_ams1$license<-ifelse(df_ams1$license=='',0,1) 
#Replacing N/A's with NA 
df_ams1$host_response_time[df_ams1$host_response_time=='N/A'] <- NA
df_ams1$host_response_rate[df_ams1$host_response_rate=='N/A'] <- NA
df_ams1$host_accept_rate[df_ams1$host_accept_rate=='N/A'] <- NA

#Create separate columns for each amenity 
df_ams2<-df_ams1 
#Clean up the amenities columns
df_ams2$amenities<-gsub('\\[', '',df_ams2$amenities) 
df_ams2$amenities<-gsub('\\]', '',df_ams2$amenities)
df_ams2$amenities<-gsub('\\"', '',df_ams2$amenities) 
df_ams2$amenities<-gsub('\\"', '',df_ams2$amenities)
df_ams2$amenities<-gsub(',', ';',df_ams2$amenities) 
df_ams2$amenities<-gsub(' ','', df_ams2$amenities) 
df_ams2<-df_ams2%>%filter(df_ams1$amenities!='') 
df_ams2$amenities<-as.character(df_ams2$amenities)
head(df_ams2$amenities)
df_ams2<-df_ams2%>%mutate(amenities_lower=str_to_lower(amenities)) 
head(df_ams2$amenities_lower)
#remove old amenities column (not needed anymore)
df_ams3<-df_ams2%>%select(-amenities)


#IFELSE/GREPL: method for separating the amenity columns
head(df_ams3$amenities_lower)
colnames(df_ams3)

#1. Space attributes
#room_type
df_ams3$waterfront<-ifelse(grepl('waterfront',df_ams3$amenities_lower),1,0) #waterfront
df_ams3$balcony<-ifelse(grepl('balcony',df_ams3$amenities_lower),1,0) #balcony

#2. Listing quality attributes 
#n_reviews
#mean_review
#bedrooms
#beds 

#3. Common listing attributes 
df_ams3$kitchen <- ifelse(grepl('kitchen', df_ams3$amenities_lower),1,0)
df_ams3$oven<-ifelse(grepl('oven',df_ams3$amenities_lower),1,0)
df_ams3$wifi<-ifelse(grepl('wifi',df_ams3$amenities_lower),1,0)
df_ams3$tv<-ifelse(grepl('tv',df_ams3$amenities_lower),1,0)
df_ams3$coffee_maker<-ifelse(grepl('coffeemaker',df_ams3$amenities_lower),1,0)
df_ams3$washer <- ifelse(grepl('washer', df_ams3$amenities_lower),1,0)
df_ams3$dishwasher <- ifelse(grepl('dishwasher', df_ams3$amenities_lower),1,0)

#4. Extra listing attributes
df_ams3$free_parking<-ifelse(grepl('freeparking',df_ams3$amenities_lower),1,0)
df_ams3$fireplace <- ifelse(grepl('fireplace', df_ams3$amenities_lower),1,0)
df_ams3$hot_tub<-ifelse(grepl('hottub',df_ams3$amenities_lower),1,0)
df_ams3$gym <- ifelse(grepl('gym', df_ams3$amenities_lower),1,0)
df_ams3$airco <- ifelse(grepl('airconditioning', df_ams3$amenities_lower),1,0)

#5. Host quality attributes
df_ams3$greeting_host <- ifelse(grepl('hostgreetsyou', df_ams3$amenities_lower),1,0)
#superhost
#license
#n_host_listings
#instant_bookable

#6. Convenience attributes
df_ams3$crib <- ifelse(grepl('crib', df_ams3$amenities_lower),1,0)
df_ams3$luggage_dropoff <- ifelse(grepl('luggagedropoffallowed', df_ams3$amenities_lower),1,0) 
df_ams3$single_level <- ifelse(grepl('singlelevelhome', df_ams3$amenities_lower),1,0) 
df_ams3$changing_table <- ifelse(grepl('changingtable', df_ams3$amenities_lower),1,0)

#7. Safety attributes
df_ams3$fire_extinguisher<-ifelse(grepl('fireextinguisher',df_ams3$amenities_lower),1,0)
df_ams3$security_cameras<-ifelse(grepl('securitycameras',df_ams3$amenities_lower),1,0)
df_ams3$carbon_monoxide_alarm<-ifelse(grepl('carbonmonoxidealarm',df_ams3$amenities_lower),1,0)
df_ams3$private_entry<-ifelse(grepl('privateentrance',df_ams3$amenities_lower),1,0)


#after generating all necessary amenities columns we can remove the amenities_lower column for the dataset
ams_amenities<-df_ams3%>%select(-amenities_lower)

#save as csv.file
write.csv(ams_amenities, "../../gen/data-preparation/temp/ams_amenities.csv")
