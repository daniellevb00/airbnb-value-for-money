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
dir.create('../../gen/data-preparation/temp')

######### Analysis for Amsterdam ##########
# Merges, pre-processes and aggregates the data into a dataframe and writes it to aggregated_df.csv
list_ams <- read.csv(gzfile('../../data/listings-amsterdam.csv.gz')) 

# For our analysis we use different amenities categories, which are all price determinants:
# 1. space attributes; 2. listing quality attributes; 3. common listing attributes; 4. extra listing attributes; 5. host quality attributes; 6. convenience attributes; 7. safety attributes

#Filtering columns 
cols_to_keep <- c('id', 'host_id', 'price', 'room_type', 'bedrooms', 'beds', 'amenities','number_of_reviews', 'review_scores_rating','review_scores_accuracy','review_scores_cleanliness','review_scores_checkin','review_scores_communication', 'review_scores_location', 'review_scores_value','reviews_per_month', 'license','host_listings_count','host_is_superhost','host_acceptance_rate','host_response_time','host_response_rate','instant_bookable')
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
         n_reviews_month = reviews_per_month,
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
#1. space attributes
df_ams3$balcony<-ifelse(grepl('balcony',df_ams3$amenities_lower),1,0)
df_ams3$lake_access<-ifelse(grepl('lakeaccess',df_ams3$amenities_lower),1,0)
df_ams3$waterfront<-ifelse(grepl('waterfront',df_ams3$amenities_lower),1,0)
df_ams3$private_entry<-ifelse(grepl('privateentrance',df_ams3$amenities_lower),1,0)
df_ams3$workspace<-ifelse(grepl('workspace',df_ams3$amenities_lower),1,0)
df_ams3$backyard<-ifelse(grepl('backyard',df_ams3$amenities_lower),1,0)
df_ams3$beachfront<-ifelse(grepl('beachfront',df_ams3$amenities_lower),1,0)

#2. listing attributes (already separated in existing dataset)

#3. common listing attributes 
df_ams3$kitchen <- ifelse(grepl('kitchen', df_ams3$amenities_lower),1,0)
df_ams3$oven<-ifelse(grepl('oven',df_ams3$amenities_lower),1,0)
df_ams3$stove<-ifelse(grepl('stove',df_ams3$amenities_lower),1,0)
df_ams3$wifi<-ifelse(grepl('wifi',df_ams3$amenities_lower),1,0)
df_ams3$fridge<-ifelse(grepl('refrigerator',df_ams3$amenities_lower),1,0)
df_ams3$iron<-ifelse(grepl('iron',df_ams3$amenities_lower),1,0)
df_ams3$bed_linens<-ifelse(grepl('bedlinens',df_ams3$amenities_lower),1,0)
df_ams3$tv<-ifelse(grepl('tv',df_ams3$amenities_lower),1,0)
df_ams3$dryer<-ifelse(grepl('dryer',df_ams3$amenities_lower),1,0)
df_ams3$coffee_maker<-ifelse(grepl('coffeemaker',df_ams3$amenities_lower),1,0)
df_ams3$washer <- ifelse(grepl('washer', df_ams3$amenities_lower),1,0)
df_ams3$microwave <- ifelse(grepl('microwave', df_ams3$amenities_lower),1,0)
df_ams3$shampoo <- ifelse(grepl('shampoo', df_ams3$amenities_lower),1,0)
df_ams3$hot_water <- ifelse(grepl('hotwater', df_ams3$amenities_lower),1,0)
df_ams3$hangers <- ifelse(grepl('hangers', df_ams3$amenities_lower),1,0)
df_ams3$dishwasher <- ifelse(grepl('dishwasher', df_ams3$amenities_lower),1,0)
df_ams3$freezer <- ifelse(grepl('freezer', df_ams3$amenities_lower),1,0)

#4. extra listing attributes
df_ams3$heating<-ifelse(grepl('heating',df_ams3$amenities_lower),1,0)
df_ams3$free_parking<-ifelse(grepl('freeparking',df_ams3$amenities_lower),1,0)
df_ams3$breakfast <- ifelse(grepl('breakfast', df_ams3$amenities_lower),1,0)
df_ams3$fireplace <- ifelse(grepl('fireplace', df_ams3$amenities_lower),1,0)
df_ams3$hair_dryer <- ifelse(grepl('hairdryer', df_ams3$amenities_lower),1,0)
df_ams3$pool <- ifelse(grepl('pool', df_ams3$amenities_lower),1,0) #this one is difficult as we don't want to select pooltable or whirlpool refrigerator e.g... (find a smart way to do this)
df_ams3$sauna <- ifelse(grepl('sauna', df_ams3$amenities_lower),1,0)
df_ams3$hot_tub<-ifelse(grepl('hottub',df_ams3$amenities_lower),1,0)
df_ams3$gym <- ifelse(grepl('gym', df_ams3$amenities_lower),1,0)
df_ams3$bbq <- ifelse(grepl('bbq', df_ams3$amenities_lower),1,0)
df_ams3$airco <- ifelse(grepl('airconditioning', df_ams3$amenities_lower),1,0)

#5. host quality attributes
df_ams3$greeting_host<-ifelse(grepl('hostgreetsyou',df_ams3$amenities_lower),1,0)

#6. convenience attributes
df_ams3$crib <- ifelse(grepl('crib', df_ams3$amenities_lower),1,0)
df_ams3$high_chair <- ifelse(grepl('highchair', df_ams3$amenities_lower),1,0)
df_ams3$pets_allow <- ifelse(grepl('petsallowed', df_ams3$amenities_lower),1,0) 
df_ams3$elevator <- ifelse(grepl('elevator', df_ams3$amenities_lower),1,0) 
df_ams3$single_level <- ifelse(grepl('singlelevelhome', df_ams3$amenities_lower),1,0) 
df_ams3$baby_safety_gates <- ifelse(grepl('babysafetygates', df_ams3$amenities_lower),1,0) 
df_ams3$baby_bath <- ifelse(grepl('babybath', df_ams3$amenities_lower),1,0)
df_ams3$changing_table <- ifelse(grepl('changingtable', df_ams3$amenities_lower),1,0)
df_ams3$baby_monitor <- ifelse(grepl('babymonitor', df_ams3$amenities_lower),1,0) 

#7. safety attributes
df_ams3$fire_extinguisher<-ifelse(grepl('fireextinguisher',df_ams3$amenities_lower),1,0)
df_ams3$smoke_alarm<-ifelse(grepl('smokealarm',df_ams3$amenities_lower),1,0)
df_ams3$security_cameras<-ifelse(grepl('securitycameras',df_ams3$amenities_lower),1,0)
df_ams3$carbon_monoxide_alarm<-ifelse(grepl('carbonmonoxidealarm',df_ams3$amenities_lower),1,0)
df_ams3$smart_lock<-ifelse(grepl('smartlock',df_ams3$amenities_lower),1,0)
df_ams3$first_aid <- ifelse(grepl('firstaidkit', df_ams3$amenities_lower),1,0)

#after generating all necessary amenities columns we can remove the amenities_lower column for the dataset
ams_amenities<-df_ams3%>%select(-amenities_lower)

#save as csv.file
write.csv(ams_amenities, "../../gen/data-preparation/temp/ams_amenities.csv")
