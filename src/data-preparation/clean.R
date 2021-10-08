### DPREP PROJECT - TEAM 3 - InsideAirbnb - CLEANING FILE ###
## Merges, preprocesses and aggregates the data into a dataframe and writes it to aggregated_df.csv
## This file should load the listings data from the data folder and store the output in gen/temp (is done by the makefile I think)

# ANALYSIS FOR AMSTERDAM

## STEP 0: STARTING-UP
# Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

# Loading the data (from the data folder)
setwd("C:/Users/danie/OneDrive/Documents/Repositories/dPREP-project-team-3/data")
listings<-read.csv('Listings.gz') #Amsterdam; if we add multiple datasets for different cities, of course change the csv file names!
View(listings)
summary(listings)
head(listings)

## STEP 1: DATA TRANSFORMATION
# For our analysis of question 1: use space and quality attributes
# General attributes = price, id, host_id
# Space attributes = room_type (entire home/private room/shared room/hotel), minimum_nights, maximum_nights
# Listing quality attributes = bedrooms, beds, amenities, number_of_reviews, reviews_per_month, review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value, accommodates
# Host quality attributes = license, host_listings_count, host_is_superhost, host_acceptance_rate, host_response_time, host_response_rate, instant bookable
# (I didn't use bathrooms as that only returned NA's when loaded)

# Filtering columns 
cols_to_keep <- c('id', 'host_id', 'price', 'room_type','accommodates', 'minimum_nights', 'maximum_nights', 'bedrooms', 'beds', 'amenities','number_of_reviews', 'review_scores_rating','review_scores_accuracy','review_scores_cleanliness','review_scores_checkin','review_scores_communication', 'review_scores_location', 'review_scores_value','reviews_per_month', 'license','host_listings_count','host_is_superhost','host_acceptance_rate','host_response_time','host_response_rate','instant_bookable')
df<-listings[,which(colnames(listings)%in%cols_to_keep)]
head(df) 
ncol(df) #we keep 27 columns
colnames(df)
View(df)
#Renaming columns 
df1 <- df %>%
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
         min_nights = minimum_nights,
         max_nights = maximum_nights,
         host_accept_rate = host_acceptance_rate,
         superhost = host_is_superhost)
colnames(df1)
View(df1)
head(df1)
#Turning instant_bookable and superhost binary variables (instead of t/f), and license too (empty=not licensed)
df1$superhost<-ifelse(df1$superhost=='t',1,0) #1=superhost, 0 = not superhost
df1$instant_bookable<-ifelse(df1$instant_bookable=='t',1,0) #1=yes, 0=no
df1$license<-ifelse(df1$license=='',0,1) #1=licensed, 0=not licensed. 
#Replacing N/A's with NA for host_response_time, host_response_rate, host_accept_rate
df1$host_response_time[df1$host_response_time=='N/A'] <- NA
df1$host_response_rate[df1$host_response_rate=='N/A'] <- NA
df1$host_accept_rate[df1$host_accept_rate=='N/A'] <- NA

#Splitting the amenities column for each amenity using pivot_wider() 
head(df1$amenities)
#other additions to attribute categories we now identify = 
#Safety attributes = fire extinguisher, smoke alarm, security cameras on property, first aid kit, carbon monoxide alarm, smart lock, private entrance
#Extra comfort attributes = wifi, hair dryer, cofee maker, heating, TV, breakfast, refrigerator, hot water, dryer, iron, bed linens, oven, stove, kitchen
#Space attributes = patio or balcony, waterfront, lake access, free street parking
#Host attributes = host greets you
#first, clean up the amenities column to prevent errors in splitting columns
df1$amenities<-gsub('\\[', '',df1$amenities)
df1$amenities<-gsub('\\]', '',df1$amenities)
df1$amenities<-gsub('\\"', '',df1$amenities)
df1$amenities<-gsub('\\"', '',df1$amenities)
df1$amenities<-gsub(',', ';',df1$amenities)
df1$amenities<-as.character(df1$amenities)
df1<-df1%>%filter(df1$amenities!='') #filtered out listings where amenities ='' (which solved the problem)
#NOW FIXED-> ERROR: this last line with pivot_wider returns error: column 211 must be named (tibble column names can't be empty)
head(df1$amenities)
#then, split the columns
df1_split <- df1 %>%
  separate_rows(amenities, sep = ";") %>%
  replace_na(list(amenities = "no_amenities")) %>%
  mutate(amenities_logical = TRUE) %>%
  pivot_wider(names_from = amenities,
              names_sep =';',
              names_repair = 'check_unique',
              values_from = amenities_logical,
              values_fill = list(amenities_logical = FALSE))
?pivot_wider()
nrow(df1_split)
View(df1_split)

#trying to solve pivot_wider issue here (hasn't worked yet)
df1_split[,grepl('stove',names(df1_split))]
library(writexl)
write_xlsx(df1_split, "C:\\Users\\danie\\OneDrive\\Documents\\Repositories\\dPREP-project-team-3\\gen\\data-preparation\\df1_split.xlsx")
l <- sapply(df1_split, is.logical)
try<-cbind(df1_split[!l], lapply(split(as.list(df1_split[l]), names(df1_split)[l]), Reduce, f = `|`))
View(try1)

#now we only keep some of the extra columns (here I just used some I thought were interesting, but look if you find extra ones or like to remove somee! :) 
#also observe whether the amenities apply to many listings or not, because the data needs to be representative! (in the end you see for example that host_greetings appears only for 1 listing, so we shouldnt include that amenity I think)
cols_to_keep1<-c('id','host_id','host_response_time','host_response_rate','host_accept_rate','superhost','n_host_listings','room_type','accommodates','bedrooms','beds','price','min_nights','max_nights','n_reviews','rev_rating','rev_accuracy','rev_clean','rev_checkin','rev_comm','rev_location','rev_value','license','instant_bookable','n_reviews_month', 'Carbon monoxide alarm','Fire extinguisher','Smoke alarm','Security cameras on property','First aid kit','Smart lock','Private entrance','Wifi','Hair dryer','Dryer','Coffee maker','Heating','TV','Breakfast','Refrigerator','Hot water','Iron','Bed linens','Oven','Stove','Kitchen','Kitchen','Patio or balcony','Waterfront','Lake access','Free street parking','Host greets you')
df2_split<-df1_split[,which(colnames(df1_split)%in%cols_to_keep1)]
View(df2_split) #not sure if really all variables are included as some may be named slightly different such that I missed them
head(df2_split)
summary(df2_split)
#first we rename these new columns (as we don't want to use spaces or capitals)
df3_split <- df2_split %>%
  rename(carbon_monoxide_alarm = 'Carbon monoxide alarm', 
         stove = 'Stove',
         hot_water = 'Hot water',
         security_cameras = 'Security cameras on property',
         wifi = 'Wifi',
         heating = 'Heating',
         fire_extinguisher = 'Fire extinguisher',
         free_parking = 'Free street parking',
         coffee_maker = 'Coffee maker',
         kitchen = 'Kitchen',
         smoke_alarm =  'Smoke alarm',
         dryer = 'Dryer',
         private_entrance = 'Private entrance',
         hair_dryer = 'Hair dryer',
         waterfront = 'Waterfront',
         host_greeting = 'Host greets you'
         )

View(df3_split)
#then we want to convert these amenities columns from TRUE/FALSE into binary variables (like we did to the other variables before)
df3_split$carbon_monoxide_alarm<-ifelse(df3_split$carbon_monoxide_alarm=='TRUE',1,0) 
df3_split$stove<-ifelse(df3_split$stove=='TRUE',1,0) 
df3_split$hot_water<-ifelse(df3_split$hot_water=='TRUE',1,0) 
df3_split$security_cameras<-ifelse(df3_split$security_cameras=='TRUE',1,0) 
df3_split$wifi<-ifelse(df3_split$wifi=='TRUE',1,0) 
df3_split$heating<-ifelse(df3_split$heating=='TRUE',1,0) 
df3_split$fire_extinguisher<-ifelse(df3_split$fire_extinguisher=='TRUE',1,0) 
df3_split$free_parking<-ifelse(df3_split$free_parking=='TRUE',1,0) 
df3_split$coffee_maker<-ifelse(df3_split$coffee_maker=='TRUE',1,0) 
df3_split$kitchen<-ifelse(df3_split$kitchen=='TRUE',1,0)
df3_split$smoke_alarm<-ifelse(df3_split$smoke_alarm=='TRUE',1,0) 
df3_split$dryer<-ifelse(df3_split$dryer=='TRUE',1,0) 
df3_split$private_entrance<-ifelse(df3_split$private_entrance=='TRUE',1,0) 
df3_split$hair_dryer<-ifelse(df3_split$hair_dryer=='TRUE',1,0) 
df3_split$waterfront<-ifelse(df3_split$waterfront=='TRUE',1,0) 
df3_split$TV<-ifelse(df3_split$TV=='TRUE',1,0) 
df3_split$host_greeting<-ifelse(df3_split$host_greeting=='TRUE',1,0) 

## STEP 2: CLEANING DATA
# Checking datatypes columns and correcting some
lapply(df3_split, class)
# Make price numeric
df3_split$price<-as.numeric(gsub('\\$','',df3_split$price)) #remove the dollar sign such that price doesn't introduce ERRORS anymore
class(df3_split$price)
# Convert some character variables into factors, e.g. host_response_time, room_type
df3_split$host_response_time<-as.factor(df3_split$host_response_time)
class(df3_split$host_response_time)
df3_split$room_type<-as.factor(df3_split$room_type)
class(df3_split$room_type)
# Convert some character variables into numerics, e.g. host_response_rate, host_accept_rate (Should we do this?? )
# Convert some numeric variables into factors (binary variables should be factors, e.g. all the amenities columns)
df3_split$license<-as.factor(df3_split$license)
df3_split$superhost<-as.factor(df3_split$superhost)
df3_split$instant_bookable<-as.factor(df3_split$instant_bookable)
df3_split$carbon_monoxide_alarm<-as.factor(df3_split$carbon_monoxide_alarm)
df3_split$stove<-as.factor(df3_split$stove)
df3_split$hot_water<-as.factor(df3_split$hot_water)
df3_split$security_cameras<-as.factor(df3_split$security_cameras)
df3_split$wifi<-as.factor(df3_split$wifi)
df3_split$heating<-as.factor(df3_split$heating)
df3_split$fire_extinguisher<-as.factor(df3_split$fire_extinguisher)
df3_split$free_parking<-as.factor(df3_split$free_parking)
df3_split$coffee_maker<-as.factor(df3_split$coffee_maker)
df3_split$kitchen<-as.factor(df3_split$kitchen)
df3_split$smoke_alarm<-as.factor(df3_split$smoke_alarm)
df3_split$dryer<-as.factor(df3_split$dryer)
df3_split$private_entrance<-as.factor(df3_split$private_entrance)
df3_split$hair_dryer<-as.factor(df3_split$hair_dryer)
df3_split$TV<-as.factor(df3_split$TV)
df3_split$waterfront<-as.factor(df3_split$waterfront)
df3_split$host_greeting<-as.factor(df3_split$host_greeting)

sapply(df3_split, class)

#Filter for missings/0 values 
df4_split<-df3_split%>%filter(price != '0') #removing listings with price=$0.00
df4_splt<-df4_split%>%filter(n_reviews !=0) #exclude listings with no reviews to provide more accurate estimates (listings with at least one review are said to be already closer to the equilibrium price, which may be important here!)
View(df4_split)
#df4_split<-df4_split%>%filter(rev_rating != 0.00, rev_clean !=0.00, rev_accuracy !=0.00, rev_comm !=0.00, rev_location !=0.00,rev_value !=0.00) #when rev_rating = 0.00, all other ratings for all other categories were NA so this data isnt useable -> now the review columns don't contain NA values anymore either. 

# Checking range constraints: do star ratings really fall between 1-5? 
#for rev_rating (passed test)
breaks<-unique(c(min(df4_split$rev_rating),1,5,max(df4_split$rev_rating))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(df4_split,aes(rev_rating))+geom_histogram(breaks=breaks)
#for rev_accuracy (passed test)
breaks<-unique(c(min(df4_split$rev_accuracy),1,5,max(df4_split$rev_accuracy))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(df4_split,aes(rev_accuracy))+geom_histogram(breaks=breaks)
#for rev_clean (passed test)
breaks<-unique(c(min(df4_split$rev_clean),1,5,max(df4_split$rev_clean))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(df4_split,aes(rev_clean))+geom_histogram(breaks=breaks)
#for rev_checkin (passed test)
breaks<-unique(c(min(df4_split$rev_checkin),1,5,max(df4_split$rev_checkin))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(df4_split,aes(rev_checkin))+geom_histogram(breaks=breaks)
#for rev_comm (passed test)
breaks<-unique(c(min(df4_split$rev_comm),1,5,max(df4_split$rev_comm))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(df4_split,aes(rev_comm))+geom_histogram(breaks=breaks)
#for rev_location (passed test)
breaks<-unique(c(min(df4_split$rev_location),1,5,max(df4_split$rev_location))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(df4_split,aes(rev_location))+geom_histogram(breaks=breaks)
#for rev_value (passed test)
breaks<-unique(c(min(df4_split$rev_value),1,5,max(df4_split$rev_value))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(df4_split,aes(rev_value))+geom_histogram(breaks=breaks)


# Checking uniqueness constraints
#Checking for full duplicates
duplicated(df4_split)
sum(duplicated(df4_split)) #0 full duplicates (passed test)
#Checking for partial duplicates
df4_split%>%count(id)%>%filter(n>1) #0 partial duplicates (passed test)
df4_split%>%count(host_id,price)%>%filter(n>1) #some host-id and price combinations are the same, but no id is the same (so these must be similar but different listings we suppose)

# Cleaning text data: text data already clean!


## STEP 3: DATA WRANGLING 
# Arranging dataset based on price
df5_split<-df4_split%>%arrange(price) 
# Creating new column for average star rating based on the 7 categories
df5_split<-df5_split%>%mutate(mean_review = ((rev_rating+rev_accuracy+rev_clean+rev_checkin+rev_comm+rev_location+rev_value)/7))
#STATUS: now this variable mean_review contains NA values as then one of the categories = NA so there is no mean.. (fix this?)
View(df5_split)
# (here were some simple plots of the data, maybe we need to add those to the analysis file? )


## STEP 4: DATA EXPLORATION
summary(df5_split)
#SOME SUMMARY STATISTICS
#Average overall rating per price class 
overallrating_price<-df5_split%>%group_by(price)%>%summarize(mean_rating=mean(rev_rating)) 
#Average review scores per price class
df5_split%>%group_by(price)%>%summarize(mean_accuracy=mean(rev_accuracy),
                                       mean_comm=mean(rev_comm),
                                       mean_clean=mean(rev_clean),
                                       mean_location=mean(rev_location),
                                       mean_checkin=mean(rev_checkin),
                                       mean_value=mean(rev_value))
#...

## STEP 5: EXPORTING CLEANED DATASET FOR ANALYSIS
#For now, export what we got although the amenity columns aren't correct yet
write.csv(df5_split, "C://Users//danie//OneDrive//Documents//Repositories//dPREP-project-team-3//gen//data-preparation//aggregated_df.csv" ,row.names=FALSE)










