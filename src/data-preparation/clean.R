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
library(stringr)

# Loading the data (from the data folder)
setwd("C:/Users/danie/OneDrive/Documents/Repositories/dPREP-project-team-3/data")
listings<-read.csv('Listings.gz') #Amsterdam; if we add multiple datasets for different cities, of course change the csv file names!
View(listings)
summary(listings)
head(listings)

## STEP 1: DATA TRANSFORMATION
# For our analysis of question 1, we use different amenities categories, which are all price determinants. 
# #1 SPACE ATTRIBUTES = room_type, dedicated workspace, beachfront, waterfront, backyard, patio/balcony, private entrance, lake access. 
# #2 LISTING QUALITY ATTRIBUTES = number of reviews, mean review rating, (bedrooms, beds??)
# #3 COMMON LISTING ATTRIBUTES = kitchen, washer, dryer, wifi, TV, microwave, shampoo, iron, hot water, bed linens, coffee maker, refrigerator, hangers, stove, dishwasher, oven, freezer
# #4 EXTRA ATTRIBUTES LISTING = indoor fireplace, hair dryer, heating, air conditioning, breakfast, pool (but not pool table!), sauna/hot tub, gym, free parking, BBQ
# #5 HOST QUALITY ATTRIBUTES = superhost, license, host_listings_count, host greets you, host_response_rate, instant bookable
# #6 CONVENIENCE ATTRIBUTES (children, pets, disabilities) = crib, high chair, pets allowed, elevator, single-level home, baby safety gates, baby bath, changing table, baby monitor
# #7 SAFETY ATTRIBUTES = carbon monoxide alarm, smoke alarm, fire extinguisher, first-aid kist, smart lock, security cameras

# Filtering columns 
cols_to_keep <- c('id', 'host_id', 'price', 'room_type', 'bedrooms', 'beds', 'amenities','number_of_reviews', 'review_scores_rating','review_scores_accuracy','review_scores_cleanliness','review_scores_checkin','review_scores_communication', 'review_scores_location', 'review_scores_value','reviews_per_month', 'license','host_listings_count','host_is_superhost','host_acceptance_rate','host_response_time','host_response_rate','instant_bookable')
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

#We now want to create separate columns for each amenity that we want to include in our analysis 
head(df1$amenities)
df2<-df1 #first copy the dataset under another name
#Now first clean up the amenities column to make it easier to extract amenities from 
df2$amenities<-gsub('\\[', '',df2$amenities) #remove the list brackets
df2$amenities<-gsub('\\]', '',df2$amenities)
df2$amenities<-gsub('\\"', '',df2$amenities) #remove the ''
df2$amenities<-gsub('\\"', '',df2$amenities)
df2$amenities<-gsub(',', ';',df2$amenities) #replace , by ; 
df2$amenities<-gsub(' ','', df2$amenities) #remove the whitespace
df2<-df2%>%filter(df1$amenities!='') #filtered out listings where amenities ='' (which resolved the error we previously got)
df2$amenities<-as.character(df2$amenities)
head(df2$amenities)
df2<-df2%>%mutate(amenities_lower=str_to_lower(amenities)) #now change the amenity names to lowercase (place these into a new column)
head(df2$amenities_lower)
#now we don't need the old amenities column anymore, so we can remove it
df3<-df2%>%select(-amenities)
View(df3)

### START METHOD 1: IFELSE/GREPL ###
#Method 1 for amenity columns: construct them using ifelse and grepl combined (such that we dont have to use pivot_wider())
#(We may still need to add/remove some of the amenities based on their results in their regression analysis; based on their significance level and effect on DV=price)
head(df3$amenities_lower)
# #1 = SPACE ATTRIBUTES
#room_type
df3$balcony<-ifelse(grepl('balcony',df3$amenities_lower),1,0)
df3$lake_access<-ifelse(grepl('lakeaccess',df3$amenities_lower),1,0)
df3$waterfront<-ifelse(grepl('waterfront',df3$amenities_lower),1,0)
df3$private_entry<-ifelse(grepl('privateentrance',df3$amenities_lower),1,0)
df3$workspace<-ifelse(grepl('workspace',df3$amenities_lower),1,0)
df3$backyard<-ifelse(grepl('backyard',df3$amenities_lower),1,0)
df3$beachfront<-ifelse(grepl('beachfront',df3$amenities_lower),1,0)

# #2 = LISTING QUALITY ATTRIBUTES 
#number of reviews (n_reviews)
#mean review rating 
#bedrooms
#beds

# #3 = COMMON LISITNG ATTRIBUTES
df3$kitchen <- ifelse(grepl('kitchen', df3$amenities_lower),1,0)
df3$oven<-ifelse(grepl('oven',df3$amenities_lower),1,0)
df3$stove<-ifelse(grepl('stove',df3$amenities_lower),1,0)
df3$wifi<-ifelse(grepl('wifi',df3$amenities_lower),1,0)
df3$fridge<-ifelse(grepl('refrigerator',df3$amenities_lower),1,0)
df3$iron<-ifelse(grepl('iron',df3$amenities_lower),1,0)
df3$bed_linens<-ifelse(grepl('bedlinens',df3$amenities_lower),1,0)
df3$tv<-ifelse(grepl('tv',df3$amenities_lower),1,0)
df3$dryer<-ifelse(grepl('dryer',df3$amenities_lower),1,0)
df3$coffee_maker<-ifelse(grepl('coffeemaker',df3$amenities_lower),1,0)
df3$washer <- ifelse(grepl('washer', df3$amenities_lower),1,0)
df3$microwave <- ifelse(grepl('microwave', df3$amenities_lower),1,0)
df3$shampoo <- ifelse(grepl('shampoo', df3$amenities_lower),1,0)
df3$hot_water <- ifelse(grepl('hotwater', df3$amenities_lower),1,0)
df3$hangers <- ifelse(grepl('hangers', df3$amenities_lower),1,0)
df3$dishwasher <- ifelse(grepl('dishwasher', df3$amenities_lower),1,0)
df3$freezer <- ifelse(grepl('freezer', df3$amenities_lower),1,0)

# #4 = EXTRA ATTRIBUTES LISTINGS
df3$heating<-ifelse(grepl('heating',df3$amenities_lower),1,0)
df3$free_parking<-ifelse(grepl('freeparking',df3$amenities_lower),1,0)
df3$breakfast <- ifelse(grepl('breakfast', df3$amenities_lower),1,0)
df3$fireplace <- ifelse(grepl('fireplace', df3$amenities_lower),1,0)
df3$hair_dryer <- ifelse(grepl('hairdryer', df3$amenities_lower),1,0)
df3$pool <- ifelse(grepl('pool', df3$amenities_lower),1,0) #this one is difficult as we don't want to select pooltable or whirlpool refrigerator e.g... (find a smart way to do this)
df3$sauna <- ifelse(grepl('sauna', df3$amenities_lower),1,0)
df3$hot_tub<-ifelse(grepl('hottub',df3$amenities_lower),1,0)
df3$gym <- ifelse(grepl('gym', df3$amenities_lower),1,0)
df3$bbq <- ifelse(grepl('bbq', df3$amenities_lower),1,0)
df3$airco <- ifelse(grepl('airconditioning', df3$amenities_lower),1,0)

# #5 = HOST QUALITY ATTRIBUTES
#superhost
#license
#host_listings_count
#host_response_rate
#instant_bookable
df3$greeting_host<-ifelse(grepl('hostgreetsyou',df3$amenities_lower),1,0)

# #6 = CONVENIENCE ATTRIBUTES (children, pets, disabilities)
df3$crib <- ifelse(grepl('crib', df3$amenities_lower),1,0)
df3$high_chair <- ifelse(grepl('highchair', df3$amenities_lower),1,0)
df3$pets_allow <- ifelse(grepl('petsallowed', df3$amenities_lower),1,0) #only concerns 1 listing.. 
df3$elevator <- ifelse(grepl('elevator', df3$amenities_lower),1,0) 
df3$single_level <- ifelse(grepl('singlelevelhome', df3$amenities_lower),1,0) 
df3$baby_safety_gates <- ifelse(grepl('babysafetygates', df3$amenities_lower),1,0) 
df3$baby_bath <- ifelse(grepl('babybath', df3$amenities_lower),1,0)
df3$changing_table <- ifelse(grepl('changingtable', df3$amenities_lower),1,0)
df3$baby_monitor <- ifelse(grepl('babymonitor', df3$amenities_lower),1,0) 

# #7 = SAFETY ATTRIBUTES 
df3$fire_extinguisher<-ifelse(grepl('fireextinguisher',df3$amenities_lower),1,0)
df3$smoke_alarm<-ifelse(grepl('smokealarm',df3$amenities_lower),1,0)
df3$security_cameras<-ifelse(grepl('securitycameras',df3$amenities_lower),1,0)
df3$carbon_monoxide_alarm<-ifelse(grepl('carbonmonoxidealarm',df3$amenities_lower),1,0)
df3$smart_lock<-ifelse(grepl('smartlock',df3$amenities_lower),1,0)
df3$first_aid <- ifelse(grepl('firstaidkit', df3$amenities_lower),1,0)
class(df3$smart_lock) #now the columns are seen as numerics, we need to change this into logicals

#after generating all necessary amenities columns we can remove the amenities_lower column for the dataset
df4<-df3%>%select(-amenities_lower)

### END METHOD 1: IFELSE/GREPL  ###

### START METHOD 2: PIVOT_WIDER() ###
#Method 2 for amenity columns: using pivot_wider() --> WARNING: doesn't work correctly yet so don't run (instead the previous method works!!): that's why i put it in comments
#df1_split <- df1 %>%
  separate_rows(amenities, sep = ";") %>%
  replace_na(list(amenities = "no_amenities")) %>%
  mutate(amenities_logical = TRUE) %>%
  pivot_wider(names_from = amenities,
              names_sep =';',
              names_repair = 'check_unique',
              values_from = amenities_logical,
              values_fill = list(amenities_logical = FALSE))
#nrow(df1_split)
#trying to make pivot_wider work for our amenities column (didn't work yet):
df1_split[,grepl('stove',names(df1_split))]
l <- sapply(df1_split, is.logical)
try<-cbind(df1_split[!l], lapply(split(as.list(df1_split[l]), names(df1_split)[l]), Reduce, f = `|`))
View(try1)

#now we only keep some of the extra columns (here I just used some I thought were interesting, but look if you find extra ones or like to remove somee! :) 
#also observe whether the amenities apply to many listings or not, because the data needs to be representative! (in the end you see for example that host_greetings appears only for 1 listing, so we shouldnt include that amenity I think)
cols_to_keep1<-c('id','host_id','host_response_time','host_response_rate','host_accept_rate','superhost','n_host_listings','room_type','accommodates','bedrooms','beds','price','min_nights','max_nights','n_reviews','rev_rating','rev_accuracy','rev_clean','rev_checkin','rev_comm','rev_location','rev_value','license','instant_bookable','n_reviews_month', 'Carbon monoxide alarm','Fire extinguisher','Smoke alarm','Security cameras on property','First aid kit','Smart lock','Private entrance','Wifi','Dryer','Coffee maker','Heating','TV','Refrigerator','Hot water','Iron','Bed linens','Oven','Stove','Kitchen','Kitchen','Patio or balcony','Waterfront','Lake access','Free street parking','Host greets you')
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
df3_split$waterfront<-ifelse(df3_split$waterfront=='TRUE',1,0) 
df3_split$TV<-ifelse(df3_split$TV=='TRUE',1,0) 
df3_split$host_greeting<-ifelse(df3_split$host_greeting=='TRUE',1,0) 

#use this for data cleaning in pivot_wider method: adjusting classes
df4$license<-as.factor(df4$license)
df4$superhost<-as.factor(df4$superhost)
df4$instant_bookable<-as.factor(df4$instant_bookable)
df4$carbon_monoxide_alarm<-as.factor(df4$carbon_monoxide_alarm)
df4$stove<-as.factor(df4$stove)
df4$hot_water<-as.factor(df4$hot_water)
df4$security_cameras<-as.factor(df4$security_cameras)
df4$wifi<-as.factor(df4$wifi)
df4$heating<-as.factor(df4$heating)
df4$fire_extinguisher<-as.factor(df4$fire_extinguisher)
df4$free_parking<-as.factor(df4$free_parking)
df4$coffee_maker<-as.factor(df4$coffee_maker)
df4$kitchen<-as.factor(df4$kitchen)
df4$smoke_alarm<-as.factor(df4$smoke_alarm)
df4$dryer<-as.factor(df4$dryer)
df4$private_entrance<-as.factor(df4$private_entrance)
df4$TV<-as.factor(df4$TV)
df4$waterfront<-as.factor(df4$waterfront)
df4$host_greeting<-as.factor(df4$host_greeting)

### END METHOD 2: PIVOT_WIDER() ###

#(From now on we use the dataset we ended with in method 1: grepl/ifelse, named: df4)
## STEP 2: CLEANING DATA
# Checking datatypes columns and correcting some
View(df4)
lapply(df4, class)
# Make price numeric
df4$price<-as.numeric(gsub('\\$','',df4$price)) #remove the dollar sign such that price doesn't introduce ERRORS anymore
class(df4$price)
# Convert some character variables into factors, e.g. host_response_time, room_type
df4$host_response_time<-as.factor(df4$host_response_time)
class(df4$host_response_time)
df4$room_type<-as.factor(df4$room_type)
class(df4$room_type)
# Convert some character variables into numerics, e.g. host_response_rate, host_accept_rate (Should we do this??)

# Convert some numeric variables into logicals (as they are binary variables)
# as.logical did change the variables from 0,1 to FALSE, TRUE
df4$license<-as.logical(df4$license)
df4$superhost<-as.logical(df4$superhost)
df4$instant_bookable<-as.logical(df4$instant_bookable)
# Also convert the created amenities columns from numerics into logicals (also binary variables)
# as.logical did change the variables from 0,1 to FALSE,TRUE 
# #1: SPACE ATTRIBUTES
df4$balcony<-as.logical(df4$balcony)
df4$lake_access<-as.logical(df4$lake_access)
df4$waterfront<-as.logical(df4$waterfront)
df4$beachfront<-as.logical(df4$beachfront)
df4$private_entry<-as.logical(df4$private_entry)
df4$workspace<-as.logical(df4$workspace)
df4$backyard<-as.logical(df4$backyard)
# #2: LISTING QUALITY ATTRIBUTES
#not needed
# #3: COMMON LISTING ATTRIBUTES
df4$kitchen<-as.logical(df4$kitchen)
df4$oven<-as.logical(df4$oven)
df4$stove<-as.logical(df4$stove)
df4$wifi<-as.logical(df4$wifi)
df4$fridge<-as.logical(df4$fridge)
df4$iron<-as.logical(df4$iron)
df4$bed_linens<-as.logical(df4$bed_linens)
df4$tv<-as.logical(df4$tv)
df4$dryer<-as.logical(df4$dryer)
df4$coffee_maker<-as.logical(df4$coffee_maker)
df4$washer<-as.logical(df4$washer)
df4$microwave<-as.logical(df4$microwave)
df4$hot_water<-as.logical(df4$hot_water)
df4$hangers<-as.logical(df4$hangers)
df4$dishwasher<-as.logical(df4$dishwasher)
df4$freezer<-as.logical(df4$freezer)
# #4 EXTRA ATTRIBUTES LISTINGS
df4$fireplace<-as.logical(df4$fireplace)
df4$heating<-as.logical(df4$heating)
df4$hair_dryer<-as.logical(df4$hair_dryer)
df4$airco<-as.logical(df4$airco)
df4$breakfast<-as.logical(df4$breakfast)
df4$pool<-as.logical(df4$pool)
df4$sauna<-as.logical(df4$sauna)
df4$hot_tub<-as.logical(df4$hot_tub)
df4$gym<-as.logical(df4$gym)
df4$free_parking<-as.logical(df4$free_parking)
df4$bbq<-as.logical(df4$bbq)

# #5 HOST QUALITY ATTRIBUTES
df4$host_greet<-as.logical(df4$host_greet)
# #6 CONVENIENCE ATTRIBUTES 
df4$crib<-as.logical(df4$crib)
df4$high_chair<-as.logical(df4$high_chair)
df4$pets_allowed<-as.logical(df4$pets_allowed)
df4$elevator<-as.logical(df4$elevator)
df4$single_level<-as.logical(df4$single_level)
df4$baby_safety_gates<-as.logical(df4$baby_safety_gates)
df4$baby_bath<-as.logical(df4$baby_bath)
df4$changing_table<-as.logical(df4$changing_table)
df4$baby_monitor<-as.logical(df4$baby_monitor)

# #7 SAFETY ATTRIBUTES
df4$fire_extinguisher<-as.logical(df4$fire_extinguisher)
df4$smoke_alarm<-as.logical(df4$smoke_alarm)
df4$security_cameras<-as.logical(df4$security_cameras)
df4$carbon_monoxide_alarm<-as.logical(df4$carbon_monoxide_alarm)
df4$smart_lock<-as.logical(df4$smart_lock)
df4$first_aid<-as.logical(df4$first_aid)

sapply(df4, class)

#Filter for missings/0 values 
df5<-df4
View(df5)
df5<-df5%>%filter(price != '0') #removing listings with price=$0.00
df5<-df5%>%filter(n_reviews !='0') #exclude listings with no reviews to provide more accurate estimates (listings with at least one review are said to be already closer to the equilibrium price, which may be important here!)
#df5<-df5%>%filter(rev_rating != 0.00, rev_clean !=0.00, rev_accuracy !=0.00, rev_comm !=0.00, rev_location !=0.00,rev_value !=0.00) #when rev_rating = 0.00, all other ratings for all other categories were NA so this data isnt useable -> now the review columns don't contain NA values anymore either. 

# Checking range constraints: do star ratings really fall between 1-5? 
#for rev_rating (passed test)
breaks<-unique(c(min(df5$rev_rating),1,5,max(df5$rev_rating))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(df5,aes(rev_rating))+geom_histogram(breaks=breaks)
#for rev_accuracy (passed test)
breaks<-unique(c(min(df5$rev_accuracy),1,5,max(df5$rev_accuracy))) 
ggplot(df5,aes(rev_accuracy))+geom_histogram(breaks=breaks)
#for rev_clean (passed test)
breaks<-unique(c(min(df5$rev_clean),1,5,max(df5$rev_clean))) 
ggplot(df5,aes(rev_clean))+geom_histogram(breaks=breaks)
#for rev_checkin (passed test)
breaks<-unique(c(min(df5$rev_checkin),1,5,max(df5$rev_checkin))) 
ggplot(df5,aes(rev_checkin))+geom_histogram(breaks=breaks)
#for rev_comm (passed test)
breaks<-unique(c(min(df5$rev_comm),1,5,max(df5$rev_comm))) 
ggplot(df5,aes(rev_comm))+geom_histogram(breaks=breaks)
#for rev_location (passed test)
breaks<-unique(c(min(df5$rev_location),1,5,max(df5$rev_location))) 
ggplot(df5,aes(rev_location))+geom_histogram(breaks=breaks)
#for rev_value (passed test)
breaks<-unique(c(min(df5$rev_value),1,5,max(df5$rev_value))) 
ggplot(df5,aes(rev_value))+geom_histogram(breaks=breaks)


# Checking uniqueness constraints
#Checking for full duplicates
duplicated(df5)
sum(duplicated(df5)) #0 full duplicates (passed test)
#Checking for partial duplicates
df5%>%count(id)%>%filter(n>1) #0 partial duplicates (passed test)
df5%>%count(host_id,price)%>%filter(n>1) #some host-id and price combinations are the same, but no id is the same (so these must be similar but different listings we suppose)

# Cleaning text data: text data already clean!


## STEP 3: DATA WRANGLING 
# Arranging dataset based on price
df6<-df5%>%arrange(price) 
# Creating new column for average star rating based on the 7 categories
df6<-df6%>%mutate(mean_review = ((rev_rating+rev_accuracy+rev_clean+rev_checkin+rev_comm+rev_location+rev_value)/7))
#STATUS: now this variable mean_review contains NA values as then one of the categories = NA so there is no mean.. (fix this?)
View(df6)
# (here were some simple plots of the data, maybe we need to add those to the analysis file? )


## STEP 4: DATA EXPLORATION
summary(df6)
#SOME SUMMARY STATISTICS
#Average overall rating per price class 
overallrating_price<-df6%>%group_by(price)%>%summarize(mean_rating=mean(rev_rating)) 
#Average review scores per price class
df6%>%group_by(price)%>%summarize(mean_accuracy=mean(rev_accuracy),
                                       mean_comm=mean(rev_comm),
                                       mean_clean=mean(rev_clean),
                                       mean_location=mean(rev_location),
                                       mean_checkin=mean(rev_checkin),
                                       mean_value=mean(rev_value))
#...

## STEP 5: EXPORTING CLEANED DATASET FOR ANALYSIS
#For now, export what we got although the amenity columns aren't correct yet
write.csv(df6, "C://Users//danie//OneDrive//Documents//Repositories//dPREP-project-team-3//gen//data-preparation//aggregated_df.csv" ,row.names=FALSE)










