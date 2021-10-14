###############################
###### 2. CLEANING DATA  ######
###############################

# Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

df_ams4 <- read.csv('../../gen/data-preparation/temp/ams_amenities.csv') 
df_ams4 <- df_ams4[2:74]

#Correcting datatypes
df_ams4$price<-as.numeric(gsub('\\$','',df_ams4$price)) 
df_ams4$host_response_time<-as.factor(df_ams4$host_response_time)
df_ams4$room_type<-as.factor(df_ams4$room_type)
df_ams4$license<-as.logical(df_ams4$license)
df_ams4$superhost<-as.logical(df_ams4$superhost)
df_ams4$instant_bookable<-as.logical(df_ams4$instant_bookable)

#Convert amenities to logical (also binary variables): as.logical: 0,1 to FALSE,TRUE 
#1. space attributes
df_ams4$balcony<-as.logical(df_ams4$balcony)
df_ams4$lake_access<-as.logical(df_ams4$lake_access)
df_ams4$waterfront<-as.logical(df_ams4$waterfront)
df_ams4$beachfront<-as.logical(df_ams4$beachfront)
df_ams4$private_entry<-as.logical(df_ams4$private_entry)
df_ams4$workspace<-as.logical(df_ams4$workspace)
df_ams4$backyard<-as.logical(df_ams4$backyard)

#3. common listing attributes
df_ams4$kitchen<-as.logical(df_ams4$kitchen)
df_ams4$oven<-as.logical(df_ams4$oven)
df_ams4$stove<-as.logical(df_ams4$stove)
df_ams4$wifi<-as.logical(df_ams4$wifi)
df_ams4$fridge<-as.logical(df_ams4$fridge)
df_ams4$iron<-as.logical(df_ams4$iron)
df_ams4$bed_linens<-as.logical(df_ams4$bed_linens)
df_ams4$tv<-as.logical(df_ams4$tv)
df_ams4$dryer<-as.logical(df_ams4$dryer)
df_ams4$coffee_maker<-as.logical(df_ams4$coffee_maker)
df_ams4$washer<-as.logical(df_ams4$washer)
df_ams4$microwave<-as.logical(df_ams4$microwave)
df_ams4$hot_water<-as.logical(df_ams4$hot_water)
df_ams4$hangers<-as.logical(df_ams4$hangers)
df_ams4$dishwasher<-as.logical(df_ams4$dishwasher)
df_ams4$freezer<-as.logical(df_ams4$freezer)
df_ams4$shampoo<-as.logical(df_ams4$shampoo)

#4. extra attributes listings
df_ams4$fireplace<-as.logical(df_ams4$fireplace)
df_ams4$heating<-as.logical(df_ams4$heating)
df_ams4$hair_dryer<-as.logical(df_ams4$hair_dryer)
df_ams4$airco<-as.logical(df_ams4$airco)
df_ams4$breakfast<-as.logical(df_ams4$breakfast)
df_ams4$pool<-as.logical(df_ams4$pool)
df_ams4$sauna<-as.logical(df_ams4$sauna)
df_ams4$hot_tub<-as.logical(df_ams4$hot_tub)
df_ams4$gym<-as.logical(df_ams4$gym)
df_ams4$free_parking<-as.logical(df_ams4$free_parking)
df_ams4$bbq<-as.logical(df_ams4$bbq)

#5. host quality attributes
df_ams4$greeting_host<-as.logical(df_ams4$greeting_host)

#6. convenience attributes
df_ams4$crib<-as.logical(df_ams4$crib)
df_ams4$high_chair<-as.logical(df_ams4$high_chair)
df_ams4$pets_allow<-as.logical(df_ams4$pets_allow)
df_ams4$elevator<-as.logical(df_ams4$elevator)
df_ams4$single_level<-as.logical(df_ams4$single_level)
df_ams4$baby_safety_gates<-as.logical(df_ams4$baby_safety_gates)
df_ams4$baby_bath<-as.logical(df_ams4$baby_bath)
df_ams4$changing_table<-as.logical(df_ams4$changing_table)
df_ams4$baby_monitor<-as.logical(df_ams4$baby_monitor)

#7. safety attributes
df_ams4$fire_extinguisher<-as.logical(df_ams4$fire_extinguisher)
df_ams4$smoke_alarm<-as.logical(df_ams4$smoke_alarm)
df_ams4$security_cameras<-as.logical(df_ams4$security_cameras)
df_ams4$carbon_monoxide_alarm<-as.logical(df_ams4$carbon_monoxide_alarm)
df_ams4$smart_lock<-as.logical(df_ams4$smart_lock)
df_ams4$first_aid<-as.logical(df_ams4$first_aid)


#Filter for missings/0 values 
df_ams5<-df_ams4
df_ams5<-df_ams5%>%filter(price != '0') 
df_ams5<-df_ams5%>%filter(n_reviews !='0') #
ams_amenities_dv<-df_ams5%>%filter(rev_rating != 0.00, rev_clean !=0.00, rev_accuracy !=0.00, rev_comm !=0.00, rev_location !=0.00,rev_value !=0.00) #when rev_rating = 0.00, all other ratings for all other categories were NA so this data isnt useable -> now the review columns don't contain NA values anymore either. 

#Checking range constraints: do star ratings really fall between 1-5? 
#rev_rating (p)
breaks<-unique(c(min(df_ams5$rev_rating),1,5,max(df_ams5$rev_rating))) #wrapped with unique() to omit the error of 'breaks are not unique' message
ggplot(df_ams5,aes(rev_rating))+geom_histogram(breaks=breaks)
#rev_accuracy (p)
breaks<-unique(c(min(df_ams5$rev_accuracy),1,5,max(df_ams5$rev_accuracy))) 
ggplot(df_ams5,aes(rev_accuracy))+geom_histogram(breaks=breaks)
#rev_clean (p)
breaks<-unique(c(min(df_ams5$rev_clean),1,5,max(df_ams5$rev_clean))) 
ggplot(df_ams5,aes(rev_clean))+geom_histogram(breaks=breaks)
#rev_checkin (p)
breaks<-unique(c(min(df_ams5$rev_checkin),1,5,max(df_ams5$rev_checkin))) 
ggplot(df_ams5,aes(rev_checkin))+geom_histogram(breaks=breaks)
#rev_comm (p)
breaks<-unique(c(min(df_ams5$rev_comm),1,5,max(df_ams5$rev_comm))) 
ggplot(df_ams5,aes(rev_comm))+geom_histogram(breaks=breaks)
#rev_location (p)
breaks<-unique(c(min(df_ams5$rev_location),1,5,max(df_ams5$rev_location))) 
ggplot(df_ams5,aes(rev_location))+geom_histogram(breaks=breaks)
#rev_value (p)
breaks<-unique(c(min(df_ams5$rev_value),1,5,max(df_ams5$rev_value))) 
ggplot(df_ams5,aes(rev_value))+geom_histogram(breaks=breaks)


#Checking uniqueness constraints
#Checking for full duplicates
duplicated(df_ams5)
sum(duplicated(df_ams5)) #0 full duplicates (passed test)
#Checking for partial duplicates
df_ams5%>%count(id)%>%filter(n>1) #0 partial duplicates (passed test)
df_ams5%>%count(host_id,price)%>%filter(n>1) #some host-id and price combinations are the same, but no id is the same (so these must be similar but different listings we suppose)


#save as csv.file
ams_amenities <- df_ams5
write.csv(ams_amenities_dv, "../../gen/data-preparation/temp/ams_amenities_dv.csv")




