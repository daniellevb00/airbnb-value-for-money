#####################################
######## 5. ANALYSIS Q2 #############
#####################################

## RESEARCH QUESTION 2 = IS THE PRICE RATIONALIZATION WELL-REFLECTED IN CONSUMER'S RATINGS OF THE PRICE-QUALITY RATIO? (I.E. THE PERCEIVED VALUE OF MONEY)
#i.e. the presence of which amenities in the listing will have the most positive effect on consumer's price-quality perceptions? 

# Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(broom)
library(car)
library(ggfortify)
library(stargazer)

# Load dataset
ams_complete2 <- read.csv('../../gen/data-preparation/data/ams_complete.csv')
ams_complete3 <- ams_complete2[-1]

# Select relevant columns
ams_complete4<-ams_complete3%>%select(id, host_id, price, #general information
                                      rev_rating, rev_accuracy, rev_clean, rev_location, rev_comm, rev_checkin, rev_value, #review scores
                                      room_type, waterfront, balcony, #space attributes
                                      n_reviews, mean_review, bedrooms, beds, #listing quality attributes
                                      kitchen, washer, wifi, tv, coffee_maker, dishwasher, oven, #common listing attributes
                                      fireplace, airco, gym, hot_tub, free_parking, #extra listing attributes
                                      superhost, license, n_host_listings, instant_bookable, greeting_host, #host quality attributes
                                      crib, luggage_dropoff, single_level, changing_table, #convenience attributes
                                      private_entry, fire_extinguisher, security_cameras, carbon_monoxide_alarm) #safety attributes


##1. Summary statistics (but this will be similar to the analysis in q1)
summary(ams_complete4)
ams_complete4%>%count(room_type)

##2. Construct linear model (DV = rev_value = price/quality ratio)
bnb_lm_pq<-lm(rev_value ~ room_type + waterfront + balcony + 
              n_reviews + mean_review + bedrooms + beds + 
              kitchen + washer + wifi + tv + coffee_maker + dishwasher + oven + 
              fireplace + airco + gym + hot_tub + free_parking +
              superhost + license + n_host_listings + instant_bookable + greeting_host +
              crib + luggage_dropoff + single_level + changing_table + 
              private_entry + fire_extinguisher + security_cameras + carbon_monoxide_alarm, ams_complete4)
summary(bnb_lm_pq) 

##3. Outlier screening =  using leverage and influence to identify outliers
#high leverage = explanatory variables have values that are different from other points in the dataset (values with a very high/very low exploratory value)
#influence = how much would a model change if each observation was left out of the model calculations, one at a time (how different the prediction line would look if you run a linear regression on all data points except that point, compared to running a linear regression on the whole dataset)
leverage_influence<-bnb_lm_pq%>%augment()%>%
  select(leverage =.hat, cooks_dist = .cooksd, rev_value, n_host_listings, room_type, waterfront, balcony, kitchen, n_reviews, mean_review, bedrooms, beds, washer, wifi, tv, coffee_maker, dishwasher, oven, fireplace, airco, gym, hot_tub, free_parking, superhost, license, instant_bookable, greeting_host, crib, luggage_dropoff, single_level, changing_table, private_entry, fire_extinguisher, security_cameras, carbon_monoxide_alarm)%>% 
  arrange(desc(cooks_dist))%>% head(n=20)
leverage_influence
#Conclusion:the rev-value=3.2 and n_reviews=5 row has a cooks_dist = 0.0856 which is too far of from the cooks_dist values of the other observations
ams_complete4%>%count(n_host_listings==1992) #we see that there's only one observation with this combination
ams_complete5<-ams_complete4%>%filter(n_host_listings!=1992| is.na(n_host_listings))
#Now run the linear model again 
bnb_lm_pq1<-lm(rev_value ~ room_type + waterfront + balcony + 
              n_reviews + mean_review + bedrooms + beds + 
              kitchen + washer + wifi + tv + coffee_maker + dishwasher + oven + 
              fireplace + airco + gym + hot_tub + free_parking +
              superhost + license + n_host_listings + instant_bookable + greeting_host +
              crib + luggage_dropoff + single_level + changing_table + 
              private_entry + fire_extinguisher + security_cameras + carbon_monoxide_alarm, ams_complete3)
summary(bnb_lm_pq1) 
#Looking at leverage and cooks_dist again
leverage_influence1<-bnb_lm_pq1%>%augment()%>%
  select(leverage =.hat, cooks_dist = .cooksd, rev_value, room_type, waterfront, balcony, kitchen, n_reviews, mean_review, bedrooms, beds, washer, wifi, tv, coffee_maker, dishwasher, oven, fireplace, airco, gym, hot_tub, free_parking, superhost, license, n_host_listings, instant_bookable, greeting_host, crib, luggage_dropoff, single_level, changing_table, private_entry, fire_extinguisher, security_cameras, carbon_monoxide_alarm)%>% 
  arrange(desc(cooks_dist))%>% head(n=20)
leverage_influence1
#Conclusion: Now it looks much better 

##4. Model reporting (for comparing models) = used for publication (for the paper); exports multiple model coefficients/fit statistics into a well-formatted HTML file that can be copy-pasted into Word (while still being editable)
stargazer(bnb_lm_pq, bnb_lm_pq1, 
          title = 'Table 3: Determinants of consumers price-quality ratings of Airbnb listings',
          dep.var.caption = 'Price/quality ratio rating',
          dep.var.labels= '',
          column.labels = c('Full model', 'Outliers excluded'),
          notes.label = 'Significance levels',
          type = 'html',
          out = '../../gen/analysis/model_report_airbnb_pq.html')


