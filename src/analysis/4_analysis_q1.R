#####################################
######## 4. ANALYSIS Q1 #############
#####################################

## RESEARCH QUESTION 1 = HOW DO THE DIFFERENT ATTRIBUTE FEATURES INFLUENCE THE LISTING PRICE? 
# Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(broom)
library(car)
library(ggfortify)
library(stargazer)

# Create directories
dir.create('../../gen/analysis')

# Load dataset
ams_complete <- read.csv('../../gen/data-preparation/data/ams_complete.csv')
ams_complete1 <- ams_complete[-1]
colnames(ams_complete1)

# Select relevant columns
ams_complete2<-ams_complete1%>%select(id, host_id, price, #general information
                                     rev_rating, rev_accuracy, rev_clean, rev_location, rev_comm, rev_checkin, rev_value, #review scores
                                     room_type, waterfront, balcony, #space attributes
                                     n_reviews, mean_review, bedrooms, beds, #listing quality attributes
                                     kitchen, washer, wifi, tv, coffee_maker, dishwasher, oven, #common listing attributes
                                     fireplace, airco, gym, hot_tub, free_parking, #extra listing attributes
                                     superhost, license, n_host_listings, instant_bookable, greeting_host, #host quality attributes
                                     crib, luggage_dropoff, single_level, changing_table, #convenience attributes
                                     private_entry, fire_extinguisher, security_cameras, carbon_monoxide_alarm) #safety attributes


##1. Summary statistics
summary(ams_complete2)
ams_complete2%>%count(room_type)

##2. Construct linear model (DV = price)
bnb_lm1<-lm(price ~ room_type + waterfront + balcony + 
              n_reviews + mean_review + bedrooms + beds + 
              kitchen + washer + wifi + tv + coffee_maker + dishwasher + oven + 
              fireplace + airco + gym + hot_tub + free_parking +
              superhost + license + n_host_listings + instant_bookable + greeting_host +
              crib + luggage_dropoff + single_level + changing_table + 
              private_entry + fire_extinguisher + security_cameras + carbon_monoxide_alarm, ams_complete2)
summary(bnb_lm1) #for analyzing our estimates ourselves

##3. Outlier screening =  using leverage and influence to identify outliers
#high leverage = explanatory variables have values that are different from other points in the dataset (values with a very high/very low exploratory value)
#influence = how much would a model change if each observation was left out of the model calculations, one at a time (how different the prediction line would look if you run a linear regression on all data points except that point, compared to running a linear regression on the whole dataset)
leverage_influence<-bnb_lm1%>%augment()%>%
  select(leverage =.hat, cooks_dist = .cooksd, price, room_type, n_host_listings, waterfront, balcony, kitchen, n_reviews, mean_review, bedrooms, beds, washer, wifi, tv, coffee_maker, dishwasher, oven, fireplace, airco, gym, hot_tub, free_parking, superhost, license, instant_bookable, greeting_host, crib, luggage_dropoff, single_level, changing_table, private_entry, fire_extinguisher, security_cameras, carbon_monoxide_alarm)%>% 
  arrange(desc(cooks_dist))%>% head(n=20)
leverage_influence
#Conclusion: for observation: n_host_listings=1992 the cooks_dist = 3.42, while for the second highest observation = 0.0721, therefore we need to remove this outlier
ams_complete2%>%count(n_host_listings==1992) #we see that there's only one observation; but there are 2 NA's so: 
ams_complete3<-ams_complete2%>%filter(n_host_listings!=1992 | is.na(n_host_listings))
ams_complete3<-ams_complete3[,-1]
summary(ams_complete3)
ams_complete%>%count(room_type)%>%summarize(perc=n/sum(n))
summary(ams_complete3$mean_review)


#Now run the linear model again 
bnb_lm2<-lm(price ~ room_type + waterfront + balcony + 
              n_reviews + mean_review + bedrooms + beds + 
              kitchen + washer + wifi + tv + coffee_maker + dishwasher + oven + 
              fireplace + airco + gym + hot_tub + free_parking +
              superhost + license + n_host_listings + instant_bookable + greeting_host +
              crib + luggage_dropoff + single_level + changing_table + 
              private_entry + fire_extinguisher + security_cameras + carbon_monoxide_alarm, ams_complete3)
summary(bnb_lm2) 
#Looking at leverage and cooks_dist again
leverage_influence1<-bnb_lm2%>%augment()%>%
  select(leverage =.hat, cooks_dist = .cooksd, price, room_type, waterfront, balcony, kitchen, n_reviews, mean_review, bedrooms, beds, washer, wifi, tv, coffee_maker, dishwasher, oven, fireplace, airco, gym, hot_tub, free_parking, superhost, license, n_host_listings, instant_bookable, greeting_host, crib, luggage_dropoff, single_level, changing_table, private_entry, fire_extinguisher, security_cameras, carbon_monoxide_alarm)%>% 
  arrange(desc(cooks_dist))%>% head(n=20)
leverage_influence1
#Conclusion: with the outliers removed, the cooks_dist now looks a lot better (and the estimates will be more accurate)

##4. Model reporting (for comparing models) = used for publication (for the paper); exports multiple model coefficients/fit statistics into a well-formatted HTML file that can be copy-pasted into Word (while still being editable)
#in html format
stargazer(bnb_lm1, bnb_lm2, 
          title = 'Figure 1: Price determinants of Airbnb listings',
          dep.var.caption = 'Price',
          dep.var.labels= '',
          column.labels = c('Full model', 'Outliers excluded'),
          notes.label = 'Significance levels',
          type = 'html',
          out = '../../gen/analysis/model_report_airbnb.html')

stargazer(bnb_lm1, bnb_lm2, 
          title = 'Figure 1: Price determinants of Airbnb listings',
          dep.var.caption = 'Price',
          dep.var.labels= '',
          column.labels = c('Full model', 'Outliers excluded'),
          notes.label = 'Significance levels',
          type = 'text',
          out = '../../gen/analysis/model_report_airbnb.text')

##4. Visualisations (for comparing models) = visualises most important relationships of the model to help the reader grasp the analysis
#because we included multiple IV's in our model, I took one of them: license and looked at the differences in the relation in the full model and the model where the outliers are removed
ggplot(ams_complete2, aes(x=license, y=price))+
         geom_point(alpha=0.5)+
         geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
         geom_smooth(method='lm',se=FALSE, data=ams_complete3,aes(color='Outliers excluded')) + 
         labs(x= 'License needed',y='Price')+
         ggtitle('Figure 2: Linear trend between license and price')+
         scale_colour_manual(name='Legend',values=c('red','blue'))

#so we can also look at the relationship between n_reviews and price:
ggplot(ams_complete2, aes(x=n_reviews, y=price))+
  geom_point(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
  geom_smooth(method='lm',se=FALSE, data=ams_complete3,aes(color='Outliers excluded')) + 
  labs(x= 'Number of reviews',y='Price')+
  ggtitle('Figure 2: Linear trend between n_reviews and price')+
  scale_colour_manual(name='Legend',values=c('red','blue'))
#such plots for binary variables are less informative it seems

#or mean_review and price:
ggplot(ams_complete2, aes(x=mean_review, y=price))+
  geom_point(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
  geom_smooth(method='lm',se=FALSE, data=ams_complete3,aes(color='Outliers excluded')) + 
  labs(x= 'Mean review',y='Price')+
  ggtitle('Figure 2: Linear trend between mean_review and price')+
  scale_colour_manual(name='Legend',values=c('red','blue'))

#or n_host_listings and price: (interesting because n_host_listings has become more significant with the outliers removed)
ggplot(ams_complete2, aes(x=n_host_listings, y=price))+
  geom_point(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
  geom_smooth(method='lm',se=FALSE, data=ams_complete3,aes(color='Outliers excluded')) + 
  labs(x= 'Number of hosts listings',y='Price')+
  ggtitle('Figure 2: Linear trend between n_host_listings and price')+
  scale_colour_manual(name='Legend',values=c('red','blue'))

#mean price for each type of room listed on Airbnb 
price_roomtype <-ams_complete3%>%group_by(room_type)%>%summarize(mean_price = mean(price))
price_roomtype%>%ggplot(aes(x=room_type,y=mean_price, fill=room_type))+geom_bar(stat='identity', position='dodge')

#relationship n_host_listings and price
mean(ams_complete3$n_host_listings, na.rm=TRUE) #the mean is only 1.81 
max(ams_complete3$n_host_listings,na.rm=TRUE) #while the max = 462, so we focus on most of the n_host_listings datapoints 
price_hostlistings <- ams_complete3 %>% filter(n_host_listings <= 50)
price_hostlistings%>%ggplot(aes(x=n_host_listings,y=price)) +geom_line()
#Do hosts with more listings and a superhost status ask higher prices? 
price_hostlistings%>%ggplot(aes(x=n_host_listings,y=price)) +geom_point(aes(color=superhost)) #here we see how price varies with the n_host_listings and which data points belong to superhosts (according to the colours)
price_hostlistings_superhost <- ams_complete3 

#relationship mean_review and price
#Does a higher mean_review translate into higher prices? Is this more or less so for superhosts? 
ams_complete3 %>% ggplot(aes(x=mean_review,y=price))+geom_point(aes(color=superhost))

#relationship n_reviews and price
summary(ams_complete$n_reviews) #mean = 28.78, max = 877. 
#How does the number of reviews relate to the listing price? 
ams_complete3%>%ggplot(aes(x=n_reviews,y=price)) +geom_point()
#Is this less or more so for superhosts? 
ams_complete3%>%ggplot(aes(x=n_reviews,y=price)) +geom_point(aes(color=superhost))

#relationship host_greeting and price: are 'friendler' hosts also more friendlier in their pricing? 
ams_complete3%>%ggplot(aes(x=greeting_host,y=price)) +geom_point()
price_hostgreeting <- ams_complete3%>%group_by(greeting_host)%>%summarize(meanprice=mean(price))
price_hostgreeting
price_hostgreeting%>%ggplot(aes(x=greeting_host,y=meanprice)) +geom_bar(stat='identity')














