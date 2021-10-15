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
dir.create('../../gen/analysis/output')

# Load dataset
ams_complete <- read.csv('../../gen/data-preparation/data/ams_complete.csv')
ams_complete1 <- ams_complete[2:75]

# Select relevant columns
ams_complete1<-ams_complete%>%select(id, host_id, n_host_listings,  #general information
                                     rev_rating,rev_accuracy,rev_clean,rev_location,rev_comm,rev_checkin, rev_value,license,mean_review, #review scores
                                     bedrooms, beds, #bed information
                                     balcony, lake_access, waterfront, backyard, #space attributes
                                     room_type,price,n_reviews, #listing quality attributes
                                     kitchen, wifi, bed_linens, tv, washer, #common listing attributes
                                     heating, free_parking, breakfast, pool, #extra listing attributes
                                     superhost, host_accept_rate, greeting_host, #host attributes
                                     crib, pets_allow, baby_safety_gates, #convenience attributes
                                     fire_extinguisher, security_cameras, carbon_monoxide_alarm) #safety attributes

##1. Summary statistics
summary(ams_complete1)
ams_complete1%>%count(room_type)

##2. Construct linear model (DV = price)
bnb_lm1<-lm(price ~ mean_review +
              bedrooms + beds +
              balcony + lake_access + waterfront + backyard +
              room_type +
              kitchen + wifi + bed_linens + tv + washer +
              heating + free_parking + breakfast + pool +
              superhost + greeting_host +
              crib + baby_safety_gates +
              fire_extinguisher + security_cameras + carbon_monoxide_alarm,
            ams_complete1)
summary(bnb_lm1) #for analyzing our estimates ourselves

##3. Model reporting (for comparing models) = used for publication (for the paper); exports multiple model coefficients/fit statistics into a well-formatted HTML file that can be copy-pasted into Word (while still being editable)
#(this still needs to be adjusted for the new analysis)
# stargazer(bnb_lm1, bnb_lm_cleaned, 
#          title = 'Figure 1: Price determinants of Airbnb listings',
#          dep.var.caption = 'Price',
#          dep.var.labels= '',
#          column.labels = c('Full model', 'Outliers excluded'),
#          notes.label = 'Significance levels',
#          type = 'html',
#          out = 'model_report_airbnb.html')

##4. Visualisations (for comparing models) = visualises most important relationships of the model to help the reader grasp the analysis
#(this still needs to be adjusted for the new analysis)
#because we included multiple IV's in our model, I took one of them: license and looked at the differences in the relation in the full model and the model where the outliers are removed
#ggplot(aggregated1, aes(x=license, y=price))+
#         geom_point(alpha=0.5)+
#         geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
#         geom_smooth(method='lm',se=FALSE, data=bnb_lm_cleaned,aes(color='Outliers excluded')) + 
#         labs(x= 'License needed',y='Price')+
#         ggtitle('Figure 2: Linear trend between license and price')+
#         scale_colour_manual(name='Legend',values=c('red','blue'))
#so we can also look at the relationship between n_reviews and price:
#ggplot(aggregated1, aes(x=n_reviews, y=price))+
#  geom_point(alpha=0.5)+
#  geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
#  geom_smooth(method='lm',se=FALSE, data=bnb_lm_cleaned,aes(color='Outliers excluded')) + 
#  labs(x= 'Number of reviews',y='Price')+
#  ggtitle('Figure 2: Linear trend between n_reviews and price')+
#  scale_colour_manual(name='Legend',values=c('red','blue'))

## 5. Predicting 
#(be careful with extrapolating outside the ranges of our data, but e.g. we want to know the stop distance of a car that drives 45, 50, 60 km per hour, then we can create a dataframe with these inputs and predict our linear model by plugging in these inputs into our regression equation.)

## 6. Accessing certain coefficients
bnb_lm1$coef[[2]] #selects estimate the 1st variable (its effect on the DV)
#Report for each of the feature attributes of the categories the coefficient, t-statistic, significance (p-value), relative % (maybe also the general R2 statistic?)














