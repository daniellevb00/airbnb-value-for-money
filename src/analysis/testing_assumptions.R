#####################################
######## TESTING ASSUMPTIONS ########
#####################################

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

## CHECKING ASSUMPTIONS FOR THE FIRST RESEARCH QUESTION LM ##
# Construct linear model
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

# Check for multicollinearity
vif(bnb_lm1)

# Assumptions linear models

## 1 Independence = Observations should be independent, therefore there should be no pattern to detect in its distribution (very important)
plot(bnb_lm1)
# 1st ENTER = scatter plot which shows whether the observations are random. 
# Or use autoplot(): 
autoplot(bnb_lm1, which=1:3, nrow=1, ncol=3)
# 1st PLOT= residuals and data points should center around the horizontal axis (mean residuals = 0)

## 2 Homoscedasticity = equal variance across treatment groups (somewhat important)
autoplot(bnb_lm1, which=1:3, nrow=1, ncol=3)
# 3rd PLOT = error term should be the same across all values of the IVs; it shows the standarized residuals for all fitted values (does anything stand out?)

## 2 Normality = Residuals are (approximately) normally distributed (but only affects standard errors when the sample size is small)
bnb_res <-  augment(bnb_lm1)
ggplot(bnb_res,aes(.resid))+geom_histogram(aes(y=..density..),binwidth=5)+stat_function(fun=dnorm,args=list(mean=mean(bnb_res$.resid),sd=sd(bnb_res$.resid)),color='red',size=2) 
# Or use plot(): 
plot(bnb_lm1)
# 2nd ENTER = whether residuals are normally distributed = residuals should lie on the diagonal line (otherwise violated)
# Or use autoplot(): 
autoplot(bnb_lm1, which=1:3, nrow=1, ncol=3)
# 2nd PLOT = distribution of the residuals should look like a bell-shaped distribution (Gaussian distribution) such that the residuals are normally distributed. Data points in the QQ-plot need to be close to the diagonal.

# Outlier screening =  using leverage and influence to identify outliers

#high leverage = explanatory variables have values that are different from other points in the dataset (values with a very high/very low exploratory value)
#influence = how much would a model change if each observation was left out of the model calculations, one at a time (how different the prediction line would look if you run a linear regression on all data points except that point, compared to running a linear regression on the whole dataset)
leverage_influence<-bnb_lm1%>%augment()%>%
  select(price, mean_review,bedrooms,beds, balcony, lake_access, waterfront, backyard, room_type, kitchen, wifi, bed_linens, tv, washer, heating, free_parking, breakfast, pool, superhost, greeting_host, crib, baby_safety_gates, fire_extinguisher, security_cameras, carbon_monoxide_alarm, leverage =.hat, cooks_dist = .cooksd)%>%
  arrange(desc(cooks_dist))%>% head()
leverage_influence


## CHECKING ASSUMPTIONS FOR THE SECOND RESEARCH QUESTION LM ##
# Construct linear model
bnb_lm_pq<-lm(rev_value ~ mean_review +
                bedrooms + beds +
                balcony + lake_access + waterfront + backyard +
                room_type +
                kitchen + wifi + bed_linens + tv + washer +
                heating + free_parking + breakfast + pool +
                superhost + greeting_host +
                crib + baby_safety_gates +
                fire_extinguisher + security_cameras + carbon_monoxide_alarm,
              ams_complete1)
summary(bnb_lm_pq) 

# Check for multicollinearity
vif(bnb_lm_pq)

# Assumptions linear models

## 1 Independence = Observations should be independent, therefore there should be no pattern to detect in its distribution (very important)
plot(bnb_lm_pq)
# 1st ENTER = scatter plot which shows whether the observations are random. 
# Or use autoplot(): 
autoplot(bnb_lm_pq, which=1:3, nrow=1, ncol=3)
# 1st PLOT= residuals and data points should center around the horizontal axis (mean residuals = 0)

## 2 Homoscedasticity = equal variance across treatment groups (somewhat important)
autoplot(bnb_lm_pq, which=1:3, nrow=1, ncol=3)
# 3rd PLOT = error term should be the same across all values of the IVs; it shows the standarized residuals for all fitted values (does anything stand out?)

## 2 Normality = Residuals are (approximately) normally distributed (but only affects standard errors when the sample size is small)
bnb_res1 <-  augment(bnb_lm_pq)
ggplot(bnb_res1,aes(.resid))+geom_histogram(aes(y=..density..),binwidth=5)+stat_function(fun=dnorm,args=list(mean=mean(bnb_res1$.resid),sd=sd(bnb_res1$.resid)),color='red',size=2) 
# Or use plot(): 
plot(bnb_lm_pq)
# 2nd ENTER = whether residuals are normally distributed = residuals should lie on the diagonal line (otherwise violated)
# Or use autoplot(): 
autoplot(bnb_lm_pq, which=1:3, nrow=1, ncol=3)
# 2nd PLOT = distribution of the residuals should look like a bell-shaped distribution (Gaussian distribution) such that the residuals are normally distributed. Data points in the QQ-plot need to be close to the diagonal.

# Outlier screening =  using leverage and influence to identify outliers

#high leverage = explanatory variables have values that are different from other points in the dataset (values with a very high/very low exploratory value)
#influence = how much would a model change if each observation was left out of the model calculations, one at a time (how different the prediction line would look if you run a linear regression on all data points except that point, compared to running a linear regression on the whole dataset)
leverage_influence1<-bnb_lm_pq%>%augment()%>%
  select(price, mean_review,bedrooms,beds, balcony, lake_access, waterfront, backyard, room_type, kitchen, wifi, bed_linens, tv, washer, heating, free_parking, breakfast, pool, superhost, greeting_host, crib, baby_safety_gates, fire_extinguisher, security_cameras, carbon_monoxide_alarm, leverage =.hat, cooks_dist = .cooksd)%>%
  arrange(desc(cooks_dist))%>% head()
leverage_influence1








