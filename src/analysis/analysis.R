### DPREP PROJECT - TEAM 3 - InsideAirbnb - ANALYSIS FILE ###
## Should load the aggregated_df.csv file and then perform analysis on it, then export results. 

# ANALYSIS FOR AMSTERDAM

## RESEARCH QUESTION: HOW DO DIFFERENT ATTRIBUTE FEATURES INFLUENCE THE LISTING PRICE? (WHICH THE MOST?) 
## PART 2: AND DOES THE RELATION OF THESE ATTRIBUTE FEATURES WITH THE PRICE TRANSLATE TO HIGH PRICE/QUALITY RATIO REVIEW SCORES BY CONSUMERS? 

# Here we will focus on the first question, file 'analysis2' will focus on the second question. 

# To research this relation we perform an Linear Regression to determine the relationship between a metric DV (ratio: price) and some IV's which represent the attribute features of 'experiencing a stay at an Airbnb listing'
# For this we identified different attribute feature categories which are seen as the price determinants, these are based on some columns of the original dataset both also some amenities values which have been transformed into columns using pivot_wider into dummy variables. 
# The different attribute feature categories
# 1, Space attributes = room_type (entire home, private room, shared room, hotel), balcony, etc. 
# 2. Listing quality attributes = number of reviews, review star ratings, etc.
# 3. Freebies attributes = kitchen, oven, free parking, etc. 
# 4. Host quality attributes = superhost, host response rate/time, host listings count, license, etc. 
# 5. Safety attributes = smoke alarm, security camera's, fire extinguisher, carbon monoxide detector, etc.
# We use OLS = ordinary least squares linear regression to model the linear relation between the response variable and several predictor varaibles. 
# This is a multiple linear regression model in the form of: Y= b0 + b1X1 + b2X2 + ... + ei, where b0 is the baseline price. 

## STEP 0: STARTING-UP
# Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(broom)
library(car)
# Load the aggregated_df.csv which is the output of the cleaning file (fix this in the makefile)
aggregated<-read.csv('aggregated_df.csv') 

## STEP 1: GENERATING SUMMARY STATISTICS
#Firstly, we generate some summary statitics for the variables we decided to include in our analysis. 
#We include these statistics in our paper to generate some insights into the range of values (idk whether this should be for all European cities on average or what is applicable)
#Look at the mean, standard deviation, minimum value, maximum value (look for each case which of these statistics is applicable); categorize these statistics based on to which attribute features category the variables belong.


## STEP 2: CONSTRUCT THE LINEAR MODEL
#Linear model of the effect of the IV's on the metric DV price
bnb_lm1<-lm(price~room_type+rev_accuracy+rev_comm+rev_clean+rev_location+rev_checkin+rev_rating+n_reviews+n_host_listings+superhost+...+...,aggregated) #make sure al IV's are added here (also the amenities)
summary(bnb_lm1)
#Look at R2 how much of the variaiton in the DV is explained by the model (in %)

## STEP 3: CHECK WHETHER THERE IS MULTICOLLINEARITY (as we're using a multiple linear regression model)
#Therefore, look at the VIFS
vif(bnb_lm1) #the VIFS should be approximately 1, then multicollinearity isn't an issue (more so if they would be >10)

## STEP 4: CHECKING THE ASSUMPTIONS
# Checking for observations that are approximately normally distributed (bell-curve)
bnb_res <-augment(bnb_lm1)
ggplot(bnb_res,aes(.resid))+geom_histogram(aes(y=..density..),binwidth=5)+stat_function(fun=dnorm,args=list(mean=mean(bnb_res$.resid),sd=sd(bnb_res$.resid)),color='red',size=2) #may need to change the binwidth and size based on our figure

# Checking for randomness in observations (no pattern)
plot(sat_lm5) #1st time we press enter, we get a scatter plot to show us whether the observations are random. 
#2nd time we press enter, we see whether the residuals are normally distributed (the residuals should lie on the diagonal line; otherwise normality is violated)

## STEP 5: ACCESSING COEFFICIENTS
#Access coefficients for interpretation?
bnb_lm1$coef[[2]] #to retrieve the estimate of the first variable (its effect on the DV)
#Report for each of the feature attributes of the categories the coeffcient, t-statistic, signficance (p-value), relative % (maybe also the general R2 statistic?)



