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
library(ggfortify)
library(stargazer)
# Load the aggregated_df.csv which is the output of the cleaning file (fix this in the makefile)
setwd('C://Users//danie//OneDrive//Documents//Repositories//dPREP-project-team-3//gen//data-preparation')
aggregated<-read.csv('aggregated_df.csv') 
View(aggregated)
# For now, we only filter for the columns we're using that already work (amenities columns aren't corrected yet so have to be left out) -> i added amenity carbon_monoxide_alarm to try out because I thought that amenity column was valid
aggregated1<-aggregated%>%select(id, host_id, superhost, n_host_listings, room_type,price,n_reviews,rev_rating,rev_accuracy,rev_clean,rev_location,rev_comm,rev_checkin, rev_value,license,carbon_monoxide_alarm)
View(aggregated1)

## STEP 1: GENERATING SUMMARY STATISTICS
#Firstly, we generate some summary statistics for the variables we decided to include in our analysis. 
#We include these statistics in our paper to generate some insights into the range of values (idk whether this should be for all European cities on average or what is applicable)
#Look at the mean, standard deviation, minimum value, maximum value (look for each case which of these statistics is applicable); categorize these statistics based on to which attribute features category the variables belong.
summary(aggregated1)
#superhost mean = 0.124 (binary)
#n_host_listings, mean =  2.029, min = 0, max=2018 (consider: why can a host included have 0 listings?, why 55 NA's?)
aggregated1%>%count(room_type)
#room_type, entire homes = 12790, hotel rooms =  94, private rooms = 3488, shared rooms = 44
#price, mean = 152.1, min = 4 and max = 999
#n_reviews, mean =  24.36, min=0, max = 871 (consider: didn't we filter out the listings with n_reviews=0?)
#rev_rating, mean = 4.692, min=0, max=5
#license, mean = 0.2516 (binary variable)
#carbon monoxide alarm, mean = 0.06756 (binary variable)

## STEP 2: CONSTRUCT THE LINEAR MODEL
#Linear model of the effect of the IV's on the metric DV price
#Estimate the model
bnb_lm1<-lm(price~superhost + n_host_listings + room_type + n_reviews + rev_rating + license + carbon_monoxide_alarm, aggregated1) #make sure all IV's are added here in the end (also the amenities)
summary(bnb_lm1) #I now only included rev_rating of the rating columns as otherwise there may be too much multicollinearity introduced which may bias our estimates/standard errors (consider this later!)
#superhost -> significant effect = 16.27377 (+)
#n_host_listings -> not significant
#room_type: hotel -> significant = -41.13922 (-)
#room_type: private room -> signifcant = -71.47718 (-)
#room_type: shared room -> signifcant = -88.91 (-)
#n_reviews -> significant = -0.08397 (-)
#rev_rating -> not signifcant
#license -> significant  = 20.55202 (+)
#carbon monoxide alarm ->significant = 10.82637 (+)

#Look at R2 how much of the variation in the DV is explained by the model (in %)
#Multiple R2 = 0.1092, Adjusted R2 = 0.1089, F-statistic = 192.9, p-value < 2.2e-16 = significant

## STEP 3: CHECK WHETHER THERE IS MULTICOLLINEARITY (as we're using a multiple linear regression model)
#Therefore, look at the VIFS
vif(bnb_lm1) #the VIFS should be approximately 1, then multicollinearity isn't an issue (more so if they would be >10)
# in our case, the values in the GVIF's columns are all approximately 1 so multicollinearity doesn't seem to be an issue.  

## STEP 4: CHECKING THE ASSUMPTIONS
# Checking for observations that are approximately normally distributed (bell-curve)
bnb_res <-augment(bnb_lm1)
ggplot(bnb_res,aes(.resid))+geom_histogram(aes(y=..density..),binwidth=5)+stat_function(fun=dnorm,args=list(mean=mean(bnb_res$.resid),sd=sd(bnb_res$.resid)),color='red',size=2) #may need to change the binwidth and size based on our figure
# in our case, it seems pretty close to a normal distribution only a long right-tail it seems. 

# Checking for randomness in observations (no pattern)
plot(bnb_lm1) #1st time we press enter, we get a scatter plot to show us whether the observations are random. 
# in our case, the observations seem pretty random I think. 
#2nd time we press enter, we see whether the residuals are normally distributed (the residuals should lie on the diagonal line; otherwise normality is violated)
# in our case, for theoretical quantities up until around 1, the residuals are on  the diagonal, from 1 on it deviates.. 

# We can also check the linear model assumptions using autoplot():
autoplot(bnb_lm1, which=1:3, nrow=1, ncol=3) #from code snippet building blocks
#left plot = the residuals and data points should center around the horizontal axis (such that the mean of the residuals is 0)
# in our case, the blue line deviates from the horizontal axis a bit for very high fitted values, for smaller/middle range of fitted values the residuals seem to be fitted well on the horizontal axis (some observations that are quite far of the horizontal axis towards the end..)
#middle plot = the residuals should be normally distributed approximately; the distribution of residuals should look like a bell-shaped distribution (Gaussian distribution). There data points in the QQ-plot need to be close to the diagonal. 
# in our case, we see that on the right-side of the plot there are quite a few values that deviate from the diagonal..(we already identified this before too)
#right plot = shows the standardized residuals for all fitted values. Homoscedasticity = the error term should be the same across all values of the IVs; so is there any pattern that stands out? 
# in our case, relatively the blue line stays pretty flat when looking at the y-axis

## STEP 5: OUTLIER SCREENING
#Use leverage and influence to identify outliers. High leverage = explanatory variable has values that are different from other points in the dataset = values with a very high or very low exploratory value. Influence = how much a model would change if each observation was left out of the model calcuulaions, one at a time; how different the prediction line would look if you run a linear regression on all data points except that point, compared to running a linear regression on the whole dataset. 
#Standard metric for influence = Cook's distance = calculates the influence based on the size of the residual and the leverage of the point. 
leverage_influence<-bnb_lm1%>%augment()%>%
  select(price,superhost,n_host_listings,room_type,n_reviews,rev_rating,license,carbon_monoxide_alarm, leverage =.hat, cooks_dist = .cooksd)%>%
  arrange(desc(cooks_dist))%>% head()
leverage_influence
# in our case, cooks_dist and leverage is very large for observation -> with price=125 and n_host_listings=2018 (so we probability need to exclude this observation as it really changes our model)
# cooks_dist is also pretty high for 2 other observations: price=900/n_host_listings=20, and observation; price = 890 & n_host_listings=4 (we may need to exclude these too)

#Now we exclude this data point from our analyysis and estimate another linear model and see the difference in model coefficients
aggregated2<-aggregated1%>%filter(n_host_listings !=2018) #I now only excluded the variable with the highest cooks_dist/leverage, but left the other two in for now
summary(aggregated2)
bnb_lm2<-lm(price~superhost + n_host_listings + room_type + n_reviews + rev_rating + license + carbon_monoxide_alarm, aggregated2) #make sure all IV's are added here in the end (also the amenities)
summary(bnb_lm2) #now we get the following estimates (only named if they were significant:
#superhost + 16.30 
#n_host_listings + 0.3673
#hotel room - 43.23
#private room -71.68
#shared room -89.36
#n_reviews = -0.085
#license = 20.38
#carbon_monoxide_alarm = 11.06
#Multiple R2 = 0.1103, Adjusted R2 = 0.1097, F statistic = 194.5, p-value < 2.2e-16 (significant)

#now screening the outliers for this model again 
leverage_influence1<-bnb_lm2%>%augment()%>%
  select(price,superhost,n_host_listings,room_type,n_reviews,rev_rating,license,carbon_monoxide_alarm, leverage =.hat, cooks_dist = .cooksd)%>%
  arrange(desc(cooks_dist))%>% head()
leverage_influence1 #it seems like we want to exclude the observation with price=140 + n_host_listings=484

#we now too remove that observation
aggregated3<-aggregated2%>%filter(n_host_listings !=484)
summary(aggregated3)
bnb_lm3<-lm(price~superhost + n_host_listings + room_type + n_reviews + rev_rating + license + carbon_monoxide_alarm, aggregated3) #make sure all IV's are added here in the end (also the amenities)
summary(bnb_lm3) #the estimates are now = (only named signficant ones)
# super host + 16.30
# n_host_listings + 0.62
# hotel room -44.86
# private room -71.86
# shared room -89.74
# n-reviews -0.08
# license +20.28
# carbon monoxide alarm + 11.12
# Multiple R2 = 0.1104, Adjusted R2 = 0.1098, F-statistic = 194.8, p-value<2.2e-16 (significant)

#now screening the outliers for this model again 
leverage_influence2<-bnb_lm3%>%augment()%>%
  select(price,superhost,n_host_listings,room_type,n_reviews,rev_rating,license,carbon_monoxide_alarm, leverage =.hat, cooks_dist = .cooksd)%>%
  arrange(desc(cooks_dist))%>% head()
leverage_influence2 #now I think it seems good enough (keep the rest of the observations)

#we now call the dataframe with the 2 outliers removed = bnb_lm_cleaned, to more easily distinguish between the full model and the outlier excluded model
bnb_lm_cleaned<-bnb_lm3 #model with 2 outliers excluded
bnb_lm1 #full model

#we may now check the assumptions again using the bnb_lm_cleaned dataset to see if the assumptions are better satisfied with the outliers removed.

## STEP 7: MODEL REPORTING
#summary() suffices for your own analysis but for publication use stargazer package = exports multipple model coefficients and fit statistics into a well-formatted HTML file that can be copy-pasted in Word while still being editable
stargazer(bnb_lm1, bnb_lm_cleaned, 
          title = 'Figure 1: Price determinants of Airbnb listings',
          dep.var.caption = 'Price',
          dep.var.labels= '',
          column.labels = c('Full model', 'Outliers excluded'),
          notes.label = 'Significance levels',
          type = 'html',
          out = 'model_report_airbnb.html')

## STEP 8: VISUALIZING LINEAR RELATIONSHIPS
#Visualise the most important relationships of the model to help the reader grasp the analysis; e.g. illustrate that the outlier severely impacts the regression slope, yet the direction may be consistent for both plots. 
#Because we included multiple IV's in our model, I took one of them: license and looked at the differences in the relation in the full model and the model where the outliers are removed
ggplot(aggregated1, aes(x=license, y=price))+
         geom_point(alpha=0.5)+
         geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
         geom_smooth(method='lm',se=FALSE, data=bnb_lm_cleaned,aes(color='Outliers excluded')) + 
         labs(x= 'License needed',y='Price')+
         ggtitle('Figure 2: Linear trend between license and price')+
         scale_colour_manual(name='Legend',values=c('red','blue'))
#because the y-axis has pretty big steps between the labels (of price=250); the full model and outliers excluded model seem pretty much the same; but as we would zoom in more we would certaintly see a difference I think
#here the graph doens't look as nice as license is a binary variable and not a metric DV, so it only returns values for license needed = 0 and license needed =1. 
#so we can also look at the relationship between n_reviews and price (but it's effect isn't as big, but still significant):
ggplot(aggregated1, aes(x=n_reviews, y=price))+
  geom_point(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
  geom_smooth(method='lm',se=FALSE, data=bnb_lm_cleaned,aes(color='Outliers excluded')) + 
  labs(x= 'Number of reviews',y='Price')+
  ggtitle('Figure 2: Linear trend between n_reviews and price')+
  scale_colour_manual(name='Legend',values=c('red','blue'))
#provides us with a more interesting plot (still; we might need to look at filtering n_reviews=0 again because in the plot there seem to be a few, altough it's hard to read as the steps for teh number of reviews are again quite big (250 reviews))

## STEP 9: MAKING PREDICTIONS FOR NEW DATA
#Be careful with extrapolating outside the ranges of our data, but e.g. we want to know the stop distance of a car that drives 45, 50, 60 km per hour, then we can create a dataframe with these inputs and predict our linear model by plugging in these inputs into our regression equation.


## STEP 10: ACCESSING COEFFICIENTS
#Access coefficients for interpretation?
bnb_lm1$coef[[2]] #to retrieve the estimate of the first variable (its effect on the DV)
#Report for each of the feature attributes of the categories the coefficient, t-statistic, significance (p-value), relative % (maybe also the general R2 statistic?)



