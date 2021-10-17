########################
###### ANALYSIS 1 ######
########################

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


##2. Construct linear model
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
              
              
summary(bnb_lm1) 


##3. Check for multicollinearity
vif(bnb_lm1)


##4. Assumptions
# Normally distribution (bell-curve)
bnb_res <-augment(bnb_lm1)
ggplot(bnb_res,aes(.resid))+geom_histogram(aes(y=..density..),binwidth=5)+stat_function(fun=dnorm,args=list(mean=mean(bnb_res$.resid),sd=sd(bnb_res$.resid)),color='red',size=2) 

# Random observations
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

##5. Outlier screening
#Use leverage and influence to identify outliers. High leverage = explanatory variable has values that are different from other points in the dataset = values with a very high or very low exploratory value. Influence = how much a model would change if each observation was left out of the model calcuulaions, one at a time; how different the prediction line would look if you run a linear regression on all data points except that point, compared to running a linear regression on the whole dataset. 
#Standard metric for influence = Cook's distance = calculates the influence based on the size of the residual and the leverage of the point. 
lm(price ~ mean_review +
     bedrooms + beds +
     balcony + lake_access + waterfront + backyard +
     room_type +
     kitchen + wifi + bed_linens + tv + washer +
     heating + free_parking + breakfast + pool +
     superhost + greeting_host +
     crib + baby_safety_gates +
     fire_extinguisher + security_cameras + carbon_monoxide_alarm,
   ams_complete1)

leverage_influence<-bnb_lm1%>%augment()%>%
  select(price, mean_review,bedrooms,beds, balcony, lake_access, waterfront, backyard, room_type, kitchen, wifi, bed_linens, tv, washer, heating, free_parking, breakfast, pool, superhost, greeting_host, crib, baby_safety_gates, fire_extinguisher, security_cameras, carbon_monoxide_alarm, leverage =.hat, cooks_dist = .cooksd)%>%
  arrange(desc(cooks_dist))%>% head()
leverage_influence #the leverage points are not high, so this indicates that there are no extreme outliers that should be deleted from the model


##7. model reporting (this step is for comparing models, havent done this for our new analysis (Lesley))
#summary() suffices for your own analysis but for publication use stargazer package = exports multiple model coefficients and fit statistics into a well-formatted HTML file that can be copy-pasted in Word while still being editable
# stargazer(bnb_lm1, bnb_lm_cleaned, 
#          title = 'Figure 1: Price determinants of Airbnb listings',
#          dep.var.caption = 'Price',
#          dep.var.labels= '',
#          column.labels = c('Full model', 'Outliers excluded'),
#          notes.label = 'Significance levels',
#          type = 'html',
#          out = 'model_report_airbnb.html')



##8. Visualizations (this step is for comparing models, havent done this for our new analysis (Lesley))
#Visualize the most important relationships of the model to help the reader grasp the analysis; e.g. illustrate that the outlier severely impacts the regression slope, yet the direction may be consistent for both plots. 
#Because we included multiple IV's in our model, I took one of them: license and looked at the differences in the relation in the full model and the model where the outliers are removed
#ggplot(aggregated1, aes(x=license, y=price))+
#         geom_point(alpha=0.5)+
#         geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
#         geom_smooth(method='lm',se=FALSE, data=bnb_lm_cleaned,aes(color='Outliers excluded')) + 
#         labs(x= 'License needed',y='Price')+
#         ggtitle('Figure 2: Linear trend between license and price')+
#         scale_colour_manual(name='Legend',values=c('red','blue'))
#because the y-axis has pretty big steps between the labels (of price=250); the full model and outliers excluded model seem pretty much the same; but as we would zoom in more we would certaintly see a difference I think
#here the graph doens't look as nice as license is a binary variable and not a metric DV, so it only returns values for license needed = 0 and license needed =1. 
#so we can also look at the relationship between n_reviews and price (but it's effect isn't as big, but still significant):
#ggplot(aggregated1, aes(x=n_reviews, y=price))+
#  geom_point(alpha=0.5)+
#  geom_smooth(method='lm',se=FALSE,aes(color='Full model'))+
#  geom_smooth(method='lm',se=FALSE, data=bnb_lm_cleaned,aes(color='Outliers excluded')) + 
#  labs(x= 'Number of reviews',y='Price')+
#  ggtitle('Figure 2: Linear trend between n_reviews and price')+
#  scale_colour_manual(name='Legend',values=c('red','blue'))
#provides us with a more interesting plot (still; we might need to look at filtering n_reviews=0 again because in the plot there seem to be a few, altough it's hard to read as the steps for teh number of reviews are again quite big (250 reviews))

##9. Predicting
#Be careful with extrapolating outside the ranges of our data, but e.g. we want to know the stop distance of a car that drives 45, 50, 60 km per hour, then we can create a dataframe with these inputs and predict our linear model by plugging in these inputs into our regression equation.


##10. Accesing coefficients
bnb_lm1$coef[[2]] #to retrieve the estimate of the first variable (its effect on the DV)
#Report for each of the feature attributes of the categories the coefficient, t-statistic, significance (p-value), relative % (maybe also the general R2 statistic?)



