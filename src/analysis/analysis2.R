### DPREP PROJECT - TEAM 3 - InsideAirbnb - ANALYSIS 2 FILE ###
## Should load the aggregated_df.csv file and then perform analysis on it, then export results. 

# ANALYSIS FOR AMSTERDAM

## RESEARCH QUESTION: HOW DO DIFFERENT ATTRIBUTE FEATURES INFLUENCE THE LISTING PRICE? (WHICH THE MOST?) 
## PART 2: AND DOES THE RELATION OF THESE ATTRIBUTE FEATURES WITH THE PRICE TRANSLATE TO HIGH PRICE/QUALITY RATIO REVIEW SCORES BY CONSUMERS? 

# Here we will focus on the second question, file 'analysis' will focus on the first question. 

# We want to assess the price/quality ratio review score; rev_value, to see whether the how hosts determine their prices is perceived as reasonable by consumers. High price/quality ratings would indicate that consumers too find it reasonable that a private room also cost more relatively to a shared room etc. 
# Moreover, we want to compare the average rev_value scores between countries in Europe (or maybe we decide that it isn't duable for multiple cities and that we should just focus on one and extend on it more?)

## STEP 0: STARTING-UP
# Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
# Load the aggregated_df.csv which is the output of the cleaning file (fix this in the makefile)
aggregated1<-read.csv('aggregated_df.csv') 

## STEP 1: COMPUTE MEAN REV_VALUE RATINGS
# Calculate the mean pqratio rating for all observations of the dataset: for this city (returns 1 value)
aggregated1 %>% summarize(mean_pqratio = mean(rev_value)) 
# Calculate the mean pqratio rating for different listing price categories: relatively cheap, moderate, high price.  
aggregated1 %>% filter(price<x) %>% summarize(mean_pqratio=mean(rev_value)) #relatively cheap price
aggregated1 %>% filter(price>x & price<y) %>% summarize(mean_pqratio=mean(rev_value)) #relatively moderate price
aggregated1 %>% filter(price>x) %>% summarize(mean_pqratio=mean(rev_value)) #relatively high price
# Calculate the mean pqratio rating for different room_types: which types of rooms have the best quality/price ratio? 
aggregated1 %>% group_by(room_type) %>% summarize(mean_pqratio=mean(rev_value))
# Calculate the mean pqratio rating for different hosts: which hosts price their listings the best, are these hosts the ones with the most listings provided or who only rent one room?
aggregated1 %>% group_by(host_id) %>% summarize(mean_pqratio=mean(rev_value)) %>% n() 
# Make a list of the listings, with their pqratio rating, sorted based on the number of reviews the listing got. 
aggregated1 %>% select(id, rev_value, n_reviews, price) %>% order(rev_value, n_reviews)

## STEP 2: REPEAT THIS FOR ALL CITIES WE WANT TO ANALYSE (EUROPE)
# Put these results together in one dataset/dataframe, with a column for country, mean(rev_rating), mean(price)? 
# Then we maybe create a histogram for all cities, with the city names on the x-axis and the mean(rev_rating) of that city on the y-axis, and then sort deze bars from large to small (highest rev-value to lowest)
...
# Try this too for the other values we calculated for this city, and compare them: which things catch our eye? what is remarkable?


