### DPREP PROJECT - TEAM 3 - InsideAirbnb - DOWNLOAD FILE ###
## Downloads the raw listings data from InsideAirbnb 
## This file should store the data inside the data folder (is done by the makefile I think)



# Downloads the raw listing data for Amsterdam 
download.file('http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2021-08-06/data/listings.csv.gz', 'Listings.csv')

#...add the download links for the other cities we want to analyse... 




# I'm not sure whether we need to include this, maybe this step goes with the clean.R file. 
library(readr)
df<-read.csv('Listings.csv')
head(df)
View(df)

