### DPREP PROJECT - TEAM 3 - InsideAirbnb - DOWNLOAD FILE ###
## Downloads the raw listings data from InsideAirbnb 
## This file should store the data inside the data folder
# Run this script from the terminal using: R --vanilla < download.R

#To download data programmatically from the web; downloads a file from a URL and stores it on your local machine. 
download_data <- function(url, filename, filepath){ #create a function to download the data
  dir.create(filepath) #create a directory
  download.file(url=url, destfile = paste0(filepath,filename)) #download the file
}

#Downloading the raw listings data for Amsterdam
#REMEMBER: the filepath is dependent on the location from where your script is called, so the use of absolute directory names should be avoided so that the code remains portable to other computers and work environments!
donwload_data(url = 'http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2021-08-06/data/listings.csv.gz', filename='Listings.csv',filepath='data/')

#Opening the raw listings data for Amsterdam
airbnb<-read.table('http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2021-08-06/data/listings.csv.gz', sep =',', header=TRUE)





#(What the code looked like before:)
download.file('http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2021-08-06/data/listings.csv.gz', 'Listings.csv')

#...add the download links for the other cities we want to analyse... 




# I'm not sure whether we need to include this, maybe this step goes with the clean.R file. 
library(readr)
df<-read.csv('Listings.csv')
head(df)
View(df)

