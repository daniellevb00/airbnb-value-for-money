######################
### DOWNLOAD DATA ####
######################

# Creating the data folder
dir.create('../../data')

# creating a list with the download link and filenames of the raw data 
files = list(c(url='http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2021-09-07/data/listings.csv.gz',
               fn='listings-amsterdam.csv.gz'),
             c(url='http://data.insideairbnb.com/belgium/vlg/antwerp/2021-09-29/data/listings.csv.gz',
               fn='listings-antwerp.csv.gz'))

# looping over the list to download and save the file in the data folder
for (item in files) {
  
  download.file(item['url'], paste0('../../data/', item['fn']))
}

