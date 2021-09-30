DATA = ../../data
TEMP = ../../gen/temp

all: $(DATA)/Listings.csv $(TEMP)/aggregated_df.csv

../../data/Listings.csv: download.R
	mkdir -p ../../data
	R --vanilla < download.R
	
../..gen/temp/aggregated_df.csv: ../../data/Listings.csv clean.R
	mkdir - p ../../gen/temp
	R --vanilla < clean.R