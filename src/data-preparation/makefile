DATA = ../../data
TEMP = ../../gen/data-preparation/temp
GEN = ../../gen/data-preparation/data

all: $(DATA)/listings-amsterdam.csv.gz $(DATA)/listings-antwerp.csv.gz $(TEMP)/ams_amenities.csv $(TEMP)/ams_amenities_dv.csv $(GEN)/ams_complete.csv

$(DATA)/listings-amsterdam.csv.gz $(DATA)/listings-antwerp.csv.gz: download.R	
		R --vanilla < download.R 

$(TEMP)/ams_amenities.csv: 1_data_transformation.R
		R --vanilla < 1_data_transformation.R

$(TEMP)/ams_amenities_dv.csv: $(TEMP)/ams_amenities.csv 2_data_cleaning.R
		R --vanilla < 2_data_cleaning.R
	
$(GEN)/ams_complete.csv: $(TEMP)/ams_amenities_dv.csv 3_data_exploration.R
		R --vanilla < 3_data_exploration.R