all: hw3.html

hw3.html: hw3.Rmd raster.Rdata nyc_merged_clean.Rdata
	Rscript -e "library(rmarkdown);render('hw3.Rmd')"

raster.Rdata:	SVM_nyc.R
	R --no-save < SVM_nyc.R

SVM_nyc.R: nyc_merged_clean.Rdata 
	R --no-save < nyc_merged_clean.Rdata 

nyc_merged_clean.Rdata: cleaning_nyc_311.R
	R --no-save < cleaning_nyc_311.R

clean: 
	rm -f hw3.html

.PHONY: all clean