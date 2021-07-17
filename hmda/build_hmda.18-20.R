#******************************************************************************
#*
#*      build_hmda.18-20..R
#*      IN: raw individual HMDA .csv files for 2018-2020
#*      OUT: cleaned 2018-2020 HMDA .csv files formatted for merge with previous years
#*
#******************************************************************************

library(dplyr)
library(data.table)
library(jsonlite)
library(purrr)
library(stringr)

read_write <- function(year, data.dir='/scratch/gpfs/mfineman/hmda/'){
  filename <- paste0('hmda_', year, '_nationwide_all-records_codes.csv')
  hmda <- fread(file=file.path(data.dir, 'lar', filename))
  
  #fix fields
  hmda <- hmda %>% rename(trans_year = activity_year,
                          arid = lei,
                          owner_occupancy = occupancy_type)
  hmda[, county_code := as.character(county_code)]
  hmda[, county_code := str_pad(county_code, 5, 'left', '0')]
  hmda[, `:=`(census_tract_number = str_sub(census_tract, 6,11),
              state_code = str_sub(county_code, 1,2),
              county_code = str_sub(county_code, 3,5))]
  #write
  fwrite(hmda, file=file.path(data.dir, 'lar', filename))
}


data.dir <- '/scratch/gpfs/mfineman/hmda/'
years <- 2018:2020
for (year in years){read_write(year=year)}



