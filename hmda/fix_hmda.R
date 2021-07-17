library(dplyr)
library(purrr)
library(data.table)
library(stringr)

fix_hmda <- function(year, data.dir='/scratch/gpfs/mfineman/hmda/lar/'){
  filename <- paste0('hmda_', year, '_nationwide_all-records_codes.csv')
  hmda <- fread(file=file.path(data.dir, filename))
  #fix var names
  #hmda <- hmda %>% rename(trans_year = as_of_year)
  
  #fix state and county codes
  hmda[, `:=`(state_code = str_pad(state_code, 2, 'left', '0'),
              county_code = str_pad(county_code, 3, 'left', '0'))]
  
  #create lender id
  hmda[, arid := str_c(agency_code, respondent_id)]
  
  #make sure loan_amount_000s is class numeric
  hmda[, loan_amount_000s := as.numeric(loan_amount_000s)]
  
  #loan amount expanded zeros
  hmda[, loan_amount := loan_amount_000s*1000]
  
  fwrite(hmda, file=file.path(data.dir, filename))
}

years <- 2000:2017
map(years, fix_hmda)