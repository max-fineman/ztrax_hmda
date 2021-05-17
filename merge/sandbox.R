#prelims
library(data.table)
library(dplyr)

source('~/code/ztrax_hmda/ztrax_build.R')

dir <- '/scratch/gpfs/mfineman'
setwd(dir)

#prototyping values (year 2002 in NJ)
year <- '2002'
state <- '34'
nrows <- 1000000
#hmda filename
hmda.file <- paste0('hmda_', year, '_nationwide_all-records_codes.csv')

#read in first nrows of 2002 hmda
hmda <- fread(file.path('hmda', 'lar', hmda.file),
              nrows = nrows,
              colClasses = list(character=c('census_tract_number',
                                            'county_code',
                                            'state_code')))[
                #filter to only transactions in state
                #only home purchases
                #only loans originated
                #owner occupied                              
                state_code == state & 
                  loan_purpose == 1 &
                  action_taken == 1 &
                  owner_occupancy == 1,
                #select merging variables
                .(as_of_year, respondent_id, loan_amount_000s, state_code,
                  county_code, census_tract_number, sequence_number)]
#loan amount expanded zeros
hmda[, loan_amount := loan_amount_000s*1000]
#add 


ztrax <- read.ztrax(state, rows2load = nrows)

#filter to current year
ztrax[, DocYear := substr(DocumentDate, 1, 4)]
ztrax <- ztrax[year, on = 'DocYear']
#make DocYear class compatible for merge
ztrax[, DocYear := as.integer(DocYear)]

#extract census tract
ztrax[, `:=`(CensusTract = substr(PropertyAddressCensusTractAndBlock, 6,12),
             CountyFIPS = substr(FIPS, 3,5))]



#test simple merge
test <- merge.data.table(x = hmda, 
                          y = ztrax,
                          by.x = c('as_of_year', 'loan_amount', 
                                   'county_code', 'census_tract_number'),
                          by.y = c('DocYear', 'LoanAmount', 
                                   'CountyFIPS', 'CensusTract'))


test[, .(respondent_id, LenderName)] %>% distinct(respondent_id, LenderName)
