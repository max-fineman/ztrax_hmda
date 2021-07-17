#prelims
library(data.table)
library(dplyr)
library(stringr)
library(stringdist)

source('ztrax/ztrax_build.R')
source('hmda/hmda_lenders.R')

dir <- '/scratch/gpfs/mfineman'

#prototyping values (year 2002 in NJ)
year <- '2002'
state <- '34'
nrows <- 2000000
#hmda filename
hmda.file <- paste0('hmda_', year, '_nationwide_all-records_codes.csv')

#read in first nrows of 2002 hmda
hmda <- fread(file.path(dir, 'hmda', 'lar', hmda.file),
              nrows = nrows*2,
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
                  county_code, census_tract_number, sequence_number, agency_code)]
#loan amount expanded zeros
hmda[, loan_amount := loan_amount_000s*1000]

#pull in more HMDA lender info
lenders <- get_lenders(year=year,
                       hmda_dir = file.path(dir, 'hmda'))[
                         , .(respondent_id, agency_code, resp_name_panel, 
                         parent_id, parent_name, resp_name_ts)
                       ]

#merge lender names into HMDA
hmda <- merge.data.table(hmda, lenders, by = c('respondent_id', 'agency_code'),
                         all.x = T)

#create hmda id
hmda[, `:=`(respondent_id = formatC(respondent_id, width=10, format='d', flag='0'),
            sequence_number = formatC(sequence_number, width=7, format='d', flag='0'))]
hmda[, `:=`(hmda.id = paste0(respondent_id, agency_code, sequence_number))]

#read in ztrax
ztrax <- read.ztrax(state, rows2load = nrows)

#create ztrax id
ztrax[, ztrax.id := paste0(TransId, PropertySequenceNumber)]

#filter to current year
ztrax[, DocYear := substr(DocumentDate, 1, 4)]
ztrax <- ztrax[year, on = 'DocYear']
#make DocYear class compatible for merge
ztrax[, DocYear := as.integer(DocYear)]

#extract census tract
ztrax[, `:=`(CensusTract = substr(PropertyAddressCensusTractAndBlock, 6,12),
             CountyFIPS = substr(FIPS, 3,5))]

#fix empty LoanAmountStndCode cells
ztrax[LoanAmountStndCode == '', LoanAmountStndCode := NA]

#drop any rows where LoanAmount is aggregated 
# (indicated by a non-NA value for LoanAmountStndCode)
ztrax <- ztrax[is.na(LoanAmountStndCode)]

#test simple merge
test <- merge.data.table(x = hmda, 
                          y = ztrax,
                          by.x = c('as_of_year', 'loan_amount', 
                                   'county_code', 'census_tract_number',
                                   'resp_name_ts'),
                          by.y = c('DocYear', 'LoanAmount', 
                                   'CountyFIPS', 'CensusTract',
                                   'LenderName'))

test2 <- merge.data.table(x = hmda, 
                            y = ztrax,
                            by.x = c('as_of_year', 'loan_amount', 
                                     'county_code', 'census_tract_number'),
                            by.y = c('DocYear', 'LoanAmount', 
                                     'CountyFIPS', 'CensusTract'))

test2[, `:=`(lenderdist.panel = stringdist(resp_name_panel, LenderName, 
                                              method = 'jw', p = 0.1),
                lenderdist.ts = stringdist(resp_name_ts, LenderName,
                                           method = 'jw', p = 0.1))]

test2 <- test2[lenderdist.panel < .2 | lenderdist.ts < .2]

#test2 %>% select(hmda.id, respondent_id, resp_name_panel, resp_name_ts, LenderName, lenderdist.panel, lenderdist.ts, TransId, PropertySequenceNumber) %>% View

#dups <- test2 %>% count(hmda.id) %>% arrange(desc(n)) %>% filter(n > 1) %>% pull(hmda.id)
#test2 %>% filter(hmda.id %in% dups) %>% 
#  select(hmda.id, respondent_id, resp_name_panel, resp_name_ts, LenderName, lenderdist.panel, lenderdist.ts, TransId, PropertySequenceNumber) %>% View

#choose the closest lender name match when a HMDA transaction matches to multiple ZTRAX transactions/properties
test2 <- test2 %>% group_by(hmda.id) %>% slice_min(order_by = lenderdist.panel + lenderdist.ts, n=1, with_ties=F) %>% ungroup()

#choose the closest lender name match when a ZTRAX transaction matches to multiple HMDA transactions
test2 <- test2 %>% group_by(TransId) %>% slice_min(order_by = lenderdist.panel + lenderdist.ts, n=1, with_ties=F)