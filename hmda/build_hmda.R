#******************************************************************************
#*
#*      build_hmda.R
#*      IN: raw individual HMDA data files for 2000-2017
#*            - 2000-2006 are fixed-width files with only HMDA codes
#*            - 2007-2017 are .csv files with HMDA codes and plain language labels
#*      OUT: single .csv file containing all HMDA data 2000-2018
#*
#******************************************************************************

#************************************************************
#* Preliminaries: load libraries and set working directory
#************************************************************
library(tidyverse)
library(readr)
setwd('/scratch/gpfs/mfineman/hmda')

#***********************************************************************
#* Read 2000-2006 fixed-width files using manually coded column widths
#***********************************************************************

#manually code irregular file names
filenames00_06 = c('HMS.U2000.LARS',
              'HMS.U2001.LARS.PUBLIC.DATA',
              'HMS.U2002.LARS',
              'HMS.U2003.LARS',
              'u2004lar.public.dat',
              'LARS.ULTIMATE.2005.DAT',
              'LARS.ULTIMATE.2006.DAT')

#code column widths separately for 2000-03 and 2004-06
#(coding system changed in 2004)
#and store resultant tibbles in named list
cols <- list('00-03' = fwf_cols(as_of_year = 4, respondent_id = 10,  agency_code = 1, loan_type = 1, 
                      loan_purpose = 1, owner_occupancy = 1, loan_amount_000s = 5, 
                      action_taken = 1, msamd = 4, state_code = 2, county_code = 3, 
                      census_tract_number = 7, applicant_race_1 = 1, co_applicant_race_1 = 1, 
                      applicant_sex = 1, co_applicant_sex = 1, applicant_income_000s = 4, 
                      purchaser_type = 1, denial_reason_1 = 1, denial_reason_2 = 1, 
                      denial_reason_3 = 1, edit_status = 1, sequence_number = 7),
             '04-06' = fwf_cols(as_of_year = 4, respondent_id = 10,  agency_code = 1, loan_type = 1, 
                      loan_purpose = 1, owner_occupancy = 1, loan_amount_000s = 5, 
                      action_taken = 1, msamd = 5, state_code = 2, county_code = 3, 
                      census_tract_number = 7, applicant_sex = 1, co_applicant_sex = 1, 
                      applicant_income_000s = 4, purchaser_type = 1, denial_reason_1 = 1, 
                      denial_reason_2 = 1, denial_reason_3 = 1, edit_status = 1, property_type = 1,
                      preapproval = 1, applicant_ethnicity = 1, co_applicant_ethnicity = 1,
                      applicant_race_1 = 1, applicant_race_2 = 1, applicant_race_3 = 1, 
                      applicant_race_4 = 1, applicant_race_5 = 1, co_applicant_race_1 = 1,
                      co_applicant_race_2 = 1, co_applicant_race_3 = 1, co_applicant_race_4 = 1,
                      co_applicant_race_5 = 1, rate_spread = 5, hoepa_status = 1, lien_status = 1,
                      sequence_number = 7))

#read then write function
read_write <- function(filename){
  #read
  hmda <- read_fwf(filename, col_positions = if_else(str_detect(filename, '200[0-3]'),
                                               '00-03', 
                                               '04-06') %>% cols[[.]], 
                   #column type specifications to fix parsing failures
                   col_types = cols(state_code = 'c',
                                    loan_amount_000s = 'c',
                                    county_code = 'c',
                                    applicant_income_000s = 'c',
                                    census_tract_number = 'c',
                                    msamd = 'c',
                                    applicant_race_3 = 'c',
                                    applicant_race_4 = 'c',
                                    applicant_race_5 = 'c',
                                    co_applicant_race_2 = 'c',
                                    co_applicant_race_3 = 'c',
                                    co_applicant_race_4 = 'c',
                                    co_applicant_race_5 = 'c',
                                    denial_reason_2 = 'c',
                                    denial_reason_3 = 'c'))
  #write
  write_csv(hmda, 
            paste0('hmda_', hmda$as_of_year[1], '_nationwide_all-records_codes.csv'))
}

#vectorized call to read_write function
map(.x = filenames00_06,
    .f = read_write)

