#*******************************************************************************
#*
#*     hmda_lenders.R
#*     Builds a Respondent ID-Lender Name correspondence for HMDA data
#*     using panel and transmittal files
#*     Useful for merging HMDA transaction level data with other data
#*     That includes lender names
#*
#*******************************************************************************

clean_lenders <- function(year, hmda_dir){
  library(dplyr)
  library(readr)
  setwd(hmda_dir)
  
  #create filename template based on year
  filename.panel <- case_when(year %in% 2000:2003 ~ paste0('panel/HMS.U', year, '.PANEL'),
                        year == 2004 ~ 'panel/u2004pan.public.dat',
                        year %in% 2005:2006 ~ paste0('panel/PANEL.U.', year, '.DAT'),
                        year %in% 2007:2017 ~ paste0('panel/hmda_', year, '_panel'),
                        TRUE ~ '')
  filename.ts <- case_when(year %in% 2000:2003 ~ paste0('transmittal/HMS.U', year, '.TS'),
                           year == 2004 ~ 'transmittal/u2004ts.public.dat',
                           year %in% 2005:2006 ~ paste0('transmittal/TS.ULTIMATE.', year, '.DAT'),
                           year %in% 2007:2017 ~ paste0('transmittal/hmda_', year, '_transmittal_sheet'),
                           TRUE ~ '')
  
  #throw error if year is out of data range
  if (filename.panel == ''){
    stop('No data for year')
  }
  
  #if year is between 2000-2006, read in formatted files and save as CSVs
  if (year %in% 2000:2006){
    print(paste0('Reading in year: ', year))
    #column position specifications, depending on year
    if (year %in% 2000:2003){
      col.pos.panel <- fwf_cols(respondent_id = 10,
                                msa_number = 4,
                                agency_code = 1,
                                agency_group_code = 2,
                                resp_name = 30,
                                resp_city = 25,
                                resp_state = 2,
                                resp_statefp = 2,
                                total_assets = 10,
                                other_lender_code = 1,
                                parent_id = 10,
                                parent_name = 30,
                                parent_city = 25,
                                parent_state = 2,
                                as_of_year = 4)
      col.type.panel = cols(respondent_id = col_character(),
                            msa_number = col_character(),
                            agency_code = col_double(),
                            agency_group_code = col_character(),
                            resp_name = col_character(),
                            resp_city = col_character(),
                            resp_state = col_character(),
                            resp_statefp = col_character(),
                            total_assets = col_character(),
                            other_lender_code = col_double(),
                            parent_id = col_character(),
                            parent_name = col_character(),
                            parent_city = col_character(),
                            parent_state = col_character(),
                            as_of_year = col_double()
                          )
      col.pos.ts <- fwf_cols(as_of_year = 4,
                             agency_code = 1,
                             respondent_id = 10,
                             resp_name = 30,
                             resp_address = 40,
                             resp_city = 25,
                             resp_state = 2,
                             resp_zip = 10,
                             edit_status = 1,
                             tax_id = 10)
      col.type.ts <- cols(as_of_year = col_double(),
                          agency_code = col_double(),
                          respondent_id = col_character(),
                          resp_name = col_character(),
                          resp_address = col_character(),
                          resp_city = col_character(),
                          resp_state = col_character(),
                          resp_zip = col_character(),
                          edit_status = col_character(),
                          tax_id = col_character()
      )
    }else{
      col.pos.panel <- fwf_cols(respondent_id = 10,
                                 msa_number = 5,
                                 agency_code = 1,
                                 agency_group_code = 2,
                                 resp_name = 30,
                                 resp_city = 25,
                                 resp_state = 2,
                                 resp_statefp = 2,
                                 total_assets = 10,
                                 other_lender_code = 1,
                                 parent_id = 10,
                                 parent_name = 30,
                                 parent_city = 25,
                                 parent_state = 2,
                                 as_of_year = 4,
                                 resp_rssd = 10)
      col.type.panel <- cols(respondent_id = col_character(),
                              msa_number = col_character(),
                              agency_code = col_double(),
                              agency_group_code = col_character(),
                              resp_name = col_character(),
                              resp_city = col_character(),
                              resp_state = col_character(),
                              resp_statefp = col_character(),
                              total_assets = col_character(),
                              other_lender_code = col_double(),
                              parent_id = col_character(),
                              parent_name = col_character(),
                              parent_city = col_character(),
                              parent_state = col_character(),
                              as_of_year = col_double(),
                              resp_rssd = col_character()
        )
      col.pos.ts <- fwf_cols(as_of_year = 4,
                             agency_code = 1,
                             respondent_id = 10,
                             resp_name = 30,
                             resp_address = 40,
                             resp_city = 25,
                             resp_state = 2,
                             resp_zip = 10,
                             parent_name = 30,
                             parent_address = 40,
                             parent_city = 25,
                             parent_state = 2,
                             parent_zip = 10,
                             edit_status = 1,
                             tax_id = 10)
      col.type.ts <- 
        cols(as_of_year = col_double(),
              agency_code = col_double(),
              respondent_id = col_character(),
              resp_name = col_character(),
              resp_address = col_character(),
              resp_city = col_character(),
              resp_state = col_character(),
              resp_zip = col_character(),
              parent_name = col_character(),
              parent_address = col_character(),
              parent_city = col_character(),
              parent_state = col_character(),
              parent_zip = col_character(),
              edit_status = col_double(),
              tax_id = col_character()
        )
    }
    
    #read in panel
    panel <- read_fwf(filename.panel,
                      col_positions = col.pos.panel,
                      col_types = col.type.panel)
    #write panel
    write_csv(panel, 
              paste0('panel/hmda_', year, '_panel.csv'))
    
    #read transmittal
    ts <- read_fwf(filename.ts,
                   col_positions = col.pos.ts,
                   col_types = col.type.ts)
    #write transmittal
    write_csv(ts,
              paste0('transmittal/hmda_', year, '_transmittal_sheet.csv'))
  }else{
    #if 2007-2017, unzip files
    unzip(paste0(filename.panel, '.zip'), exdir = 'panel/')
    unzip(paste0(filename.ts, '.zip'), exdir = 'transmittal/')

    if(year %in% 2007:2009){
      col.names.pan <- c('as_of_year',
                         'respondent_id',
                         'agency_code',
                         'parent_id',
                         'parent_name',
                         'parent_city',
                         'parent_state',
                         'region', 
                         'total_assets',
                         'other_lender_code',
                         'resp_name',
                         'resp_address',
                         'resp_city',
                         'resp_state',
                         'resp_zip',
                         'tax_id')
    }else {
      col.names.pan <- c('as_of_year',
                          'respondent_id',
                          'agency_code',
                          'parent_id',
                          'parent_name',
                          'parent_city',
                          'parent_state',
                          'region', 
                          'total_assets',
                          'other_lender_code',
                          'resp_name',
                          'resp_city',
                          'resp_state',
                          'top_holder_rssd_id',
                          'top_holder_name',
                          'top_holder_city',
                          'top_holder_state',
                          'top_holder_country',
                          'resp_rssd_id',
                          'parent_rssd_id',
                          'resp_statefp')
    }
    if(year %in% 2007:2016){
      col.names.ts <- c('as_of_year',
                        'respondent_id',
                        'agency_code',
                        'tax_id',
                        'resp_name',
                        'resp_address',
                        'resp_city',
                        'resp_state',
                        'resp_zip',
                        'parent_name',
                        'parent_address',
                        'parent_city',
                        'parent_state',
                        'parent_zip',
                        'resp_name.pan',
                        'resp_city.pan',
                        'resp_state.pan',
                        'total_assets',
                        'other_lender_code',
                        'region',
                        'lar_count',
                        'validity_error')
    }else{
      col.names.ts <- c('as_of_year',
                        'respondent_id',
                        'agency_code',
                        'tax_id',
                        'resp_name',
                        'resp_address',
                        'resp_city',
                        'resp_state',
                        'resp_zip',
                        'parent_name',
                        'parent_address',
                        'parent_city',
                        'parent_state',
                        'parent_zip',
                        'lar_count')
    }
    
    
                        
    
    
    
    panel <- read_csv(paste0(filename.panel, '.csv'),
                      col_names = col.names.pan,
                      skip=1)  
    write_csv(panel, paste0(filename.panel, '.csv'))
    ts <- read_csv(paste0(filename.ts, '.csv'),
                   col_names = col.names.ts,
                   skip=1)
    write_csv(ts, paste0(filename.ts, '.csv'))
  }
}

get_lenders <- function(year, hmda_dir){
  library(data.table)
  setwd(hmda_dir)
  
  #read in panel
  panel <- fread(paste0('panel/hmda_', year, '_panel.csv')) %>% 
    select(as_of_year, agency_code, starts_with('resp'), starts_with('parent'))
  #read in transmittal
  ts <- fread(paste0('transmittal/hmda_', year, '_transmittal_sheet.csv')) %>%
    select(as_of_year, agency_code, starts_with('resp'), starts_with('parent'))
  
  #merge them so that we can get multiple lender name spellings and locations
  lenders <- full_join(panel, ts,
                       by = c('respondent_id', 'agency_code', 'as_of_year'),
                       suffix = c('_panel', '_ts')) %>% distinct()
  
  return(lenders)
}
