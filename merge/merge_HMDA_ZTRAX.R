#******************************************************************************
#*
#*      merge_HMDA_ZTRAX.R
#*      IN: raw .csv HMDA data files for give year, 
#*          raw .txt ZTRAX files for given state
#*          
#*      OUT: linked ZTRAX-HMDA file for one state-year, merged on year, 
#*      census tract, loan amount and lender name
#*
#******************************************************************************

#merge function
merge_HMDA_ZTRAX <- function(state, data.dir, code.dir, 
                             nrows = NULL, ztrax_version = '20210405', skip=0){
  
  #************************************************************
  #* Preliminaries: load libraries and set working directory
  #************************************************************
  #prelims
  library(data.table)
  library(dplyr)
  library(stringr)
  library(stringdist)
  library(tictoc)
  
  tic('read ZTRAX')
  print(paste('starting reading ZTRAX for', state))
  
  #source functions called in script
  source(paste0(code.dir, 'ztrax/ztrax_build.R'))
  source(paste0(code.dir, 'hmda/hmda_lenders.R'))
  
  #read in ztrax
  ztrax_full<- read.ztrax(state, rows2load = nrows, ztrax_version=ztrax_version, skip=skip)
  #fix date classes
  ztrax_full[, c('DocumentDate', 'SignatureDate', 'RecordingDate') := lapply(.SD, as.IDate), 
             .SDcols=c('DocumentDate', 'SignatureDate', 'RecordingDate')]
  #extract year from DocumentDate, 
  #if missing then SignatureDate, if missing then RecordingDate
  ztrax_full[, `:=` (trans_date = fifelse(!is.na(DocumentDate), 
                                          DocumentDate,
                                          fifelse(!is.na(SignatureDate),
                                                  SignatureDate,
                                                  RecordingDate)))]
  ztrax_full[, trans_year := year(trans_date)]
  
  #create ztrax id
  ztrax_full[, ztrax.id := paste0(TransId, PropertySequenceNumber)]
  
  #extract census tract
  ztrax_full[, `:=`(PropertyAddressCensusTractAndBlock = str_remove(PropertyAddressCensusTractAndBlock, '\\.'))]
  ztrax_full[PropertyAddressCensusTractAndBlock=='', PropertyAddressCensusTractAndBlock := NA]
  ztrax_full[, PropertyAddressCensusTractAndBlock := str_trim(PropertyAddressCensusTractAndBlock, 'both')]
  if (str_sub(state, start = 1, end=1) == '0'){
    ztrax_full[str_sub(PropertyAddressCensusTractAndBlock,1,1) != '0', PropertyAddressCensusTractAndBlock := paste0('0', PropertyAddressCensusTractAndBlock)]
  }
  ztrax_full[, `:=`(PropertyAddressCensusTractAndBlock = str_pad(PropertyAddressCensusTractAndBlock, 15, 'right', '0'))]
  ztrax_full[, census_tract_id := substr(PropertyAddressCensusTractAndBlock, 1, 11)]
  
  #drop rows with no census tract
  ztrax_full <- ztrax_full[!is.na(PropertyAddressCensusTractAndBlock)]
  
  #fix empty LoanAmountStndCode cells
  ztrax_full[LoanAmountStndCode == '', LoanAmountStndCode := NA]
  
  #drop any rows where LoanAmount is aggregated 
  # (indicated by a non-NA value for LoanAmountStndCode)
  ztrax_full <- ztrax_full[is.na(LoanAmountStndCode)]
  
  #fix var names
  ztrax_full <- ztrax_full %>% rename(loan_amount = LoanAmount)
  
  toc()
  
  stopifnot(class(ztrax_full) == c('data.table', 'data.frame'))
  
  
  for (year in 2000:2020){
    #benchmarking
    tic('script')
    print(paste('starting script for year', year, 'and state', state))
    
    #************************************************************
    #* Build HMDA
    #************************************************************
    
    #hmda filename
    hmda.file <- paste0('hmda_', year, '_nationwide_all-records_codes.csv')
    
    tic('read HMDA')
    print('starting reading hmda')
    
    #read in hmda
    hmda <- fread(file.path(data.dir, 'hmda', 'lar', hmda.file),
                  nrows = ifelse(is.null(nrows), Inf, nrows),
                  colClasses = list(character=c('census_tract_number',
                                                'county_code',
                                                'state_code',
                                                'arid')))[
                                                  #filter to only transactions in state    
                                                  state_code == state #& 
                                                  #only home purchases
                                                  #loan_purpose == 1 &
                                                  #only loans originated
                                                  #action_taken == 1 &
                                                  #owner occupied  
                                                  #owner_occupancy == 1
                                                  ]
    toc()
    
    #build full census tract code
    hmda[, `:=`(state_code = str_pad(state_code, 2, 'left', '0'),
                county_code = str_pad(county_code, 3, 'left', '0'))]
    hmda[, `:=`(census_tract_number = str_remove(census_tract_number, '\\.'))]
    hmda[, `:=`(census_tract_number = str_pad(census_tract_number, 6, 'left', '0'))]
    hmda[, census_tract_id := str_c(state_code, county_code, census_tract_number)]

    
    #pull in more HMDA lender info
    lenders <- get_lenders(year=year,
                           hmda_dir = file.path(data.dir, 'hmda')) %>% 
      select(arid, resp_name_panel, 
             starts_with('parent_id'), starts_with('parent_name'), resp_name_ts)
    
    #merge lender names into HMDA
    hmda <- merge.data.table(hmda, lenders, by = c('arid'),
                             all.x = T)
    
    #create hmda id
    if (year %in% 2000:2017){
      hmda[, `:=`(arid = formatC(arid, width=11, format='d', flag='0'),
                  sequence_number = formatC(sequence_number, width=7, format='d', flag='0'))]
    hmda[, `:=`(hmda.id = paste0(arid, sequence_number))]
    }else{
      hmda[, hmda.id := 1:nrow(hmda)]
    }
    #************************************************************
    #* Build ZTRAX
    #************************************************************
    
    #filter to current year
    ztrax <- ztrax_full[trans_year == year]
    #make trans_year class compatible for merge
    ztrax[, trans_year := as.integer(trans_year)]
    

    
    #************************************************************
    #* Merge HMDA and ZTRAX
    #************************************************************
    
    tic('merge')
    print('starting merge')
    #merge just on year, loan amount, and census tract
    mg <- merge.data.table(x = hmda, 
                           y = ztrax,
                           by = c('trans_year', 'loan_amount', 'census_tract_id'),
                           allow.cartesian = TRUE)
    
    #calculate string distances between ztrax lender name and 
    #panel/trasmittal sheet versions of HMDA lender name
    mg[, `:=`(lenderdist.panel = stringdist(resp_name_panel, LenderName, 
                                            method = 'jw', p = 0.1),
              lenderdist.ts = stringdist(resp_name_ts, LenderName,
                                         method = 'jw', p = 0.1))]
    
    #keep only matches where one of the distances is less than .2
    mg <- mg[lenderdist.panel < .2 | lenderdist.ts < .2]
    
    ## dealing with duplicate matches
    #choose the closest lender name match when a HMDA transaction matches to multiple ZTRAX transactions/properties
    mg <- mg %>% group_by(hmda.id) %>% slice_min(order_by = lenderdist.panel + lenderdist.ts, n=1, with_ties=F) %>% ungroup()
    
    #choose the closest lender name match when a ZTRAX transaction matches to multiple HMDA transactions
    mg <- mg %>% group_by(TransId) %>% slice_min(order_by = lenderdist.panel + lenderdist.ts, n=1, with_ties=F) %>% ungroup()
    
    toc()
    
    tic('write')
    print('start writing')
    #write merged data
    if (!dir.exists(file.path(data.dir, 'ztrax_hmda', year))) {dir.create(file.path(data.dir, 'ztrax_hmda', year))}
    if (!dir.exists(file.path(data.dir, 'ztrax_hmda', year, state))) {dir.create(file.path(data.dir, 'ztrax_hmda', year, state))}
    fwrite(mg, file.path(data.dir, 'ztrax_hmda', year, state, paste0('ztrax_hmda_', year, '_', state, '.csv')))
    toc()
    toc()
  }
  message <- paste0('Finished all years of merge for state ', state)
  return(message)
}