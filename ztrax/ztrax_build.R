#******************************************************************************
#*
#*     Building simple prototype dataset from ZTRAX
#*     Prototyping with NJ data
#*     To be generalized and merged with HMDA
#*
#*     (Much of this code comes from Skylar Olsen,
#*     see https://github.com/zillow-research/ztrax)
#******************************************************************************

read.ztrax <- function(state, rows2load = NULL, ztrax_version='20210405', skip=0){
  
  #load packages
  library(data.table)
  library(readxl)
  
  ## These lines set several options
  options(scipen = 999) # Do not print scientific notation
  options(stringsAsFactors = FALSE) ## Do not load strings as factors
  
  # Save directory where you've stored ZTRAX
  ztrax.dir <- paste0('/scratch/gpfs/mfineman/ztrax/', ztrax_version)
  
  #  Pull in layout information
  layoutZTrans <- read_excel(file.path(ztrax.dir, 'Layout.xlsx'), 
                             sheet = 2,
                             col_types = c("text", "text", "numeric", "text", "text"))
  
  ###############################################################################
  ###############################################################################
  #  Load transaction dataset.
  #     For now, just
  #      1) PropertyInfo table provided ImportParcelID to match transaction to assessor data loaded above
  #.     2) Main table in Ztrans database provides information on real estate events
  
  col_namesProp <- layoutZTrans[layoutZTrans$TableName == 'utPropertyInfo', c('FieldName', 'column_id')]
  col_namesMainTr <- layoutZTrans[layoutZTrans$TableName == 'utMain', c('FieldName', 'column_id')]
  
  ###############################################################################
  #   Load PropertyInfo table for later merge
  
  col_namesProp <- col_namesProp %>% filter(FieldName %in% c('TransId', 'PropertyHouseNumber', 'PropertyHouseNumber', 'PropertyHouseNumberExt', 'PropertySequenceNumber', 'PropertyAddressCensusTractAndBlock', 
                                                                  'PropertyAddressLatitude', 'PropertyAddressLongitude', 'PropertyStreetName', 'PropertyStreetPreDirectional',
                                                                  'PropertyStreetPostDirectional', 'PropertyStreetSuffix', 'PropertyFullStreetAddress',
                                                                  'PropertyBuildingNumber', 'PropertyAddressUnitDesignator', 'PropertyAddressUnitNumber',
                                                                  'PropertyCity', 'PropertyState', 'PropertyZip', 'PropertyZip4', 'PropertyAddressStndCode',
                                                                  'LegalLot', 'LegalOtherLot', 'LegalBlock', 'LegalSubdivisionName', 'LegalTract', 'LegalDistrict',
                                                                  'LegalMunicipality', 'LegalCity', 'LegalTownship', 'LegalSecTwnRngMer', 'LegalLotSize',
                                                                  'PropertySequenceNumber', 'PropertyAddressMatchcode', 'PropertyAddressGeoCodeMatchCode', 
                                                                  'ImportParcelID', 'PropertyAddressConfidenceScore', 'PropertyAddressCBSACode', 
                                                                  'PropertyAddressCBSADivisionCode', 'PropertyAddressMatchType', 'PropertyGeocodeQualityCode', 
                                                                  'PropertyAddressQualityCode', 'FIPS'))
  
  propTrans <- data.table::fread(file.path(ztrax.dir, state, 'ZTrans\\PropertyInfo.txt'),
                          nrows = ifelse(is.null(rows2load), Inf, rows2load),                    # this is set just to test it out. Remove when code runs smoothly.
                          sep = '|',
                          header = FALSE,
                          stringsAsFactors = FALSE,             
                          quote = "",                                # this tells R not to read quotation marks as a special symbol
                          skip = skip,
                          col.names = col_namesProp$FieldName,
                          select = col_namesProp$column_id)
  
  #names(propTrans) <- col_namesProp$FieldName
  
  propTrans <- as.data.table(propTrans)
  
  # Keep only one record for each TransID and PropertySequenceNumber. 
  # TransID is the unique identifier of a transaction, which could have multiple properties sequenced by PropertySequenceNumber. 
  # Multiple entries for the same TransID and PropertySequenceNumber are due to updated records.
  # The most recent record is identified by the greatest LoadID. 
  # **This may not be necessary for the published dataset since they published most updated record. 
  #setkeyv(propTrans, c("TransId", "PropertySequenceNumber", "LoadID"))
  #keepRows <- propTrans[ ,.I[.N], by = c("TransId", "PropertySequenceNumber")]
  #propTrans <- propTrans[keepRows[[3]], ]
  #propTrans[ , LoadID:= NULL]
  
  ## For now, just check that there are not multiple entries for any 
  ## TransID-PropertySequenceNumber combination. If there are, give warning.
  if (nrow(propTrans[, .N, by = c('TransId', 'PropertySequenceNumber')][
    N != 1]) != 0){
    warning(paste('Multiple entries in PropertyInfo for same TransID and PropertySequenceNumber
                   in state', state))
  }
  
  # Drop transactions of multiple parcels (transIDs associated with PropertySequenceNumber > 1)
  # ** Don't think we want to drop these but leaving here to revisit **
  #dropTrans <- unique(propTrans[PropertySequenceNumber > 1, TransId])
  #propTrans <- propTrans[!(TransId %in% dropTrans), ]   # ! is "not"
  
  #######################################################################################
  #  Load main table in Ztrans database, which provides information on real estate events
  
  col_namesMainTr <- col_namesMainTr %>% filter(FieldName %in% c('TransId', 'FIPS', 'State', 'County', 'DataClassStndCode', 'RecordingDate', 'ReRecordedCorrectionStndCode', 'PriorRecordingDate', 'PriorDocumentDate',
                                                                 'DocumentTypeStndCode', 'DocumentDate', 'SignatureDate', 'EffectiveDate', 'BuyerVestingStndCode', 'BuyerMultiVestingFlag', 'PartialInterestTransferStndCode',
                                                                 'PartialInterestTransferPercent', 'SalesPriceAmount', 'SalesPriceAmountStndCode', 'CityTransferTax', 'CountyTransferTax', 'StateTransferTax', 'TotalTransferTax',
                                                                 'IntraFamilyTransferFlag', 'TransferTaxExemptFlag', 'PropertyUseStndCode', 'AssessmentLandUseStndCode', 'OccupancyStatusStndCode', 'LegalStndCode', 'BorrowerVestingStndCode',
                                                                 'LenderName', 'LenderTypeStndCode', 'LenderIDStndCode', 'LenderDBAName', 'DBALenderTypeStndCode', 'DBALenderIDStndCode', 'LoanAmount', 'LoanAmountStndCode',
                                                                 'MaximumLoanAmount', 'LoanTypeStndCode', 'LoanTypeClosedOpenEndStndCode', 'LoanTypeFutureAdvanceFlag', 'LoanTypeProgramStndCode', 'LoanRateTypeStndCode',
                                                                 'LoanDueDate', 'LoanTermMonths', 'LoanTermYears', 'InitialInterestRate', 'ARMFirstAdjustmentDate', 'ARMFirstAdjustmentMaxRate', 'ARMFirstAdjustmentMinRate', 
                                                                 'ARMIndexStndCode', 'ARMAdjustmentFrequencyStndCode', 'ARMMargin', 'ARMInitialCap', 'ARMPeriodicCap', 'ARMLifetimeCap', 'ARMMaxInterestRate', 'ARMMinInterestRate',
                                                                 'InterestOnlyFlag', 'InterestOnlyTerm', 'PrepaymentPenaltyFlag', 'PrepaymentPenaltyTerm', 'BiWeeklyPaymentFlag', 'AssumabilityRiderFlag', 'BalloonRiderFlag', 'CondominiumRiderFlag', 
                                                                 'PlannedUnitDevelopmentRiderFlag', 'SecondHomeRiderFlag', 'OneToFourFamilyRiderFlag', 'LoanNumber'))
  
  trans <- data.table::fread(file.path(ztrax.dir, state, 'ZTrans\\Main.txt'),
                      nrows = ifelse(is.null(rows2load), Inf, rows2load),                    # this is set just to test it out. Remove when code runs smoothly.
                      sep = '|',
                      header = FALSE,
                      stringsAsFactors = FALSE,
                      quote = "",                           # this tells R not to read quotation marks as a special symbol
                      skip = skip,
                      col.names = col_namesMainTr$FieldName,
                      select = col_namesMainTr$column_id)
  
  #column names
  #names(trans) <- col_namesMainTr$FieldName
  
  trans <- as.data.table(trans)
  
  ## For now, just check that there are not multiple entries for any 
  ## TransID. If there are, give warning.
  if (nrow(trans[, .N, by = c('TransId')][
    N != 1]) != 0){
    warning(paste('Multiple entries in Main for same TransID
                   in state', state))
  }  
  
  ###############################################################################
  #   Merge previous two datasets together to form transaction table
  
  transComplete <- merge(propTrans, trans, by = c('TransId'))
  return(transComplete)
}

