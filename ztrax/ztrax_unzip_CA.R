#******************************************************************************
#*
#*     ztrax_unzip_CA.R
#*     Extracting relevant tables from ZTRAX California zip files
#*
#******************************************************************************

## prelims
library(data.table)
library(stringr)
library(purrr)
ztrax.dir <- '/scratch/gpfs/mfineman/ztrax/20201012'
setwd(ztrax.dir)

#FIPS code for CA is 06
state <- '06.copy'

#extract files for CA
unzip(zipfile = paste0(state, '.zip'), 
      files = c('ZTrans\\Main.txt',
                'ZTrans\\PropertyInfo.txt'),
      exdir = state)