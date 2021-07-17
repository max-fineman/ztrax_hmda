#******************************************************************************
#*
#*     ztrax.unzip.R
#*     Extracting relevant tables from ZTRAX zip files
#*
#******************************************************************************

## prelims
library(data.table)
library(stringr)
library(purrr)
ztrax.dir <- '/scratch/gpfs/mfineman/ztrax/20210405'
setwd(ztrax.dir)

#list of .zip files to extract tables from
states <- list.files() %>% 
  stringr::str_subset('.zip$') %>% 
  stringr::str_sub(1,2)

#extract files by state
map(states, ~ unzip(zipfile = paste0(.x, '.zip'), 
                  files = c('ZTrans\\Main.txt',
                            'ZTrans\\PropertyInfo.txt'),
                  exdir = .x))
