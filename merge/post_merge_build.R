#******************************************************************************
#*
#*      post_merge.R
#*      IN: individual state-year linked HMDA-ZTRAX files, 
#*          
#*      OUT: binded dataframes of all matches from all states and all years
#*
#******************************************************************************

library(dplyr)
library(stringr)
library(purrr)
library(data.table)
library(rslurm)

#directory locations
data.dir <- '/scratch/gpfs/mfineman'
code.dir <- '~/code/ztrax_hmda/'

source(paste0(code.dir, 'merge/post_merge_utils.R'))


ztrax_version <- '20210405'

data <- build_full(data.dir) 
fwrite(data, file.path(data.dir, 'ztrax_hmda', 'full', 'ztrax_hmda_full.csv'))




