#******************************************************************************
#*
#*      merge_HMDA_ZTRAX.R
#*      IN: raw .csv HMDA data files for 2000-2017, 
#*          ___ ZTRAX files
#*          
#*      OUT: 
#*
#******************************************************************************

#************************************************************
#* Preliminaries: load libraries and set working directory
#************************************************************
library(tidyverse)
library(readr)
setwd('/scratch/gpfs/mfineman/hmda')

