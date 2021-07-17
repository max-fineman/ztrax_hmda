#******************************************************************************
#*
#*      slurm_merge.R
#*      IN: raw .csv HMDA data files for 2000-2017, 
#*          raw .txt ZTRAX files
#*          
#*      OUT: linked ZTRAX-HMDA file, merged on year, census tract, loan amount
#*      and lender name
#*
#******************************************************************************

library(rslurm)
library(stringr)
library(dplyr)

#parameters for slurm_apply
#directory locations
data.dir <- '/scratch/gpfs/mfineman/'
code.dir <- '~/code/ztrax_hmda/'
ztrax_version <- '20210405'

#merge function
source(file.path(code.dir, 'merge', 'merge_HMDA_ZTRAX.R'))

#states
states <- list.files(file.path(data.dir, 'ztrax', ztrax_version)) %>% str_subset('\\d{2}$')

#years
#years <- 2000:2017

#create state parameter data frame
parameters <- data.frame(states)
names(parameters) <- c('state')

#include parameters for directories (same for all function calls)
parameters$data.dir <- data.dir
parameters$code.dir <- code.dir
parameters$ztrax_version <- ztrax_version

#create separate parameters DF for California
parameters.06 <- parameters %>% filter(state == '06')
parameters.12 <- parameters %>% filter(state %in% c('12'))
parameters.48 <- parameters %>% filter(state == '48')
parameters <- parameters %>% filter(!state %in% c('06', '12', '48'))


#parallelized call to merge function for every state-year
job <- slurm_apply(f = merge_HMDA_ZTRAX, 
                   params = parameters,
                   jobname = 'ztrax_hmda',
                   nodes = 16,
                   slurm_options = list(time = '3:00:00',
                                       'mem-per-cpu' = '50G',
                                       'mail-type' = list('end', 'fail'),
                                       'mail-user' = 'mfineman@princeton.edu'))

job.06 <- slurm_apply(f = merge_HMDA_ZTRAX, 
                      params = parameters.06,
                      jobname = 'ztrax_hmda.06',
                      nodes = 1,
                      slurm_options = list(time = '4:00:00',
                                           'mem' = '185G',
                                           'mail-type' = list('end', 'fail'),
                                           'mail-user' = 'mfineman@princeton.edu'))

job.12 <- slurm_apply(f = merge_HMDA_ZTRAX, 
                      params = parameters.12,
                      jobname = 'ztrax_hmda.12',
                      nodes = 1,
                      slurm_options = list(time = '2:00:00',
                                           'mem-per-cpu' = '60G',
                                           'mail-type' = list('end', 'fail'),
                                           'mail-user' = 'mfineman@princeton.edu'))

job.48 <- slurm_apply(f = merge_HMDA_ZTRAX, 
                      params = parameters.48,
                      jobname = 'ztrax_hmda.48',
                      nodes = 1,
                      slurm_options = list(time = '2:00:00',
                                           'mem-per-cpu' = '40G',
                                           'mail-type' = list('end', 'fail'),
                                           'mail-user' = 'mfineman@princeton.edu'))
