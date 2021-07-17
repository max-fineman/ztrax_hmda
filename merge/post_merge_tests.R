library(dplyr)
library(stringr)
library(purrr)
library(data.table)
library(rslurm)

source('merge/post_merge_utils.R')

job <- slurm_job('ztrax_hmda', 13)
job.06 <- slurm_job('ztrax_hmda06', 1)
job.12 <- slurm_job('ztrax_hmda12', 1)
job.48 <- slurm_job('ztrax_hmda48', 1)
results <- get_slurm_out(job)
results12 <- get_slurm_out(job.12)
results06 <- get_slurm_out(job.06)
results48 <- get_slurm_out(job.48)

#directory locations
data.dir <- '/scratch/gpfs/mfineman'
code.dir <- '~/code/ztrax_hmda/'
ztrax_version <- '20210405'

#read
data <- fread(file=file.path(data.dir, 'ztrax_hmda', 'full', 'ztrax_hmda_full.csv'))

#see matches by state
data %>% group_by(state) %>% summarize(n = n()) %>% View()

#check what states the merge didn't run for (i.e. there is no output file)
states_not_run(data.dir)
#check what states the merge didn't have any matches for (i.e. the output file is empty)
states_no_matches(data, data.dir)
