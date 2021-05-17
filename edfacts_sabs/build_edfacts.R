#**********************************************************************************************   
#*
#*     build_edfacts.R
#*     Build school assessment dataset linked to school attendance boundaries
#*     
#*     1. Read in individual by-year math and RLA test results
#*     2. Clean, wrangle, and extract 5th grade math and RLA pct proficiencies at school level
#*     3. Clean scores (remove/adjust results that were suppressed for privacy reasons)
#*     4. Calculate avg proficiency pct across both subjects
#*     5. Read in school attendance boundary file (SABS) and merge with assessment data
#*     
#**********************************************************************************************   

#preliminaries
library(tidyverse)
setwd('/scratch/gpfs/mfineman')

#create function to read and clean files by year (for vectorized call later)
clean_by_year <- function(year) {
  #filename for year
  filename <- paste0('-achievement-sch-sy', year, '-', (year + 1) %>% as.character() %>% str_sub(3, 4), '.csv')
  
  #read in math and standardize var names
  math <- read_csv(paste0('edfacts/math', filename), guess_max = 10000,
                   col_types = cols(.default = col_character())) %>%
    rename_with(tolower) %>%
    rename(schnam = matches('schnam\\d+')) %>%
    rename(leanm = matches('leanm\\d+'))
  
  #read in rla and standardize var names
  rla <- read_csv(paste0('edfacts/rla', filename), guess_max = 10000,
                  col_types = cols(.default = col_character())) %>%
    rename_with(tolower) %>%
    rename(schnam = matches('schnam\\d+')) %>%
    rename(leanm = matches('leanm\\d+'))
  
  
  #merge math and rla assessment data
  full <- full_join(math, rla) %>%
    #gather test results spread across columns
    pivot_longer(cols = !any_of(c("stnam", "fipst", "leaid", "st_leaid", "leanm", 
                                  "ncessch", "st_schid","schnam", "date_cur")),
                 names_to = c('subgroup', 'subject', 'grade', 'metric', 'sch_yr'),
                 names_pattern = '(\\w{1,3})_(\\w{3})(\\d{2})(pctprof|numvalid)_(\\d{4})') %>%
    #extract only grade 5, all subgroups
    filter(subgroup == 'all', grade == '05') %>%
    #spread variables contained in 'metric' column
    pivot_wider(names_from = metric, values_from = value) %>% 
    #drop numvalid var
    select(-numvalid) %>%
    #spread mth and rla scores
    pivot_wider(names_from = subject, values_from = pctprof) %>%
    #drop suppressed data
    filter(!is.na(mth) | !is.na(rla), 
           mth != 'PS', mth != '.', mth != 'n/a',
           rla != 'PS', rla != '.', rla != 'n/a')
  
  #function to clean score intervals by assuming the score is the middle of the interval
  clean_scores <- function(score) {
    score <- case_when(
      str_starts(score, 'GE') ~ str_remove(score, 'GE') %>% paste0(., '-99'),
      str_starts(score, 'GT') ~ str_remove(score, 'GT') %>% paste0(., '-99'),
      str_starts(score, 'LE') ~ str_remove(score, 'LE') %>% paste0('00-', .),
      str_starts(score, 'LT') ~ str_remove(score, 'LT') %>% paste0('00-', .),
      TRUE ~ score
    )
    str_split(score, '-') %>% map(as.numeric) %>% map_dbl(mean)
  }
  
  #call clean scores on mth and rla variables and add avg score across subjects
  full %>% 
    mutate(across(c(mth, rla), clean_scores),
           mean_prof = (mth + rla) / 2)
}

#years to vectorize clean_by_year
years <- 2009:2017

#vectorized call to clean_by_year
edfacts <- map_dfr(years, clean_by_year)

#save results
write_csv(edfacts, 'edfacts/edfacts.csv')
