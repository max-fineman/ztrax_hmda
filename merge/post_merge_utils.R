#function to see for which states some year the merge did not run
states_not_run <- function(data.dir, ztrax_version='20210405'){
  states.in <- list.files(file.path(data.dir, 'ztrax', ztrax_version)) %>% str_subset('^\\d{2}$')
  states.out <- list.files(file.path(data.dir, 'ztrax_hmda')) %>% 
    str_subset('^\\d{4}$') %>%
    map(~ list.files(file.path(data.dir, 'ztrax_hmda', .x)))
  
  not_run <- map(states.out, ~ states.in[!states.in %in% .x]) %>% unlist() %>% unique()
  return(not_run)
}

states_no_matches <- function(data, data.dir, ztrax_version='20210405'){
  states.in <- list.files(file.path(data.dir, 'ztrax', ztrax_version)) %>% str_subset('^\\d{2}$')
  states.out <- data %>% count(state) %>% pull(state) %>% as.character() %>% str_pad(2, 'left', '0')
  missing <- states.in[!states.in %in% states.out]
  return(missing)
}


build_by_year <- function(year, data.dir){
  library(dplyr)
  library(data.table)
  library(purrr)
  states <- list.files(file.path(data.dir, 'ztrax_hmda', year))
  data <- map(states, 
              ~ mutate(fread(file=file.path(data.dir, 'ztrax_hmda', year, .x, 
                                            paste0('ztrax_hmda_', year, '_', .x, '.csv'))),
                       state = .x))
  data <- map(data, ~ mutate(.x, across(.fns=as.character)))
  data <- bind_rows(data)
}

build_by_state <- function(state, data.dir){
  library(dplyr)
  library(data.table)
  years <- list.files(file.path(data.dir, 'ztrax_hmda'))
  data <- data.frame()
  for (year in years){
    if (dir.exists(file.path(data.dir, 'ztrax_hmda', year, state))){
      data.year <- fread(file.path(data.dir, 'ztrax_hmda', year, state,
                                   paste0('ztrax_hmda_', year, '_', state, '.csv')))
      data.year <- data.year %>% mutate(across(.fns=as.character))
      data <- bind_rows(data, data.year)
    }
  }
  return(data)
}

build_full <- function(data.dir){
  library(dplyr)
  library(purrr)
  years <- list.files(file.path(data.dir, 'ztrax_hmda')) %>% str_subset('^\\d{4}$')
  data <- map_dfr(years, build_by_year, data.dir=data.dir)
  return(data)
}