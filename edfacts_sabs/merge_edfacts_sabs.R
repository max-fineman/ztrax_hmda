#load libraries and data
library(sf)
library(tidyverse)
setwd('/scratch/gpfs/mfineman')
edfacts <- read_csv('edfacts/edfacts.csv', col_types = cols(.default = col_character()))

#read in SABS spatial files
sabs1516 <- st_read('sabs/SABS_1516/SABS_1516.shp')
sabs1314 <- st_read('sabs/SABS_1314/SABS_1314.shp')

#split edfacts into 2009-2014 and 2015-2018 (to merge with corresponding SABS files)
edfacts09to14 <- edfacts %>% filter(sch_yr %>% str_sub(1,2) %>% str_c('20', .) %>% as.numeric() <= 2014)
edfacts15to17 <- edfacts %>% filter(sch_yr %>% str_sub(1,2) %>% str_c('20', .) %>% as.numeric() > 2014)

#join with edfacts
scores_sf <- map2_dfr(.x = list(edfacts09to14, edfacts15to17),
                      .y = list(sabs1314, sabs1516),
                      .f = ~ left_join(.x, .y, 
                                       by = c('ncessch', 'leaid'),
                                       suffix = c('_edf', '_sabs')) %>% st_as_sf())

#for checking results
scores_sample <- sample_n(edfacts_sabs, 100) %>% select(stnam, schnam_edf, schnam_sabs, stAbbrev, sch_year)

scores_sf <- scores_sf %>% select(-Shape_Area)

#save results (errors here, need to debug)
st_write(scores_sf, 'edfacts_sabs/edfacts_sabs.shp', append = FALSE)

