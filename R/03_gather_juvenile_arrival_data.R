#------------------------------------------------------------------------------
# This script uses the queryJuvenilePIT function to gather juvenile arrival
# dates from the Columbia River Data in Real Time (DART) website.  After
# gathering all the data it then saves it as an .Rda file for summarizing.
#
# Author: Ryan N. Kinzer
# Created on: 7 July 2018
#------------------------------------------------------------------------------

library(purrr)
library(tidyverse)
library(lubridate)
library(RSQLite)
library(DBI)

source('./R/queryJuvenilePIT.R')

# run query to get proper table format and field names
tmp_dat <- queryJuvenilePIT(species = 'Chinook',
                            run = 'Spring',
                            rear_type = 'Wild',
                            life_stage = 'Juvenile',
                            site = 'LWG',
                            year = 2018,
                            start_day = '03/1',
                            end_day = '07/01')

tmp_dat <- mutate_all(tmp_dat, as.character)

field_names <- str_c(names(tmp_dat), ' TEXT', collapse = ', ')

#------------------------------------------------------------------------------
# Iniitalize data base for storing data using field names collected above
#------------------------------------------------------------------------------

con <- dbConnect(SQLite(), dbname = './data/arrival.sqlite')

# create table with proper format
dbSendQuery(con,
            paste("CREATE TABLE obs (", field_names, ")"))

#dbWriteTable(con, "obs", tmp_dat, append = TRUE)

#------------------------------------------------------------------------------
# Set up lists of species, dams and years to gather all data
#------------------------------------------------------------------------------
spp <- c('Chinook', 'Steelhead')

dams <- c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON')
#dams_list = as.list(dams)
#names(dams_list) = dams

year_range <- 2006:year(Sys.Date())
year_list = as.list(year_range)
names(year_list) = year_range

for(i in 1:length(dams)){
  for(j in 1:length(spp)){
  #tmp <- purrr::map_dfr(.x = year_list, .id = 'Year',  #.x = dams_list, .y = year_list,
  #                      .f = function(x){
    for(l in 1:length(year_range)){
   tmp <- queryJuvenilePIT(species = spp[j],
                                           run = 'All',
                                           rear_type = 'Wild',
                                           life_stage = 'Juvenile',
                                           site = dams[i],
                                           year = year_range[l],
                                           start_day = '03/01',
                                           end_day = '07/01')

  dbWriteTable(con, "obs", tmp, append = TRUE)
  #df <- rbind(df, tmp)
    }
  }  
}





#save(all_will_juv, file = "./data/wild_arrival_data.rda")
