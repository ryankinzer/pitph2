#------------------------------------------------------------------------------
# This script uses the queryJuvenilePIT function to gather juvenile arrival
# dates from the Columbia River Data in Real Time (DART) website.  After
# gathering all the data it then saves it as an .Rda file for summarizing.
#
# Author: Ryan N. Kinzer
# Created on: 7 July 2018
#------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(RSQLite)
library(DBI)

source('./R/queryJuvenilePIT.R')

#------------------------------------------------------------------------------
# Iniitalize data base for storing data using field names collected above
#------------------------------------------------------------------------------

# create db connection
con <- dbConnect(SQLite(), dbname = './data/arrival.sqlite')

# create table with proper format
if(!dbExistsTable(con, 'obs')){
  
  # run query to get proper table format and field names
  tmp_dat <- queryJuvenilePIT(species = 'Chinook',
                              run = 'All',
                              rear_type = 'Wild',
                              life_stage = 'Juvenile',
                              site = 'BON',
                              year = 2006,
                              start_day = '05/1',
                              end_day = '05/30')
  
  # get names of fields returned from DART query
  field_names <- str_c(names(tmp_dat), ' TEXT', collapse = ', ')
  
  dbSendQuery(con, paste("CREATE TABLE obs (", field_names, ")"))
}

#------------------------------------------------------------------------------
# Set up lists of species, dams and years to gather all data
#------------------------------------------------------------------------------
spp <- c('Chinook', 'Steelhead')
dams <- c('LWG', 'LGS', 'LMN', 'IHA', 'MCN', 'JDA', 'BON')
year_range <- 2010:2018#2006:year(Sys.Date())

for(i in 1:length(dams)){
  for(j in 1:length(spp)){
    for(l in 1:length(year_range)){
   tmp <- queryJuvenilePIT(species = spp[j],
                                           run = 'All',
                                           rear_type = 'Wild',
                                           life_stage = 'Juvenile',
                                           site = dams[i],
                                           year = year_range[l],
                                           start_day = '03/01',
                                           end_day = '07/01')
   tmp <- mutate_all(tmp, as.character)

  dbWriteTable(con, "obs", tmp, append = TRUE)
    }
  }  
}

