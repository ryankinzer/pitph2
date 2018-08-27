#------------------------------------------------------------------------------
# This script uses the queryRiverData function to gather hydrosystem data from
# the Columbia River Data in Real Time (DART) website.  After gathering all the
# data it organizes it into the long format.
#
# Author: Ryan N. Kinzer
# Created on: 7 July 2018
#------------------------------------------------------------------------------

library(purrr)
library(dplyr)

source('./R/queryRiverData.R')

dams <- c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON')
dams_list = as.list(dams)
names(dams_list) = dams

year_range <-2006:year(Sys.Date())
year_list = as.list(year_range)
names(year_list) = year_range

df <- NULL

for(i in 1:length(dams)){
  tmp <- purrr::map_dfr(.x = year_list, .id = 'Year',  #.x = dams_list, .y = year_list,
                 .f = function(x){
                   queryRiverData(site = dams[i], year = x, start_day = '03/01', end_day = '07/01') %>%  #input$project
                   mutate_all(as.character)}
  )
   df <- rbind(df, tmp)
}  

hydro_data <- df %>%
  gather(variable, value, Outflow:Elevation, na.rm = TRUE) %>%
  mutate(value = as.numeric(value),
         Date = ymd(Date),
         md = format(as.Date(Date), "%m/%d"),
         Date = as.POSIXct(md,format="%m/%d"),
         Day = yday(Date))

save(hydro_data, file = "./data/hydro_data.rda")