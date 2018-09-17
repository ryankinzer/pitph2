#------------------------------------------------------------------------------
# This script summarizes the juvenile arrival data gathered after sourcing the 
# 03_gather_juvenile_arrival_data script.

# Author: Ryan N. Kinzer
# Created on: 14 August 2018
#------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(RSQLite)
library(DBI)

project_code = c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON')
juv_code = c('GRJ', 'GOJ','LMJ', 'ICH', 'MCJ', 'JDJ', 'TDA', 'BON')

code_df <- tibble(juv_code, project_code)
# create db connection
con <- dbConnect(SQLite(), dbname = './data/arrival.sqlite')

df <- dbGetQuery(con, 
                 paste0("select * from obs"))

df <- df %>%
  mutate_at(vars(matches(".time")), ymd_hms) %>%
  mutate(dam = ifelse(grepl("B", obs_site), "BON", obs_site),
         release_date = ymd(release_date),
         obs_year = year(obs_time),
         day = yday(obs_time),         
         species = ifelse(str_sub(sprrt,1,1) == 1, 'Chinook', 'Steelhead'),
         run = str_sub(sprrt,2,2),
         rear = str_sub(sprrt,3,3))

df <- left_join(df, code_df, by = c('dam' = 'juv_code')) %>%
  mutate(project_code = fct_inorder(project_code))

ggplot(df, aes(x = day, fill = species)) +
  geom_density(alpha = .5) +
  scale_fill_viridis_d() +
  facet_grid(project_code~year) +
  theme_bw()
