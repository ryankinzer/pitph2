#------------------------------------------------------------------------------
# This script summarizes the juvenile arrival data gathered after sourcing the 
# 03_gather_juvenile_arrival_data script.

# Author: Ryan N. Kinzer
# Created on: 14 August 2018
#------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)

load("./data/wild_arrival_data.rda")

names(df)

df <- all_will_juv %>%
  mutate(species = ifelse(str_sub(sprrt,1,1) == 1, 'Chinook', 'Steelhead'),
         run = str_sub(sprrt,2,2),
         rear = str_sub(sprrt,3,3),
         day = yday(obs_time)) 

rm(all_will_juv)

df %>%
  group_by(species, obs_site, year) %>%
  summarise(nfish = n())

ggplot(df, aes(x = day, fill = species)) +
  geom_histogram(alpha = .5) +
  scale_fill_viridis_d() +
  #facet_grid(obs_site~year) +
  theme_bw()
