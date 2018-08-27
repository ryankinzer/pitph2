#------------------------------------------------------------------------------
# This script calculates the season-wide PITph based on variable flows, fish 
# arrival and spill settings.

# Author: Ryan N. Kinzer
# Created on: 7 July 2018
#------------------------------------------------------------------------------

pitph_season <- function(flow_year = c('Low', 'Average', 'High'), Snake_spill, Columbia_spill){
  
  #source('./R/simFlow.R')
  #source('./R/pitph_dam.R')
  
  flow_year <- match.arg(flow_year)

  # Gather pitph estimates for Snake dams
  snake_dams <- c('LWG', 'LGS', 'LMN', 'IHR')

  snake_spill_df <- tibble(Site = snake_dams, prop_spill = Snake_spill)

  snake_pitph <- map_df(.x = snake_dams, .f = function(x){
  
    simFlow('Snake', flow_year) %>%
    mutate(Site = x) %>%
      left_join(snake_spill_df, by = 'Site') %>%
      mutate(PITPH = pitph_dam(x, inflow = sim_flow, prop_spill = prop_spill))
  })

  
  # Gather pitph estimates for Columbia dams
  col_dams <- c('MCN', 'JDA', 'TDA', 'BON')

  col_spill_df <- tibble(Site = col_dams, prop_spill = Columbia_spill)
  
  col_pitph <- map_df(.x = col_dams, .f = function(x){
    
      simFlow('Columbia', flow_year) %>%
      mutate(Site = x) %>%
      left_join(col_spill_df, by = 'Site') %>%
      mutate(PITPH = pitph_dam(x, inflow = sim_flow, prop_spill = prop_spill))
  })
  
  return(bind_rows(snake_pitph, col_pitph))

}
