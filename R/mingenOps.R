#------------------------------------------------------------------------------
# Function binds project operational constraints with flow and deired 
# spill level data.
#
# Ryan N. Kinzer
# August 2018
#------------------------------------------------------------------------------

mingenOps <- function(x){

  #------------------------------------------------------------------------------
  # Create dataframe of true operation constriants and PITPH coefficients
  #------------------------------------------------------------------------------
  
  #df <- tibble(species, project_code, flow, spill)
  
  ops_df <- tibble(project_code = rep(c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON'),2),
                       species = rep(c('Chinook', 'Steelhead'), each = 8),
                        min_ph_flow = rep(c(11.8, 11.3, 10.8, 8.4, 50, 50, 50, 30),2), # Lower values from FOP Table 5
                       #min_ph_flow = rep(c(12.4, 11.6, 11.6, 9.1, 55, 55, 55, 35),2), # NOAA values - averaged from FOP Table 5
                          gas_cap_spill = rep(c(41, 40, 36, 85, 146, 146, 135, 130),2),
                          ph_capacity = rep(c(130, 130, 130, 106, 232, 322, 375, 288),2), # NOAA values - USACE manual (CR and Tribs. Review Study)
                          uncontrolled_spill = rep(c(171, 170, 166, 191, 378, 468, 510, 518),2),
                        fge = c(0.78,	0.8, 0.8,	0.86,	0.81,	0.65, 1, 1, 0.83, 0.87, 0.94, 0.95, 0.76, 0.87, 1, 1))
  
  # ops_df <- tibble(project_code = rep(c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON'),2),  # old values used from Jay
  #                  species = rep(c('Chinook', 'Steelhead'), each = 8),
  #                  min_ph_flow = rep(c(12, 12, 14.5, 10.5, 55, 51.2, 56, 42),2),
  #                  gas_cap_spill = rep(c(41, 40, 36, 85, 146, 146, 135, 130),2),
  #                  ph_capacity = rep(c(118, 118, 115.5, 95.5, 177, 270.8, 319, 349),2),
  #                  uncontrolled_spill = rep(c(171, 170, 166, 191, 378, 468, 510, 518),2),
  #                  fge = c(0.78,	0.8, 0.8,	0.86,	0.81,	0.65, 1, 1, 0.83, 0.87, 0.94, 0.95, 0.76, 0.87, 1, 1))
                       
  tmp_df <- left_join(x, ops_df, by = c('species', 'project_code')) %>%
    mutate(set_spill_prop = ifelse(spill >= 0.0 & spill <= 1.0, spill, spill/flow),
           set_spill_vol = flow*set_spill_prop,
           actual_spill_vol = ifelse(flow > (min_ph_flow + set_spill_vol), set_spill_vol, flow - min_ph_flow),
           actual_spill_vol = ifelse(flow < (ph_capacity + actual_spill_vol), actual_spill_vol, flow - ph_capacity),
           actual_spill_vol = ifelse(actual_spill_vol < 0, 0, actual_spill_vol),
           actual_spill_prop = actual_spill_vol/flow)
                       
return(tmp_df)
}
