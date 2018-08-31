#------------------------------------------------------------------------------
# Function binds project operational constraints with flow and deired 
# spill level data.
#
# Ryan N. Kinzer
# August 2018
#------------------------------------------------------------------------------

pitphOps <- function(x, spp, project, flow, spill){

  #------------------------------------------------------------------------------
  # Create dataframe of true operation constriants and PITPH coefficients
  #------------------------------------------------------------------------------
  
  ops_df <- tibble(project_code = rep(c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON'),2),
                       species = rep(c('Chinook', 'Steelhead'), each = 8),
                       min_ph_flow = rep(c(12, 12, 14.5, 10.5, 55, 51.2, 56, 42),2),
                          gas_cap_spill = rep(c(41, 40, 36, 85, 146, 146, 135, 130),2),
                          ph_capacity = rep(c(118, 118, 115.5, 95.5, 177, 270.8, 319, 349),2),
                          uncontrolled_spill = rep(c(171, 170, 166, 191, 378, 468, 510, 518),2))
                       
  tmp_df <- left_join(x, ops_df, by = c('species', 'project_code')) %>% #
    mutate(set_spill_prop = ifelse(spill >= 0.0 & spill <= 1.0, spill, spill/flow),
           set_spill_vol = flow*set_spill_prop,
           actual_spill_vol = ifelse(flow > (min_ph_flow + set_spill_vol), set_spill_vol, flow - min_ph_flow),
           actual_spill_vol = ifelse(flow < (ph_capacity + actual_spill_vol), actual_spill_vol, flow - ph_capacity),
           actual_spill_vol = ifelse(actual_spill_vol < 0, 0, actual_spill_vol),
           actual_spill_prop = actual_spill_vol/flow)
                       
return(tmp_df)
}
