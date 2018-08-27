# Power house PITPH function for total system


pitph_mods <- function(river_flow, prop_spill){

  #river_flow <- match.arg(river_flow)
  # spill_volume is a vector of targeted spill volume at each of the 4 Snake and Columnbia River FCRPS projects
  
  # if(!match.arg(river_flow)){
  #   cat('Designated river flow must be one of three values: "low", "medium" or "high"')
  # }

  inflow_ls <- list(low = c(60, 60, 60, 60, 200, 200, 200, 200, 200),
                    medium = c(80, 80, 80, 80, 250, 250, 250, 250, 250),
                    high = c(100, 100, 100, 100, 300, 300, 300, 300, 300))
  
  inflow <- c(river_flow, river_flow[8]) #inflow_ls[[river_flow]]
  
  #prop_spill <- c(1, 1, 1, 1, .75, .5, .5, .75) # need to commment out
  
  spill_volume <- prop_spill * inflow[-9] # not needed
  
  prop_spill <- c(prop_spill, prop_spill[8])

  fge <- c(0.78,	0.8,	0.8,	0.86,	0.81,	0.65)
  
  mod_loc <- c('Lower Granite', 'Little Goose', 'Lower Monumental', 'Ice Harbor', 'McNary', 'John Day', 'Dalles', 'Bonneville')

  b0 <- c(1.5651, 0.8561, -0.6181, -1.2703, 0.8503, -0.9628, 5.8182, -0.5574, -4.8073) # intercept
  b1 <- c(-0.0055,	0.0071,	0.0107,	0.0158,	0.0028,	0.0062,	-0.0138,	-0.0073,	0.0008) # flow
  b2 <- c(-7.5713,	-8.8597,	-7.379,	-5.0724,	-2.8578,	-7.1784,	-21.4291,	0,	10.2069) # prop_spill
  b3 <- c(0.0237,	0.0185,	0.0176,	0,	-0.0092,	0,	0.0496,	0,	0) # flow:spill interaction
  b4 <- c(0,	0,	-0.2556,	0,	0,	-0.4641,	0,	0,	0) # weir

  mod_ests <- b0 + (b1*inflow) + (b2*prop_spill) +  (b3*inflow*prop_spill) + b4
  inv_logit <- exp(mod_ests)/(1 + exp(mod_ests))
  
  pitph <- c(inv_logit[1:6]/fge, inv_logit[7], (1-inv_logit[8])*(1-inv_logit[9]))
  
  project_tab <- data.frame(Project = mod_loc, Inflow = inflow[-9], Spill = spill_volume,
                       Spill_Proportion = prop_spill[-9], FGE = c(fge,rep(NA,2)),
                       PITPH = pitph, SPE = 1-pitph)
  
  sum_tab <- data.frame(Total_PITPH = sum(project_tab$PITPH), Average_SPE = mean(project_tab$SPE))
  
  output <- list(project_tab, sum_tab)
  
  return(output)
  
} # close function


  
  