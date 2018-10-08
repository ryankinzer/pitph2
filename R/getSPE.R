## This R script provides guidance for calculations of PITPH using SPE and sluiceway passage
##  relationships developed by NOAA Fisheries.

## Depends on the following input files:
##  1) "noaa_spe_params.csv" -- contains SPE parameters by species, dam, and reartype
##  2) "data/example_flow_data.csv" -- contains flow at LGS (Snake) and JDA (Columbia) for 2010 (low), 2014 (med), and 2017 (high)

#### NOTES:
##  All SPE models use the probit link function for the expected value of the response variable.
##  In R, the qnorm function can be used for the probit transformation and pnorm for the inverse probit.
##  The proportion of spill is also probit transformed, which forces the relationship between fish proportion
##    and spill proportion to go through (0,0) and (1,1).
##  The dams with RSW or TSW are assumed to have them turned on for PITPH calculations, and the regression parameters
##    reflect this.
##  There are only HW models for The Dalles and Bonneville.  It is therefore recommended to use the HW models for all sites.
##  The flow and spill vectors used here are only for the purpose of demonstrating the calculations.

## Prepared by:
## Jim Faulkner
## NOAA Fisheries, Northwest Fisheries Science Center, Seattle
## phone: 206-302-2463
## email: jim.faulkner@noaa.gov
## September 14, 2018

## Ryan N. Kinzer modified the script on 10/3/18
## Modifications included:
# changed values of species and dam; spelled out species and uppercase dam: project_code
# removed paramvec arguement and included it in the function - need to include params themselves
# changed flowvec to a single value to work with mutate
# changed spillvec to a single value to work with mutate
# returns a SPE values

## Function for calculating SPE from regression model parameters and inputs
getSPE <- function(species = c('Chinook', 'Steelhead'),
                   project_code = c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON'),
                   flow = NULL,
                   spill = NULL){

  # adjust spill values at boundaries for transformations
  spill[spill==1] <- 0.9999
  spill[spill==0] <- 0.0001  
  
  df <- tibble(species, project_code, flow, spill)
  # removed paramvec arguement and included the dataset in the function
  # changed flowvec to a single value to work with mutate
  # changed spillvec to a single value to work with mutate
  # returns a SPE values
  
  ## extract SPE parameters - USING ALL HW
  load('./data/noaa_spe_params.rda')
  
  pdat <- pdat %>% filter(reartype == 'hw')
  
  df <- left_join(df, pdat, by = c('species', 'project_code')) %>%
    mutate(qspe = intercept + qnorm(spill)*qnorm.pspill + flow.x*flow.y + qnorm.pspill.by.flow*flow.x*qnorm(spill),
           spe = pnorm(qspe))
  
  spe <- df %>% pull(spe)
  
  return(spe)
}
