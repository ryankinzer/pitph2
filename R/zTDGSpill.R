#------------------------------------------------------------------------------
# The function estimates the amount of TDG generated from spill.  The output
# doesn't represent the amount of TGD being reported at monitoring sites.
# Monitoring site TDG is calculated with the companion function zTDGMON().
# Both TDG functions were originally written by Nick Beer at Columbia Basin
# Research (GasGen.R).  The current version is altered to handle vectorized 
# inputs and to include the lookup data within the function, so we don't need
# to call them from a .csv file.
# 
# To vectorize I removed the "if" statements which demand a rowwise/for loop
# proceedure, and instead combine inputs and parameters with to run across input vectors.
#
# Author and Source: Nick Beer
# Modified by Ryan Kinzer
#------------------------------------------------------------------------------

zTDGSpill <- function(project_code, flow, spill_prop){
  
  df <- tibble(id = 1:length(project_code), project_code, flow, spill_prop)
  
  coef_df <- tibble(project_code = c("BON",	"TDA",	"JDA",	"MCN",	"PRD",	"WAN",	"RIS",	"RRH",	"WEL",	"IHR",	"LMN",	"LGS",	"LWG",	"CHJ",	"DWR"),
                    EQN = c(62,	62,	62,	62,	30,	62,	62,	30,	62,	62,	30,	62,	62,	30,	30),
                    D0 =	c(16.16,	21.9,	11.04,	12.38,	34.9,	17.63,	21.6,	24.47,	20.56,	11.15,	22.12,	9.304,	7.007,	20.92,	36.65),
                    D1 =	c(0.02983,	0.02109,	0.05969,	0.04007,	-16.23,	0.08495,	0.007694,	-47.83,	0.05935,	0.1009,	-11.4,	0.1675,	0.2261,	-14.74,	-40.22),
                    D2 = c(0,	0,	0,	0,	-0.002783,	0,	0,	-0.2692,	0,	0,	-0.03437,	0,	0,	-0.01815,	-0.3211))
  
  Gspill <- inner_join(df, coef_df, by = 'project_code') %>%
    mutate(Gspill = ifelse(EQN == 62, 
                           D0 + D1*flow*spill_prop,
                           D0 + D1*exp(D2*flow*spill_prop))) %>%
    arrange(id) %>%
    pull(Gspill)
  
  return(Gspill) 
  
  # CAUTION: It is possible to ask for a flow that exceeds the powerhouse's hydraulic capacity. 
  # These formulas will compute a gas level, but it will be impossible to attain in the field.
  # Extra flow above the hydraulic capacity SHOULD be converted into spill.
  # This is not trapped in these computations. For example, IHR powerhouse capacity is 106 KCFS.
  # A flow of 200 with spill fraction of 0.15 will never happen there.
  
  # TEST
  # Compute the expected TDG in the spill water.
  #print(zTDGSpill("MCN",spill=0.45, flow=100))  
  
}