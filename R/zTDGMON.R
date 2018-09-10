#------------------------------------------------------------------------------
# The function estimates the amount of TDG at the monitoring site.  
# Both TDG functions were originally written by Nick Beer at Columbia Basin
# Research (GasGen.R).  The current version is altered to handle vectorized 
# inputs and to include the lookup data within the function, so we don't need
# to call them from a .csv file.
# 
# To vectorize I removed the "if" statements which demand a rowwise/for loop
# proceedure, and instead combine inputs and parameters to run across input vectors.
#
# Author and Source: Nick Beer
# Modified by Ryan Kinzer
#------------------------------------------------------------------------------

zTDGMON <- function(project_code, forebay_gas, flow, spill_prop){
  #site="LWG",FBgas=10,spill=.15,flow=100, 
  # based on gas params and other inputs what is the gas the TDG monitoring site?
  # FBgas is the Forebay Gas TDG level ABOVE 100%
  
  df <- tibble(id = 1:length(project_code), project_code, forebay_gas, flow, spill_prop)
  
  coef_df <- tibble(project_code = c("BON",	"TDA",	"JDA",	"MCN",	"PRD",	"WAN",	"RIS",	"RRH",	"WEL",	"IHR",	"LMN",	"LGS",	"LWG",	"CHJ",	"DWR"),
                    mxfrac =	c(0,	0,	0.2070539,	0.1763421,	1,	1,	0.9633741,	1,	1,	0,	0,	0.3290093,	0.107742,	0.2358567,	1),
                    k =	c(0,	0,	0.3,	0.25,	0,	0,	1,	1,	1,	0.6,	1,	1,	1.75,	0,	0),
                    TDGsideDown = c("R"   ,"L"   ,"R"   ,"R"   ,"C"   ,"C"   ,"L"   ,"C"   ,"L"   ,"R"   ,"L"   ,"R"   ,"R"  , "R",  "C"),
                    spillside  = c("R"   ,"R"   ,"R"   ,"R"   ,"R"   ,"R"   ,"R"   ,"L"   ,"L"   ,"R"   ,"L"   ,"R"   ,"R"  , "R",  "L"))
  
  MONgas <- inner_join(df, coef_df, by = 'project_code') %>%
    mutate(Gspill = zTDGSpill(project_code, flow, spill_prop),
           MONgas = ifelse(TDGsideDown == spillside, 
                           Gspill*(spill_prop*(1+k) + mxfrac*(1-spill_prop) - mxfrac*spill_prop*k) + forebay_gas*(1-spill_prop*(1+k)-mxfrac*(1-spill_prop)+mxfrac*spill_prop*k),
                           Gspill*(spill_prop*(1+k) - spill_prop*mxfrac) + forebay_gas*(1-spill_prop*(1+k) + spill_prop*mxfrac))
           ) %>%
    arrange(id) %>%
    pull(MONgas)
  
  return(MONgas)
  
  # CAUTION: It is possible to ask for a flow that exceeds the powerhouse's hydraulic capacity. 
  # These formulas will compute a gas level, but it will be impossible to attain in the field.
  # Extra flow above the hydraulic capacity SHOULD be converted into spill.
  # This is not trapped in these computations. For example, IHR powerhouse capacity is 106 KCFS.
  # A flow of 200 with spill fraction of 0.15 will never happen there.
  
  # TEST
  # Compute the expected TDG at the monitoring site
  #print(zTDGMON("LWG",FBgas=10, spill=0.9, flow=100))
  
}