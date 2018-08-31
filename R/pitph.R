#------------------------------------------------------------------------------
# Function calculates the estiamte PITPH based on CSS model coefficients.  
#
# Ryan N. Kinzer
# August 2018
#------------------------------------------------------------------------------

pitph <- function(x,
                  species = c('Chinook', 'Steelhead'), 
                  project = c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON'),
                  flow = NULL,
                  spill = NULL,
                  na.rm = TRUE){
  
   # if(!na.rm){
   # !is.null(flow)|| stop("flow is null")
   # is.numeric(flow) & flow >= 0||stop("flow is not a positive number")
   # }
   # 
   #  if(!na.rm){  
   # !is.null(spill)||stop("spill is null")
   # is.numeric(spill) & spill >= 0||stop("spill is not a positive number")
   #  }

  #------------------------------------------------------------------------------
  # Check flow and spill volumes and reset if out of compliance
  #------------------------------------------------------------------------------
  # 
   tmp_spill_prop <- ifelse(spill >= 0.0 & spill <= 1.0, spill, spill/flow)
   tmp_spill_vol <- flow*tmp_spill_prop
   tmp_spill <- tmp_spill_prop
   
  #------------------------------------------------------------------------------
  # Get model coefficients
  #------------------------------------------------------------------------------
    
fge <- data.frame(Chinook = c(0.78,	0.8,	0.8,	0.86,	0.81,	0.65),
            Steelhead = c(0.83, 0.87, 0.94, 0.95, 0.76, 0.87),
            row.names = c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA'))

coef <- list(Chinook = data.frame( # coefficients to estimate detection probs and produce CSS SPE curves
                  coef = c('Intercept', 'Flow', 'PropSpill', 'Flow*PropSpill', 'Weir'),
                  LWG = c(1.5651,	-0.0055,	-7.5713,	0.0237, 0),
                  LGS = c(0.8561,	0.0071,	-8.8597,	0.0185, 0),
                  LMN = c(-0.6181,	0.0107,	-7.379,	0.0176,	-0.2556),
                  IHR = c(-1.2703,	0.0158,	-5.0724, 0, 0),
                  MCN = c(0.8503,	0.0028,	-2.8578,	-0.0092, 0),
                  JDA = c(-0.9628,	0.0062,	-7.1784, 0,	-0.4641),
                  TDA = c(5.8182, -0.0138,	-21.4291,	0.0496, 0),
                  BON_cc = c(-0.5574, -0.0073, 0, 0, 0),
                  BON_sp = c(-4.8073, 0.0008,	10.2069, 0, 0)
                ),
             Steelhead = data.frame(
                  coef = c('Intercept', 'Flow', 'PropSpill', 'Flow*PropSpill', 'Weir'),
                  LWG = c(2.4229, -.0024, -7.5950, .0249, -.8620),
                  LGS = c(.4188, .0160, -7.8669, 0, 0),
                  LMN = c(-.8413, .0161, -4.9650, 0, -.6506),
                  IHR = c(-.5026, .0188, -7.0273, 0, 0),
                  MCN = c(-.5389, .0078, -5.1579, -.0067, -.17),
                  JDA = c(-1.3532, .0096, -8.2799, 0, -.2759),
                  TDA = c(6.4876, -.0189, -20.9678, .0517, 0),
                  BON_cc = c(1.7723, -.0123, 0, 0, 0),
                  BON_sp = c(-5.3714, .0031, 9.556, 0, 0)
             )
  )

if(project == 'BON'){
  
  mod_ests_cc <- coef[[species]][1,'BON_cc'] + coef[[species]][2,'BON_cc']*flow + coef[[species]][3,'BON_cc']*tmp_spill + coef[[species]][4,'BON_cc']*flow*tmp_spill + coef[[species]][5,'BON_cc']
  mod_ests_sp <- coef[[species]][1,'BON_sp'] + coef[[species]][2,'BON_sp']*flow + coef[[species]][3,'BON_sp']*tmp_spill + coef[[species]][4,'BON_sp']*flow*tmp_spill + coef[[species]][5,'BON_sp']
  inv_logit_cc <- exp(mod_ests_cc)/(1 + exp(mod_ests_cc))
  inv_logit_sp <- exp(mod_ests_sp)/(1 + exp(mod_ests_sp))
  pitph <- (1-inv_logit_cc)*(1-inv_logit_sp)
  
} else {
  mod_ests <- coef[[species]][1,project] + coef[[species]][2,project]*flow + coef[[species]][3,project]*tmp_spill + coef[[species]][4,project]*flow*tmp_spill + coef[[species]][5,project]
  inv_logit <- exp(mod_ests)/(1 + exp(mod_ests))
  
  if(project == 'TDA'){
    pitph <- inv_logit
  } else {
    pitph <- inv_logit/fge[project, species]
  }

}

return(pitph)

}
