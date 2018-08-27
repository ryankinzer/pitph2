# Power house PITPH function for single dam

pitph_dam <- function(dam = c('LWG', 'LGS', 'LMN', 'IHR', 'MCN', 'JDA', 'TDA', 'BON'),
                      inflow, prop_spill){
  
  dam = match.arg(dam)
  
  fge <- c(0.78,	0.8,	0.8,	0.86,	0.81,	0.65)
  
  LWG <- c(1.5651,	-0.0055,	-7.5713,	0.0237, 0)
  LGS <- c(0.8561,	0.0071,	-8.8597,	0.0185, 0)
  LMN <- c(-0.6181,	0.0107,	-7.379,	0.0176,	-0.2556)
  IHR <- c(-1.2703,	0.0158,	-5.0724, 0, 0)
  MCN <- c(0.8503,	0.0028,	-2.8578,	-0.0092, 0)
  JDA <- c(-0.9628,	0.0062,	-7.1784, 0,	-0.4641)
  TDA <- c(5.8182, -0.0138,	-21.4291,	0.0496, 0)
  BON_cc <- c(-0.5574, -0.0073, 0, 0, 0)
  BON_sp <- c(-4.8073, 0.0008,	10.2069, 0, 0)
  

  if(dam == 'LWG'){
    mod_ests <- LWG[1] + LWG[2]*inflow + LWG[3]*prop_spill + LWG[4]*inflow*prop_spill + LWG[5]
    inv_logit <- exp(mod_ests)/(1 + exp(mod_ests))
    pitph <- inv_logit/fge[1]
  }

  if(dam == 'LGS'){
    mod_ests <- LGS[1] + LGS[2]*inflow + LGS[3]*prop_spill + LGS[4]*inflow*prop_spill + LGS[5]
    inv_logit <- exp(mod_ests)/(1 + exp(mod_ests))
    pitph <- inv_logit/fge[1]
  }
  
  if(dam == 'LMN'){
    mod_ests <- LMN[1] + LMN[2]*inflow + LMN[3]*prop_spill + LMN[4]*inflow*prop_spill + LMN[5]
    inv_logit <- exp(mod_ests)/(1 + exp(mod_ests))
    pitph <- inv_logit/fge[1]
  }
  
  if(dam == 'IHR'){
    mod_ests <- IHR[1] + IHR[2]*inflow + IHR[3]*prop_spill + IHR[4]*inflow*prop_spill + IHR[5]
    inv_logit <- exp(mod_ests)/(1 + exp(mod_ests))
    pitph <- inv_logit/fge[1]
  }
  
  if(dam == 'MCN'){
    mod_ests <- MCN[1] + MCN[2]*inflow + MCN[3]*prop_spill + MCN[4]*inflow*prop_spill + MCN[5]
    inv_logit <- exp(mod_ests)/(1 + exp(mod_ests))
    pitph <- inv_logit/fge[1]
  }
  
  if(dam == 'JDA'){
    mod_ests <- JDA[1] + JDA[2]*inflow + JDA[3]*prop_spill + JDA[4]*inflow*prop_spill + JDA[5]
    inv_logit <- exp(mod_ests)/(1 + exp(mod_ests))
    pitph <- inv_logit/fge[1]
  }
  
  if(dam == 'TDA'){
    mod_ests <- TDA[1] + TDA[2]*inflow + TDA[3]*prop_spill + TDA[4]*inflow*prop_spill + TDA[5]
    inv_logit <- exp(mod_ests)/(1 + exp(mod_ests))
    pitph <- inv_logit
  }
  
  if(dam == 'BON'){
    mod_ests_cc <- BON_cc[1] + BON_cc[2]*inflow + BON_cc[3]*prop_spill + BON_cc[4]*inflow*prop_spill + BON_cc[5]
    mod_ests_sp <- BON_sp[1] + BON_sp[2]*inflow + BON_sp[3]*prop_spill + BON_sp[4]*inflow*prop_spill + BON_sp[5]
    inv_logit_cc <- exp(mod_ests_cc)/(1 + exp(mod_ests_cc))
    inv_logit_sp <- exp(mod_ests_sp)/(1 + exp(mod_ests_sp))
    pitph <- (1-inv_logit_cc)*(1-inv_logit_sp)
  }
  
  return(pitph)
}
