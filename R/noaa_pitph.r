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




#### ---------------------------------
##         Input Files
#### ---------------------------------

## --- Read in regression parameters
pdat <- read.csv("./data/noaa_spe_params.csv", stringsAsFactors=F)

## -- Read in flow file -- this is just used to facilitate the example calculations in this script
fdat <- read.csv("./data/example_flow_data.csv", stringsAsFactors=F)



#### ---------------------------------
##         Parameter set up
#### ---------------------------------

## this describes flow category (low,med,high) and is just for testing example calculations
flowyear <- "high"


## NOTE: these parameters should be used in the real PITPH calcuations on the website

## Minumum turbine flow requirements (see Table 1 of Appendix E of 2017 Fish Passage Plan)
## These are necessary for power generation and take precendence over spill requirements
lgr.min.turb.flow <- 12.4
lgs.min.turb.flow <- 11.6
lmn.min.turb.flow <- 11.6
ihr.min.turb.flow <- 9.1
mcn.min.turb.flow <- 55
jda.min.turb.flow <- 55
tda.min.turb.flow <- 55
bon.min.turb.flow <- 35

## Powerhouse capacities -- these are max kcfs through all units combined
lgr.ph.cap <- 130
lgs.ph.cap <- 130
lmn.ph.cap <- 130
ihr.ph.cap <- 106
mcn.ph.cap <- 232
jda.ph.cap <- 322
tda.ph.cap <- 375
bon.ph1.cap <- 136   #PH1  
bon.ph2.cap <- 152   #PH2 (priority)

## flow kcfs through BON CC
bon.cc.flow <- 5

## Conditional proporitons of fish passing sluice and CC
## Values from acoustic tag studies at TDA and BON 2010-2011
bon.sluice.ch1 <- 0.284   #proportion of PH1 chinook that pass through sluiceway
bon.sluice.sthd <- 0.334  #proportion of PH1 steelhead that pass through sluiceway
bon.cc.ch1 <- 0.325   #proportion of PH2 chinook that pass through corner collector
bon.cc.sthd <- 0.621  #proportion of PH2 steelhead that pass through corner collector
tda.sluice.ch1 <- 0.586 #proportion of TDA PH chinook that pass through sluice
tda.sluice.sthd <- 0.591 #proportion of TDA PH steelhead that pass through sluice




#### ---------------------------------
##     Flow and Spill Calculations
#### ---------------------------------

## Create flow and spill vectors for demonstrating PITPH calculations using the sluiceway proportions

## note: flowyear describes flow category (low,med,high) and is set near top of script

snk.flow <- fdat[ , paste("snk.flow",flowyear,sep=".") ]
col.flow <- fdat[ , paste("col.flow",flowyear,sep=".") ]

## -- Spill settings by dam (defaults)
## These are "requested" and may get adjusted depending on flow
# dams with kcfs targets
req.lgr.spill.kcfs <- 20
req.lmn.spill.kcfs  <- 25
req.ihr.spill.kcfs <- 30
req.bon.spill.kcfs <- 100
# dams with percentage targets
req.lgs.spill.prop <- 0.30
req.mcn.spill.prop <- 0.50
req.jda.spill.prop <- 0.30
req.tda.spill.prop <- 0.40


## Flow Routing Rules: 
## 1) priority PH get minimum turbine values first. 
## 2) spill filled with remainder to requested amount
## 3) powerhouses filled to capacity with remaining
## 4) any left over put into spill

## Dams with kcfs targets
lgr.spill.kcfs <- snk.flow - lgr.min.turb.flow
lgr.spill.kcfs[lgr.spill.kcfs < 0] <- 0
lgr.spill.kcfs[lgr.spill.kcfs>req.lgr.spill.kcfs] <- req.lgr.spill.kcfs

lmn.spill.kcfs <- snk.flow - lmn.min.turb.flow
lmn.spill.kcfs[lmn.spill.kcfs < 0] <- 0
lmn.spill.kcfs[lmn.spill.kcfs > req.lmn.spill.kcfs] <- req.lmn.spill.kcfs

ihr.spill.kcfs <- snk.flow - ihr.min.turb.flow
ihr.spill.kcfs[ihr.spill.kcfs < 0] <- 0
ihr.spill.kcfs[ihr.spill.kcfs > req.ihr.spill.kcfs] <- req.ihr.spill.kcfs

bon.spill.kcfs <- col.flow - (bon.min.turb.flow + bon.cc.flow)
bon.spill.kcfs[bon.spill.kcfs < 0] <- 0
bon.spill.kcfs[bon.spill.kcfs > req.bon.spill.kcfs] <- req.bon.spill.kcfs

## Dams with percentage targets
req.lgs.spill.kcfs <- req.lgs.spill.prop*snk.flow
lgs.spill.kcfs <- snk.flow - lgs.min.turb.flow
lgs.spill.kcfs[lgs.spill.kcfs < 0] <- 0
lgs.spill.kcfs[lgs.spill.kcfs > req.lgs.spill.kcfs] <- req.lgs.spill.kcfs[lgs.spill.kcfs > req.lgs.spill.kcfs]

req.mcn.spill.kcfs <- req.mcn.spill.prop*col.flow
mcn.spill.kcfs <- col.flow - mcn.min.turb.flow
mcn.spill.kcfs[mcn.spill.kcfs < 0] <- 0
mcn.spill.kcfs[mcn.spill.kcfs > req.mcn.spill.kcfs] <- req.mcn.spill.kcfs[mcn.spill.kcfs > req.mcn.spill.kcfs]

req.jda.spill.kcfs <- req.jda.spill.prop*col.flow
jda.spill.kcfs <- col.flow - jda.min.turb.flow
jda.spill.kcfs[jda.spill.kcfs < 0] <- 0
jda.spill.kcfs[jda.spill.kcfs > req.jda.spill.kcfs] <- req.jda.spill.kcfs[jda.spill.kcfs > req.jda.spill.kcfs]

req.tda.spill.kcfs <- req.tda.spill.prop*col.flow
tda.spill.kcfs <- col.flow - tda.min.turb.flow
tda.spill.kcfs[tda.spill.kcfs < 0] <- 0
tda.spill.kcfs[tda.spill.kcfs > req.tda.spill.kcfs] <- req.tda.spill.kcfs[tda.spill.kcfs > req.tda.spill.kcfs]



## -- Powerhouse flows 

lgr.ph.flow <- snk.flow - lgr.spill.kcfs
adj.lgr.ph.flow <- lgr.ph.flow
adj.lgr.ph.flow[lgr.ph.flow > lgr.ph.cap] <- lgr.ph.cap
adj.lgr.spill.kcfs <- lgr.spill.kcfs + (lgr.ph.flow - adj.lgr.ph.flow)

lgs.ph.flow <- snk.flow - lgs.spill.kcfs
adj.lgs.ph.flow <- lgs.ph.flow
adj.lgs.ph.flow[lgs.ph.flow > lgs.ph.cap] <- lgs.ph.cap
adj.lgs.spill.kcfs <- lgs.spill.kcfs + (lgs.ph.flow - adj.lgs.ph.flow)

lmn.ph.flow <- snk.flow - lmn.spill.kcfs
adj.lmn.ph.flow <- lmn.ph.flow
adj.lmn.ph.flow[lmn.ph.flow > lmn.ph.cap] <- lmn.ph.cap
adj.lmn.spill.kcfs <- lmn.spill.kcfs + (lmn.ph.flow - adj.lmn.ph.flow)

ihr.ph.flow <- snk.flow - ihr.spill.kcfs
adj.ihr.ph.flow <- ihr.ph.flow
adj.ihr.ph.flow[ihr.ph.flow > ihr.ph.cap] <- ihr.ph.cap
adj.ihr.spill.kcfs <- ihr.spill.kcfs + (ihr.ph.flow - adj.ihr.ph.flow)

mcn.ph.flow <- col.flow - mcn.spill.kcfs
adj.mcn.ph.flow <- mcn.ph.flow
adj.mcn.ph.flow[mcn.ph.flow > mcn.ph.cap] <- mcn.ph.cap
adj.mcn.spill.kcfs <- mcn.spill.kcfs + (mcn.ph.flow - adj.mcn.ph.flow)

jda.ph.flow <- col.flow - jda.spill.kcfs
adj.jda.ph.flow <- jda.ph.flow
adj.jda.ph.flow[jda.ph.flow > jda.ph.cap] <- jda.ph.cap
adj.jda.spill.kcfs <- jda.spill.kcfs + (jda.ph.flow - adj.jda.ph.flow)

tda.ph.flow <- col.flow - tda.spill.kcfs
adj.tda.ph.flow <- tda.ph.flow
adj.tda.ph.flow[tda.ph.flow > tda.ph.cap] <- tda.ph.cap
adj.tda.spill.kcfs <- tda.spill.kcfs + (tda.ph.flow - adj.tda.ph.flow)


## Bon assumes PH2 filled to capacity then PH1 gets remaining to capacity
bon.ph.flow <- col.flow - bon.spill.kcfs
bon.ph2.flow <- bon.ph.flow
bon.ph2.flow[bon.ph2.flow > bon.ph2.cap] <- bon.ph2.cap
bon.ph1.flow <- bon.ph.flow - bon.ph2.flow
bon.ph1.flow[bon.ph1.flow > bon.ph1.cap] <- bon.ph1.cap
adj.bon.ph.flow <- bon.ph2.flow + bon.ph1.flow
adj.bon.spill.kcfs <- bon.spill.kcfs + (bon.ph.flow - adj.bon.ph.flow)


### -- Adjusted Spill Proportions
adj.lgr.spill.prop <- adj.lgr.spill.kcfs/snk.flow
adj.lgs.spill.prop <- adj.lgs.spill.kcfs/snk.flow
adj.lmn.spill.prop <- adj.lmn.spill.kcfs/snk.flow
adj.ihr.spill.prop <- adj.ihr.spill.kcfs/snk.flow

adj.mcn.spill.prop <- adj.mcn.spill.kcfs/col.flow
adj.jda.spill.prop <- adj.jda.spill.kcfs/col.flow
adj.tda.spill.prop <- adj.tda.spill.kcfs/col.flow
adj.bon.spill.prop <- adj.bon.spill.kcfs/col.flow


## proportion of total flow to each BON powerhouse
bon.ph2.prop <- bon.ph2.flow/col.flow
bon.ph1.prop <- bon.ph1.flow/col.flow



### -------------------------------------
##     SPE and PITPH Calculations
### -------------------------------------

## Function for calculating SPE from regression model parameters and inputs
getSPE <- function(parmvec, flowvec, spillpvec){
  #parmvec is vector of model parameters
  #flowvec is vector of flow in kcfs
  #spillpvec is vector of spill proportion
  #returns vector of SPE values
  
  # adjust spill values at boundaries for transformations
  spillpvec[spillpvec==1] <- 0.9999
  spillpvec[spillpvec==0] <- 0.0001
  
  # create design matrix
  xmat <- cbind(1, qnorm(spillpvec), flowvec, flowvec*qnorm(spillpvec))
  # X*beta = b0 + b1*qnorm(pspill) + b2*flow + b3*flow*qnorm(pspill) 
  qspe <- as.vector(xmat%*%as.matrix(parmvec))
  # back transform
  spe <- pnorm(qspe)
  return(spe)
}



####--- --- SPE calculations ----------

## extract SPE parameters
lgr.ch1.parms <- as.vector(as.matrix(pdat[pdat$dam=="lgr" & pdat$species=="ch1" & pdat$reartype=="hw", -(1:3)]))
lgr.sthd.parms <- as.vector(as.matrix(pdat[pdat$dam=="lgr" & pdat$species=="sthd" & pdat$reartype=="hw", -(1:3)]))
lgs.ch1.parms <- as.vector(as.matrix(pdat[pdat$dam=="lgs" & pdat$species=="ch1" & pdat$reartype=="hw", -(1:3)]))
lgs.sthd.parms <- as.vector(as.matrix(pdat[pdat$dam=="lgs" & pdat$species=="sthd" & pdat$reartype=="hw", -(1:3)]))
lmn.ch1.parms <- as.vector(as.matrix(pdat[pdat$dam=="lmn" & pdat$species=="ch1" & pdat$reartype=="hw", -(1:3)]))
lmn.sthd.parms <- as.vector(as.matrix(pdat[pdat$dam=="lmn" & pdat$species=="sthd" & pdat$reartype=="hw", -(1:3)]))
ihr.ch1.parms <- as.vector(as.matrix(pdat[pdat$dam=="ihr" & pdat$species=="ch1" & pdat$reartype=="hw", -(1:3)]))
ihr.sthd.parms <- as.vector(as.matrix(pdat[pdat$dam=="ihr" & pdat$species=="sthd" & pdat$reartype=="hw", -(1:3)]))
mcn.ch1.parms <- as.vector(as.matrix(pdat[pdat$dam=="mcn" & pdat$species=="ch1" & pdat$reartype=="hw", -(1:3)]))
mcn.sthd.parms <- as.vector(as.matrix(pdat[pdat$dam=="mcn" & pdat$species=="sthd" & pdat$reartype=="hw", -(1:3)]))
jda.ch1.parms <- as.vector(as.matrix(pdat[pdat$dam=="jda" & pdat$species=="ch1" & pdat$reartype=="hw", -(1:3)]))
jda.sthd.parms <- as.vector(as.matrix(pdat[pdat$dam=="jda" & pdat$species=="sthd" & pdat$reartype=="hw", -(1:3)]))
tda.ch1.parms <- as.vector(as.matrix(pdat[pdat$dam=="tda" & pdat$species=="ch1" & pdat$reartype=="hw", -(1:3)]))
tda.sthd.parms <- as.vector(as.matrix(pdat[pdat$dam=="tda" & pdat$species=="sthd" & pdat$reartype=="hw", -(1:3)]))
bon.ch1.parms <- as.vector(as.matrix(pdat[pdat$dam=="bon" & pdat$species=="ch1" & pdat$reartype=="hw", -(1:3)]))
bon.sthd.parms <- as.vector(as.matrix(pdat[pdat$dam=="bon" & pdat$species=="sthd" & pdat$reartype=="hw", -(1:3)]))


## calculate SPE
lgr.ch1.spe <- getSPE(lgr.ch1.parms, snk.flow, adj.lgr.spill.prop)
lgr.sthd.spe <- getSPE(lgr.sthd.parms, snk.flow, adj.lgr.spill.prop)
lgs.ch1.spe <- getSPE(lgs.ch1.parms, snk.flow, adj.lgs.spill.prop)
lgs.sthd.spe <- getSPE(lgs.sthd.parms, snk.flow, adj.lgs.spill.prop)
lmn.ch1.spe <- getSPE(lmn.ch1.parms, snk.flow, adj.lmn.spill.prop)
lmn.sthd.spe <- getSPE(lmn.sthd.parms, snk.flow, adj.lmn.spill.prop)
ihr.ch1.spe <- getSPE(ihr.ch1.parms, snk.flow, adj.ihr.spill.prop)
ihr.sthd.spe <- getSPE(ihr.sthd.parms, snk.flow, adj.ihr.spill.prop)
mcn.ch1.spe <- getSPE(mcn.ch1.parms, col.flow, adj.mcn.spill.prop)
mcn.sthd.spe <- getSPE(mcn.sthd.parms, col.flow, adj.mcn.spill.prop)
jda.ch1.spe <- getSPE(jda.ch1.parms, col.flow, adj.jda.spill.prop)
jda.sthd.spe <- getSPE(jda.sthd.parms, col.flow, adj.jda.spill.prop)
tda.ch1.spe <- getSPE(tda.ch1.parms, col.flow, adj.tda.spill.prop)
tda.sthd.spe <- getSPE(tda.sthd.parms, col.flow, adj.tda.spill.prop)
bon.ch1.spe <- getSPE(bon.ch1.parms, col.flow, adj.bon.spill.prop)
bon.sthd.spe <- getSPE(bon.sthd.parms, col.flow, adj.bon.spill.prop)


## TDA proportion of fish to sluiceway
tda.sluice.prop.ch1 <- (1 - tda.ch1.spe)*tda.sluice.ch1
tda.sluice.prop.sthd <- (1 - tda.sthd.spe)*tda.sluice.sthd

## BON Proportion of fish to the sluiceway and corner collector
bon.ph1.prop.ch1 <- (1 - bon.ch1.spe)*bon.ph1.prop
bon.ph2.prop.ch1 <- (1 - bon.ch1.spe)*bon.ph2.prop
bon.ph1.prop.sthd <- (1 - bon.sthd.spe)*bon.ph1.prop
bon.ph2.prop.sthd <- (1 - bon.sthd.spe)*bon.ph2.prop
bon.sluice.prop.ch1 <- bon.ph1.prop.ch1*bon.sluice.ch1
bon.sluice.prop.sthd <- bon.ph1.prop.sthd*bon.sluice.sthd
bon.cc.prop.ch1 <- bon.ph2.prop.ch1*bon.cc.ch1
bon.cc.prop.sthd <- bon.ph2.prop.sthd*bon.cc.sthd


##  Calculate daily (instantaneous) PITPH	

lgr.pitph.ch1 <- 1 - lgr.ch1.spe
lgr.pitph.sthd <- 1 - lgr.sthd.spe
lgs.pitph.ch1 <- 1 - lgs.ch1.spe
lgs.pitph.sthd <- 1 - lgs.sthd.spe
lmn.pitph.ch1 <- 1 - lmn.ch1.spe
lmn.pitph.sthd <- 1 - lmn.sthd.spe
ihr.pitph.ch1 <- 1 - ihr.ch1.spe
ihr.pitph.sthd <- 1 - ihr.sthd.spe
mcn.pitph.ch1 <- 1 - mcn.ch1.spe
mcn.pitph.sthd <- 1 - mcn.sthd.spe
jda.pitph.ch1 <- 1 - jda.ch1.spe
jda.pitph.sthd <- 1 - jda.sthd.spe


tda.pitph.ch1 <- 1 - (tda.ch1.spe + tda.sluice.prop.ch1)
tda.pitph.sthd <- 1 - (tda.sthd.spe + tda.sluice.prop.sthd)


bon.pitph.ch1 <- 1 - (bon.ch1.spe + bon.sluice.prop.ch1 + bon.cc.prop.ch1)
bon.pitph.sthd <- 1 - (bon.sthd.spe + bon.sluice.prop.sthd + bon.cc.prop.sthd)

## mean of daily summation across dams
mean(lgr.pitph.ch1 + lgs.pitph.ch1 + lmn.pitph.ch1 + ihr.pitph.ch1 + mcn.pitph.ch1 + jda.pitph.ch1 + tda.pitph.ch1 + bon.pitph.ch1)

mean(lgr.pitph.sthd + lgs.pitph.sthd + lmn.pitph.sthd + ihr.pitph.sthd + mcn.pitph.sthd + jda.pitph.sthd + tda.pitph.sthd + bon.pitph.sthd)



######-------------------------
###     Plots for checking
######-------------------------

### -- PITPH across the season at each dam -------------

pdf(paste("test_pitph_", flowyear, "_flow_plots.pdf", sep=""))
  plot(fdat$day, snk.flow, main="Snake River Flow", xlab="day", ylab="flow", type="l", col="blue", lwd=3)
  plot(fdat$day, col.flow, main="Columbia River Flow", xlab="day", ylab="flow", type="l", col="blue", lwd=3)

  plot(fdat$day, lgr.pitph.ch1, main="LGR Chinook PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, lgs.pitph.ch1, main="LGS Chinook PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, lmn.pitph.ch1, main="LMN Chinook PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, ihr.pitph.ch1, main="IHR Chinook PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, mcn.pitph.ch1, main="MCN Chinook PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, jda.pitph.ch1, main="JDA Chinook PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, tda.pitph.ch1, main="TDA Chinook PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, bon.pitph.ch1, main="BON Chinook PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))

  plot(fdat$day, lgr.pitph.sthd, main="LGR Steelhead PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, lgs.pitph.sthd, main="LGS Steelhead PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, lmn.pitph.sthd, main="LMN Steelhead PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, ihr.pitph.sthd, main="IHR Steelhead PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, mcn.pitph.sthd, main="MCN Steelhead PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, jda.pitph.sthd, main="JDA Steelhead PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, tda.pitph.sthd, main="TDA Steelhead PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))
  plot(fdat$day, bon.pitph.sthd, main="BON Steelhead PITPH", xlab="day", ylab="PITPH", type="l", col="blue", lwd=3, ylim=c(0,1))

dev.off()



### ----- General SPE curves by spill and flow --------------------

flow.levs.snk <- c(50, 100, 150) 
flow.levs.col <- c(175, 250, 400) 

species <- c("ch1", "sthd")
dam <- c("lgr", "lgs", "lmn", "ihr", "mcn", "jda", "tda", "bon")

pspvec <- seq(1e-10, 1-1e-10, length=1000) #proportion spill -- endpoints for continuity at boundaries
colv <- c("red", "purple", "blue")


pdf("spe_functions_plots.pdf")
for (i in 1:2){
	for (j in 1:length(dam)){
		if (j <= 4) tmp.flow.levs <- flow.levs.snk
    if (j > 4) tmp.flow.levs <- flow.levs.col
    tmp.parms <- as.vector(as.matrix(pdat[pdat$dam==dam[j] & pdat$species==species[i] & pdat$reartype=="hw", -(1:3)]))

    for (k in 1:3) {
      tmp.flow <- rep(tmp.flow.levs[k], length(pspvec))
      tmp.spe <- getSPE(tmp.parms, tmp.flow,  pspvec)
      if (k==1) plot(pspvec, tmp.spe, type="l", col=colv[k], lwd=3, ylim=c(0,1),
							main=paste(species[i], dam[j]), xlab="spill proportion", ylab="probability of spillway passage")
      if (k>1)  lines(pspvec, tmp.spe, col=colv[k], lwd=3)
      if (k==3) legend(x="bottomright", legend=tmp.flow.levs, title="Flow", lwd=3, col=colv, bty="n")
    }
	}
}
dev.off()



