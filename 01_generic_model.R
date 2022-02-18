library(tidyverse)
library(scales)
library(patchwork)

## Goal: simulate nutrient yields for generic 10 species community

source('BalticSea_R/baseparameters.R')
source('BalticSea_R/compareBiomass.R')
source('BalticSea_R/calcSSB.R')
source('BalticSea_R/IterateSpectrum.R')
source('BalticSea_R/calcFishedBiomass.R')
source('BalticSea_R/plotBiomasstime.R')
source('BalticSea_R/YieldCalc.R')


## set parameters for 10 species case
W <- 10^seq(log10(30),log10(15000),length.out = 15) # 10 species in logspace 
param0 <- baseparameters(W,kappa = 0.005,h = 15)
param0$F0 <- rep(0,param0$nSpecies)
param0$fishing <- "Trawl" # See "fishing.R" for definitions
param0$eRepro <- 0.05
param0$mu0prefactor <- 2
param0$mu0exponent <- -1/4
param0$ks <- 0.2*param$h;         # Activity

## run model to equilibrium
SF0 <- IterateSpectrum(param0,S=NA)

## setup F multiplier for yield curves
Fsim <- seq(0,3,length.out = 30)

df.export <- data.frame(F0 = Fsim, Yield = NA, cspecies = 0, meanWinf = NA, biomass = NA,
                        exploit = NA,
                        nut.under = NA,
                        nut.over = NA)

# Get generic nutrient data
nutrients <- read.table('JPWR_nutrients.txt', header = TRUE)
nutrients$wInf <- .01*nutrients$Lmax^3

## add nutrients approx to yield (random nutrients)
nutrients$nutrient_conc_equal<-runif(10, 0, 1)

# Create generic function for nutrients 
nut.under <- lm(nutrient_conc_underfishing ~ wInf, data = nutrients)
nut.over <- lm(nutrient_conc_overfishing ~ wInf, data = nutrients)
nut.equal <- lm(nutrient_conc_equal ~ wInf, data = nutrients)

for(i in 1:length(Fsim)){
  
  param <- baseparameters(W,kappa = 0.005,h = 15)
  param$mu0prefactor <- 2
  param$mu0exponent <- -1/4
  param$ks <- 0.2*param$h;         #
  param$eRepro <- 0.05
  
  param$F0 <- rep(Fsim[i],param$nSpecies)
  param$fishing <- "Trawl" # See "fishing.R" for definitions
  
  if(i == 1){
    SF <- SF0
  }
  
  SF <- IterateSpectrum(param, SF)
  
  # Relative biomass 
  bio.rel <- SF$Biomass[param$tEnd/param$dt,]/SF0$Biomass[param0$tEnd/param0$dt,]
  
  df.export$Yield[i] <- sum(YieldCalc(param, SF))
  df.export$cspecies[i] <- length(which(bio.rel < 0.2))
  df.export$meanWinf[i] <- weighted.mean(param$wInf, SF$Biomass[param$tEnd/param$dt,])
  df.export$biomass[i] <- sum(calcFishedBiomass(param,SF))
  df.export$exploit[i] <- sum(as.numeric(YieldCalc(param,SF)))/sum(as.numeric(calcFishedBiomass(param, SF)))
  
  # Predict nutrients 

  df.export$nut.under[i] <- sum(YieldCalc(param, SF)*predict(nut.under, newdata =data.frame(wInf = as.numeric(param$wInf))))
  df.export$nut.over[i] <- sum(YieldCalc(param, SF)*as.numeric(predict(nut.over, newdata =data.frame(wInf = as.numeric(param$wInf)))))  # Calculate nutrients 
  df.export$nut.equal[i] <- sum(YieldCalc(param, SF)*as.numeric(predict(nut.equal, newdata =data.frame(wInf = as.numeric(param$wInf)))))  # Calculate nutrients 
  
}

df.pp <- df.export
# Scale to max 
df.pp$Yield <- df.pp$Yield/max(df.pp$Yield)
df.pp$cspecies <- df.pp$cspecies/max(df.pp$cspecies)
df.pp$meanWinf <- df.pp$meanWinf/max(df.pp$meanWinf)
df.pp$biomass <- df.pp$biomass/max(df.pp$biomass)
df.pp$nut.under <- df.pp$nut.under/max(df.pp$nut.under)
df.pp$nut.over <- df.pp$nut.over/max(df.pp$nut.over)
df.pp$nut.equal <- df.pp$nut.equal/max(df.pp$nut.equal)

write.csv(df.pp, file = 'generic_model.csv', row.names=FALSE)


## plot
df.plot <- df.pp %>% pivot_longer(2:6)

p1 <- ggplot(df.plot[-which(df.plot$name == 'exploit'),], aes(x = F0, y = value/max(value), group = name, color = name))+geom_line(size = 1.4)+
  theme_classic()+scale_y_continuous('proportion of maximum')+scale_x_continuous('')+
  theme(legend.position = 'top')

p2 <- ggplot(df.pp, aes(x = F0, y = nut.under))+geom_line(size = 1.4)+geom_line(aes(y = Yield), col = 'red',size = 1.4)+theme_classic()+
  scale_x_continuous('ecosystem exploitation')+scale_y_continuous('')

p3 <- ggplot(df.pp, aes(x = F0, y = nut.over))+geom_line(size = 1.4)+geom_line(aes(y = Yield), col = 'red', size = 1.4)+theme_classic()+
  scale_x_continuous('')+scale_y_continuous('')

p1/(p2+p3)





