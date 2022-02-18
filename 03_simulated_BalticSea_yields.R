library(rPref)
library(scales)
library(tidyverse)

## Goal: simulate Baltic Sea community under varying fishing pressure

## load data and model parameters
load('Baltic_timeseries.Rdata')
load('BalticSea.Rdata')
source('baseparameters.R')
source('compareBiomass.R')
source('calcSSB.R')
source('IterateSpectrum.R')
source('calcFishedBiomass.R')
source('plotBiomasstime.R')
source('YieldCalc.R')


kappaNew <- 694348*1e6 # Convert to gram 

h <- 3*state$k/(0.6*state$wInf^(-1/3))
h[is.na(h)] <- mean(h,na.rm=T)
cV <- 1/mean(h,na.rm=T)

param <- baseparameters(state$wInf,kappaNew,h = h)
param$v <- param$h*cV
param$F0 <- state$F0
param$fishing <- "Trawl"
param$tEnd <- 120
Rmax <- as.numeric(as.matrix(read.table('Rmax_Baltic.csv')))*1e6
param$Rmax <- Rmax
param$eRepro <- matrix(0.01,param$nSpecies)
param$eRepro[3] <- 0.001


## get species-fmsy from PellaT model
fmsy<-read.csv('Pella_T_MSY_baltic.csv') %>% mutate(F0 = Fmsy)

# fix order
fmsy<-fmsy[c(3,2,1),]
param$F0<-fmsy$F0

## run model
SF <- IterateSpectrum(param,S=NA)

# Calculate catch curve with effort multiplier 
Fmulti <- seq(0,7, length.out = 80)

# setup dataframe with Fmultiplier to iterate and store results
df.save <- data.frame(Biomass = NA,
                      FBiomass = NA,
                      SSB = NA,
                      Yield = NA,
                      Species = rep(c('sprat', 'herring', 'cod'),length(Fmulti)),
                      Fmsy = fmsy$F0,
                      Fmulti = rep(Fmulti, each = 3)) 

## create F from MSY * multiplier
df.save$F0<-df.save$Fmsy * df.save$Fmulti

spp <- df.save$Species[1:param$nSpecies]
param$tEnd <- 100

## loop over each f value, save results
for(i in 1:length(Fmulti)){
  param$F0 <- df.save$F0[df.save$Fmulti == Fmulti[i]]
  print(param$F0)

  if(i == 1){
    Stmp <- SF
  }
  Stmp <- IterateSpectrum(param, Stmp)

  # species biomass
  df.save$Biomass[df.save$Fmulti %in% Fmulti[i]] <- Stmp$Biomass[param$tEnd/param$dt,]

  # fishable biomass (sum species biomass )
  df.save$FBiomass[df.save$Fmulti %in% Fmulti[i]] <- sum(Stmp$Biomass[param$tEnd/param$dt,])

  # yield
  df.save$Yield[df.save$Fmulti %in% Fmulti[i]] <- YieldCalc(param,Stmp)

  # spawning stock biomass
  df.save$SSB[df.save$Fmulti %in% Fmulti[i]] <- calcSSB(param,Stmp, param$tEnd/param$dt)

}

# estimate community mortality
commF<-df.save %>% group_by(Fmulti) %>% summarise(Fbiomass = sum(FBiomass), Yield = sum(Yield)) %>% 
      mutate(F = Yield / Fbiomass) %>% 
      select(F, Fmulti)

df.export <- df.save %>% pivot_longer(cols = 1:4, names_to = 'Unit')  %>% 
      left_join(commF, by = 'Fmulti') %>% 
      select(-Fmsy, -F0) ## drop other F metrics too confusing

ggplot(df.export, aes(x = F, y = value/1e6, color = Species))+
  facet_wrap(~Unit, scales='free')+ 
  geom_line() +
  scale_y_continuous('tonnes')

ggplot(df.export %>% filter(Unit=="Yield"), aes(x = F, y = value, color = Species))+
    geom_line()

# write.csv(df.export, file = 'BalticSea_curves', row.names=FALSE)

