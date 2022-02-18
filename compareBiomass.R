compareBiomass <-function(param,SF,state){


p <- ggplot(data = state, aes(x = wInf, y = SSBio))+geom_point(size = 4)+
  #geom_smooth(method = 'lm', se = F, colour = 'black')+
  scale_x_log10('Asymptotic weight')+scale_y_log10('Spawner biomass')+theme_classic()
tEnd <- param$tEnd/param$dt
SSB <- calcSSB(param,SF,tEnd)/1e6
Biomass <- SF$Biomass[tEnd,]

SSBio <- matrix(NA,param$nSpecies)
SSBio[state$totFlag == 0] <- SSB[state$totFlag == 0]
SSBio[is.na(SSBio)] <- Biomass[which(is.na(SSBio) == 1)]

SSdata <- data.frame(x=state$wInf,y=SSBio)
p <- p + geom_line(data = SSdata,aes (x = x, y = y), color = 'red')+
  geom_point(data = SSdata, aes(x = x, y = y), color = 'red')
  #geom_smooth(data = SSdata, aes(x = x, y = y), color = 'red', se = F, method = "lm")
print(p)

}