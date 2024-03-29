calcSSB <- function(param,S,t = param$tEnd/param$dt){
w <- S$w  

SSB <- matrix(NA,param$nSpecies)
for( i in 1:param$nSpecies){
  tmp <- w/param$wInf[i];
  psi <- tmp^(1-param$n)*1/(1+(tmp/param$alphaMature)^(-10));     # cut off before maturation
  co <- 0.1; #
  psi[w<co*param$alphaMature*param$wInf[i]] <- 0
  psi[tmp>1] <- 1
  
  
  
  SSB[i] <- sum(psi*S$N[t,i,]*S$dw*S$w)


}
return(SSB)
}






