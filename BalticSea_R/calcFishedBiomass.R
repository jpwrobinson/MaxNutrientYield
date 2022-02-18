calcFishedBiomass <- function(param, S, tEnd = param$tEnd/param$dt,
                              Ftype = 'Trawl'){
  
  Bvul <- matrix(NA, length(tEnd), param$nSpecies)
  colnames(Bvul) <- param$wInf
  
  
  for(iSpecies in 1:param$nSpecies){
  
    
    if(param$F0[iSpecies]>0){
      sel <- fishing(param,iSpecies, S$w, type = Ftype)
  # Scale the trawl to one 
      sel <- sel/param$F0[iSpecies]
    }else{
      ptmp <- param
      ptmp$F0 <- 1
      sel <- fishing(ptmp, iSpecies, S$w, type = Ftype)
    
    }
  
    if(length(tEnd) == 1){
  
    N <- S$N[tEnd, iSpecies, ]
    Bvul[iSpecies] <- sum(N*sel*S$w*S$dw)
  
    }else{
    
    N <- S$N[tEnd, iSpecies, ]
    Bvul[,iSpecies] <- rowSums(sweep(N, MARGIN=2, sel*S$w*S$dw, "*"))
    rownames(Bvul) <- tEnd
    
  }
  
  
  
  }
  return(Bvul)
  
}