fishing = function(param,iSpecies,w,type){  

## JR add to fix fishable biomass NA when F0 = 0
    if (length(param$F0) == 1){
    param$F0 <- param$F0 * rep(1,length.out = param$nSpecies)
  }

  if (length(param$nF) == 1){
    param$nF <- param$nF * rep(1,length.out = param$nSpecies)
  }
  switch (type, 
          no      = Fin[iSpecies,] <- 0, # No fishing
          BH_Gill = w^(-1/4)*param$F0[iSpecies]*exp((-log(w/(param$nF[iSpecies]
                                                             *param$wInf[iSpecies]))^2/(2*param$gSigma))) ,
          # Balanced selective gillnet fishing
          BH_sel  = param$F0[iSpecies]*param$wInf[iSpecies]^(-1/4)*((1+(w/(param$nF[iSpecies]
                                                                           * param$wInf[iSpecies]))^-param$myF)^-1),
          # Balanced selective trawl fishing
          BH_non  = param$F0[iSpecies] * w^(-1/4),
          # Non-selective balanced fishing 
         
          Trawl   = param$F0[iSpecies] * (1+(w/(param$nF[iSpecies] * param$wInf[iSpecies]))^-param$myF)^-1,

          unsel   = param$F0[iSpecies],
          
          Kariba  =param$F0[iSpecies]*exp((-log(w/(param$aF))^2/(2*param$bF)))
        )
}