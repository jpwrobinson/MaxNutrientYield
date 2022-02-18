# Calculate baltic sea MSY based on schaefer model # 
library(TMB)


compile('schaefer_biomass.cpp') # Estimates reference points in a surplus production model 
dyn.load('schaefer_biomass')
load('Baltic_timeseries.Rdata') # Load data used in Jacobsen 2016



for(i in 1:length(baltic_timeseries)){
  
  
  tmp <- baltic_timeseries[[i]]
  nyear <- nrow(tmp)
  
  B <- tmp$SSBiomass 
  C <- tmp$Landings
  
  
  df.tmb <- list(
    Biomass = B,
    Catch = C,
    nyear = nyear,
    SDB = 0.1
    
  )
  
  parms <- list(
    logr = log(1),
    logK = log(max(B)*2),
    logSD = log(0.1),
    #logSDC = log(0.1),    
    logm = log(1),
    logBpred = log(B)
    #logFzero = log(C/B)
  )
  
  
  # Phase 1 
  obj <-MakeADFun(df.tmb,parms,DLL="schaefer_biomass", 
                  map = list('logm' = factor(NA)), random = c('logBpred'))
  
  
  lower <- obj$par-Inf
  upper <- obj$par+Inf
  # 
  # lower[names(lower) == 'logr'] <- log(.5)
  # lower[names(lower) == 'logK'] <- log(max(B)*1.5)
  # lower[names(lower) == 'logm'] <- log(.5)
  # 
  # #
  # upper[names(upper) == 'logr'] <- log(4)
  # upper[names(upper) == 'logK'] <- log(max(B)*5)
  # 
  # 
  
<<<<<<< HEAD
  opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper, 
=======
  opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,  # Minimize the loglikelihood to get population parameters
>>>>>>> fdb87d2bdf3169be8261415ed1cd3e3d01094d5f
              control = list(iter.max = 1e5,
                             eval.max = 1e5), silent = TRUE) #
  
  
  reps <- obj$report()
  rep<-TMB::sdreport(obj)
  rep
  
  xl <- range(c(reps$Bpred, df.tmb$Biomass))
    

  
  plot(reps$Bpred, ylim = xl)
  lines(df.tmb$Biomass)
  
  
  K <- exp(rep$par.fixed)[2]
  r <- exp(rep$par.fixed)[1]
  m <- exp(parms$logm)
  
<<<<<<< HEAD
  
=======
  # Calculate MSY and Bmsy
>>>>>>> fdb87d2bdf3169be8261415ed1cd3e3d01094d5f
  MSY <- r*K/((m+1)^(1/m))
  Bmsy <- K/((m+1)^(1/m))
  

<<<<<<< HEAD

=======
  print(Bmsy/K)
  
>>>>>>> fdb87d2bdf3169be8261415ed1cd3e3d01094d5f
    if(i == 1){
    df <- data.frame(S0 = K,
                     MSY = MSY,
                     Fmsy = r,
                     species = names(baltic_timeseries[i])
    )  
  }else{
    df <- rbind(data.frame(S0 = K,
                           MSY = MSY,
                           Fmsy = r,
                           species = names(baltic_timeseries[i])),
                df)
  }
  
  
  
  
  
}

<<<<<<< HEAD

write.csv(df, file = 'Pella_T_MSY_baltic.csv')
=======
# Save parameters to file 
write.csv(df, file = 'Pella_T_MSY_baltic.csv')


# Test the stuff 
plot(rep$value[names(rep$value) == 'Bpred'])
lines(df.tmb$Biomass)
>>>>>>> fdb87d2bdf3169be8261415ed1cd3e3d01094d5f
