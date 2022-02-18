# Plot the temporal evolution of biomasses in the system (check for species dying)

plotBiomasstime <- function(param,S){
  
  time <- S$t
  Biomass <- S$Biomass
  
  dfnew <- data.frame(time = time, Biomass = Biomass[,1])
  p <- ggplot(data = dfnew, aes(x = time, y = Biomass))+geom_line()+theme_bw()+
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() )+
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))+
  scale_y_log10('Biomass', 
                breaks = trans_breaks("log10", function(x) 10^x),  
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous('time (years)')+theme(text = element_text(size=9))
  
  if (param$nSpecies> 1){
    for (i in 2:param$nSpecies){
    p <- p + geom_line(data = data.frame(x=time,y = Biomass[,i]),aes(x=x,y=y))
  }
}
p
return(p)
}

