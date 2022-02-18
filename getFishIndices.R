getFishIndices = function (ts_values,fishnames, singleR = FALSE){
  # Find the min and max idx for each assessment 
  
#   names_split <- strsplit(as.character(ts_values[,1]),split = '-') # Split names into seperate words 
#   names.list <- c() # We're looking for species in row 2 and 3
#   names.list2 <- c()
#   
#   for (j in 1:length(names_split)){
#     names.list[j] <- names_split[[j]][2] # Turn from list into vector 
#     names.list2[j] <- names_split[[j]][3]
#   }
#   
  Errorsave <- c()
  
  if (singleR == FALSE){
    fVector <- matrix(NA,length(fishnames),2) # Save the indices in a vector 
  }else{
    fVector <- matrix(NA,length(fishnames),1) # Save the indices in a vector 
  }
  
  ts_values$stockid <- as.character(ts_values$stockid)
  fishnames <- as.character(fishnames)
  for (i in 1:length(fishnames)){ # Loop over fishnames 
    
    temp.name <- fishnames[i]
    # First run through the second column
    idx <- which(temp.name == ts_values$stockid) # Look for indices in the second column 
    
    
    if (length(idx) > 0){
      
      if (singleR == FALSE){
        fVector[i,1] <-  idx[1]
        fVector[i,2] <-  idx[length(idx)]
      }else{
        fVector[i] <- idx[1]
        if (length(idx) > 1){
        #  print('Check')
        }
      }
      
      
      
    }else{
      #print(i)
      Errorsave[i] <- i
    }

  }
  
  
  #   if (singleR == TRUE){
  #   colnames(fVector) <- c('Start','End')}
  #   else{
  #     colnames(fVector) <- 'index'
  #   }
  #   
  return(list(fVector,Errorsave)) # Returns  the start and stop indices in fVector and the lacking values in Errorsave
}

