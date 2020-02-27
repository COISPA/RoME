###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if the coordinates are in the Mediterranean Sea

check_position_in_Med<-function(ResultDataTA){
  numberError = 0
  ResultData = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check if the hauls positions are in Mediterranean Sea - ",ResultData$YEAR[1]), file = Errors, append = TRUE)
  
  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData=convert_coordinates(ResultData)
  for (i in 1:nrow(ResultData)){
    if (as.character(ResultData$HAULING_QUADRANT[i])=="7"){
      ResultData$lon_start[i]=(-1)* ResultData$lon_start[i]
      ResultData$lon_end[i]=(-1)* ResultData$lon_end[i] }
  }
  
  for (k in 1:nrow(ResultData)){
    if (min(ResultData$lon_start[k])<(-5.4) | max(ResultData$lon_start[k])>35){ 
      numberError = numberError +1 
      write(paste("Haul ",ResultData$HAUL_NUMBER[k]," Error in start longitude", sep=""), file = Errors, append = TRUE) 
    }   
  }
  for (k in 1:nrow(ResultData)){
    if (min(ResultData$lon_end[k])<(-5.4) | max(ResultData$lon_end[k])>35){ 
      numberError = numberError +1   
      write(paste("Haul ",ResultData$HAUL_NUMBER[k]," Error in end longitude", sep=""), file = Errors, append = TRUE)
    }    }
  for (k in 1:nrow(ResultData)){ 
    if (min(ResultData$lat_start[k])<(34) | max(ResultData$lat_start[k])>46){
      numberError = numberError +1 
      write(paste("Haul ",ResultData$HAUL_NUMBER[k]," Error in start latitude", sep=""), file = Errors, append = TRUE)
    }   
  }
  for (k in 1:nrow(ResultData)){
    if (min(ResultData$lat_end[k])<(34) | max(ResultData$lat_end[k])>46){
      numberError = numberError +1 
      write(paste("Haul ",ResultData$HAUL_NUMBER[k]," Error in end latitute", sep=""), file = Errors, append = TRUE)
    }   }
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }  
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
}
################################################################################