###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check consistency among duration, start time and end time of the haul in TA

check_consistencyTA_duration<-function(ResultDataTA){
  
  numberError = 0
  
 
  Matrix = ResultDataTA # read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check consistency between duration and time TA - ", Matrix$YEAR[1]), file = Errors, append = TRUE) 
			  
  Matrix=Matrix[Matrix$VALIDITY=="V",]
  Matrix$Start=timeDate("01-01-2001.0000", format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT")
  for (i in 1:nrow(Matrix)){
    Matrix$Start[i]= timeDate(paste(paste(ifelse(str_length(Matrix$DAY[i])==1,paste("0",Matrix$DAY[i],sep=""),Matrix$DAY[i]),ifelse(str_length(Matrix$MONTH[i])==1,paste("0",Matrix$MONTH[i],sep=""),Matrix$MONTH[i]),Matrix$YEAR[i],sep="-"),ifelse(str_length(Matrix$SHOOTING_TIME[i])==3,paste("0",Matrix$SHOOTING_TIME[i],sep=""),Matrix$SHOOTING_TIME[i]),sep="."), format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT")
  }
  Matrix$End=timeDate("01-01-2001.0000", format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT")
  for (i in 1:nrow(Matrix)){
    Matrix$End[i]= timeDate(paste(paste(ifelse(str_length(Matrix$DAY[i])==1,paste("0",Matrix$DAY[i],sep=""),Matrix$DAY[i]),ifelse(str_length(Matrix$MONTH[i])==1,paste("0",Matrix$MONTH[i],sep=""),Matrix$MONTH[i]),Matrix$YEAR[i],sep="-"),ifelse(str_length(Matrix$HAULING_TIME[i])==3,paste("0",Matrix$HAULING_TIME[i],sep=""),Matrix$HAULING_TIME[i]),sep="."), format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT")
  }
  Matrix$difference=Matrix$End-Matrix$Start
  # one_hour = difftime(timeDate("01-01-2001.0200", format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT"), timeDate("01-01-2001.0100", format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT"),units="mins")
  for (j in nrow(Matrix))  {
    if (is.na(str_extract(Matrix$difference[j],"[0-9]{3,}?"))){
      Matrix$difference[j]=str_extract(Matrix$difference[j],"[0-9]{2,}?")  } else {
        Matrix$difference[j]=str_extract(Matrix$difference[j],"[0-9]{3,}?")
      }
    
  }
  # Matrix$difference=str_extract(Matrix$difference,"[0-9]{2,}?")
  duration=which((Matrix$difference==Matrix$HAUL_DURATION)==FALSE)
  if (length(duration)!=0){
    for (j in 1:length(duration)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[duration[j]],"inconsistency between SHOOTING-HAULING_TIME and HAUL_DURATION in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
      numberError = numberError +1
    }                
  }
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }	
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}

################################################################################