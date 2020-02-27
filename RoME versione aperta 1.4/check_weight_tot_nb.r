###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
################################################################################
# Check consistency between not null weight and not null total number

check_weight_tot_nb<-function(ResultDataTB){
  numberError = 0
  ResultData = ResultDataTB #read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check consistency between not null weight and not null total number in TB - ",ResultData$YEAR[1]), file = Errors, append = TRUE)
			  
  for (i in 1:nrow(ResultData)){
    if (Format=="before_2012"){
    
    if ((ResultData$TOTAL_WEIGHT_IN_HAUL[i]==0) & (ResultData$TOTAL_NUMBER_IN_HAUL[i]!=0)){ 
      
      write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," species ",ResultData$GENUS[i],ResultData$SPECIES[i]," Total weight equals 0, but total number is not null ", sep=""), file = Errors, append = TRUE)
    }
    
    } else {
      if ((ResultData$TOTAL_WEIGHT_IN_THE_HAUL[i]==0) & (ResultData$TOTAL_NUMBER_IN_THE_HAUL[i]!=0)){ 
        
        write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," species ",ResultData$GENUS[i],ResultData$SPECIES[i]," Total weight equals 0, but total number is not null ", sep=""), file = Errors, append = TRUE)
      }  
    }
  
    if (Format=="before_2012") {
    if ((ResultData$TOTAL_NUMBER_IN_HAUL[i]==0) & (ResultData$TOTAL_WEIGHT_IN_HAUL[i]!=0)&(ResultData$FAUNISTIC_CATEGORY[i]!="E")){ 
      
      write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," species ",ResultData$GENUS[i],ResultData$SPECIES[i]," Total number equals 0, but total weight is not null", sep=""), file = Errors, append = TRUE)
    }
  } else {
    if ((ResultData$TOTAL_NUMBER_IN_THE_HAUL[i]==0) & (ResultData$TOTAL_WEIGHT_IN_THE_HAUL[i]!=0) & 
          (   ((str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="E") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="D") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="V") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="G") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="H")  )){ 
      
      write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," species ",ResultData$GENUS[i],ResultData$SPECIES[i]," Total number equals 0, but total weight is not null", sep=""), file = Errors, append = TRUE)
    }  
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