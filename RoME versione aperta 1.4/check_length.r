###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check about the consistency of length classes in TC

check_length<-function(ResultData,DataSpecies){ 
  numberError = 0
  Result = ResultData #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
----------- check consistency of length classes TC - ",Result$YEAR[1]), file = Errors, append = TRUE)
  
  Target=read.csv(file=paste(DataTargetSpecies,".csv",sep=""),sep=";",header=TRUE)
  Target=Target[which(is.na(Target$MIN_LEN)==FALSE),]
  
  
  ResultData= Result[,which(names(Result)=="TYPE_OF_FILE" | names(Result)=="HAUL_NUMBER" | names(Result)=="GENUS" | names(Result)=="SPECIES" | names(Result)=="SEX" | names(Result)=="LENGTH_CLASS")]
  
  ResultData$species=paste(ResultData$GENUS,ResultData$SPECIES,sep="")
  
  for (i in 1:nrow(ResultData)){
    FoundInTable=Target[as.character(Target$SPECIES)==as.character(ResultData$species[i]),]
    FoundInTable=FoundInTable[is.na(FoundInTable$MIN_LEN[1])==FALSE,]
    if (nrow(FoundInTable)!=0){
      if (((ResultData$LENGTH_CLASS[i]<FoundInTable$MIN_LEN[1]) | (ResultData$LENGTH_CLASS[i]>FoundInTable$MAX_LEN[1]))==TRUE)
      {
        write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," ",ResultData$species[i]," sex ",ResultData$SEX[i]," length ",ResultData$LENGTH_CLASS[i]," : LENGTH_CLASS out of boundaries (",FoundInTable$MIN_LEN[1],",",FoundInTable$MAX_LEN[1],") in ", ResultData$TYPE_OF_FILE[1],sep=""), file = Errors, append = TRUE) 
      }
    }
    
  }
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
    unlink("length.csv")
  }  
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
}
################################################################################