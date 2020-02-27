###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
################################################################################
# Check if LENGTH_CLASSES_CODE is correct according to INSTRUCTION MANUAL VERSION 6 MEDITS 2012

check_length_class_codeTC<-function(ResultDataTC,DataSpecies){
  numberError = 0
  ResultData = ResultDataTC #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check correctness of LENGTH_CLASSES_CODE in TC - ",ResultData$YEAR[1]), file = Errors, append = TRUE)
  
  ResultSpecies = read.csv(paste(DataSpecies,".csv",sep=""), sep=";", header=TRUE)
  if (nrow(ResultData)!=0){
    for (j in 1:nrow(ResultData)){
      FoundSpecies=ResultSpecies[as.character(ResultSpecies$MeditsCode)==paste(as.character(ResultData$GENUS[j]),as.character(ResultData$SPECIES[j]),sep=""),] 
      if (length(FoundSpecies$CODLON[1])!=0){
        if (as.character(FoundSpecies$CODLON[1])!=as.character(ResultData$LENGTH_CLASSES_CODE[j]))   {
          #if ((as.character(FoundSpecies$CODLON[1])=="0")|(as.character(FoundSpecies$CODLON[1])=="1"))
          #{write(paste("Haul",ResultData$HAUL_NUMBER[j],": code species", as.character(ResultData$GENUS[j]) , as.character(ResultData$SPECIES[j]) ,"wrong LENGTH_CLASSES_CODE according to MEDITS FM list in Tables directory"), file = Errors, append = TRUE)
          #numberError = numberError+1
          #}  else if ((as.character(FoundSpecies$CODLON[1])!="m")){
          #write(paste("Haul",ResultData$HAUL_NUMBER[j],": code species", as.character(ResultData$GENUS[j]) , as.character(ResultData$SPECIES[j]) ,"wrong LENGTH_CLASSES_CODE according to MEDITS FM list in Tables directory"), file = Errors, append = TRUE)
          #numberError = numberError+1
          # }      
          write(paste("Haul",ResultData$HAUL_NUMBER[j],": code species", as.character(ResultData$GENUS[j]) , as.character(ResultData$SPECIES[j]) ,"wrong LENGTH_CLASSES_CODE according to MEDITS FM list in Tables directory"), file = Errors, append = TRUE)
          numberError = numberError+1
        } 
      } else {
        write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": code species", as.character(ResultData$GENUS[j]) , as.character(ResultData$SPECIES[j]) ," species not present in FM list: LENGTH_CLASSES_CODE not verified."), file = Errors, append = TRUE)
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