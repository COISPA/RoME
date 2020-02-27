###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if, in case of sub-sampling in TC, the number per sex in TB is raised correctly

check_subsampling<-function(ResultTC){
  numberError = 0
  #ResultTC = read.csv(paste(DataTC,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check correctness of the number per sex in TB in case of sub-sampling in TC - ",ResultTC$YEAR[1]), file = Errors, append = TRUE)
  

  #if (Type_of_files==".csv"){
#    #write.xlsx(ResultTC,file=paste(DataTC,".xls", sep = ""))
#    }
  #ResultTB = read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  
  
  
  #channelTC <- odbcConnectExcel(paste(DataTC,".xls", sep = ""))
  
#  if(Format=="before_2012"){
#  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES" | names(ResultTB)=="TOTAL_WEIGHT_IN_HAUL" | names(ResultTB)=="TOTAL_NUMBER_IN_HAUL" | names(ResultTB)=="NUMBER_OF_FEMALES" | names(ResultTB)=="NUMBER_OF_MALES" | names(ResultTB)=="NUMBER_OF_UNDETERMINED")]
#} else {
#  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES" | names(ResultTB)=="TOTAL_WEIGHT_IN_THE_HAUL" | names(ResultTB)=="TOTAL_NUMBER_IN_THE_HAUL" | names(ResultTB)=="NB_OF_FEMALES" | names(ResultTB)=="NB_OF_MALES" | names(ResultTB)=="NB_OF_UNDETERMINED")] 
#  
#}
  
  queryTCpivot = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, SUM(NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE) AS Sum, WEIGHT_OF_THE_FRACTION,  WEIGHT_OF_THE_SAMPLE_MEASURED from ResultTC where  HAUL_NUMBER is not NULL ","group by YEAR, HAUL_NUMBER, GENUS, SPECIES, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED", sep="" )
  
  ResultTCpivot=sqldf(queryTCpivot)
  
  # check if sub-sampling in TC are greater than 10%
  
for (ii in (1:nrow(ResultTCpivot))){
if ((ResultTCpivot$WEIGHT_OF_THE_SAMPLE_MEASURED[ii]/ResultTCpivot$WEIGHT_OF_THE_FRACTION[ii])<0.1 ){
write(paste("Warning: Year", ResultTCpivot$YEAR[ii], "Haul",ResultTCpivot$HAUL_NUMBER[ii],ResultTCpivot$GENUS[ii], ResultTCpivot$SPECIES[ii], "the sub-sample is less than 10%. Please verify and run the check again"), file = Errors, append = TRUE)
}

}  



  
   
  
  
  
  # check sum per sex
  
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }  
  
  if (Type_of_files==".csv"){
    unlink(paste(DataTC,".xls", sep = ""))}
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
}


################################################################################