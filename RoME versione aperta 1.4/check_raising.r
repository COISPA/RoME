###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if, in case of sub-sampling in TC, the number per sex in TB is raised correctly

check_raising<-function(ResultDataTB,ResultDataTC){
  numberError = 0
  ResultTC = ResultDataTC #read.csv(paste(DataTC,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check correctness of the number per sex in TB in case of sub-sampling in TC - ",ResultTC$YEAR[1]), file = Errors, append = TRUE)
  

  if (Type_of_files==".csv"){
    #write.xlsx(ResultTC,file=paste(DataTC,".xls", sep = ""))
    }
  ResultTB = ResultDataTB #read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  
  
  
  #channelTC <- odbcConnectExcel(paste(DataTC,".xls", sep = ""))
  
  if(Format=="before_2012"){
  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES" | names(ResultTB)=="TOTAL_WEIGHT_IN_HAUL" | names(ResultTB)=="TOTAL_NUMBER_IN_HAUL" | names(ResultTB)=="NUMBER_OF_FEMALES" | names(ResultTB)=="NUMBER_OF_MALES" | names(ResultTB)=="NUMBER_OF_UNDETERMINED")]
} else {
  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES" | names(ResultTB)=="TOTAL_WEIGHT_IN_THE_HAUL" | names(ResultTB)=="TOTAL_NUMBER_IN_THE_HAUL" | names(ResultTB)=="NB_OF_FEMALES" | names(ResultTB)=="NB_OF_MALES" | names(ResultTB)=="NB_OF_UNDETERMINED")] 
  
}
  
  queryTCpivot = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, SUM(NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE) AS Sum, WEIGHT_OF_THE_FRACTION,  WEIGHT_OF_THE_SAMPLE_MEASURED from ResultTC where  HAUL_NUMBER is not NULL ",
                       "group by YEAR, HAUL_NUMBER, GENUS, SPECIES, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED", sep="" )
  
  ResultTCpivot=sqldf(queryTCpivot)
  
  # check consistency between WEIGHT_OF_THE_FRACTION in TC and TOTAL_WEIGHT_IN_HAUL in TB
  
  if ( (nrow(ResultTB)!=0)){
    for (k in 1:nrow(ResultTB)){
      foundSpec = ResultTCpivot[as.character(ResultTCpivot$GENUS)==as.character(ResultTB$GENUS[k])
                                & as.character(ResultTCpivot$SPECIES)==as.character(ResultTB$SPECIES[k])
                                & as.numeric(ResultTCpivot$HAUL_NUMBER)==as.numeric(ResultTB$HAUL_NUMBER[k]),]
      if (nrow(foundSpec) > 1) {
        if (ResultTB[k,5]!=sum(foundSpec$WEIGHT_OF_THE_FRACTION)  ){
          write(paste("Haul",ResultTB$HAUL_NUMBER[k],ResultTB$GENUS[k], ResultTB$SPECIES[k], "WEIGHT_OF_THE_FRACTION in TC is not consistent with TOTAL_WEIGHT_IN_HAUL in TB. Impossible to continue with the other checks. Please correct and run the check again"), file = Errors, append = TRUE)
          numberError = numberError+1 
        }  }
    }
  }
  
  
  
  
  # check sum per sex
  
  queryTCpivotSex = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, SEX, SUM(NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE) AS SumSex, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED from ResultTC where  HAUL_NUMBER is not NULL group by YEAR, HAUL_NUMBER, GENUS, SPECIES, SEX, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED", sep="" )
  
  ResultTCpivotSex=sqldf(queryTCpivotSex)
  #odbcClose(channelTC)
  
  ResultTCpivotSex$codedsex = ifelse(((as.character(ResultTCpivotSex$SEX)=="I") | (as.character(ResultTCpivotSex$SEX)=="N")), "I", as.character(ResultTCpivotSex$SEX))
  
  #write.xlsx(ResultTCpivotSex,file="ResultTCpivotSex.xls",colNames=TRUE)
#   write.csv(ResultTCpivotSex,file="ResultTCpivotSex.csv",col.names=TRUE)
  write.table(ResultTCpivotSex,file="ResultTCpivotSex.csv",col.names=TRUE)
  ResultTCpivotSexFile=read.csv("ResultTCpivotSex.csv", header=TRUE)
  
  #channel <- odbcConnectExcel(ResultTCpivotSexFile)
  
  queryTCpivotSex = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, codedsex, SUM(SumSex) AS SumSexTotal, WEIGHT_OF_THE_FRACTION,  WEIGHT_OF_THE_SAMPLE_MEASURED from ResultTCpivotSex where  HAUL_NUMBER is not NULL ", "group by YEAR, HAUL_NUMBER, GENUS, SPECIES, codedsex, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED", sep="" )
  
  ResultTCpivotSex=sqldf(queryTCpivotSex)
  #odbcClose(channel)
  unlink(ResultTCpivotSexFile)
  
  molt= as.numeric(as.character(ResultTCpivotSex$WEIGHT_OF_THE_FRACTION))/ as.numeric(as.character(ResultTCpivotSex$WEIGHT_OF_THE_SAMPLE_MEASURED))
  
  ResultTCpivotSex$raising=ResultTCpivotSex$SumSexTotal *  molt                               
  
  #write.xlsx(ResultTCpivotSex,file="ResultTCpivotSex.xls",colNames=TRUE)
#   write.csv(ResultTCpivotSex,file="ResultTCpivotSex.csv",col.names=TRUE)
  write.table(ResultTCpivotSex,file="ResultTCpivotSex.csv",col.names=TRUE)
  ResultTCpivotSexFile="ResultTCpivotSex.csv"
  #channel <- odbcConnectExcel(ResultTCpivotSexFile)
  queryTCpivotSex = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, codedsex, SUM(raising) AS Sum from ResultTCpivotSex where  HAUL_NUMBER is not NULL ", "group by YEAR, HAUL_NUMBER, GENUS, SPECIES, codedsex", sep="" )                   
  ResultTCpivotSex=sqldf(queryTCpivotSex)
  #odbcClose(channel)
  unlink(ResultTCpivotSexFile)
  
  if ( (nrow(ResultTCpivotSex)!=0) & (numberError== 0)){
    for (j in 1:nrow(ResultTCpivotSex)){
      
      oneRowTB = ResultTB[as.character(ResultTB$GENUS)==as.character(ResultTCpivotSex$GENUS[j])
                          & as.character(ResultTB$SPECIES)==as.character(ResultTCpivotSex$SPECIES[j])
                          & as.numeric(ResultTB$HAUL_NUMBER)==as.numeric(ResultTCpivotSex$HAUL_NUMBER[j]),]
      if (as.character(ResultTCpivotSex$codedsex[j])=="F") {
        TotalNumberTBSex = ifelse(Format=="before_2012",oneRowTB$NUMBER_OF_FEMALES[1],oneRowTB$NB_OF_FEMALES[1])
      } else if (as.character(ResultTCpivotSex$codedsex[j])=="M") {
        TotalNumberTBSex = ifelse(Format=="before_2012",oneRowTB$NUMBER_OF_MALES[1],oneRowTB$NB_OF_MALES[1])
      } else {
        TotalNumberTBSex = ifelse(Format=="before_2012",oneRowTB$NUMBER_OF_UNDETERMINED[1],oneRowTB$NB_OF_UNDETERMINED[1])
      }
      
      
      if ((round(ResultTCpivotSex$Sum[j],0))!= round(TotalNumberTBSex,0)) {
        numberError = numberError+1
        
        if (as.character(ResultTCpivotSex$codedsex[j])=="F") { 
          labelsex= "FEMALES"
        } else if (as.character(ResultTCpivotSex$codedsex[j])=="M") { 
          labelsex = "MALES"
        } else {
          labelsex = "UNDETERMINED"
        }    
        write(paste("Haul ",ResultTCpivotSex$HAUL_NUMBER[j], " ",  ResultTCpivotSex$GENUS[j], " ",  ResultTCpivotSex$SPECIES[j], " NUMBER_OF_", labelsex, " in TB (", round(TotalNumberTBSex,0),") not consistent with the sum of individuals raised per sex (",round(ResultTCpivotSex$Sum[j],0) ,") in TC", sep=""), file = Errors, append = TRUE)
      }
    } }else {
      write(paste("Found errors in TC! Check raising not executed"), file = Errors, append = TRUE)
    }
  
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