###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013 
################################################################################
# Internal check  in TC (the number per sex must be equal to the sum of nb per length per sex)

check_nb_per_sexTC<-function(ResultDataTC){
  numberError = 0
  Result = ResultDataTC #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check consistency of number per sex in TC - ", Result$YEAR[1]), file = Errors, append = TRUE)
  
  if (Type_of_files==".csv"){
    #write.xlsx(Result,file=paste(Data,".xls", sep = ""))
  }
  #channel <- odbcConnectExcel(paste(Data,".xls", sep = ""))
  query1 = paste("select sum(NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE) as SumSex,TYPE_OF_FILE,  COUNTRY,	AREA,	VESSEL,	YEAR,	HAUL_NUMBER,	CODEND_CLOSING,	PART_OF_THE_CODEND,	GENUS,	SPECIES,	LENGTH_CLASSES_CODE,	WEIGHT_OF_THE_FRACTION,	WEIGHT_OF_THE_SAMPLE_MEASURED,	SEX, NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED from Result Group by TYPE_OF_FILE,	COUNTRY,	AREA,	VESSEL,	YEAR,	HAUL_NUMBER,	CODEND_CLOSING,	PART_OF_THE_CODEND,	GENUS,	SPECIES,	LENGTH_CLASSES_CODE,	WEIGHT_OF_THE_FRACTION,	WEIGHT_OF_THE_SAMPLE_MEASURED,	SEX, NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED", sep="") 
  table1=sqldf(query1)
  colnames(table1)[1] = "Sum"
  
  #table1$codedsex = ifelse(((as.character(table1$SEX)=="I") | (as.character(table1$SEX)=="N")), "I", as.character(table1$SEX))
  #write.xlsx(table1,file="ResultTCpivotSex.xls",colNames=TRUE)
  #odbcClose(channel)
  
  #channelPivotSex <- odbcConnectExcel("ResultTCpivotSex.xls")
  #queryTCpivotSex = paste("SELECT TYPE_OF_FILE,	COUNTRY,	AREA,	VESSEL,	YEAR,	HAUL_NUMBER,	CODEND_CLOSING,	PART_OF_THE_CODEND,	GENUS,	SPECIES,	LENGTH_CLASSES_CODE,	WEIGHT_OF_THE_FRACTION,	WEIGHT_OF_THE_SAMPLE_MEASURED, NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED, codedsex, SUM(SumSex) AS Sum from [Sheet1$] where  HAUL_NUMBER is not NULL group by TYPE_OF_FILE,	COUNTRY,	AREA,	VESSEL,	YEAR,	HAUL_NUMBER,	CODEND_CLOSING,	PART_OF_THE_CODEND,	GENUS,	SPECIES,	LENGTH_CLASSES_CODE,	WEIGHT_OF_THE_FRACTION,	WEIGHT_OF_THE_SAMPLE_MEASURED, NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED, codedsex", sep="" )                   
  #table1=sqlQuery(channelPivotSex, queryTCpivotSex)
  #odbcClose(channelPivotSex)
  #unlink("ResultTCpivotSex.xls")
  
  #Result = read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  #if (Type_of_files==".csv"){
  #write.xlsx(Result,file=paste(Data,".xls", sep = ""))}
  #channel <- odbcConnectExcel(paste(Data,".xls", sep = ""))
  #queryData= paste("SELECT * from [Sheet1$] where HAUL_NUMBER is not null", sep="")
  #ResultData=sqlQuery(channel, queryData)  
  #odbcClose(channel)
  #ResultData$codedsex = ifelse(((as.character(ResultData$SEX)=="I") | (as.character(ResultData$SEX)=="N")), "I", as.character(ResultData$SEX))
  
  if (!all(is.na(table1$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED))){
    errori= (which(table1$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED != table1$Sum))
    if (length(errori) != 0)    {
      for (j in 1:length(errori)){
        write(paste("Haul",table1$HAUL_NUMBER[errori[j]],table1$GENUS[errori[j]],table1$SPECIES[errori[j]],table1$SEX[errori[j]],"number per sex not consistent with the sum of individuals"), file = Errors, append = TRUE)
        numberError = numberError + 1
      }
    }} else {
      write("The column nb per sex has been found empty, then it was computed automatically.", file = Errors, append = TRUE)
      print("The column nb per sex has been found empty, then it was computed automatically and saved in the TC_file_with_computed_nb_per_sex.csv.Please, fill in the nb per sex field in the original file, using the file produced and run again the code.",quote=FALSE)
      for (k in 1:nrow(table1)){
        ResultData[which(ResultData$GENUS==table1$GENUS[k] & ResultData$SPECIES==table1$SPECIES[k] & ResultData$HAUL_NUMBER==table1$HAUL_NUMBER[k] & ResultData$COUNTRY==table1$COUNTRY[k] & ResultData$codedsex==table1$codedsex[k] & ResultData$WEIGHT_OF_THE_FRACTION==table1$WEIGHT_OF_THE_FRACTION[k] & ResultData$WEIGHT_OF_THE_SAMPLE_MEASURED==table1$WEIGHT_OF_THE_SAMPLE_MEASURED[k]),which(names(ResultData)=="NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED")]= table1$Sum[k]
      } 
      write.table(ResultData[,1:ncol(ResultData)-1],file=paste("TC_file_with_computed_nb_per_sex.csv",sep=""),sep=";",row.names=FALSE)                   
      numberError=numberError+1
    }
  
  
  
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }	
  if (Type_of_files==".csv"){
#     unlink(paste(Data,".xls", sep = ""))
  }
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
}
################################################################################