############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if, in case of sub-sampling in TC, the number per sex in TB is raised correctly
if (FALSE){
ResultDataTB = RoME::TB # read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")
ResultDataTC = RoME::TC # read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")
ResultDataTB = ResultDataTB[ResultDataTB$YEAR==2007,]
ResultDataTC = ResultDataTC[ResultDataTC$YEAR==2007,]


  wd <- tempdir() # "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  #check_raising(ResultDataTB,ResultDataTC,wd,suffix)
}


check_raising<-function(ResultDataTB,ResultDataTC,wd,suffix){
Format="from_2012"

if (!file.exists(file.path(wd, "Logfiles"))){
  dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
}
if (!file.exists(file.path(wd,"Graphs"))){
  dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
}
if (!exists("suffix")){
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
}
numberError = 0
Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
if (!file.exists(Errors)){
  file.create(Errors)
}

  ResultTC = ResultDataTC
  write(paste("\n----------- check correctness of the number per sex in TB in case of sub-sampling in TC - ",ResultTC$YEAR[1]), file = Errors, append = TRUE)

  ResultTB = ResultDataTB

  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES" | names(ResultTB)=="TOTAL_WEIGHT_IN_THE_HAUL" | names(ResultTB)=="TOTAL_NUMBER_IN_THE_HAUL" | names(ResultTB)=="NB_OF_FEMALES" | names(ResultTB)=="NB_OF_MALES" | names(ResultTB)=="NB_OF_UNDETERMINED")]

class(ResultTC$WEIGHT_OF_THE_FRACTION)="numeric"

class(ResultTC$WEIGHT_OF_THE_SAMPLE_MEASURED)="numeric"

    ResultTCpivot=aggregate(ResultTC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE, by=list(ResultTC$YEAR,ResultTC$HAUL_NUMBER,ResultTC$GENUS,ResultTC$SPECIES,as.numeric(as.character(ResultTC$WEIGHT_OF_THE_FRACTION)), as.numeric(as.character(ResultTC$WEIGHT_OF_THE_SAMPLE_MEASURED))),FUN="sum")
  colnames(ResultTCpivot)=c("YEAR", "HAUL_NUMBER", "GENUS", "SPECIES","WEIGHT_OF_THE_FRACTION",  "WEIGHT_OF_THE_SAMPLE_MEASURED","Sum")

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

  #queryTCpivotSex = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, SEX, SUM(NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE) AS SumSex, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED from ResultTC where  HAUL_NUMBER is not NULL group by YEAR, HAUL_NUMBER, GENUS, SPECIES, SEX, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED", sep="" )
  ResultTCpivotSex=aggregate(ResultTC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE, by=list(ResultTC$YEAR,ResultTC$HAUL_NUMBER,ResultTC$GENUS,ResultTC$SPECIES,ResultTC$SEX,as.numeric(as.character(ResultTC$WEIGHT_OF_THE_FRACTION)), as.numeric(as.character(ResultTC$WEIGHT_OF_THE_SAMPLE_MEASURED))),FUN="sum")
  colnames(ResultTCpivotSex)=c("YEAR", "HAUL_NUMBER", "GENUS", "SPECIES","SEX","WEIGHT_OF_THE_FRACTION",  "WEIGHT_OF_THE_SAMPLE_MEASURED","SumSex")

  #ResultTCpivotSex=sqldf(queryTCpivotSex)
  #odbcClose(channelTC)

  ResultTCpivotSex$codedsex = ifelse(((as.character(ResultTCpivotSex$SEX)=="I") | (as.character(ResultTCpivotSex$SEX)=="N")), "I", as.character(ResultTCpivotSex$SEX))


  #write.table(ResultTCpivotSex,file="ResultTCpivotSex.csv",col.names=TRUE)
  #ResultTCpivotSexFile=read.csv("ResultTCpivotSex.csv", header=TRUE)


  #queryTCpivotSex = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, codedsex, SUM(SumSex) AS SumSexTotal, WEIGHT_OF_THE_FRACTION,  WEIGHT_OF_THE_SAMPLE_MEASURED from ResultTCpivotSex where  HAUL_NUMBER is not NULL ", "group by YEAR, HAUL_NUMBER, GENUS, SPECIES, codedsex, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED", sep="" )

  #ResultTCpivotSex=sqldf(queryTCpivotSex)

  ResultTCpivotSex=aggregate(ResultTCpivotSex$SumSex, by=list(ResultTCpivotSex$YEAR,ResultTCpivotSex$HAUL_NUMBER,ResultTCpivotSex$GENUS,ResultTCpivotSex$SPECIES,ResultTCpivotSex$codedsex,as.numeric(as.character(ResultTCpivotSex$WEIGHT_OF_THE_FRACTION)), as.numeric(as.character(ResultTCpivotSex$WEIGHT_OF_THE_SAMPLE_MEASURED))),FUN="sum")
  colnames(ResultTCpivotSex)=c("YEAR", "HAUL_NUMBER", "GENUS", "SPECIES","SEX","WEIGHT_OF_THE_FRACTION",  "WEIGHT_OF_THE_SAMPLE_MEASURED","SumSexTotal")


  #odbcClose(channel)
  #unlink(ResultTCpivotSexFile)

  molt= as.numeric(as.character(ResultTCpivotSex$WEIGHT_OF_THE_FRACTION))/ as.numeric(as.character(ResultTCpivotSex$WEIGHT_OF_THE_SAMPLE_MEASURED))

  ResultTCpivotSex$raising=ResultTCpivotSex$SumSexTotal *  molt

  #write.xlsx(ResultTCpivotSex,file="ResultTCpivotSex.xls",colNames=TRUE)
#   write.csv(ResultTCpivotSex,file="ResultTCpivotSex.csv",col.names=TRUE)
 # write.table(ResultTCpivotSex,file="ResultTCpivotSex.csv",col.names=TRUE)
  #ResultTCpivotSexFile="ResultTCpivotSex.csv"
  #channel <- odbcConnectExcel(ResultTCpivotSexFile)
  #queryTCpivotSex = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, codedsex, SUM(raising) AS Sum from ResultTCpivotSex where  HAUL_NUMBER is not NULL ", "group by YEAR, HAUL_NUMBER, GENUS, SPECIES, codedsex", sep="" )
  #ResultTCpivotSex=sqldf(queryTCpivotSex)

  ResultTCpivotSex=aggregate(ResultTCpivotSex$raising, by=list(ResultTCpivotSex$YEAR,ResultTCpivotSex$HAUL_NUMBER,ResultTCpivotSex$GENUS,ResultTCpivotSex$SPECIES,ResultTCpivotSex$SEX),FUN="sum")
  colnames(ResultTCpivotSex)=c("YEAR", "HAUL_NUMBER", "GENUS", "SPECIES","SEX","Sum")


  #odbcClose(channel)
  #unlink(ResultTCpivotSexFile)

  if ( (nrow(ResultTCpivotSex)!=0) & (numberError== 0)){
    for (j in 1:nrow(ResultTCpivotSex)){

      oneRowTB = ResultTB[as.character(ResultTB$GENUS)==as.character(ResultTCpivotSex$GENUS[j])
                          & as.character(ResultTB$SPECIES)==as.character(ResultTCpivotSex$SPECIES[j])
                          & as.numeric(ResultTB$HAUL_NUMBER)==as.numeric(ResultTCpivotSex$HAUL_NUMBER[j]),]
      if (as.character(ResultTCpivotSex$SEX[j])=="F") {
        TotalNumberTBSex = ifelse(Format=="before_2012",oneRowTB$NUMBER_OF_FEMALES[1],oneRowTB$NB_OF_FEMALES[1])
      } else if (as.character(ResultTCpivotSex$SEX[j])=="M") {
        TotalNumberTBSex = ifelse(Format=="before_2012",oneRowTB$NUMBER_OF_MALES[1],oneRowTB$NB_OF_MALES[1])
      } else {
        TotalNumberTBSex = ifelse(Format=="before_2012",oneRowTB$NUMBER_OF_UNDETERMINED[1],oneRowTB$NB_OF_UNDETERMINED[1])
      }


      if ((round(ResultTCpivotSex$Sum[j],0))!= round(TotalNumberTBSex,0)) {
        numberError = numberError+1

        if (as.character(ResultTCpivotSex$SEX[j])=="F") {
          labelsex= "FEMALES"
        } else if (as.character(ResultTCpivotSex$SEX[j])=="M") {
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

   if (file.exists(file.path(tempdir(), "Logfiles"))){
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
    }
	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}
