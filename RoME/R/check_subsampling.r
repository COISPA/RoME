###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

# Check if, in case of sub-sampling in TC, the number per sex in TB is raised correctly

if (FALSE){
  ResultDataTC = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")
  ResultTC=ResultDataTC[ResultDataTC$YEAR==1994,]
  #ResultTB = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_subsampling(ResultDataTC,wd,suffix)
}


check_subsampling<-function(ResultDataTC,wd,suffix){

  Format="from_2012"
  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }

  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")


  numberError = 0

  write(paste("
              ----------- check correctness of the number per sex in TB in case of sub-sampling in TC - ",ResultTC$YEAR[1]), file = Errors, append = TRUE)

  #queryTCpivot = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, SUM(NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE) AS Sum, WEIGHT_OF_THE_FRACTION,  WEIGHT_OF_THE_SAMPLE_MEASURED from ResultTC where  HAUL_NUMBER is not NULL ","group by YEAR, HAUL_NUMBER, GENUS, SPECIES, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED", sep="" )

  ResultTCpivot=aggregate(ResultTC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE,by=list(ResultTC$YEAR,ResultTC$HAUL_NUMBER,ResultTC$GENUS,ResultTC$SPECIES,ResultTC$WEIGHT_OF_THE_FRACTION,ResultTC$WEIGHT_OF_THE_SAMPLE_MEASURED),FUN="sum")
   colnames(ResultTCpivot)=c("YEAR","HAUL_NUMBER","GENUS","SPECIES","WEIGHT_OF_THE_FRACTION","WEIGHT_OF_THE_SAMPLE_MEASURED","Sum") #sqldf(queryTCpivot)

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



  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}


################################################################################
