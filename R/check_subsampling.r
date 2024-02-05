############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################


# Check if, in case of sub-sampling in TC, the number per sex in TB is raised correctly

if (FALSE){
  ResultTC = tc #RoME::TC
  year=2015
  wd <- tempdir()
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")

  check_subsampling(ResultDataTC,year,wd,suffix)
}


check_subsampling<-function(ResultTC,year,wd,suffix){

  Format="from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'", arg, "' argument should be provided"))
  } else if (length(year) != 1) {
    stop(paste0("only one value should be provided for '", arg, "' argument"))
  } else if (is.na(year)) {
    stop(paste0(arg, " argument should be a numeric value"))
  }
  ResultTC <- ResultTC[ResultTC$YEAR == year, ]
  ########################################

  write(paste("\n----------- check correctness of the number per sex in TB in case of sub-sampling in TC - ",ResultTC$YEAR[1]), file = Errors, append = TRUE)

  #queryTCpivot = paste("SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES, SUM(NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE) AS Sum, WEIGHT_OF_THE_FRACTION,  WEIGHT_OF_THE_SAMPLE_MEASURED from ResultTC where  HAUL_NUMBER is not NULL ","group by YEAR, HAUL_NUMBER, GENUS, SPECIES, WEIGHT_OF_THE_FRACTION, WEIGHT_OF_THE_SAMPLE_MEASURED", sep="" )

  ResultTCpivot=aggregate(ResultTC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE,by=list(ResultTC$YEAR,ResultTC$HAUL_NUMBER,ResultTC$GENUS,ResultTC$SPECIES,ResultTC$WEIGHT_OF_THE_FRACTION,ResultTC$WEIGHT_OF_THE_SAMPLE_MEASURED),FUN="sum")
   colnames(ResultTCpivot)=c("YEAR","HAUL_NUMBER","GENUS","SPECIES","WEIGHT_OF_THE_FRACTION","WEIGHT_OF_THE_SAMPLE_MEASURED","Sum") #sqldf(queryTCpivot)

  # check if sub-sampling in TC are greater than 10%

for (ii in (1:nrow(ResultTCpivot))){
  if (ResultTCpivot$WEIGHT_OF_THE_FRACTION[ii] == 0 | ResultTCpivot$WEIGHT_OF_THE_SAMPLE_MEASURED[ii] == 0) {
    write(paste("Warning: 0 values in WEIGHT_OF_THE_FRACTION and WEIGHT_OF_THE_SAMPLE_MEASURED fields are allowed only for species not included in G1 and G2 lists (", ResultTCpivot$YEAR[ii], ", Haul",ResultTCpivot$HAUL_NUMBER[ii],ResultTCpivot$GENUS[ii], ResultTCpivot$SPECIES[ii]), file = Errors, append = TRUE)
  } else {
if ((ResultTCpivot$WEIGHT_OF_THE_SAMPLE_MEASURED[ii]/ResultTCpivot$WEIGHT_OF_THE_FRACTION[ii])<0.1 ){
write(paste("Warning: Year", ResultTCpivot$YEAR[ii], "Haul",ResultTCpivot$HAUL_NUMBER[ii],ResultTCpivot$GENUS[ii], ResultTCpivot$SPECIES[ii], "the sub-sample is less than 10%. Please verify and run the check again"), file = Errors, append = TRUE)
  }
}

}

  # unlink(file.path(tempdir(),"Graphs"),recursive=T)
   #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)

  # check sum per sex
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
#      if (file.exists(file.path(tempdir(), "Logfiles"))){
#   unlink(file.path(tempdir(),"Logfiles"),recursive=T)
#   }
#   if (file.exists(file.path(tempdir(), "Graphs"))){
#   unlink(file.path(tempdir(),"Graphs"),recursive=T)
#     }
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
#     }
    return(TRUE)
  } else { return(FALSE) }

}


################################################################################
