############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
#  Check about the consistency of length classes in TC

check_length<-function(DataTC,DataSpecies=NA,year,wd,suffix){

  if (FALSE){
    #library(MEDITS)
    wd <- tempdir() # "C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia" # tempdir()
    DataSpecies=NA
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTC = tc # RoME::TC # read.table(file=paste(wd, "\\2019 GSA18 TC.csv",sep=""), sep=";", header=T) # read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    DataTC$LENGTH_CLASS[1] <- NA
    # DataTC <- DataTC[DataTC$YEAR == 2018, ]


    wd=tempdir()
    DataTC <- RoME::TC[1:20,]
    DataSpecies=NA
    suffix= "2020-03-05_time_h17m44s55"
    check_length(DataTC,DataSpecies=NA,year=2008,wd,suffix)

    # check_length(DataTC,DataSpecies=NA,year=2007,wd,suffix)
  }


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
  DataTC <- DataTC[DataTC$YEAR == year, ]
  ########################################
  DataTC <- DataTC[!is.na(DataTC$LENGTH_CLASS),]
  if(nrow(DataTC)==0) {
    write("Empty TC data frame for the selected year", file = Errors, append = TRUE)
    numberError = numberError +1
  } else {
  Result = DataTC
  write(paste("\n----------- check consistency of length classes TC - ",Result$YEAR[1]), file = Errors, append = TRUE)

  if(all(is.na(DataSpecies))){
    Target <- RoME::DataTargetSpecies
  } else {
    Target <- DataSpecies
  }

  Target=Target[which(!is.na(Target$MIN_LEN)),]


  ResultData= Result[,which(names(Result)=="TYPE_OF_FILE" | names(Result)=="HAUL_NUMBER" | names(Result)=="GENUS" | names(Result)=="SPECIES" | names(Result)=="SEX" | names(Result)=="LENGTH_CLASS")]

  ResultData$species=paste(ResultData$GENUS,ResultData$SPECIES,sep="")

  i=1
  for (i in 1:nrow(ResultData)){

    if (is.na(ResultData$LENGTH_CLASS[i]) | (ResultData$LENGTH_CLASS[i]=="")){
      write(paste("Haul ",ResultData$HAUL_NUMBER[i]," ",ResultData$species[i]," sex ",ResultData$SEX[i]," length ",ResultData$LENGTH_CLASS[i]," : unexpected value in LENGTH_CLASS ", ResultData$TYPE_OF_FILE[1],sep=""), file = Errors, append = TRUE)
      numberError = numberError +1
    }

    if ((ResultData$LENGTH_CLASS[i] < 0)==TRUE){
      write(paste("Haul ",ResultData$HAUL_NUMBER[i]," ",ResultData$species[i]," sex ",ResultData$SEX[i]," length ",ResultData$LENGTH_CLASS[i]," : negative value in LENGTH_CLASS in ", ResultData$TYPE_OF_FILE[1],sep=""), file = Errors, append = TRUE)
      numberError = numberError +1
    }

    FoundInTable=Target[as.character(Target$SPECIES)==as.character(ResultData$species[i]),]
    FoundInTable=FoundInTable[is.na(FoundInTable$MIN_LEN[1])==FALSE,]
    if (nrow(FoundInTable)!=0){
      if (((ResultData$LENGTH_CLASS[i]<FoundInTable$MIN_LEN[1]) | (ResultData$LENGTH_CLASS[i]>FoundInTable$MAX_LEN[1]))==TRUE)
      {
        write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," ",ResultData$species[i]," sex ",ResultData$SEX[i]," length ",ResultData$LENGTH_CLASS[i]," : LENGTH_CLASS out of boundaries (",FoundInTable$MIN_LEN[1],",",FoundInTable$MAX_LEN[1],") in ", ResultData$TYPE_OF_FILE[1],sep=""), file = Errors, append = TRUE)
      }
    }

  }


  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
    unlink("length.csv")
  }

#    if (file.exists(file.path(tempdir(), "Logfiles"))){
#   unlink(file.path(tempdir(),"Logfiles"),recursive=T)
#   }
#   if (file.exists(file.path(tempdir(), "Graphs"))){
#   unlink(file.path(tempdir(),"Graphs"),recursive=T)
#     }
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
#     }
#   if (numberError ==0) {
#     return(TRUE)
#   } else { return(FALSE) }

}
