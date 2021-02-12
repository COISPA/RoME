

check_length_class_codeTC<-function(DataTC,Specieslist=RoME::TM_list, wd,suffix){
  if (FALSE){
    #library(MEDITS)
    wd <- tempdir()
    Specieslist=NA
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTC = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    # DataTC <- DataTC[DataTC$YEAR == 2018, ]
    DataTC$GENUS[1] <- "ENGH"
    # check_length_class_codeTC(DataTC,Specieslist=NA,wd,suffix)
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

  ResultData = DataTC
  write(paste("\n----------- check correctness of LENGTH_CLASSES_CODE in TC - ",ResultData$YEAR[1]), file = Errors, append = TRUE)

    ResultSpecies <- Specieslist

  if (nrow(ResultData)!=0){
    j=1
    for (j in 1:nrow(ResultData)){
      FoundSpecies=ResultSpecies[as.character(ResultSpecies$MeditsCode)==paste(as.character(ResultData$GENUS[j]),as.character(ResultData$SPECIES[j]),sep=""),]
      if (nrow(FoundSpecies)!=0){
        if (as.character(FoundSpecies$CODLON[1])!=as.character(ResultData$LENGTH_CLASSES_CODE[j]))   {
          write(paste("Haul",ResultData$HAUL_NUMBER[j],", code species", as.character(ResultData$GENUS[j]) , as.character(ResultData$SPECIES[j]) ,"wrong LENGTH_CLASSES_CODE according to MEDITS FM list in Tables directory"), file = Errors, append = TRUE)
          numberError = numberError+1
        }
      } else {
        write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],", code species", as.character(ResultData$GENUS[j]) , as.character(ResultData$SPECIES[j]) ," species not present in TM list: LENGTH_CLASSES_CODE not verified."), file = Errors, append = TRUE)
      }


    }
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
