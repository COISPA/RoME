
if (FALSE){
  ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")
    wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")

}


check_quadrant<-function(ResultDataTA,wd,suffix){

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

  ResultData = ResultDataTA
  write(paste("\n----------- check start quadrant and end quadrant TA - ",ResultData$YEAR[1]), file = Errors, append = TRUE)


  ResultData=ResultData[ResultData$VALIDITY=="V",]
  for (i in 1:nrow(ResultData)){
    if (ResultData$SHOOTING_QUADRANT[i]!=ResultData$HAULING_QUADRANT[i])  {
      write(paste("Warning: Haul",ResultData$HAUL_NUMBER[i]," starts in the quadrant",ResultData$SHOOTING_QUADRANT[i]," and finishes in the quadrant", ResultData$HAULING_QUADRANT[i]," in",ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)}
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
