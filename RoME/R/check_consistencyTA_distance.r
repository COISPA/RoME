

check_consistencyTA_distance<-function(DataTA, wd, suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTA = MEDITS::TA
    # check_consistencyTA_distance(DataTA, wd, suffix)
  }

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  numberError = 0

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_", suffix ,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  Matrix = DataTA
  write(paste("\n----------- check consistency between duration and distance TA - ",Matrix$YEAR[1]), file = Errors, append = TRUE)

  Matrix$mean_distance=1852*Matrix$HAUL_DURATION/20
  Matrix$low_distance=Matrix$mean_distance-Matrix$mean_distance*0.15
  Matrix$up_distance=Matrix$mean_distance+Matrix$mean_distance*0.15
  distance=which(Matrix$DISTANCE<Matrix$low_distance | Matrix$DISTANCE>Matrix$up_distance)
  if (length(distance)!=0){
    for (j in 1:length(distance)){
      write(paste("Warning: in haul",Matrix$HAUL_NUMBER[distance[j]],"distance measure",Matrix$DISTANCE[distance[j]],"inconsistent with the duration of the haul (",Matrix$HAUL_DURATION[distance[j]],"min )"), file = Errors, append = TRUE)
    }
  }


    #unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)

  if (numberError ==0) {
    write("No error occurred",file = Errors, append = TRUE)
      if (file.exists(file.path(tempdir(), "Logfiles"))){
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
    }
	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    }
return(TRUE)
  } else { return(FALSE) }

}
