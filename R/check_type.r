check_type <- function(TA, TB, TC,TE,TL, years, wd, Errors){


  Format="from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  # if (!exists("suffix")){
  #   suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  # }
  numberError = 0
  # Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  write(paste("\n----------- check on TYPE_OF_FILE field "), file = Errors, append = TRUE)

  # TA

  if (any(unique(as.character(TA$TYPE_OF_FILE)) != "TA")    | any(is.na(unique(as.character(TA$TYPE_OF_FILE))))   ) {
    write("TYPE_OF_FILE not expected in TA table", file = Errors, append = TRUE)
    numberError=numberError+1
  }

  # TB
  if (any(unique(as.character(TB$TYPE_OF_FILE)) != "TB")    | any(is.na(unique(as.character(TB$TYPE_OF_FILE))))   ) {
    write("TYPE_OF_FILE not expected in TB table", file = Errors, append = TRUE)
    numberError=numberError+1
  }

  # TC
  if (any(unique(as.character(TC$TYPE_OF_FILE)) != "TC")    | any(is.na(unique(as.character(TC$TYPE_OF_FILE))))   ) {
    write("TYPE_OF_FILE not expected in TC table", file = Errors, append = TRUE)
    numberError=numberError+1
  }

  # TE
  if (!(all(is.na(TE)) & length(TE)==1)){
    if (any(unique(as.character(TE$TYPE_OF_FILE)) != "TE")    | any(is.na(unique(as.character(TE$TYPE_OF_FILE))))   ) {
      write("TYPE_OF_FILE not expected in TE table", file = Errors, append = TRUE)
      numberError=numberError+1
    }
  }

  # TL
  if (!(all(is.na(TL)) & length(TL)==1)){
    if (any(unique(as.character(TL$TYPE_OF_FILE)) != "TL")    | any(is.na(unique(as.character(TL$TYPE_OF_FILE))))   ) {
      write("TYPE_OF_FILE not expected in TL table", file = Errors, append = TRUE)
      numberError=numberError+1
    }
  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
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
  if (numberError ==0) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}
