check_year <- function(TA, TB, TC,TE,TL, years, wd, Errors){


  Format="from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  # if (!exists("suffix")){
  #   suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  # }
  numberError = 0

write(paste("\n----------- check on YEAR field "), file = Errors, append = TRUE)

# TA
if (any(!(TA$YEAR %in% seq(1900,2100,1)))) {
  write(paste("YEAR value not expected in TA"), file = Errors, append = TRUE)
  numberError=numberError+1
}

# TB
if (any(!(TB$YEAR %in% seq(1900,2100,1)))) {
  write(paste("YEAR value not expected in TB"), file = Errors, append = TRUE)
  numberError=numberError+1
}
if (any(!(unique(TB$YEAR) %in% years))){
  write("YEAR value in TB not included in TA table", file = Errors, append = TRUE)
  numberError=numberError+1
}

# TC
if (any(!(TC$YEAR %in% seq(1900,2100,1)))) {
  write("YEAR value not expected in TC", file = Errors, append = TRUE)
  numberError=numberError+1
}
if (any(!(unique(TC$YEAR) %in% years))){
  write("YEAR value in TC not included in TA table", file = Errors, append = TRUE)
  numberError=numberError+1
}



# TE

if (!(all(is.na(TE)) & length(TE)==1)){
if (any(!(TE$YEAR %in% seq(1900,2100,1)))) {
  write("YEAR value not expected in TE", file = Errors, append = TRUE)
  numberError=numberError+1
}
if (any(!(unique(TE$YEAR) %in% years))){
  write("YEAR value in TE not included in TA table", file = Errors, append = TRUE)
  numberError=numberError+1
}
}


# TL

if (!(all(is.na(TL)) & length(TL)==1)){
  if (any(!(TL$YEAR %in% seq(1900,2100,1)))) {
    write("YEAR value not expected in TL", file = Errors, append = TRUE)
    numberError=numberError+1
  }
  if (any(!(unique(TL$YEAR) %in% years))){
    write("YEAR value in TL not included in TA table", file = Errors, append = TRUE)
    numberError=numberError+1
  }
}



if (numberError ==0) {
  write(paste("No error occurred"), file = Errors, append = TRUE)
}
if (numberError ==0) {
          return(TRUE)
        } else {
          return(FALSE)
        }

}
