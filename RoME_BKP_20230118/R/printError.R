
printError<-function(funname,check_without_errors, stop_) {
  if (check_without_errors == FALSE) {
    if (stop_ == FALSE) {
      print(paste(funname,": errors occurred! Please correct files and run again the script. For more details see Logfile.dat", sep=""),quote=FALSE)
      stop_ = TRUE
    }
  } else {
    print(paste(funname," successfully completed!", sep=""), quote = FALSE)
  }

  return(stop_)
}
