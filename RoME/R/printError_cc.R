
printError_cc<-function(funname,check_without_errors, stop_) {
  if (check_without_errors == FALSE) {
    if (stop_ == FALSE) {
      print(paste(funname,": errors occurred! For more details see Logfile.dat", sep=""),quote=FALSE)
      stop_ = FALSE
    }
  } else {
    print(paste(funname," successfully completed!", sep=""), quote = FALSE)
  }

  return(stop_)
}
