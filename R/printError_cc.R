
printError_cc<-function(funname,check_without_errors, stop_) {
  if (check_without_errors == FALSE) {
    if (stop_ == FALSE) {
      message(paste(funname,": errors occurred! For more details see Logfile.dat", sep=""))
      stop_ = FALSE
    }
  } else {
    message(paste(funname," successfully completed!", sep=""))
  }

  return(stop_)
}


