############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check empty fields in TA,TB,TC

check_no_empty_fields<-function(Data,year,wd,suffix){
  if (FALSE){
    wd <- tempdir()
    suffix= NA # paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")

    te[1,"LENGTH_CLASS"] <- NA
    Data <- te #RoME::TA

    # Data[1,"TYPE_OF_FILE"] <- NA
    # Data[1,"CODEND_CLOSING"] <- "S"
    year=2023
    check_no_empty_fields(Data, year, wd, suffix)
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
  Data <- Data[Data$YEAR == year, ]
  ########################################

  Matrix = Data # [!is.na(Data$LENGTH_CLASS),]

  if (any(is.na(Matrix$TYPE_OF_FILE))) {
    write(paste("Empty records detected in the 'TYPE_OF_FILE' field in the table. The check could not be further performed on the other fields of the table before the error is corrected."), file = Errors, append = TRUE)
    numberError = numberError + 1
  } else {

  if ((Data[1,"TYPE_OF_FILE"] == "TA") == TRUE)  {
    write(paste("\n----------- check no empty fields"), file = Errors, append = TRUE)
    write(paste("TA - ",Matrix$YEAR[1]), file = Errors, append = TRUE)

    Matrix=Matrix[is.na(Matrix$VALIDITY) |Matrix$VALIDITY=="V",]
    Mat=Matrix[,c(1:34)]

  } else if ((Data[1,"TYPE_OF_FILE"] == "TB") == TRUE) {
    write(paste("TB- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix
  } else if ((Data[1,"TYPE_OF_FILE"] == "TC") == TRUE) {
    write(paste("TC- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix[,c(1:20,22)]   #
  } else if ((Data[1,"TYPE_OF_FILE"] == "TE") == TRUE) {
    write(paste("TE- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix[,c(1:23,25)]  # c(1:23)
  } else if ((Data[1,"TYPE_OF_FILE"] == "TL") == TRUE){
    write(paste("TL- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix[,c(1:10,12)]
  }

  empty_X=which((is.na(Mat) | Mat =="" | Mat =="NA"),arr.ind=TRUE)

  if (nrow(empty_X)!=0) {
    i=1
    for (i in 1:nrow(empty_X)){
      # Exception for PART_OF_THE_CODEND field, according to MEDITS manual 2012
      if (names(Mat)[empty_X[i,2]]!="PART_OF_THE_CODEND"){
        write(paste("Haul ",Mat$HAUL_NUMBER[empty_X[i,1]],"no value for ", names(Mat)[empty_X[i,2]]," in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        numberError = numberError + 1
      } else if (Mat$CODEND_CLOSING[empty_X[i,1]] == "C" & !is.na(Mat$CODEND_CLOSING[empty_X[i,1]])) {
        write(paste("Haul ",Mat$HAUL_NUMBER[empty_X[i,1]],"no value for ", names(Mat)[empty_X[i,2]]," in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        numberError = numberError + 1
      }
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
