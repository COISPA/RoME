

check_class <- function(data, tab, suffix, wd) {

  # if (FALSE) {
  #   classes <- read.csv("D:/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/RDB/RoME_RDBFIS/TX_classes.csv", sep=";")
  #
  #   data <- read.csv("~/GitHub/RDBqc/APPOGGIO/Ioannis/MEDITS/medits_ta.csv", sep=";")
  #   data <- data[, -1]
  #   colnames(data) <- colnames(RoME::templateTA)
  #   data$AREA <- as.character(data$AREA)
  #   tab <- "TA"
  #
  #   data <- tb
  #   tb$NB_OF_FEMALES <- as.integer(tb$NB_OF_FEMALES)
  #   check_class(tb, "TB", suffix="Pippo", wd)
  #
  #   data <- tl
  #   tb$NB_OF_FEMALES <- as.integer(tb$NB_OF_FEMALES)
  #   check_class(tl, "TC", suffix="Pippo", wd)
  #
  # }

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

  if (tab == "TA") {
    template <- RoME::templateTA
  }
  if (tab == "TB") {
    template <- RoME::templateTB
  }
  if (tab == "TC") {
    template <- RoME::templateTC
  }
  if (tab == "TE") {
    template <- RoME::templateTE
  }
  if (tab == "TL") {
    template <- RoME::templateTL
  }

        ncols <- ncol(data)


    if (ncols == ncol(template)) {

      # check 'numeric' fields
      numerics <- classes[classes$type %in% c("numeric"), ]
      for (i in 1:ncols) {
         if (colnames(data)[i] %in% numerics$MEDITS){
           index <- which(numerics$MEDITS == colnames(data)[i] & numerics$table== tab)
         } else {
           index <- NA
         }
        if (!is.na(index)) {
           if ((!(class(data[,colnames(data)[i]]) == "numeric") & !(class(data[,colnames(data)[i]]) == "integer"))) {
             write(paste("The class (",class(data[,colnames(data)[i]]),") of column ",colnames(data)[i] ," in ",tab," is inconsistent with the expected one (", numerics[index,"type"],")"), file = Errors, append = TRUE)
             numberError = numberError +1
           }
        }
      }

      classes <- RoME::classes
    # check 'numeric' fields
     integers <- classes[classes$type %in% c("integer"), ]

     for (i in 1:ncols) {
        if (colnames(data)[i] %in% integers$MEDITS){
          index <- which(integers$MEDITS == colnames(data)[i] & integers$table== tab)
        } else {
          index <- NA
        }

        if (!is.na(index)) {
          if (!(class(data[,colnames(data)[i]]) == "integer")) {
            write(paste("The class (",class(data[,colnames(data)[i]]),") of column ",colnames(data)[i] ," in ",tab," is inconsistent with the expected one (", integers[index,"type"],")"), file = Errors, append = TRUE)
            numberError = numberError +1
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
  if (file.exists(file.path(tempdir(), "Graphs"))){
    unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
  }
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}
