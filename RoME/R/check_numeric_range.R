

check_numeric_range <- function(Data, Field, Values, year, wd, suffix) {


  if (FALSE) {
    Data = te #ResultDataTA
    Field = "INDIVIDUAL_WEIGHT"
    Values = c(0.1,999999) #c(3400,4600)
    suffix=NA
    check_numeric_range(Data, Field, Values, year=2015, wd, suffix=suffix)
  }



  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_", suffix ,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }


  #### CHECK TL FIELDS ####
  {
    if ("LITTER_SUB.CATEGORY" %in% colnames(Data)){
      colnames(Data)[which(colnames(Data)=="LITTER_SUB.CATEGORY")] <- "LITTER_SUB-CATEGORY"
    }
    if ("TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(Data)){
      colnames(Data)[which(colnames(Data)=="TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL"
    }
    if ("TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(Data)){
      colnames(Data)[which(colnames(Data)=="TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL"
    }
  }
  #### CHECK TL FIELDS - END ####

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

  Result = Data
  write(paste("\n----------- check range of values for field:", Field, "-", Result$YEAR[1]), file = Errors, append = TRUE)


  indexcol= which(names(Result)==Field)
  lrange <- length(Values)
  # if (class(Values)=="character") {
  #   char.val <- as.character(Values[3:lrange])
  #
  #   Result <- Result[!which(Result[,indexcol] %in% char.val), indexcol]
  # }
  Valuesf <- as.numeric(Values)


  Result <- Result[ !is.na(Result[, which(colnames(Result)==Field)]) , ]


  if ( (nrow(Result)!=0)){
    if (lrange == 2){
      k=1
        for (k in 1:nrow(Result)){
           if (!(Result[k, indexcol] >= Valuesf[1] & Result[k, indexcol] <= Valuesf[2])) {
             write(paste("Haul",Result$HAUL_NUMBER[k], ": value (",Result[k, indexcol],") out of allowed range for", Field, "in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
          numberError <- numberError + 1
           }

        } # ciclo for
    } else if (lrange > 2) {
      k=1
      for (k in 1:nrow(Result)){
        if (!((Result[k, indexcol] >= Valuesf[1] & Result[k, indexcol] <= Valuesf[2]) |
             Result[k, indexcol] %in% Valuesf[3:lrange])) {

          write(paste("Haul",Result$HAUL_NUMBER[k], ": value (",Result[k, indexcol],") out of allowed range for", Field, "in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
          numberError <- numberError + 1

        }

      } # ciclo for
      }
    }# nrow(Result)!=0

  if (numberError ==0) {
    write(paste("No error occurred for field", Field, "in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
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
