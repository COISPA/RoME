############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if the values in several fields belong to the allowed ranges according to INSTRUCTION MANUAL VERSION 9 MEDITS 2017

check_dictionary<-function(ResultData,Field,Values, wd, suffix){

  if (FALSE){
    wd <- "C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia"
    suffix=NA  # non modificare

    Field = "CODEND_CLOSING"
    Values = c("S","C") # as.character(unique(Stratification$COUNTRY))

    # ResultData = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    # ResultData = read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    # ResultData = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    ResultData = read.table(file=paste(wd, "\\2019 GSA18 TA.csv",sep=""), sep=";", header=T)

    ResultData$CODEND_CLOSING[1] <- NA


    # check_dictionary(ResultData,Field,Values, wd, suffix)
  }

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_", suffix ,".dat",sep=""))


  Result = ResultData
    write(paste("\n----------- check dictionary for field:", Field, "-", Result$YEAR[1]), file = Errors, append = TRUE)

  Valuesf <- factor(Values)


    if (any(is.na(Result[, which(colnames(Result)==Field)]))){
          na.results <- Result[ is.na(Result[, which(colnames(Result)==Field)]) , ]
          l.na <- nrow(na.results)
        for (x.na in 1:l.na){
              write(paste("Haul",na.results$HAUL_NUMBER[x.na], ": value not allowed for", Field, "in",  na.results$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
              numberError = numberError +1
            }
    }


  # if ( (Result$TYPE_OF_FILE[1] == "TA") & (Field=="CODEND_CLOSING") ){
  #   if (any(is.na(Result$CODEND_CLOSING))){
  #         na.results <- Result[ is.na(Result$CODEND_CLOSING) , ]
  #         l.na <- nrow(na.results)
  #       for (x.na in 1:l.na){
  #             write(paste("Haul",na.results$HAUL_NUMBER[x.na], ": value not allowed for", Field, "in",  na.results$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
  #             numberError = numberError +1
  #           }
  #   }
  #
  #   # Result=Result[(Result$CODEND_CLOSING != ""),]
  #   # Result=Result[(Result$CODEND_CLOSING != "") & is.na(Result$CODEND_CLOSING) == FALSE,]
  # }

  # if ( (Result$TYPE_OF_FILE[1] == "TA") & (Field=="COURSE") ){
  #   Result=Result[(Result$COURSE != "") & is.na(Result$COURSE) == FALSE ,]
  # }
  #
  # if ( (Result$TYPE_OF_FILE[1] == "TA") & (Field=="GEOMETRICAL_PRECISION") ){
  #   Result=Result[(Result$GEOMETRICAL_PRECISION != "") & is.na(Result$GEOMETRICAL_PRECISION) == FALSE ,]
  # }

  indexcol= which(names(Result)==Field)

  if ( (nrow(Result)!=0)){
    k=1
    for (k in 1:nrow(Result)){
      if ((is.na(as.character(Result[k,indexcol]))==TRUE )| (as.character(Result[k,indexcol])=="")){
        if (Result$TYPE_OF_FILE[1] == "TA") {
         #          if (!is.na(Result$VALIDITY[k])) {
         #            if (Result$VALIDITY[k]=="V") {
         #                             if (!is.na(as.character(Result$BOTTOM_TEMPERATURE_BEGINNING[k])) & !is.na(as.character(Result$BOTTOM_TEMPERATURE_END[k])) ){
                                          write(paste("Haul",as.character(Result$HAUL_NUMBER[k]), ": the field", Field, "is empty in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                                          numberError = numberError +1
         #                                }
         #          }
         # }
          } else if (Result$TYPE_OF_FILE[1] == "TB") {
                write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], ": the field", Field, "is empty in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                numberError = numberError +1
                } else if (Result$TYPE_OF_FILE[1] == "TC") {
                  write(paste("Haul",Result$HAUL_NUMBER[k], Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k], ": the field", Field, "is empty in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                  numberError = numberError +1
                } else if (Result$TYPE_OF_FILE[1] == "TE") {
                  write(paste("Haul",Result$HAUL_NUMBER[k], Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k], ": the field", Field, "is empty in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                  numberError = numberError +1
                } else if (Result$TYPE_OF_FILE[1] == "TL") {
                  write(paste("Haul",Result$HAUL_NUMBER[k], Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k], ": the field", Field, "is empty in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                  numberError = numberError +1
                }
      } else {  #  if ((is.na(as.character(Result[k,indexcol]))==TRUE )

        if (any(as.character(Result[k,indexcol])==Valuesf) == FALSE) {
          if (Result$TYPE_OF_FILE[1] == "TA") {

            write(paste("Haul",Result$HAUL_NUMBER[k], ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
            numberError = numberError +1

          } else if (Result$TYPE_OF_FILE[1] == "TB") {

            write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
            numberError = numberError +1

          } else if (Result$TYPE_OF_FILE[1] == "TC") {
          if ((Field!="SEX")) {
            write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k],  ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1] ), file = Errors, append = TRUE)
            numberError = numberError +1
          } else {
             if ((as.character(Result[k,indexcol])!="FALSE")) {
                 write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k],  ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1] ), file = Errors, append = TRUE)
                 numberError = numberError +1
             }
          }

          } else if (Result$TYPE_OF_FILE[1] == "TE") {
            if ((Field!="SEX")) {
              write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k],  ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1] ), file = Errors, append = TRUE)
              numberError = numberError +1
            } else {
              if ((as.character(Result[k,indexcol])!="FALSE")) {
                write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k],  ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1] ), file = Errors, append = TRUE)
                numberError = numberError +1
              }
            }
          } else if (Result$TYPE_OF_FILE[1] == "TL") {

              if ((as.character(Result[k,indexcol])!="FALSE")) {
                write(paste("Haul",Result$HAUL_NUMBER[k],Result$GENUS[k], Result$SPECIES[k], Result$SEX[k], Result$LENGTH_CLASS[k],  ": value not allowed for", Field, "in",  Result$TYPE_OF_FILE[1] ), file = Errors, append = TRUE)
                numberError = numberError +1

            }
          }





        }
      }
      }  #  for (k in 1:nrow(Result))
  }  # nrow(Result)!=0

  if (numberError ==0) {
    write(paste("No error occurred for field", Field, "in",  Result$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
  }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}
################################################################################
