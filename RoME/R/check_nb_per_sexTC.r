############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
#  Internal check  in TC (the number per sex must be equal to the sum of nb per length per sex)

check_nb_per_sexTC <- function(DataTC,wd,suffix){

  if (FALSE){
    #library(MEDITS)
    wd <- tempdir()
    DataSpecies=NA
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTC = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    DataTC <- DataTC[DataTC$YEAR == 2018 , ]

    # check_nb_per_sexTC(DataTC,wd,suffix)
  }


  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

  Result = DataTC
  write(paste("\n----------- check consistency of number per sex in TC - ", Result$YEAR[1]), file = Errors, append = TRUE)

  if (!all(is.na(Result$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED))){
  table1 <- aggregate(Result$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE,
                        by=list(Result$TYPE_OF_FILE,	Result$COUNTRY,	Result$AREA,	Result$VESSEL,
                        Result$YEAR,	Result$HAUL_NUMBER,	Result$CODEND_CLOSING,	Result$PART_OF_THE_CODEND,	Result$GENUS,	Result$SPECIES,
                        Result$LENGTH_CLASSES_CODE,	Result$WEIGHT_OF_THE_FRACTION,	Result$WEIGHT_OF_THE_SAMPLE_MEASURED,	Result$SEX,
                        Result$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED),FUN="sum") #

    colnames(table1) <- c("TYPE_OF_FILE",	"COUNTRY", "AREA",	"VESSEL", "YEAR",	"HAUL_NUMBER", "CODEND_CLOSING",	"PART_OF_THE_CODEND",	"GENUS",
    "SPECIES", "LENGTH_CLASSES_CODE",	"WEIGHT_OF_THE_FRACTION",	"WEIGHT_OF_THE_SAMPLE_MEASURED",
    "SEX", "NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED","Sum")


    errori= (which(table1$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED != table1$Sum))
    if (length(errori) != 0)    {
      for (j in 1:length(errori)){
        write(paste("Haul",table1$HAUL_NUMBER[errori[j]],table1$GENUS[errori[j]],table1$SPECIES[errori[j]],table1$SEX[errori[j]],"number per sex not consistent with the sum of individuals"), file = Errors, append = TRUE)
        numberError = numberError + 1
      }
    }
    } else {

      table1 <- aggregate(Result$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE,
                          by=list(Result$TYPE_OF_FILE,	Result$COUNTRY,	Result$AREA,	Result$VESSEL,
                                  Result$YEAR,	Result$HAUL_NUMBER,	Result$CODEND_CLOSING,	Result$PART_OF_THE_CODEND,	Result$GENUS,	Result$SPECIES,
                                  Result$LENGTH_CLASSES_CODE,	Result$WEIGHT_OF_THE_FRACTION,	Result$WEIGHT_OF_THE_SAMPLE_MEASURED,	Result$SEX),FUN="sum") #

      colnames(table1) <- c("TYPE_OF_FILE",	"COUNTRY", "AREA",	"VESSEL", "YEAR",	"HAUL_NUMBER", "CODEND_CLOSING",	"PART_OF_THE_CODEND",	"GENUS",
                            "SPECIES", "LENGTH_CLASSES_CODE",	"WEIGHT_OF_THE_FRACTION",	"WEIGHT_OF_THE_SAMPLE_MEASURED",
                            "SEX","Sum")

      write("The column nb per sex has been found empty, then it was computed automatically.", file = Errors, append = TRUE)
      print("The column nb per sex has been found empty, then it was computed automatically and saved in the TC_file_with_computed_nb_per_sex.csv.Please, fill in the nb per sex field in the original file, using the file produced and run again the code.",quote=FALSE)
      k=1
      for (k in 1:nrow(table1)){
        Result[which(Result$GENUS==table1$GENUS[k] & Result$SPECIES==table1$SPECIES[k] & Result$HAUL_NUMBER==table1$HAUL_NUMBER[k] &
                           Result$COUNTRY==table1$COUNTRY[k] & Result$SEX==table1$SEX[k] &
                           Result$WEIGHT_OF_THE_FRACTION==table1$WEIGHT_OF_THE_FRACTION[k] &
                           Result$WEIGHT_OF_THE_SAMPLE_MEASURED==table1$WEIGHT_OF_THE_SAMPLE_MEASURED[k]),
                   which(names(Result)=="NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED")]= table1$Sum[k]
      }
      write.table(Result[,1:ncol(Result)-1],file=file.path(wd,paste("TC_file_with_computed_nb_per_sex.csv",sep="")),sep=";",row.names=FALSE)
      numberError=numberError+1
    }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  #unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)

  if (numberError ==0) {
    return(TRUE)
  } else {
    return(FALSE)
    }
}
