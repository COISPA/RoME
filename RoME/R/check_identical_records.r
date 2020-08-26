############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
#  Check identical records
############################################################################################################################

check_identical_records<-function(Data,wd,suffix){

  check_without_errors = FALSE

  if (FALSE){
    wd <- "C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia" # tempdir()
    # suffix= NA # paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    # Data = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    # Data = read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    # Data = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    # Data = read.csv("~/GitHub/RoME/data/TE_2012-2018 _GSA18.csv", sep=";")
    Data = read.table(file=paste(wd, "\\2019 GSA18 TC.csv",sep=""), sep=";", header=T) # read.csv("~/GitHub/RoME/data/TL_GSA18 2012-2018.csv", sep=";")
    Data$MATURITY [28] <- 2
    Data$MATSUB[28] <- "D"
    # Data <- Data[Data$YEAR ==2018 , ]

    # check_identical_records(Data, wd, suffix)
  }

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

  Result = Data

###### TA ######
  if  (Result[1,"TYPE_OF_FILE"] == "TA")  {
    write(paste("\n----------- check identical records - ", Result$YEAR[1]), file = Errors, append = TRUE)
    write(paste("TA: "), file = Errors, append = TRUE)

    for (n in 1:ncol(Result)){
        Result[,n] <- as.character(Result[,n])
        Result[is.na(Result[,n]),n] <- "NA"
        }
      Matrix=aggregate(Result$TYPE_OF_FILE,by=list(Result$TYPE_OF_FILE,  Result$COUNTRY,      Result$AREA,      Result$VESSEL,      Result$GEAR,      Result$RIGGING,      Result$DOORS,
          Result$YEAR,      Result$MONTH,      Result$DAY,      Result$HAUL_NUMBER,      Result$CODEND_CLOSING,      Result$PART_OF_THE_CODEND,
          Result$SHOOTING_TIME,      Result$SHOOTING_QUADRANT,      Result$SHOOTING_LATITUDE,      Result$SHOOTING_LONGITUDE,      Result$SHOOTING_DEPTH,
          Result$HAULING_TIME,      Result$HAULING_QUADRANT,      Result$HAULING_LATITUDE,      Result$HAULING_LONGITUDE,      Result$HAULING_DEPTH,
          Result$HAUL_DURATION,      Result$VALIDITY,      Result$COURSE,      Result$RECORDED_SPECIES,      Result$DISTANCE,      Result$VERTICAL_OPENING,
          Result$WING_OPENING,      Result$GEOMETRICAL_PRECISION,      Result$BRIDLES_LENGTH,      Result$WARP_LENGTH,      Result$WARP_DIAMETER,
          Result$HYDROLOGICAL_STATION, Result$OBSERVATIONS,  Result$BOTTOM_TEMPERATURE_BEGINNING,      Result$BOTTOM_TEMPERATURE_END,  Result$MEASURING_SYSTEM,
          Result$NUMBER_OF_THE_STRATUM),FUN="length") #

      colnames(Matrix) <- c("TYPE_OF_FILE",  "COUNTRY",      "AREA",     "VESSEL",      "GEAR",      "RIGGING",      "DOORS",
          "YEAR",      "MONTH",      "DAY",      "HAUL_NUMBER",      "CODEND_CLOSING",     "PART_OF_THE_CODEND",
          "SHOOTING_TIME",      "SHOOTING_QUADRANT",     "SHOOTING_LATITUDE",      "SHOOTING_LONGITUDE",     "SHOOTING_DEPTH",
          "HAULING_TIME",      "HAULING_QUADRANT",     "HAULING_LATITUDE",    "HAULING_LONGITUDE",     "HAULING_DEPTH",
          "HAUL_DURATION",     "VALIDITY",      "COURSE",      "RECORDED_SPECIES",     "DISTANCE",      "VERTICAL_OPENING",
          "WING_OPENING",     "GEOMETRICAL_PRECISION",      "BRIDLES_LENGTH",      "WARP_LENGTH",      "WARP_DIAMETER",
          "HYDROLOGICAL_STATION", "OBSERVATIONS", "BOTTOM_TEMPERATURE_BEGINNING",     "BOTTOM_TEMPERATURE_END",  "MEASURING_SYSTEM",
          "NUMBER_OF_THE_STRATUM","count")

      # campi non inseriti nella PIVOT:

    Matrix= Matrix[Matrix$VALIDITY == "V",]
    Err=which(Matrix$count>1)
    Err=Matrix$HAUL_NUMBER[Err]

    if (length(Err)!=0){
      for (j in 1:length(Err)){
        write(paste("Haul ",Err[j],": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
      }
    }   else {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }


###### TB ######
  } else if   (Result[1,"TYPE_OF_FILE"] == "TB")  {
    write(paste("TB:"), file = Errors, append = TRUE)

    for (n in 1:ncol(Result)){
      Result[,n] <- as.character(Result[,n])
      Result[is.na(Result[,n]),n] <- "NA"
    }

   Matrix <- aggregate(Result$TYPE_OF_FILE, by=list(Result$TYPE_OF_FILE,Result$COUNTRY,Result$AREA,Result$VESSEL,Result$YEAR,Result$MONTH,
                       Result$DAY,Result$HAUL_NUMBER,Result$CODEND_CLOSING,Result$PART_OF_THE_CODEND,Result$FAUNISTIC_CATEGORY,
                       Result$GENUS,Result$SPECIES,Result$NAME_OF_THE_REFERENCE_LIST,Result$TOTAL_WEIGHT_IN_THE_HAUL,
                       Result$TOTAL_NUMBER_IN_THE_HAUL,Result$NB_OF_FEMALES,Result$NB_OF_MALES,
                       Result$NB_OF_UNDETERMINED), FUN="length")



    colnames(Matrix) <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","MONTH","DAY","HAUL_NUMBER","CODEND_CLOSING","PART_OF_THE_CODEND",
          "FAUNISTIC_CATEGORY","GENUS","SPECIES","NAME_OF_THE_REFERENCE_LIST","TOTAL_WEIGHT_IN_THE_HAUL",
          "TOTAL_NUMBER_IN_THE_HAUL","NB_OF_FEMALES","NB_OF_MALES","NB_OF_UNDETERMINED","count")



    Err=which(Matrix$count>1)
    Err=cbind(Matrix$HAUL_NUMBER[Err],as.character(Matrix$GENUS[Err]),as.character(Matrix$SPECIES[Err]))
    if (nrow(Err)!=0){
      for (j in 1:nrow(Err)){
        write(paste("Haul",Err[j,1],Err[j,2],Err[j,3],": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
      }
    } else {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }



###### TC ######
    } else if (Result[1,"TYPE_OF_FILE"] == "TC") {
    write(paste("TC:"), file = Errors, append = TRUE)



      for (n in 1:ncol(Result)){
        Result[,n] <- as.character(Result[,n])
        Result[is.na(Result[,n]),n] <- "NA"
      }

    Matrix <- aggregate(Result$TYPE_OF_FILE, by=list(Result$TYPE_OF_FILE,Result$COUNTRY,Result$AREA,
                      Result$VESSEL,Result$YEAR,Result$HAUL_NUMBER,Result$CODEND_CLOSING,Result$PART_OF_THE_CODEND,
                      Result$GENUS,Result$SPECIES,Result$LENGTH_CLASSES_CODE,Result$WEIGHT_OF_THE_FRACTION,
                      Result$WEIGHT_OF_THE_SAMPLE_MEASURED,Result$SEX,Result$NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED,
                      Result$LENGTH_CLASS,Result$MATURITY,Result$MATSUB,Result$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE),FUN="length")


    colnames(Matrix) <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","HAUL_NUMBER","CODEND_CLOSING","PART_OF_THE_CODEND",
        "GENUS","SPECIES","LENGTH_CLASSES_CODE","WEIGHT_OF_THE_FRACTION","WEIGHT_OF_THE_SAMPLE_MEASURED",
        "SEX","NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED","LENGTH_CLASS","MATURITY","MATSUB",
        "NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE","count")


    Err=which(Matrix$count>1)
    if (length(Err)!=0){
      for (j in 1:length(Err)){
        m=Matrix[Err[j],]
        write(paste("Haul" , as.character(m$HAUL_NUMBER) , ", species ",  as.character(m$GENUS), as.character(m$SPECIES), ", sex", as.character(m$SEX) ,as.character(m$MATURITY), as.character(m$MATSUB),", length", m$LENGTH_CLASS, ": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)}
    }   else {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }



###### TE ######
    }  else if (Result[1,"TYPE_OF_FILE"] == "TE") {
    write(paste("TE:"), file = Errors, append = TRUE)

    for (n in 1:ncol(Result)){
      Result[,n] <- as.character(Result[,n])
      Result[is.na(Result[,n]),n] <- "NA"
    }

    Matrix <- aggregate(Result$TYPE_OF_FILE, by=list(Result$TYPE_OF_FILE,Result$COUNTRY,Result$AREA,Result$VESSEL,
                       Result$YEAR,Result$MONTH,Result$DAY,Result$HAUL_NUMBER,Result$FAUNISTIC_CATEGORY,
                       Result$GENUS,Result$SPECIES,Result$LENGTH_CLASSES_CODE,Result$SEX,Result$NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH,
                       Result$LENGTH_CLASS,Result$MATURITY,Result$MATSUB,Result$INDIVIDUAL_WEIGHT,Result$NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT,
                       Result$OTOLITH_SAMPLED,Result$NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING,Result$OTOLITH_READ,Result$AGE,
                       Result$OTOLITH_CODE,Result$RECORD_NUMBER),FUN="length")


    colnames(Matrix) <- c("TYPE_OF_FILE","COUNTRY","AREA","VESSEL","YEAR","MONTH","DAY","HAUL_NUMBER","FAUNISTIC_CATEGORY",
          "GENUS","SPECIES","LENGTH_CLASSES_CODE","SEX","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH",
          "LENGTH_CLASS","MATURITY","MATSUB","INDIVIDUAL_WEIGHT","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT",
          "OTOLITH_SAMPLED","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING","OTOLITH_READ","AGE",
          "OTOLITH_CODE","RECORD_NUMBER","count")

    Err=which(Matrix$count>1)
    if (length(Err)!=0){
      for (j in 1:length(Err)){
        m=Matrix[Err[j],]
        write(paste("Haul" , as.character(m$HAUL_NUMBER) , ", species ",  as.character(m$GENUS), as.character(m$SPECIES), ", sex", as.character(m$SEX) ,as.character(m$MATURITY), as.character(m$MATSUB),", length", m$LENGTH_CLASS, ": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)}
    }   else {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }



########  TL  ########

  }  else if (Result[1,"TYPE_OF_FILE"] == "TL") {
    write(paste("TL:"), file = Errors, append = TRUE)

    #### CHECK TL FIELDS ####
    {
      if ("LITTER_SUB.CATEGORY" %in% colnames(Result)){
        colnames(Result)[which(colnames(Result)=="LITTER_SUB.CATEGORY")] <- "LITTER_SUB-CATEGORY"
      }
      if ("TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(Result)){
        colnames(Result)[which(colnames(Result)=="TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL"
      }
      if ("TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(Result)){
        colnames(Result)[which(colnames(Result)=="TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL"
      }
    }
    #### CHECK TL FIELDS - END ####

    for (n in 1:ncol(Result)){
      Result[,n] <- as.character(Result[,n])
      Result[is.na(Result[,n]),n] <- "NA"
    }

    Matrix <- aggregate(Result$TYPE_OF_FILE, by=list(Result$TYPE_OF_FILE,
                                                     Result$COUNTRY,
                                                     Result$AREA,
                                                     Result$VESSEL,
                                                     Result$YEAR,
                                                     Result$MONTH,
                                                     Result$DAY,
                                                     Result$HAUL_NUMBER,
                                                     Result$LITTER_CATEGORY,
                                                     Result[,"LITTER_SUB-CATEGORY"],
                                                     Result$TOTAL_WEIGHT_IN_THE_CATEGORY_HAUL,
                                                     Result$TOTAL_NUMBER_IN_THE_CATEGORY_HAUL,
                                                     Result[,"TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL"],
                                                     Result[,"TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL"]),FUN="length")

    colnames(Matrix) <- c("TYPE_OF_FILE",   "COUNTRY",    "AREA",    "VESSEL",    "YEAR",    "MONTH",    "DAY",    "HAUL_NUMBER",
    "LITTER_CATEGORY",    "LITTER_SUB-CATEGORY",    "TOTAL_WEIGHT_IN_THE_CATEGORY_HAUL",
    "TOTAL_NUMBER_IN_THE_CATEGORY_HAUL",    "TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL",
    "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL","count")

    Err=which(Matrix$count>1)
    if (length(Err)!=0){
      for (j in 1:length(Err)){
        m=Matrix[Err[j],]

        if (Result$TYPE_OF_FILE[1] == "TA"){
          write(paste("Haul" , as.character(m$HAUL_NUMBER) , ": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        }
        if (Result$TYPE_OF_FILE[1] == "TB"){
          write(paste("Haul" , as.character(m$HAUL_NUMBER) , ", species ",  as.character(m$GENUS), as.character(m$SPECIES), ", sex", as.character(m$SEX),": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        }
        if (Result$TYPE_OF_FILE[1] == "TC"){
          write(paste("Haul" , as.character(m$HAUL_NUMBER) , ", species ",  as.character(m$GENUS), as.character(m$SPECIES), ", sex", as.character(m$SEX) ,as.character(m$MATURITY), as.character(m$MATSUB),", length", m$LENGTH_CLASS, ": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        }
        if (Result$TYPE_OF_FILE[1] == "TE"){
          write(paste("Haul" , as.character(m$HAUL_NUMBER) , ", species ",  as.character(m$GENUS), as.character(m$SPECIES), ", sex", as.character(m$SEX) ,as.character(m$MATURITY), as.character(m$MATSUB),", length", m$LENGTH_CLASS, ": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        }
        if (Result$TYPE_OF_FILE[1] == "TL"){
          write(paste("Haul" , as.character(m$HAUL_NUMBER) , ": identical records in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        }

        }
    }   else {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }
  }
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  #unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)

  return(check_without_errors)
}
