###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2013                                                                                                            #
###########################################################################################################################
# Check empty fields in TA,TB,TC

check_no_empty_fields<-function(Data,wd,suffix){
  if (FALSE){
    wd <- "C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia" # tempdir()
    suffix= NA # paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    # Data = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    # Data = read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    # Data = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    # Data = read.csv("~/GitHub/RoME/data/TE_2012-2018 _GSA18.csv", sep=";")
    # Data = read.csv("~/GitHub/RoME/data/TL_GSA18 2012-2018.csv", sep=";")
    Data <- read.table(file=paste(wd, "\\2019 GSA18 TE.csv",sep=""), sep=";", header=T)

    Data$FAUNISTIC_CATEGORY[1] <- NA
    # Data <- Data[Data$YEAR ==2018 , ]

    # check_no_empty_fields(Data, wd, suffix)
  }

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  Matrix = Data
  if ((Data[1,"TYPE_OF_FILE"] == "TA") == TRUE)  {
    write(paste("\n----------- check no empty fields"), file = Errors, append = TRUE)
    write(paste("TA - ",Matrix$YEAR[1]), file = Errors, append = TRUE)

    Matrix=Matrix[Matrix$VALIDITY=="V",]
    Mat=Matrix[,c(1:34)]

  } else if ((Data[1,"TYPE_OF_FILE"] == "TB") == TRUE) {
    write(paste("TB- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix
  } else if ((Data[1,"TYPE_OF_FILE"] == "TC") == TRUE) {
    write(paste("TC- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix[,c(1:20,22)]   #
  } else if ((Data[1,"TYPE_OF_FILE"] == "TE") == TRUE) {
    write(paste("TE- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix[,c(1:16,18:23)]  # c(1:23)
  } else if ((Data[1,"TYPE_OF_FILE"] == "TL") == TRUE){
    write(paste("TL- ",Matrix$YEAR[1]), file = Errors, append = TRUE)
    Mat=Matrix[,c(1:10,12,14)]
  }

  empty_X=which((is.na(Mat) | Mat ==""),arr.ind=TRUE)

  if (nrow(empty_X)!=0) {
    i=1
    for (i in 1:nrow(empty_X)){
      # Exception for PART_OF_THE_CODEND field, according to MEDITS manual 2012
      if (names(Mat)[empty_X[i,2]]!="PART_OF_THE_CODEND"){
        write(paste("Haul ",Mat$HAUL_NUMBER[empty_X[i,1]],"no value for ", names(Mat)[empty_X[i,2]]," in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        numberError = numberError + 1
      } else if (Mat$CODEND_CLOSING[empty_X[i,1]+1] == "C") {
        write(paste("Haul ",Mat$HAUL_NUMBER[empty_X[i,1]],"no value for ", names(Mat)[empty_X[i,2]]," in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        numberError = numberError + 1
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
