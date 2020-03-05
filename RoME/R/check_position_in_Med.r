############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if the coordinates are in the Mediterranean Sea

check_position_in_Med<-function(DataTA,wd,suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";") #     MEDITS::TA   #
    DataTA[DataTA$YEAR ==2008 , ]

    # check_position_in_Med(DataTA, wd, suffix)
  }


  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")

  ResultData = DataTA

  write(paste("\n----------- check if the hauls positions are in Mediterranean Sea - ",ResultData$YEAR[1]), file = Errors, append = TRUE)

  ResultData=ResultData[ResultData$VALIDITY=="V",]

  k=1
  for (k in 1:nrow(ResultData)){
    df <- ResultData[k,]
    haul_on_land <- MEDITS::land.points(df, land=MEDITS::countries, verbose=FALSE)
    if (length(haul_on_land)>1){
      numberError = numberError +1
      write(paste("Haul ",haul_on_land$HAUL_NUMBER[1]," coordinates could likely fall on land", sep=""), file = Errors, append = TRUE)
    }
    }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}
