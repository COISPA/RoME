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
    DataTA = DataTA[DataTA$YEAR ==2008 , ]
    DataTA[1,"SHOOTING_LONGITUDE"] <- 2300
    DataTA[1,"SHOOTING_LATITUDE"] <- 4700
    DataTA[1,"HAULING_LONGITUDE"] <- 2300
    DataTA[1,"HAULING_LATITUDE"] <- 4700
    DataTA[1, "SHOOTING_LATITUDE" ] <- 435.11
    # check_position_in_Med(DataTA, wd, suffix)
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

  ResultData = DataTA

  write(paste("\n----------- check if the hauls positions are in Mediterranean Sea - ",ResultData$YEAR[1]), file = Errors, append = TRUE)

  ResultData=ResultData[ResultData$VALIDITY=="V",]

  haul_on_land <- haul_at_sea(ResultData, seas=RoME::MedSea, verbose=FALSE)
  if (length(haul_on_land)>1){
    res_class <- class(haul_on_land)

    if (res_class=="data.frame"){
    k=1
    for (k in 1:nrow(haul_on_land)){
        if (any(colnames(haul_on_land)=="SHOOTING_LONGITUDE")){kind="shooting"}
        if (any(colnames(haul_on_land)=="HAULING_LONGITUDE")){kind="hauling"}
        numberError = numberError +1
        write(paste("Haul ",haul_on_land$HAUL_NUMBER[k]," ",kind," coordinates could likely fall on land", sep=""), file = Errors, append = TRUE)
      } # close for cicle
    } else if (res_class=="list") { # close if "data.frame"

      l=2 # open (res_class=="list")
    for (l in 1:length(haul_on_land)){
      k=1
      for(k in 1:nrow(haul_on_land[[l]])){
        numberError = numberError +1
          if (l==1){
            write(paste("Haul ",haul_on_land[[l]]$HAUL_NUMBER[k]," shooting coordinates could likely fall on land", sep=""), file = Errors, append = TRUE)
          } else if (l==2){
            write(paste("Haul ",haul_on_land[[l]]$HAUL_NUMBER[k]," hauling coordinates could likely fall on land", sep=""), file = Errors, append = TRUE)
          }
      } # close for in element in sigle list
      } # close for in list elements
      } # close if "list
    } # close if (length(haul_on_land)>1)


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
