############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
#  Check consistency of the hauls coordinates with the distance

 if (FALSE){
    #library(MEDITS)
    wd <- tempdir() # "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\temp"
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = MEDITS::TA #read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")

    check_distance(DataTA,wd,suffix)
  }
check_distance<-function(DataTA, wd, suffix){
  oldpar <- par(no.readonly = TRUE)




  if (!file.exists(file.path(wd,"Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

  ResultData = DataTA
  write(paste("\n----------- check consistency of the hauls coordinates with the distance - ", ResultData$YEAR[1]), file = Errors, append = TRUE)


  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData=MEDITS::MEDITS.to.dd(ResultData)

  for (i in 1:nrow(ResultData)){
    ResultData$computed_distance[i]= MEDITS::dd.distance(ResultData[i,], unit = "m", verbose=FALSE)
  }
  if (nrow(ResultData)!=0){
    for (j in 1:nrow(ResultData)){
      if (  (ResultData$DISTANCE[j]<=ResultData$computed_distance[j]-0.3*ResultData$computed_distance[j]) | (ResultData$DISTANCE[j]>=ResultData$computed_distance[j]+0.3*ResultData$computed_distance[j])){
        write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": the distance in TA ",ResultData$DISTANCE[j],"is quite different from the computed distance",round(ResultData$computed_distance[j],4),"(haul duration:",ResultData$HAUL_DURATION[j],"min)"), file = Errors, append = TRUE)
        tiff(file.path(wd,"Graphs",paste("haul ", ResultData[j,"HAUL_NUMBER"], " AREA ",ResultData[1,"AREA"],"_",ResultData[1,"YEAR"],".tiff",sep="")),width=12, height=8, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
        plot(1,1,type="p",xlim=c(min(ResultData$SHOOTING_LONGITUDE)-0.1, max(ResultData$SHOOTING_LONGITUDE)+0.1), ylim=c(min(ResultData$SHOOTING_LATITUDE)-0.1, max(ResultData$SHOOTING_LATITUDE)+0.1), xlab="Longitude", ylab="Latitude",main=paste("Haul",ResultData[j,"HAUL_NUMBER"],"-",ResultData[j,"YEAR"]))
        maps::map("world", fill=T, col="yellow",add=T)
        points(ResultData[j,"SHOOTING_LONGITUDE"],ResultData[j,"SHOOTING_LATITUDE"],col="blue",pch=16)
        points(ResultData[j,"HAULING_LONGITUDE"],ResultData[j,"HAULING_LATITUDE"],col="green",pch=16)
        legend("topleft", paste(c("start position","end position")), pch=c(16,16), col=c("blue","green")  )
        dev.off()
      }
    }
  }
  write("Some of the hauls coordinates may be inconsistent with the computed distance. For a visual check, look at the .tiff files in Graphs directory",file = Errors, append = TRUE)
  write(paste("No error occurred"), file = Errors, append = TRUE)

  on.exit(suppressWarnings(par(oldpar)))

  return(TRUE)
}
################################################################################
