############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Visual check of the haul positions
if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";") #     MEDITS::TA   #
    DataTA = DataTA[DataTA$YEAR == 2007 , ]

    DataTA[1, "SHOOTING_LATITUDE" ] <- 435.11
    # check_position(DataTA, wd, suffix)
}

check_position<-function(DataTA,wd,suffix){


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
  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData=MEDITS::MEDITS.to.dd(ResultData)

  lx = (max(ResultData$SHOOTING_LONGITUDE)+0.1) - (min(ResultData$SHOOTING_LONGITUDE)-0.1)
  ly = (max(ResultData$SHOOTING_LATITUDE)+0.1) - (min(ResultData$SHOOTING_LATITUDE)-0.1)
  ratio <- ly/lx*1.1

  img_width <- 12
  img_height <- img_width * ratio
  oldoptions <- options()$warn
  old_par <- list()
  old_par$mfrow <- par()$mfrow
  old_par$mar <-par()$mar
  old_par$fin <-par()$fin
  old_par$mai <- par()$mai
  old_par$omi <- par()$omi


  ### HAUL POSITIONS ###

  tiff(filename=file.path(wd,"Graphs",paste("hauls_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".tiff",sep="")),width=img_width, height=img_height, bg="white", units="in", res=300, compression = 'lzw', pointsize = 12)
      par(mfrow=c(1,1), mai=c(0.6,0.6,0.6,0.3), omi=c(0.6,0.8,0.8,0.8))
      plot(1,1,type="p",xlim=c(min(ResultData$SHOOTING_LONGITUDE)-0.1, max(ResultData$SHOOTING_LONGITUDE)+0.1), ylim=c(min(ResultData$SHOOTING_LATITUDE)-0.1, max(ResultData$SHOOTING_LATITUDE)+0.1), xlab="Longitude", ylab="Latitude",main=paste("Hauls position - ",ResultData$YEAR[1]))
      maps::map("world", fill=T, col="yellow",add=T)
      points(ResultData$SHOOTING_LONGITUDE,ResultData$SHOOTING_LATITUDE,col="blue",pch=16)
      points(ResultData$HAULING_LONGITUDE,ResultData$HAULING_LATITUDE,col="green",pch=16)
      legend("topleft", paste(c("start position","end position")), pch=c(16,16), col=c("blue","green")  )
  dev.off()

  ### STARTING POSITIONS ###

  tiff(filename=file.path(wd,"Graphs",paste("Start_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".tiff",sep="")),width=img_width, height=img_height, bg="white", units="in", res=300, compression = 'lzw', pointsize = 12)
      par(mfrow=c(1,1), mai=c(0.6,0.6,0.6,0.3), omi=c(0.6,0.8,0.8,0.8))
      plot(1,1,type="p",xlim=c(min(ResultData$SHOOTING_LONGITUDE)-0.1, max(ResultData$SHOOTING_LONGITUDE)+0.1), ylim=c(min(ResultData$SHOOTING_LATITUDE)-0.1, max(ResultData$SHOOTING_LATITUDE)+0.1), xlab="Longitude", ylab="Latitude",main=paste("Hauls start position- ",ResultData$YEAR[1]))
      maps::map("world", fill=T, col="yellow",add=T)
      points(ResultData$SHOOTING_LONGITUDE,ResultData$SHOOTING_LATITUDE,col="blue",pch=16)
      text(ResultData$SHOOTING_LONGITUDE+0.1,ResultData$SHOOTING_LATITUDE,labels=ResultData$HAUL_NUMBER)
  dev.off()

  ### END POSITIONS ###

  tiff(filename=file.path(wd,"Graphs",paste("End_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".tiff",sep="")),width=img_width, height=img_height, bg="white", units="in", res=300, compression = 'lzw', pointsize = 12)
      par(mfrow=c(1,1), mai=c(0.6,0.6,0.6,0.3), omi=c(0.6,0.8,0.8,0.8))
      plot(1,1,type="p",xlim=c(min(ResultData$SHOOTING_LONGITUDE)-0.1, max(ResultData$SHOOTING_LONGITUDE)+0.1), ylim=c(min(ResultData$SHOOTING_LATITUDE)-0.1, max(ResultData$SHOOTING_LATITUDE)+0.1), xlab="Longitude", ylab="Latitude",main=paste("Hauls end position - ",ResultData$YEAR[1]))
      maps::map("world", fill=T, col="yellow",add=T)
      points(ResultData$HAULING_LONGITUDE,ResultData$HAULING_LATITUDE,col="green",pch=16)
      text(ResultData$HAULING_LONGITUDE+0.1,ResultData$HAULING_LATITUDE,labels=ResultData$HAUL_NUMBER)
  dev.off()

  write("Check of hauls position: see the graphs automatically generated in Graphs directory", file = Errors, append = TRUE)

  on.exit(c(par(mfrow=old_par$mfrow,mar=old_par$mar,fin=old_par$fin,mai=old_par$mai,omi=old_par$omi),options(warn=oldoptions)))
  options(warn=-1)

  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)


}
