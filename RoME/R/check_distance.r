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
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTA = MEDITS::TA #read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    #DataTA[1, "SHOOTING_LATITUDE" ] <- 435.11
    check_distance(DataTA,wd,suffix)
 }

check_distance<-function(DataTA, wd, suffix){
  # oldpar <- par(no.readonly = TRUE)

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }

  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  ResultData = DataTA
  write(paste("\n----------- check consistency of the hauls coordinates with the distance - ", ResultData$YEAR[1]), file = Errors, append = TRUE)


  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData=MEDITS::MEDITS.to.dd(ResultData)

  for (i in 1:nrow(ResultData)){
    ResultData$computed_distance[i]= MEDITS::dd.distance(ResultData[i,], unit = "m", verbose=FALSE)
  }

  lx = (max(ResultData$SHOOTING_LONGITUDE)+0.1) - (min(ResultData$SHOOTING_LONGITUDE)-0.1)
  ly = (max(ResultData$SHOOTING_LATITUDE)+0.1) - (min(ResultData$SHOOTING_LATITUDE)-0.1)
  ratio <- ly/lx*1.1

  img_width <- 12
  img_height <- img_width * ratio
  oldoptions <- options()$warn
  old_par <- list(deleteFile=TRUE)
  old_par <- par()
  # old_par$mfrow <- par()$mfrow
  #
  # old_par$mar <-par()$mar
  #
  # old_par$fin <-par()$fin
  #
  # old_par$mai <- par()$mai
  #
  # old_par$omi <- par()$omi


  if (nrow(ResultData)!=0){
    for (j in 1:nrow(ResultData)){
      if (  (ResultData$DISTANCE[j]<=ResultData$computed_distance[j]-0.3*ResultData$computed_distance[j]) | (ResultData$DISTANCE[j]>=ResultData$computed_distance[j]+0.3*ResultData$computed_distance[j])){
        write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": the distance in TA ",ResultData$DISTANCE[j],"is quite different from the computed distance",round(ResultData$computed_distance[j],4),"(haul duration:",ResultData$HAUL_DURATION[j],"min)"), file = Errors, append = TRUE)
        tiff(file.path(wd,"Graphs",paste("haul ", ResultData[j,"HAUL_NUMBER"], " AREA ",ResultData[1,"AREA"],"_",ResultData[1,"YEAR"],".tiff",sep="")),width=img_width, height=img_height, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
        par(mfrow=c(1,1), mai=c(0.6,0.6,0.6,0.3), omi=c(0.6,0.8,0.8,0.8))
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

  # on.exit(suppressWarnings(par(oldpar)))
  on.exit(c(par(mfrow=old_par$mfrow,mar=old_par$mar,fin=old_par$fin,mai=old_par$mai,omi=old_par$omi),options(warn=oldoptions),
            unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)))
  options(warn=-1)

  if (file.exists(file.path(tempdir(), "Logfiles"))){
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
  }

	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
	}

  dev.off()
  #unlink(old_par)
  unlink(file.path(tempdir(),list.files(file.path(tempdir()))),recursive=T)

  #  if (file.exists(file.path(tempdir()))){
  #    wd <- getwd()
  #    dirl <- list.dirs(path = tempdir(), full.names = TRUE, recursive = TRUE)
  #    i=1
  #   for (i in 1:length(dirl)){
  #   setwd(dirl[i])
  #
  #   unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
  #   }
  #    if (file.exists(file.path(tempdir()))){
  #      dirl <- list.dirs(path = tempdir(), full.names = TRUE, recursive = TRUE)
  #      i=1
  #      for (i in 1:length(dirl)){
  #        setwd(dirl[i])
  #        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
  #      }
  #
  # setwd (wd)
  # }
  # }

  return(TRUE)

}
################################################################################
