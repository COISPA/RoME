############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
#  Check consistency of the hauls coordinates with the distance

 if (FALSE){
    #library(MEDITS)
    wd <- tempdir() # "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\temp"
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTA = ta # RoME::TA #read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    year=2018
    #DataTA[1, "SHOOTING_LATITUDE" ] <- 435.11
    check_distance(DataTA,year,wd,suffix)
 }

check_distance<-function(DataTA,year, wd, suffix){
  # oldpar <- par(no.readonly = TRUE)


  lon <- lat <- NULL

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
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

  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'",arg,"' argument should be provided"))
  } else if (length(year)!= 1) {
    stop(paste0("only one value should be provided for '",arg,"' argument"))
  } else if (is.na(year)){
    stop(paste0(arg," argument should be a numeric value"))
  }
  DataTA <- DataTA[DataTA$YEAR == year, ]
  ########################################

  ResultData = DataTA[!is.na(DataTA$DISTANCE),]
  write(paste("\n----------- check consistency of the hauls coordinates with the distance - ", ResultData$YEAR[1]), file = Errors, append = TRUE)


  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData=MEDITS.to.dd(ResultData)

  i=1
  for (i in 1:nrow(ResultData)){
    ResultData$computed_distance[i]= dd.distance(ResultData[i,], unit = "m", verbose=FALSE)
    if (is.nan(ResultData$computed_distance[i])) {
      ResultData$computed_distance[i] = 0
    }
  }

  lx = (max(ResultData$SHOOTING_LONGITUDE)+0.1) - (min(ResultData$SHOOTING_LONGITUDE)-0.1)
  ly = (max(ResultData$SHOOTING_LATITUDE)+0.1) - (min(ResultData$SHOOTING_LATITUDE)-0.1)
  ratio <- ly/lx*1.1

  img_width <- 12
  img_height <- img_width * ratio
  oldoptions <- options()$warn
  old_par <- list(deleteFile=TRUE)
  old_par <- par()



  if (nrow(ResultData)!=0){
    j=1
    for (j in 1:nrow(ResultData)){
      if (  (ResultData$DISTANCE[j]<=ResultData$computed_distance[j]-0.3*ResultData$computed_distance[j]) | (ResultData$DISTANCE[j]>=ResultData$computed_distance[j]+0.3*ResultData$computed_distance[j])){
        write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": the distance in TA ",ResultData$DISTANCE[j],"is quite different from the computed distance",round(ResultData$computed_distance[j],4),"(haul duration:",ResultData$HAUL_DURATION[j],"min)"), file = Errors, append = TRUE)


        points_start <- data.frame(lon=ResultData[j,"SHOOTING_LONGITUDE"],lat=ResultData[j,"SHOOTING_LATITUDE"], Position ="start")
        points_end <- data.frame(lon=ResultData[j,"HAULING_LONGITUDE"],lat=ResultData[j,"HAULING_LATITUDE"], Position ="end")
        points <- rbind(points_start,points_end)
        world <- ne_countries(scale = "medium", returnclass = "sf")
        class(world)

        if (ratio <= 0.5) {
          text_size <- 12
          point_size <- 2
          legend_title_size <- 12
          legend_text_size <-  11
          title_text_size <- 16
        } else if (ratio >= 0.5 & ratio < 2 ) {
          text_size <- 20
          point_size <- 3.5
          legend_title_size <- 18
          legend_text_size <-  18
          title_text_size <- 22
        } else if (ratio >= 2 ) {
          text_size <- 40
          point_size <- 6
          legend_title_size <- 30
          legend_text_size <-  30
          title_text_size <- 40
        }

        theme_opts<-list(theme(text = element_text(size=text_size),
                               plot.title = element_text(size=title_text_size),
                               legend.title = element_text(size = legend_title_size),
                               legend.text = element_text(size = legend_text_size),
                               panel.background = element_rect(fill = 'light blue',linetype="solid",color="black"),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

        suppressMessages (
        ggplot(data=world) +
          geom_sf()+
          coord_sf(crs ="+proj=longlat +ellps=WGS84") +
          geom_point(data=points, aes(lon, lat, fill=Position),shape=21,color="black",size=point_size) +
          coord_sf(xlim = c(min(ResultData$SHOOTING_LONGITUDE)-0.1, max(ResultData$SHOOTING_LONGITUDE)+0.1),
                   ylim = c(min(ResultData$SHOOTING_LATITUDE)-0.1, max(ResultData$SHOOTING_LATITUDE)+0.1),crs="+proj=longlat +ellps=WGS84")+
          ggtitle(paste("Haul",ResultData[j,"HAUL_NUMBER"],"-",ResultData[j,"YEAR"]))  +
          theme_opts +
          scale_fill_manual(values=c("green", "blue"))
        )


        # tiff(file.path(wd,"Graphs",paste("haul ", ResultData[j,"HAUL_NUMBER"], " AREA ",ResultData[1,"AREA"],"_",ResultData[1,"YEAR"],".tiff",sep="")),
        #      width=img_width, height=img_height,
        #      bg="white", units="in", res=300,pointsize = 1/300, compression = "lzw")
        # print(p)
        ggsave(file.path(wd,"Graphs",paste("haul ", ResultData[j,"HAUL_NUMBER"], " AREA ",ResultData[1,"AREA"],"_",ResultData[1,"YEAR"],".jpeg",sep="")),width=img_width, height=img_height,dpi = 300, units="in")
        # dev.off()

      }
    }
  }

  write("Some of the hauls coordinates may be inconsistent with the computed distance. For a visual check, look at the jpeg files in Graphs directory",file = Errors, append = TRUE)
  write(paste("No error occurred"), file = Errors, append = TRUE)

  # on.exit(suppressWarnings(par(oldpar)))
  on.exit(c(par(mfrow=old_par$mfrow,mar=old_par$mar,fin=old_par$fin,mai=old_par$mai,omi=old_par$omi),options(warn=oldoptions)))  # , unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
  options(warn=-1)

#   if (file.exists(file.path(tempdir(), "Logfiles"))){
#   unlink(file.path(tempdir(),"Logfiles"),recursive=T)
#   }
#   if (file.exists(file.path(tempdir(), "Graphs"))){
#   unlink(file.path(tempdir(),"Graphs"),recursive=T)
#   }
#
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
# 	}

  dev.off()
  #unlink(old_par)
  # unlink(file.path(tempdir(),list.files(file.path(tempdir()))),recursive=T)

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
