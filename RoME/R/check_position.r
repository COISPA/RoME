############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Visual check of the haul positions

if (FALSE){
  library(RoME)
  wd <- tempdir()
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  DataTA = RoME::TA
  year=2007
  # DataTA[1, "SHOOTING_LATITUDE" ] <- 435.11
  # check_position(DataTA,year, wd, suffix)
}

check_position<-function(DataTA,year,wd,suffix){

  lon <- lat <- NULL

  if (!file.exists(file.path(wd,"Logfiles"))){
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

  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'", arg, "' argument should be provided"))
  } else if (length(year) != 1) {
    stop(paste0("only one value should be provided for '", arg, "' argument"))
  } else if (is.na(year)) {
    stop(paste0(arg, " argument should be a numeric value"))
  }
  DataTA <- DataTA[DataTA$YEAR == year, ]
  ########################################

  ResultData = DataTA
  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData=MEDITS.to.dd(ResultData)

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
  on.exit(c(par(mfrow=old_par$mfrow,mar=old_par$mar,fin=old_par$fin,mai=old_par$mai,omi=old_par$omi),options(warn=oldoptions)))

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
  ### HAUL POSITIONS ###

  points_start <- data.frame(lon=ResultData$SHOOTING_LONGITUDE,lat=ResultData$SHOOTING_LATITUDE, Position ="start")
  points_end <- data.frame(lon=ResultData$HAULING_LONGITUDE,lat=ResultData$HAULING_LATITUDE, Position ="end")
  points <- rbind(points_start,points_end)
  world <- ne_countries(scale = "medium", returnclass = "sf")

  suppressMessages (
    ggplot(data=world) +
      geom_sf()+
      coord_sf(crs ="+proj=longlat +ellps=WGS84") +
      geom_point(data=points, aes(lon, lat, fill=Position),shape=21,color="black",size=point_size) +
      coord_sf(xlim = c(min(ResultData$SHOOTING_LONGITUDE)-0.1, max(ResultData$SHOOTING_LONGITUDE)+0.1),
               ylim = c(min(ResultData$SHOOTING_LATITUDE)-0.1, max(ResultData$SHOOTING_LATITUDE)+0.1),crs="+proj=longlat +ellps=WGS84")+
      ggtitle(paste("Hauls position - ",ResultData$YEAR[1]))  +
      theme_opts +
      scale_fill_manual(values=c("green", "blue"))
  )

  ggsave(file.path(wd,"Graphs",paste("hauls_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".jpeg",sep="")),width=img_width, height=img_height,dpi = 300, units="in")

  # tiff(filename=file.path(wd,"Graphs",paste("hauls_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".tiff",sep="")),width=img_width, height=img_height, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
  # print(p)
  # dev.off()

  ### STARTING POSITIONS ###

  points_start <- data.frame(lon=ResultData$SHOOTING_LONGITUDE,lat=ResultData$SHOOTING_LATITUDE, Position ="start")
  points <- rbind(points_start)
  world <- ne_countries(scale = "medium", returnclass = "sf")

  suppressMessages (
    ggplot(data=world) +
      geom_sf()+
      coord_sf(crs ="+proj=longlat +ellps=WGS84") +
      geom_point(data=points, aes(lon, lat, fill=Position),shape=21,color="black",size=point_size) +
      coord_sf(xlim = c(min(ResultData$SHOOTING_LONGITUDE)-0.1, max(ResultData$SHOOTING_LONGITUDE)+0.1),
               ylim = c(min(ResultData$SHOOTING_LATITUDE)-0.1, max(ResultData$SHOOTING_LATITUDE)+0.1),crs="+proj=longlat +ellps=WGS84")+
      ggtitle(paste("Hauls position - ",ResultData$YEAR[1]))  +
      theme_opts +
      scale_fill_manual(values=c("blue"))
  )


  # tiff(filename=file.path(wd,"Graphs",paste("Start_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".tiff",sep="")),width=img_width, height=img_height, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
  # print(p)
  # dev.off()

  ggsave(file.path(wd,"Graphs",paste("Start_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".jpeg",sep="")),width=img_width, height=img_height,dpi = 300, units="in")

  ### END POSITIONS ###

  points_end <- data.frame(lon=ResultData$HAULING_LONGITUDE,lat=ResultData$HAULING_LATITUDE, Position ="end")
  points <- rbind(points_end)
  world <- ne_countries(scale = "medium", returnclass = "sf")

  suppressMessages (
    ggplot(data=world) +
      geom_sf()+
      coord_sf(crs ="+proj=longlat +ellps=WGS84") +
      geom_point(data=points, aes(lon, lat, fill=Position),shape=21,color="black",size=point_size) +
      coord_sf(xlim = c(min(ResultData$SHOOTING_LONGITUDE)-0.1, max(ResultData$SHOOTING_LONGITUDE)+0.1),
               ylim = c(min(ResultData$SHOOTING_LATITUDE)-0.1, max(ResultData$SHOOTING_LATITUDE)+0.1),crs="+proj=longlat +ellps=WGS84")+
      ggtitle(paste("Hauls position - ",ResultData$YEAR[1]))  +
      theme_opts +
      scale_fill_manual(values=c("green"))
  )


  # tiff(filename=file.path(wd,"Graphs",paste("End_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".tiff",sep="")),width=img_width, height=img_height, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
  # print(p)
  # dev.off()
  ggsave(file.path(wd,"Graphs",paste("End_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".jpeg",sep="")),width=img_width, height=img_height,dpi = 300, units="in")
  write("Check of hauls position: see the graphs automatically generated in Graphs directory", file = Errors, append = TRUE)

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

}
