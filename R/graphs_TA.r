############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Qualitative control (by means of 2 graphs) of relation between shooting depth e warp opening and between warp length e wing opening

graphs_TA<-function(DataTA,year,wd,suffix){

  if (FALSE){
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTA = RoME::TA
    year=2012
    graphs_TA(DataTA,year, wd, suffix)
  }

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
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

  oldoptions <- options()$warn
  old_par <- list()
  old_par$mfrow <- par()$mfrow
  old_par$mar <-par()$mar
  old_par$fin <-par()$fin
  old_par$mai <- par()$mai
  old_par$omi <- par()$omi

  on.exit(c(par(mfrow=old_par$mfrow,mar=old_par$mar,fin=old_par$fin,mai=old_par$mai,omi=old_par$omi),options(warn=oldoptions)))
  options(warn=-1)

suppressWarnings(
{
  ResultData = DataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  ResultData=ResultData[ResultData$VALIDITY=="V",]

    # tiff(filename=file.path(wd,"Graphs",paste("qualitative_control_TA_", ResultData$YEAR[1], "_AREA_",ResultData$AREA[1],".tif",sep="")),width=8, height=12, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
	jpeg(filename=file.path(wd,"Graphs",paste("qualitative_control_TA_", ResultData$YEAR[1], "_AREA_",ResultData$AREA[1],".jpeg",sep="")),width=8, height=12, bg="white", units="in", res=200,quality=80)
     par(mfrow=c(2,1), mai=c(0.3,0.8,0.8,0.3), omi=c(0.8,0.8,1,0.8))
       X=ResultData$SHOOTING_DEPTH
       Y=ResultData$WARP_LENGTH
       plot(X,Y,xlab="Shooting depth",ylab="Warp length",col="blue",pch=16,main = paste("Shooting depth versus Warp length- ",ResultData$YEAR[1]))
       mtext(paste("Shooting depth"),side=1)
       text(X+0.1,Y,labels=ResultData$HAUL_NUMBER)
       Z=Y
       H=ResultData$WING_OPENING
       plot(Z,H,xlab="Warp length",ylab="Wing opening",col="green",pch=16,main = paste("Warp length versus Wing opening - ",ResultData$YEAR[1]))
       text(Z+0.1,H,labels=ResultData$HAUL_NUMBER)
       mtext(paste("Warp length"),side=1)
  dev.off()
}
)
  write("Qualitative check TA: see the graphs automatically generated in Graphs directory", file = Errors, append = TRUE)
  # if (file.exists(file.path(tempdir(), "Logfiles"))){
  # unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  # }
  # if (file.exists(file.path(tempdir(), "Graphs"))){
  # unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #   }#unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
}
