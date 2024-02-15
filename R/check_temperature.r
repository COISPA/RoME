############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################


# Check if the temperature by haul is reasonable


if (FALSE){
  wd <- tempdir()
  ResultDataTA= ta #RoME::TA
  year=2015
  suffix= NA

  check_temperature(ResultDataTA,year,wd,suffix)
}

check_temperature <- function (ResultDataTA,year,wd,suffix){

  oldpar <- par(no.readonly = TRUE)
  oldpar$mfrow <- par()$mfrow
  oldpar$mai <- par()$mai
  oldpar$omi <- par()$omi
  on.exit(par(mfrow=oldpar$mfrow, omi=oldpar$omi, mai=oldpar$mai))

  Format="from_2012"
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
  ResultDataTA <- ResultDataTA[ResultDataTA$YEAR == year, ]
  ########################################

  Dataset = ResultDataTA[ResultDataTA$VALIDITY =="V", ]

  write(paste("\n----------- check temperature - ",Dataset$YEAR[1]), file = Errors, append = TRUE)

  # if (any(is.na(Dataset$BOTTOM_TEMPERATURE_BEGINNING))){
  #   na.results <- Dataset[ is.na(Dataset[, "BOTTOM_TEMPERATURE_BEGINNING"]) , ]
  #   l.na <- nrow(na.results)
  #   for (x.na in 1:l.na){
  #     write(paste("Haul",na.results$HAUL_NUMBER[x.na], ": empty value not allowed for 'BOTTOM_TEMPERATURE_BEGINNING' in",  na.results$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
  #   }
  # }
  # if (any(is.na(Dataset$BOTTOM_TEMPERATURE_END))){
  #   na.results <- Dataset[ is.na(Dataset[, "BOTTOM_TEMPERATURE_END"]) , ]
  #   l.na <- nrow(na.results)
  #   for (x.na in 1:l.na){
  #     write(paste("Haul",na.results$HAUL_NUMBER[x.na], ": empty value not allowed for 'BOTTOM_TEMPERATURE_END' in",  na.results$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
  #   }
  # }

  GSA <- unique(ResultDataTA$AREA)[1]

  if (GSA == 29) {
    minTemp <- 0
    maxTemp <- 30
  } else {
    minTemp <- 10
    maxTemp <- 30
  }

  temps <- data.frame(
                      HAUL_NUMBER=Dataset$HAUL_NUMBER,BOTTOM_TEMPERATURE_BEGINNING=as.numeric(Dataset$BOTTOM_TEMPERATURE_BEGINNING),
                      BOTTOM_TEMPERATURE_END=as.numeric(Dataset$BOTTOM_TEMPERATURE_END), MEAN_TEMP = NA,
                      DEPTH=rowMeans(Dataset[ , which(colnames(Dataset) %in% c("SHOOTING_DEPTH","HAULING_DEPTH"))],
                            na.rm=TRUE))
  t=1
  for (t in 1:nrow(temps)) {
    if (!all(is.na(temps[t,c(2:3)]))) {
      temps$MEAN_TEMP[t] <- rowMeans(temps[t,c(2:3)], na.rm = TRUE)
    } else {
      temps$MEAN_TEMP[t] <- NA
    }
  }

  temps <- temps[!is.na(temps$MEAN_TEMP),]

if (!all(is.na(Dataset$BOTTOM_TEMPERATURE_BEGINNING))){
start_temp <- data.frame(HAUL_NUMBER=Dataset$HAUL_NUMBER,BOTTOM_TEMPERATURE_BEGINNING=as.numeric(Dataset$BOTTOM_TEMPERATURE_BEGINNING))
if (all(is.na(Dataset$BOTTOM_TEMPERATURE_END))) {
  end_temp <- start_temp
}
}


if (!all(is.na(Dataset$BOTTOM_TEMPERATURE_END))){
end_temp <- data.frame(HAUL_NUMBER=Dataset$HAUL_NUMBER,BOTTOM_TEMPERATURE_END=as.numeric(Dataset$BOTTOM_TEMPERATURE_END))
if (all(is.na(Dataset$BOTTOM_TEMPERATURE_BEGINNING))) {
  start_temp <- end_temp
}
}



# if (exists("start_temp")){
# if (!is.na(start_temp)){
if (nrow(temps[!is.na(temps$BOTTOM_TEMPERATURE_BEGINNING),]) > minTemp ){
indices_start = which(!is.na(temps[,"BOTTOM_TEMPERATURE_BEGINNING"]))
start_temp  <- temps[indices_start,]


for (i in 1:nrow(start_temp)){
  # check beginning temperature
  if((start_temp[i,2] > maxTemp) | (start_temp[i,2] < minTemp)){
    write(paste("Warning: Haul",start_temp[i,1], ": the beginning temperature is out of the range (",minTemp,",",maxTemp,") in",  Dataset$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
    # numberError = numberError +1
  }
}

}


# if (exists("end_temp")) {
# if (!is.na(end_temp)) {
if (nrow(temps[!is.na(temps$BOTTOM_TEMPERATURE_END),]) > minTemp ){
  indices_end = which(!is.na(temps[,"BOTTOM_TEMPERATURE_END"]))
  end_temp  <- temps[indices_end,]
  for (j in 1:nrow(end_temp)){
# check end temperature
  if((end_temp[j,3] > maxTemp) | (end_temp[j,3] < minTemp)){
    write(paste("Warning: Haul",end_temp[j,1], ": the end temperature is out of the range (",minTemp,",",maxTemp,") in",  Dataset$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
   # numberError = numberError + 1
    }
}
}
#   if (exists("start_temp") & exists("end_temp")){
#   if (!is.na(start_temp) & !is.na(end_temp)){
  if (nrow(temps)>0){

  # tiff(filename=file.path(wd,"Graphs",paste("temperature_control_", Dataset$YEAR[1], "_AREA_",Dataset$AREA[1],".tiff",sep="")),width=12, height=8, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
    jpeg(filename=file.path(wd,"Graphs",paste("temperature_control_", Dataset$YEAR[1], "_AREA_",Dataset$AREA[1],".jpeg",sep="")),width=12, height=8, bg="white", units="in",res=200, quality=80)

    par( mfrow=c(1,1), mai=c(0.8,0.8,0.8,0.3), omi=c(0.8,0.8,0.8,0.8)) #
    X= temps$DEPTH
    Y= temps$MEAN_TEMP
    #plot(X,Y,xlab="Mean depth (m)",ylab="Mean temperature (?C)",col="blue",pch=16)
    plot(X,Y,xlab="Mean depth (m)",ylab="Mean temperature (Celsius)",col="blue",pch=16,main=paste("Temperature data - ", Dataset$YEAR[1]))
    #mtext("Shooting depth",side=1)
    text(X+0.1,Y,labels=Dataset$HAUL_NUMBER)

  dev.off()


  write("Temperature check: see the graphs automatically generated in Graphs directory", file = Errors, append = TRUE)
  }

   if (numberError ==0) {
    write("No error occurred",file = Errors, append = TRUE)
    }
#    if (file.exists(file.path(tempdir(), "Logfiles"))){
#   unlink(file.path(tempdir(),"Logfiles"),recursive=T)
#   }
#   if (file.exists(file.path(tempdir(), "Graphs"))){
#   unlink(file.path(tempdir(),"Graphs"),recursive=T)
#     }
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
#     }
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}
