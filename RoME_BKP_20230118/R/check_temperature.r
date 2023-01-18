############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################


# Check if the temperature by haul is reasonable


if (FALSE){
  wd <- tempdir() #"D:\\COISPA\\_DATI MEDITS_\\GSA18 - 2019\\" # tempdir() # "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
    # ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")
  ResultDataTA= read.table(file="D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\SAMA18\\R_MEFH\\MEDITS data\\GSA18\\TA_GSA18_1994-2020.csv", sep=";", header=T) # MEDITS::TA # ResultDataTA[ResultDataTA$YEAR==2017,]
  ResultDataTA <- ResultDataTA[ResultDataTA$YEAR==2019, ]
  suffix= NA # paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
#
#   ResultDataTA = read.table(file=paste(wd, "\\2019 GSA18 TA.csv",sep=""), sep=";", header=T)
#
#   ResultDataTA$BOTTOM_TEMPERATURE_BEGINNING[1] <- NA

  check_temperature(ResultDataTA,wd,suffix)
}

check_temperature <- function (ResultDataTA,wd,suffix){

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
if (nrow(temps[!is.na(temps$BOTTOM_TEMPERATURE_BEGINNING),]) > 0 ){
indices_start = which(!is.na(temps[,"BOTTOM_TEMPERATURE_BEGINNING"]))
start_temp  <- temps[indices_start,]


for (i in 1:nrow(start_temp)){
  # check beginning temperature
  if((start_temp[i,2] > 30) | (start_temp[i,2] < 10)){
    write(paste("Warning: Haul",start_temp[i,1], ": the beginning temperature is out of the range (10,30) in",  Dataset$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
    # numberError = numberError +1
  }
}

}


# if (exists("end_temp")) {
# if (!is.na(end_temp)) {
if (nrow(temps[!is.na(temps$BOTTOM_TEMPERATURE_END),]) > 0 ){
  indices_end = which(!is.na(temps[,"BOTTOM_TEMPERATURE_END"]))
  end_temp  <- temps[indices_end,]
  for (j in 1:nrow(end_temp)){
# check end temperature
  if((end_temp[j,3] > 30) | (end_temp[j,3] < 10)){
    write(paste("Warning: Haul",end_temp[j,1], ": the end temperature is out of the range (10,30) in",  Dataset$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
   # numberError = numberError + 1
    }
}
}
#   if (exists("start_temp") & exists("end_temp")){
#   if (!is.na(start_temp) & !is.na(end_temp)){
  if (nrow(temps)>0){

  tiff(filename=file.path(wd,"Graphs",paste("temperature_control_", Dataset$YEAR[1], "_AREA_",Dataset$AREA[1],".tiff",sep="")),width=12, height=8, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
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
