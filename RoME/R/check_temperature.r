###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

# Check if the temperature by haul is reasonable


if (FALSE){
  ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")
  ResultDataTA=MEDITS::TA # ResultDataTA[ResultDataTA$YEAR==2017,]

  wd <- tempdir() # "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")

  # check_temperature(MEDITS::TA,wd,suffix)
}

check_temperature <- function (ResultDataTA,wd,suffix){
  oldpar <- par()
  Format="from_2012"
  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))


  Dataset = ResultDataTA

  write(paste("\n----------- check temperature - ",Dataset$YEAR[1]), file = Errors, append = TRUE)

if (!all(is.na(Dataset$BOTTOM_TEMPERATURE_BEGINNING))){
start_temp <- cbind(Dataset$HAUL_NUMBER,Dataset$BOTTOM_TEMPERATURE_BEGINNING)
}
if (!all(is.na(Dataset$BOTTOM_TEMPERATURE_END))){
end_temp <- cbind(Dataset$HAUL_NUMBER,Dataset$BOTTOM_TEMPERATURE_END)
}


# if (exists("start_temp")){
# if (!is.na(start_temp)){
if (length(start_temp[,2]) > 0){
indices = which(!is.na(start_temp[,2]) & !is.na(end_temp[,2]))
start_temp  <- start_temp[indices,]
end_temp <- end_temp[indices,]

for (i in 1:nrow(start_temp)){
  # check beginning temperature
  if((start_temp[i,2] > 30) | (start_temp[i,2] < 10)){
    write(paste("Warning: Haul",start_temp[i,1], ": the beginning temperature is out of the range (10,30) in",  Dataset$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
    numberError = numberError +1
  }
}
}

# if (exists("end_temp")) {
# if (!is.na(end_temp)) {
if (length(end_temp) > 0) {
  for (j in 1:nrow(end_temp)){
# check end temperature
  if((end_temp[j,2] > 30) | (end_temp[j,2] < 10)){
    write(paste("Warning: Haul",end_temp[j,1], ": the end temperature is out of the range (10,30) in",  Dataset$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
   numberError = numberError + 1
    }
}
}
#   if (exists("start_temp") & exists("end_temp")){
#   if (!is.na(start_temp) & !is.na(end_temp)){
  if (length(start_temp) > 0 & length(end_temp) > 0 ){
  mean_temp = rowMeans(cbind(start_temp[,2],end_temp[,2]))


  mean_depth = rowMeans(cbind(Dataset$SHOOTING_DEPTH[Dataset$HAUL_NUMBER %in% start_temp[,1]],Dataset$HAULING_DEPTH[Dataset$HAUL_NUMBER %in% start_temp[,1]]))

  tiff(filename=file.path(wd,"Graphs",paste("temperature_control_", Dataset$YEAR[1], "_AREA_",Dataset$AREA[1],".tiff",sep="")),width=12, height=8, bg="white", units="in", res=300, compression = 'lzw', pointsize = 1/300)
  par(mfrow=c(2,1), mai=c(0.3,0.8,0.8,0.3), omi=c(0.8,0.8,1,0.8))
  X=mean_depth
  Y=mean_temp
  #plot(X,Y,xlab="Mean depth (m)",ylab="Mean temperature (?C)",col="blue",pch=16)
  plot(X,Y,xlab="Mean depth (m)",ylab="Mean temperature (Celsius)",col="blue",pch=16,main=paste("Temperature data - ", Dataset$YEAR[1]))
  #mtext("Shooting depth",side=1)
  text(X+0.1,Y,labels=Dataset$HAUL_NUMBER)

  dev.off()

  on.exit(suppressWarnings(par(oldpar)))

  write("Temperature check: see the graphs automatically generated in Graphs directory", file = Errors, append = TRUE)
  }

   if (numberError ==0) {
    write("No error occurred",file = Errors, append = TRUE)
    }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}
